# Load packages and functions ---------------------------------------------

require(ggplot2)
require(plotly)
require(data.table)
require(rstudioapi)
#require(xlsx)
#library(readxl)
library(openxlsx)
#library(gglaplot)
library(tidyverse)


# The following should be true when testing this file. When knitting the report together and sourcing externally, set this to false
if (!exists("run_direct")){run_direct=T}

if (run_direct == F){dir_stub = ''
} else if (run_direct == T){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  dir_stub = '../'
  source(paste(c(dir_stub, 'colour_theme.r'),collapse = ''))
} 


# Some graph parameters
adj = 0.01
label_size = 6
graph_height = 3.5 # inches
graph_aspect_ratio = 0.75 # ratio between height/width

l_dat <- function(path, start_row = 2, percentage_cut_off = 0.5){
  df <- read.xlsx(path, startRow = start_row) %>% data.table()

  # Remove rows with no data
  df <- df[!is.na(df[[1]]),]
  df <- df[!is.na(df[[2]]),]

  # Remove rows where column 1 contains Total
  df <- df[!grepl("Total", df[[1]]),]

  # Remove everything before : in column names
  names(df) = gsub(".*:", "", names(df))

  #print(names(df))

  # Replace . in column names with " "
  names(df) = gsub("\\.", " ", names(df))

  #print(names(df))

  # Replace column name X3 with Lambeth %
  names(df)[3] = "Lambeth %"
  names(df)[5] = "England %"
  names(df)[7] = "London %"

  # Melt table with first column as id
  df_m <- melt(df, id.vars = names(df)[1], variable.name = "Area name", value.name = "Value")

  # Create percentage column from if 'Area name' contains %
  df_m[, `Percentage` := ifelse(grepl("%", `Area name`), TRUE, FALSE)]

  # Replace ' %' in Area name with ''
  df_m[, `Area name` := gsub(" %", "", `Area name`)]

  # Make Value a numeric column
  df_m[, `Value` := as.numeric(`Value`)]

  # Make Area name an ordered factor with Lambeth, London, England
  df_m[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England"))]

  print(df_m)

  cat_col <- names(df)[1]

  dc_formula <- paste0("`",cat_col,"` + ","`Area name`", " ~ ", "Percentage")

  # dcast table with first column and Area name on left, percentage on right, Value as value column
  df_d <- dcast(df_m, dc_formula, value.var = "Value", fun.aggregate = mean)

  # Rename column FALSE to Population
  names(df_d)[3] = "Population"

  # Rename column TRUE to Percentage
  names(df_d)[4] = "Percentage"

  # Create new df for Area name = Lambeth
  df_lambeth <- df_d[df_d$`Area name` == "Lambeth",]

  # Keep only categories of at least X% of the population (default 0.5%)
  df_lambeth <- df_lambeth[df_lambeth$`Percentage` >= percentage_cut_off,]

  # Order df_lambeth descending by Percentage
  df_lambeth <- df_lambeth[order(-df_lambeth$`Percentage`),]

  # Create variable from first column

  cat_order <- as.data.frame(df_lambeth)[,1]

  #col <- names(df_lambeth)[1]
  #cat_order <- df_lambeth[,col, with=F]

  #print(cat_order)

  # In df_d set column 1 as ordered factor with levels cat_order
  df_d[, (1) := lapply(.SD, ordered, levels=cat_order), .SDcols = 1]

  # filter df_d by column 1 for NAs
  df_d <- df_d[!is.na(df_d[[1]]),]

  # order df_d by column 1 and `Area name`
  df_d <- df_d[order(`Area name`, -Percentage),]

  out_stuff <- list(df_d, cat_order)


  return(out_stuff)

}

path_2022 = paste(c(dir_stub, "../data/Res survey/res_survey_2022_filt.xlsx"),collapse = '')

create_surv_q_df = function(sheet_name, path = path_2022, start_text = "test col x vs y and z", end_text_select = 1){
  # Read in the Excel file and extract the data from the sheet specified by sheet_name
  surv_sat <- read.xlsx(path, startRow = 2, sheet=sheet_name) %>% data.table()
  
  # Select the first two columns
  surv_sat <- surv_sat[,1:2]

  # Take the first column name and store as a variable
  question <- names(surv_sat)[1]

  # Rename first column to 'Response'
  names(surv_sat)[1] = "Response"

  # Filter column 1 to keep rows in between 'x?yz: test col x vs y and z' and 'Don't know'
  surv_sat_f <- surv_sat[(grep(start_text, Response, fixed=T)+1):(grep("Don’t know|Prefer not to say", Response, fixed = F)[end_text_select]-1)]

  # Filter column 1 to remove NA
  surv_sat_f <- surv_sat_f[!is.na(Response)]

  # Rename X2 to 'Percentage of respondents'
  names(surv_sat_f)[2] = "Percentage of respondents"

   # Replace numbers in square brackets in the Response column with the corresponding word
  surv_sat_f[,Response := gsub("(\\[\\d\\] )", "", Response)]

  # Multiply Percentage of respondents by 100 to get percentage
  surv_sat_f[, `Percentage of respondents` := round(as.numeric(`Percentage of respondents`) * 100, 1)]

  # Make column 'Question' with values from question
  surv_sat_f[, Question := question]

  # Remove 'Q:' from Question
  surv_sat_f[, Question := gsub("Q:", "", Question)]
  
  # Remove Q from Question
  surv_sat_f[, Question := gsub("(^Q[A-Za-z0-9]+)", "", Question, fixed=F)]
  
  # Remove Q from Question
  surv_sat_f[, Question := gsub("(^_[A-Za-z0-9]+)", "", Question, fixed=F)]

  # Replace . in Question with space
  surv_sat_f[, Question := gsub("\\.", " ", Question)]
  
  # Replace . in Question with space
  surv_sat_f[, Question := stringr::str_trim(Question)]

  return(surv_sat_f)
}

make_surv_graph_facets <- function(df, ncols = 3, nrows = 1, title_wrap = 50){

graph <- ggplot(df, aes(y = `Percentage of respondents`, x =  Response, fill = Question,
text= paste("Question: ", Question, "<br>",
  "Response: ", Response, "<br>",
"Percentage of respondents: ",prettyNum(`Percentage of respondents`,big.mark=",", preserve.width="none")
))
) +
geom_col(position = position_dodge2(width = 0.9))+#, fill = "#ff9900")+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank(),
axis.title.y = element_blank(),
axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
legend.position = "none"
) +
scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +
#scale_y_continuous(labels = scales::comma) +
ylab("Percentage of respondents") +
facet_wrap(~Question, scales = "free_x", labeller = labeller(Question = label_wrap_gen(title_wrap)), nrow = nrows, ncol = ncols)
#facet_grid(~Question, scales = "free_x", labeller = labeller(Question = label_wrap_gen(40)), )

return(graph)
}
make_surv_graph_time_facets <- function(df, ncols = 3, nrows = 1, title_wrap = 50){
  
  df[, Year:=ordered(Year, levels=c(2020, 2021, 2022))]
  
  graph <- ggplot(df, aes(y = `Percentage of respondents`, x =  Year, 
                          colour = `Response group`, group = `Response group`,
                          linetype = `Response group`,
                          text= paste("Question: ", Question, "<br>",
                                      "Response: ", `Response group`, "<br>",
                                      "Percentage of respondents: ",prettyNum(`Percentage of respondents`,big.mark=",", preserve.width="none")
                          ))
  ) +
    geom_point()+#, fill = "#ff9900")+#, reverse = T)) +
    geom_line() +
    scale_colour_discrete(type=lambeth_palette_graph) +
    theme_lam() +
    theme(#axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      #axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)#,
      #legend.position = "none"
    ) +
    #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +
    #scale_y_continuous(labels = scales::comma) +
    ylab("Percentage of respondents") +
    facet_wrap(~Question, scales = "free_x", labeller = labeller(Question = label_wrap_gen(title_wrap)),
               nrow = nrows, ncol = ncols)
  #facet_grid(~Question, scales = "free_x", labeller = labeller(Question = label_wrap_gen(40)), )
  
  return(graph)
}

# Population --------------------------------------------------------------
pop <- read.xlsx(paste(c(dir_stub, "../data/Jobs/NOMIS/Total population simple 2020.xlsx"),collapse = ''),
                  startRow = 2) %>% data.table()
pop

pop_df= pop[2:4]
pop_df

pop_df_m = melt(pop_df, id.vars = "X1") %>% as.data.table()
names(pop_df_m) = c("Sex", "Area", "Population")

pop_tot <- pop_df_m[Sex=="All People"][, Sex:= NULL]

# Make Population numeric
pop_tot[, Population := as.numeric(Population)]

# Population 18+ --------------------------------------------------------------

pop_18 <- l_dat(paste(c(dir_stub, "../data/Community/Electoral/Single year age TS007 18 plus census 2021.xlsx"),collapse = ''),
                start_row = 7, percentage_cut_off = 0)
pop_18

pop_18_df <- pop_18[[1]]

# Create all_18_plus_df from pop_18_df, sum Population by Area name
all_18_plus_df <- pop_18_df[, list(Population = sum(Population)), by = "Area name"]

# Rename Population to All 18+
names(all_18_plus_df)[2] = "All 18+"

# Electoral registration ----------------------------------------------------

# Read in electoral registration data
er_loc <- read.xlsx(paste(c(dir_stub, "../data/Community/Electoral/localauthorityandparliamentaryregistrationsandattainersv4.xlsx"),collapse = ''),
                startRow = 5, sheet="Table 1") %>% data.table()

# Replace . in column names with " "
names(er_loc) = gsub("\\.", " ", names(er_loc))

names(er_loc)

# Filter Local Government areas to Lambeth, London, England
er_loc_f <- er_loc[grepl("Lambeth|^London$|ENGLAND", `Local Government Areas`),]

# Sentence case Local Government area column
er_loc_f[, `Local Government Areas` := stringr::str_to_sentence(`Local Government Areas`)]


# Rename Local Government area to Area name
names(er_loc_f)[2] = "Area name"

# Merge with all_18_plus_df
er_loc_m <- merge(er_loc_f, all_18_plus_df, by = "Area name", all.x = TRUE)

names(er_loc_m)

# Create Percentage column
er_loc_m[, `Percentage of eligible adults registered to vote in local elections (December 2021)` := (`Electoral registrations December 2021` / `All 18+`) * 100]

names(er_loc_m)

# Make Area name and ordered factor with levels Lambeth, London, England
er_loc_m[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England"))]

# Remove columns starting with 'Attainers'
er_loc_m <- er_loc_m[, !grepl("^Attainers", names(er_loc_m)), with=F]

names(er_loc_m)

# Make a graph of Percentage of eligible adults registered to vote in local elections (December 2021) by Area name in ggplot2

er_loc_m_graph <- ggplot(er_loc_m, aes(y = `Percentage of eligible adults registered to vote in local elections (December 2021)`, x =  `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Percentage of eligible adults registered to vote in local elections (December 2021): ",prettyNum(`Percentage of eligible adults registered to vote in local elections (December 2021)`,big.mark=",", preserve.width="none"), "<br>",
"All 18+: ",prettyNum(`All 18+`,big.mark=",", preserve.width="none"), "<br>"
))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
ylab("Percentage of eligible adults registered to vote\nin local elections (December 2021)")

er_loc_m_graph

# National electoral registration ----------------------------------------------------


# Read in electoral registration data
er_nat <- read.xlsx(paste(c(dir_stub, "../data/Community/Electoral/localauthorityandparliamentaryregistrationsandattainersv4.xlsx"),collapse = ''),
                startRow = 5, sheet="Table 2") %>% data.table()

# Replace . in column names with " "
names(er_nat) = gsub("\\.", " ", names(er_nat))

names(er_nat)

# Filter Parliamentary constituency to Lambeth, London, England
er_nat_f <- er_nat[grepl("Lambeth|^London$|ENGLAND", `Parliamentary constituency`),]

# ONLY ENGLAND FOUND AS THIS IS AT PARLIAMENTARY CONSTITUENCY LEVEL. NOT USEFUL

# Local election turnout ----------------------------------------------------

# Load data
let <- read.xlsx(paste(c(dir_stub, "../data/Community/Election turnout/2022 England Electoral data_0.xlsx"),collapse = ''), startRow = 2, sheet="London") %>% data.table()

# Change . in column names to " "
names(let) = gsub("\\.", " ", names(let))

# Filter let to Lambeth, Total London
let_f <- let[grepl("Lambeth|^Total London$", `X1`),]

# Rename X1 to Area name
names(let_f)[1] = "Area name"

# Replace "Total London" with "London"
let_f[, `Area name` := gsub("Total London", "London", `Area name`)]

# Replace 'London Borough of ' with ''
let_f[, `Area name` := gsub("London Borough of ", "", `Area name`)]

# Keep first four columns
let_f <- let_f[, c(1:4)]






let_eng <- read.xlsx(paste(c(dir_stub, "../data/Community/Election turnout/2022 England Electoral data_0.xlsx"),collapse = ''), startRow = 2, sheet="Total") %>% data.table()

# Change . in column names to " "
names(let_eng) = gsub("\\.", " ", names(let_eng))


# Filter let_eng to Lambeth, Total London
let_eng_f <- let_eng[grepl("Total", `X1`),]


# Rename X1 to Area name
names(let_eng_f)[1] = "Area name"

# Replace "Total" with "England"
let_eng_f[, `Area name` := gsub("Total", "England", `Area name`)]

# Keep first row
let_eng_f <- let_eng_f[1,]

# Keep first four columns
let_eng_f <- let_eng_f[, c(1:4)]


# Combine let_f and let_eng_f
let_f_g <- rbind(let_f, let_eng_f)

# Make Area name and ordered factor with levels Lambeth, London, England
let_f_g[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England"))]

# Multiply Ballot box turnout By 100 to get percentage
let_f_g[, `Ballot box turnout` := `Ballot box turnout` * 100]

# Rename to Ballot box turnout (%)
names(let_f_g)[4] = "Ballot box turnout (%)"


# Make a graph of Ballot box turnout by Area name in ggplot2

let_f_g_graph <- ggplot(let_f_g, aes(y = `Ballot box turnout (%)`, x =  `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Ballot box turnout (%): ",prettyNum(`Ballot box turnout (%)`,big.mark=",", preserve.width="none"), "<br>",
"Ballot papers at the count: ",prettyNum(`Ballot papers at the count`,big.mark=",", preserve.width="none"), "<br>"
))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
ylab("Ballot box turnout in the 2022 elections (%)")

let_f_g_graph

# Internet users ----------------------------------------------------

# Load data
internet <- read.xlsx(paste(c(dir_stub, "../data/Community/Internet users/internetusers2020.xlsx"),collapse = ''), startRow = 4, sheet="6b") %>% data.table()

# Keep first 9 columns
internet <- internet[, c(1:9)]

# Keep X2 and 2020 column
internet <- internet[, c(2, 9)]

#Rename 2020 Used the Internet in the last 3 months (2020, %)

names(internet)[2] = "Used the Internet in the last 3 months (2020, %)"

internet[,`Used the Internet in the last 3 months (2020, %)` := as.numeric(`Used the Internet in the last 3 months (2020, %)`)]

# Rename X2 to Area name
names(internet)[1] = "Area name"

# Filter Area name to UK, London, Lambeth
internet_f <- internet[grepl("UK|^London$|^Lambeth$", `Area name`),]

# Area name as ordered factor with levels Lambeth, London, UK
internet_f[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "UK"))]

internet_f

# Make a graph of Used the Internet in the last 3 months (2020, %) by Area name in ggplot2

internet_f_graph <- ggplot(internet_f, aes(y = `Used the Internet in the last 3 months (2020, %)`, x =  `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Used the Internet in the last 3 months (2020, %): ",prettyNum(`Used the Internet in the last 3 months (2020, %)`,big.mark=",", preserve.width="none"), "<br>"
))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
ylab("Used the Internet in the last 3 months (2020, %)")

# Residents survey results ----------------------------------------------------

# Load data
q01 <- create_surv_q_df("Q01_1")
q02 <- create_surv_q_df("Q02_1")
q03 <- create_surv_q_df("Q03_1")
q04 <- create_surv_q_df("Q04_1")
q05 <- create_surv_q_df("Q05_1")

q06 <- create_surv_q_df("Q06_1")[1:10] #%>% filter(`Percentage of respondents` >= 5)
q07 <- create_surv_q_df("Q07_1")[1:10] #%>% filter(`Percentage of respondents` >= 5)

# The following is the avoid having bars turn up in the wrong order in the second graph of graph page two, as there are duplicated response titles that should be placed differently
# Add a space to each row of the Response column

q07$Response = paste0(q07$Response, " ")

#q07[, Response := gsub("Clean streets", "Clean streets ", Response)]
#q07[, Response := gsub("The level of crime", "The level of crime ", Response)]

# Join all dataframes
surv_sat_f_general <- rbind(q01, q02, q03, q04, q05, q06, q07)



# Create a variable of the Response column with duplicates removed
Response_unique <- unique(surv_sat_f_general$Response)

# Make response an ordered factor with levels in the order of the Response column
surv_sat_f_general[, Response := ordered(Response, levels = Response_unique)]

# Create graph of Percentage of respondents by Response in ggplot2

surv_sat_f_general_graph <- make_surv_graph_facets(surv_sat_f_general[Question %in% unique(surv_sat_f_general$Question)[1:5]], ncols=5, title_wrap = 35)
#surv_sat_f_general_graph

surv_sat_f_general_graph_2 <- make_surv_graph_facets(surv_sat_f_general[Question %in% unique(surv_sat_f_general$Question)[6:7]], ncols=2)
#surv_sat_f_general_graph_2

# Neighbourhood specific resident survey results ----------------------------------------------------

q08 <- create_surv_q_df("Q08")
q091 <- create_surv_q_df("Q09_1")
q092 <- create_surv_q_df("Q09_2")
q093 <- create_surv_q_df("Q09_3")
q094 <- create_surv_q_df("Q09_4")

# Join all dataframes
surv_sat_f_local <- rbind(q08, q091, q092, q093, q094)

surv_sat_f_local$Question = gsub("Please say how strongly you agree or disagree with each of the following statements - ", "", surv_sat_f_local$Question)

# Create a variable of the Response column with duplicates removed
Response_unique_loc <- unique(surv_sat_f_local$Response)

# Make response an ordered factor with levels in the order of the Response column
surv_sat_f_local[, Response := ordered(Response, levels = Response_unique_loc)]

surv_sat_f_graph_local <- make_surv_graph_facets(surv_sat_f_local, ncols=5, title_wrap = 35)

surv_sat_f_graph_local

# Crime related survey results ----------------------------------------------------
# Crime related qs 	77	Q015_1
	#78	Q015_2
	#79	Q1Y22
	#80	Q2Y22

# Safe walking during the day/evening
q015_1 = create_surv_q_df("Q015_1")
q015_2 = create_surv_q_df("Q015_2")


# Confidence in police
q1y22 = create_surv_q_df("Q1Y22")

# Subject to harassment
q2y22 = create_surv_q_df("Q2Y22")

# Join all dataframes
surv_sat_f_crime <- rbind(q015_1, q015_2, q1y22, q2y22)

# Create a variable of the Response column with duplicates removed
Response_unique_loc <- unique(surv_sat_f_crime$Response)

# Make response an ordered factor with levels in the order of the Response column
surv_sat_f_crime[, Response := ordered(Response, levels = Response_unique_loc)]

surv_sat_f_crime_graph <- make_surv_graph_facets(surv_sat_f_crime, ncols=4, title_wrap = 40)
surv_sat_f_crime_graph

## Time series graph
# Load in 2021, 2020 data
path_2021 = paste(c(dir_stub, "../data/Res survey/res_survey_2021_filt.xlsx"),collapse = '')

# Safety in day/night
q015_1_21 = create_surv_q_df("Q015_1", path_2021)
q015_2_21 = create_surv_q_df("Q015_2", path_2021)


#
path_2020 = paste(c(dir_stub, "../data/Res survey/res_survey_2020_filt.xlsx"),collapse = '')

# Safety in day/night
q015_1_20 = create_surv_q_df("Q013", path_2020)[, Question := "Q015_1  To what extent would you say you are, or would be, safe from crime when walking in your local area   ? - During the day -"]
q015_2_20 = create_surv_q_df("Q013", path_2020, start_text = "Q013.A.2. In the evening -", end_text_select = 2)[, Question := "Q015_2  To what extent would you say you are, or would be, safe from crime when walking in your local area   ? - In the evening -"]

surv_sat_f_crime_22 <- rbind(q015_1, q015_2) %>% cbind(2022) %>% setnames("V2", "Year")
surv_sat_f_crime_21 <- rbind(q015_1_21, q015_2_21) %>% cbind(2021) %>% setnames("V2", "Year")
surv_sat_f_crime_20 <- rbind(q015_1_20, q015_2_20) %>% cbind(2020) %>% setnames("V2", "Year")

names(surv_sat_f_crime_22)
names(surv_sat_f_crime_21)

surv_sat_f_crime_all <- rbind(surv_sat_f_crime_22, surv_sat_f_crime_21, surv_sat_f_crime_20)

surv_sat_f_crime_all$Response = gsub("\\[.*\\]", "", surv_sat_f_crime_all$Response)
surv_sat_f_crime_all$Response = gsub(" -.*", "", surv_sat_f_crime_all$Response) %>% trimws()
surv_sat_f_crime_all$Question = gsub("^Q([^ ]+)  ", "", surv_sat_f_crime_all$Question) %>% trimws()
surv_sat_f_crime_all$Question = gsub("\\s\\s+", "", surv_sat_f_crime_all$Question) %>% trimws()

surv_sat_f_crime_all = surv_sat_f_crime_all[!is.na(`Percentage of respondents`)]


# Group together the 0123, 456, 78910 responses as generally high/med/low
surv_sat_f_crime_all[, `Response group` := Response]

Response_unique_loc_all <- unique(surv_sat_f_crime_all$`Response group`)
surv_sat_f_crime_all[, `Response group` := ordered(`Response group`, levels = Response_unique_loc_all)]

surv_sat_f_crime_all_grp <- surv_sat_f_crime_all[,.(`Percentage of respondents`=sum(`Percentage of respondents`)), by = c("Year","Question","Response group")]


# Graph for past three years

surv_sat_f_crime_time_graph <- make_surv_graph_time_facets(surv_sat_f_crime_all_grp, ncols=2, nrows=1, title_wrap = 40)
surv_sat_f_crime_time_graph

#ggplotly(surv_sat_f_crime_time_graph)



# Health/Wellbeing survey results ----------------------------------------------------
# Health/Wellbeing Qs  108	Q029
#109	Q030
#110	Q031

q029 = create_surv_q_df("Q029")[,Question:= "Overall, to what extent do you feel the things you do in your life are worthwhile?"]
q030 = create_surv_q_df("Q030")[,Question:= "How anxious did you feel yesterday?"]
q031 = create_surv_q_df("Q031")[,Question:= "How is your health in general?"]


# The following is the avoid having bars turn up in the wrong order in the second graph of graph page two, as there are duplicated response titles that should be placed differently
# Add a space to each row of the Response column
q030$Response = paste0(q030$Response, " ")


# Join all dataframes
surv_sat_f_health <- rbind(q029, q030, q031) %>% cbind(2022) %>% setnames("V2", "Year")

# Remove anything between square brackets in Responses
surv_sat_f_health$Response = gsub("\\[.*\\]", "", surv_sat_f_health$Response)

# Create a variable of the Response column with duplicates removed
Response_unique_loc <- unique(surv_sat_f_health$Response)

# Make response an ordered factor with levels in the order of the Response column
surv_sat_f_health[, Response := ordered(Response, levels = Response_unique_loc)]

# Make 2022 graph
surv_sat_f_health_graph <- make_surv_graph_facets(surv_sat_f_health, ncols=3, title_wrap = 40)
surv_sat_f_health_graph

## Time series graph
# Load in 2021, 2020 data
path_2021 = paste(c(dir_stub, "../data/Res survey/res_survey_2021_filt.xlsx"),collapse = '')

q029_21 = create_surv_q_df("Q029", path_2021)[,Question:= "Overall, to what extent do you feel the things you do in your life are worthwhile?"]
q030_21 = create_surv_q_df("Q030", path_2021)[,Question:= "How anxious did you feel yesterday?"]
q031_21 = create_surv_q_df("Q031", path_2021)[,Question:= "How is your health in general?"]

path_2020 = paste(c(dir_stub, "../data/Res survey/res_survey_2020_filt.xlsx"),collapse = '')

q029_20 = create_surv_q_df("Q027", path_2020)[,Question:= "Overall, to what extent do you feel the things you do in your life are worthwhile?"]
q030_20 = create_surv_q_df("Q029", path_2020)[,Question:= "How anxious did you feel yesterday?"]
q031_20 = create_surv_q_df("Q030", path_2020)[,Question:= "How is your health in general?"]


surv_sat_f_health_21 <- rbind(q029_21, q030_21, q031_21) %>% cbind(2021) %>% setnames("V2", "Year")
surv_sat_f_health_20 <- rbind(q029_20, q030_20, q031_20) %>% cbind(2020) %>% setnames("V2", "Year")

surv_sat_f_health_all <- rbind(surv_sat_f_health, surv_sat_f_health_21, surv_sat_f_health_20)

surv_sat_f_health_all$Response = gsub("\\[.*\\]", "", surv_sat_f_health_all$Response)
surv_sat_f_health_all$Response = gsub(" -.*", "", surv_sat_f_health_all$Response) %>% trimws()


# Can't compare the 'how is your health is general' question over time as the responses have changed significantly. I will remove it from the time series.
surv_sat_f_health_all <- surv_sat_f_health_all[Question != "How is your health in general?"]


# Group together the 0123, 456, 78910 responses as generally high/med/low
surv_sat_f_health_all[, `Response group` := cut(as.numeric(Response), breaks=c(-1, 3, 6, 11), labels = c("Not at all / not much (0-3)", "Somewhat (4-6)", "Quite a lot / a lot (7-10)"))
                      ]

Response_unique_loc_all <- unique(surv_sat_f_health_all$`Response group`)
surv_sat_f_health_all[, `Response group` := ordered(`Response group`, levels = Response_unique_loc_all)]

surv_sat_f_health_all_grp <- surv_sat_f_health_all[,.(`Percentage of respondents`=sum(`Percentage of respondents`)), by = c("Year","Question","Response group")]



# Graph for past three years

surv_sat_f_health_time_graph <- make_surv_graph_time_facets(surv_sat_f_health_all_grp, ncols=2, nrows=1, title_wrap = 40)
surv_sat_f_health_time_graph

#ggplotly(surv_sat_f_health_time_graph)

# Cost of living survey results ----------------------------------------------------
# Cost of living Qs Q032
#Q4AY22_1
#Q4AY22_2
#Q4AY22_3

q032 = create_surv_q_df("Q032")
q4ay22_1 = create_surv_q_df("Q4AY22_1")
q4ay22_2 = create_surv_q_df("Q4AY22_2")
q4ay22_3 = create_surv_q_df("Q4AY22_3")

# Join all dataframes
surv_sat_f_cost <- rbind(q032, q4ay22_1, q4ay22_2, q4ay22_3)

# Create a variable of the Response column with duplicates removed
Response_unique_loc <- rev(unique(surv_sat_f_cost$Response))

# Make response an ordered factor with levels in the order of the Response column
surv_sat_f_cost[, Response := ordered(Response, levels = Response_unique_loc)]

surv_sat_f_cost_graph <- make_surv_graph_facets(surv_sat_f_cost, ncols=4, title_wrap = 35)

surv_sat_f_cost_graph

# Internet connection survey results ----------------------------------------------------

q038 <- create_surv_q_df("Q038")


# Join all dataframes
surv_sat_f_tintenet <- q038

surv_sat_f_tintenet$Question = gsub("^Q([^ ]+)  ", "", surv_sat_f_tintenet$Question) %>% trimws()

# Create a variable of the Response column with duplicates removed
Response_unique_loc <- unique(surv_sat_f_tintenet$Response)

# Make response an ordered factor with levels in the order of the Response column
surv_sat_f_tintenet[, Response := ordered(Response, levels = Response_unique_loc)]

surv_sat_f_tintenet = surv_sat_f_tintenet[Response != "At work"]

surv_sat_f_graph_tintenet <- make_surv_graph_facets(surv_sat_f_tintenet, ncols=5, title_wrap = 100)

surv_sat_f_graph_tintenet

#ggplotly(surv_sat_f_graph_tintenet)

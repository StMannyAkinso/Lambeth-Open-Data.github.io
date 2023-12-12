# Load packages, initial variables ----------------------------------------

require(ggplot2)
require(plotly)
require(data.table)
require(rstudioapi)
#require(xlsx)
#library(readxl)
library(openxlsx)
#library(gglaplot)
library(tidyverse)

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

  print(cat_order)

  # In df_d set column 1 as ordered factor with levels cat_order
  df_d[, (1) := lapply(.SD, ordered, levels=cat_order), .SDcols = 1]

  # filter df_d by column 1 for NAs
  df_d <- df_d[!is.na(df_d[[1]]),]

  # order df_d by column 1 and `Area name`
  df_d <- df_d[order(`Area name`, -Percentage),]

  out_stuff <- list(df_d, cat_order)


  return(out_stuff)

  }


# The following should be true when testing this file. When knitting the report together and sourcing externally, set this to false
if (!exists("run_direct")){run_direct=T}

if (run_direct == F){dir_stub = ''
} else if (run_direct == T){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  dir_stub = '../'
  source(paste(c(dir_stub, 'colour_theme.r'),collapse = ''))
} 

format_crime_year <- function(path = '../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy21-22.csv',
                              year = "2021/22", major_cat_filt = "", minor_cat_filt = "", round_dig = 1){
  
  # Load data
  crime <- fread(paste(c(dir_stub, path), collapse = '')) %>% data.table()
  
  crime <- setnames(crime, c("Crime Type","Crime Subtype"),c("Offence Group","Offence Subgroup"), skip_absent=TRUE)
  
  
  # Filter to Measure = Total and to Boroughs
  crime <- crime[crime$Measure == "Offences" & crime$`Area Type`=="Borough",]
  
  #Remove rows containing Aviation in crime_b$Borough_SNT
  crime <- crime[!grepl("Aviation", Borough_SNT),] %>% as.data.table()
  
  crime[, `Offence Type` := "All crime categories"]
  
  if (major_cat_filt != ""){
    crime <- crime[`Offence Group` == major_cat_filt]
    crime[, `Offence Type` := major_cat_filt]}
  
  if (minor_cat_filt != ""){
    crime <- crime[`Offence Subgroup` == minor_cat_filt]
    crime[, `Offence Type` := minor_cat_filt]}
  
  
  crime_b <- aggregate(Count ~ Borough_SNT + `Offence Type`, data = crime, 
                       FUN = function(x) sum(x)) %>% as.data.table()
  
  
  # Make column numeric
  crime_b[, Count := as.numeric(Count)]
  
  # Create df crime_lam
  crime_lam <- crime_b[crime_b$`Borough_SNT` == "Lambeth",]
  
  #print(crime_lam)
  
  # Join on pop_tot to crime_lam
  crime_lam <- merge(crime_lam, pop_tot, by.x = "Borough_SNT", by.y = "Area")
  
  # Create crime_lam$Rate
  crime_lam[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,round_dig)]
  
  # ---
  
  # Create df crime_lon from sum of crime_b
  crime_lon <- data.table(Borough_SNT = "London", Count = sum(crime_b$Count), `Offence Type`= unique(crime_b$`Offence Type`))

  
  # Join on pop_tot to crime_lon
  crime_lon <- merge(crime_lon, pop_tot, by.x = "Borough_SNT", by.y = "Area")
  
  # Create crime_lon$Rate
  crime_lon[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,round_dig)]
  
  # ---
  
  # Combine crime_lam and crime_lon
  crime_b <- rbind(crime_lam, crime_lon)
  
  
  # Rename Borough_SNT to Area name
  names(crime_b)[1] = c("Area name")
  
  crime_b[,Year:=year]
  
  return(crime_b)
  
}

# Some graph parameters
adj = 0.01
label_size = 6
graph_height = 3.5 # inches
graph_aspect_ratio = 0.75 # ratio between height/width


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


# Overall crime rate ----------------------------------------

# Load data
crime <- fread(paste(c(dir_stub, '../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy21-22.csv'), collapse = '')) %>% data.table()


# Filter to Measure = Total and to Boroughs
crime <- crime[crime$Measure == "Offences" & crime$`Area Type`=="Borough",]

#Remove rows containing Aviation in crime_b$Borough_SNT
crime <- crime[!grepl("Aviation", Borough_SNT),] %>% as.data.table()

#table(crime$Borough_SNT)

# Sum up Count for each Borough
#crime_b <- crime %>%
#group_by(Borough_SNT) %>% 
#summarise(Count = sum(Count)) %>% 
#as.data.table()

#crime_b_g <- group_by(crime, crime$Borough_SNT) 
#crime_b_g_s <- summarise(crime_b_g, Count = sum(`Count`)) 
#crime_b <- as.data.table(crime_b_g_s)

crime_b <- aggregate(Count ~ Borough_SNT, data = crime, 
          FUN = function(x) sum(x)) %>% as.data.table()

  #print(crime_b)

  #bycol  = deparse(substitute(Borough_SNT))

  #crime_b <- crime[, .(Count = sum(Count)), by = .(Borough_SNT)]

  # Keep rows 2-33
  #crime_b <- crime_b[2:33,]

  # Make column numeric
  crime_b[, Count := as.numeric(Count)]

  # Create df crime_lam
  crime_lam <- crime_b[crime_b$`Borough_SNT` == "Lambeth",]

  print(crime_lam)

  # Join on pop_tot to crime_lam
  crime_lam <- merge(crime_lam, pop_tot, by.x = "Borough_SNT", by.y = "Area")

  # Create crime_lam$Rate
  crime_lam[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,1)]

  # ---

  # Create df crime_lon from sum of crime_b
  crime_lon <- data.table(Borough_SNT = "London", Count = sum(crime_b$Count))



  # Join on pop_tot to crime_lon
  crime_lon <- merge(crime_lon, pop_tot, by.x = "Borough_SNT", by.y = "Area")

  # Create crime_lon$Rate
  crime_lon[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,1)]

  # ---

  # Combine crime_lam and crime_lon
  crime_b <- rbind(crime_lam, crime_lon)


  # Rename Borough_SNT to Area name
  names(crime_b)[1] = c("Area name")

# Make a graph of crime_b in ggplot2

crime_graph <- ggplot(crime_b, aes(y = `Number of offences per 1,000 people`, x =  `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
"Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
ylab("Overall crime - Number of offences per 1,000 people")

crime_graph

# Overall crime rate by time merged ----------------------------------------

# Each 'year' is financial year from start of April to end of March in the next year

crime_b_2021 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy21-22.csv', year="2021/22") 
crime_b_2021
crime_b_2020 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy20-21.csv', year="2020/21") 
crime_b_2020
crime_b_2019 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy19-20.csv', year="2019/20") 
crime_b_2019
crime_b_2018 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy18-19.csv', year="2018/19") 
crime_b_2018

crime_b_time <- rbindlist(list(crime_b_2018, crime_b_2019, crime_b_2020, crime_b_2021))

# Make a graph of crime_b in ggplot2
crime_b_time_graph <- ggplot(crime_b_time, aes(y = `Number of offences per 1,000 people`, x = Year, colour = `Area name`,
                                   group = `Area name`, linetype=`Area name`,
                  text= paste("Area name: ", `Area name`, "<br>",
                        "Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
                        "Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
                  ))
) +
  geom_line()+#position = position_dodge2(width = 0.9))+#, reverse = T)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank())+#,
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
  #scale_y_continuous(labels = scales::comma) +
  expand_limits(y=0) +
  ylab("Overall crime - Number of offences per 1,000 people")

crime_b_time_graph

# Overall crime rate by time single df ----------------------------------------
require(data.table)

# Load data
crime_t <- fread(paste(c(dir_stub, '../data/Crime safety/Crime incidents/MPS Borough Level Crime (Historical).csv'), collapse = ''), skip=0, header=TRUE) %>% data.table()


# Filter to Measure = Total and to Boroughs
#crime_t <- crime_t[crime_t$Measure == "Offences" & crime_t$`Area Type`=="Borough",]

#Remove rows containing Aviation in crime_t_b$LookUp_BoroughName
crime_t <- crime_t[!grepl("Aviation", LookUp_BoroughName),] %>% as.data.table()

#table(crime_t$LookUp_BoroughName)

# Sum up Count for each Borough
#crime_t_b <- crime_t %>%
#group_by(LookUp_BoroughName) %>% 
#summarise(Count = sum(Count)) %>% 
#as.data.table()

#crime_t_b_g <- group_by(crime_t, crime_t$LookUp_BoroughName) 
#crime_t_b_g_s <- summarise(crime_t_b_g, Count = sum(`Count`)) 
#crime_t_b <- as.data.table(crime_t_b_g_s)

crime_t_m <- melt(crime_t, id.vars = c("MajorText", "MinorText", "LookUp_BoroughName"), variable.name="Date", value.name = "Count")

crime_t_m[,Date:=paste(Date, "01", sep='')]

crime_t_m <- crime_t_m[,Date := as.Date(as.character(Date), tryFormats="%Y%m%d")]

crime_t_m <- crime_t_m[,Year := format(Date, format="%Y")]
crime_t_m <- crime_t_m[,Month := format(Date, format="%m")]


crime_t_b <- aggregate(Count ~ LookUp_BoroughName + Year, data = crime_t_m, 
                     FUN = function(x) sum(x)) %>% as.data.table()

# Make column numeric
crime_t_b[, Count := as.numeric(Count)]

# Create df crime_t_lam
crime_t_lam <- crime_t_b[crime_t_b$`LookUp_BoroughName` == "Lambeth",]

print(crime_t_lam)

# Join on pop_tot to crime_t_lam
# crime_t_lam <- merge(crime_t_lam, pop_tot, by.x = "LookUp_BoroughName", by.y = "Area")
# 
# # Create crime_t_lam$Rate
# crime_t_lam[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,1)]
# 
# # ---
# 
# # Create df crime_t_lon from sum of crime_t_b
# crime_t_lon <- data.table(LookUp_BoroughName = "London", Count = sum(crime_t_b$Count))
# 
# 
# 
# # Join on pop_tot to crime_t_lon
# crime_t_lon <- merge(crime_t_lon, pop_tot, by.x = "LookUp_BoroughName", by.y = "Area")
# 
# # Create crime_t_lon$Rate
# crime_t_lon[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,1)]
# 
# # ---
# 
# # Combine crime_t_lam and crime_t_lon
# crime_t_b <- rbind(crime_t_lam, crime_t_lon)
# 
# 
# # Rename LookUp_BoroughName to Area name
# names(crime_t_b)[1] = c("Area name")
# 
# # Make a graph of crime_t_b in ggplot2
# 
# crime_t_graph <- ggplot(crime_t_b, aes(y = `Number of offences per 1,000 people`, x =  `Area name`, fill = `Area name`,
#                                    text= paste("Area name: ", `Area name`, "<br>",
#                                                "Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
#                                                "Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
#                                    ))
# ) +
#   geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
#   scale_fill_discrete(type=lambeth_palette_graph) +
#   theme_lam() +
#   theme(axis.title.x = element_blank())+#,
#   #axis.text.x = element_text(angle = 45, hjust = 1)) +
#   #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
#   #scale_y_continuous(labels = scales::comma) +
#   ylab("Overall crime - Number of offences per 1,000 people")
# 
# crime_t_graph

# Violent crime ----------------------------------------


    #table(crime$Borough_SNT)

  # Sum up Count for each Borough
  #crime_v <- crime %>%
  #group_by(Borough_SNT) %>% 
  #summarise(Count = sum(Count)) %>% 
  #as.data.table()

  #crime_v_g <- group_by(crime, Borough_SNT) 
  #crime_v_g_s <- summarise(crime_v_g, Count = sum(Count)) 
  #crime_v <- as.data.table(crime_v_g_s)


  #print(crime_v)

  crime_v <- crime[crime$`Offence Group` == "Violence Against the Person",]

  crime_v <- aggregate(Count ~ Borough_SNT, data = crime_v, 
  FUN = function(x) sum(x)) %>% as.data.table()


  #bycol  = deparse(substitute(Borough_SNT))
  #crime_v <- crime[, .(Count = sum(Count)), by = .(Borough_SNT)]

  # Keep rows 2-33
  #crime_v <- crime_v[2:33,]

  # Make column numeric
  crime_v[, Count := as.numeric(Count)]

  # Create df crime_lam
  crime_lam <- crime_v[crime_v$`Borough_SNT` == "Lambeth",]

  print(crime_lam)

  # Join on pop_tot to crime_lam
  crime_lam <- merge(crime_lam, pop_tot, by.x = "Borough_SNT", by.y = "Area")

  # Create crime_lam$Rate
  crime_lam[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,1)]

  # ---

  # Create df crime_lon from sum of crime_v
  crime_lon <- data.table(Borough_SNT = "London", Count = sum(crime_v$Count))



  # Join on pop_tot to crime_lon
  crime_lon <- merge(crime_lon, pop_tot, by.x = "Borough_SNT", by.y = "Area")

  # Create crime_lon$Rate
  crime_lon[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,1)]

  # ---

  # Combine crime_lam and crime_lon
  crime_v <- rbind(crime_lam, crime_lon)


  # Rename Borough_SNT to Area name
  names(crime_v)[1] = c("Area name")

# Make a graph of crime_v in ggplot2

crime_v_graph <- ggplot(crime_v, aes(y = `Number of offences per 1,000 people`, x =  `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
"Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
ylab("Violent crimes - Number of offences per 1,000 people")

crime_v_graph

# Violent crime rate by time merged ----------------------------------------

# Each 'year' is financial year from start of April to end of March in the next year

crime_b_v_2021 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy21-22.csv', year="2021/22", major_cat_filt = "Violence Against the Person") 
crime_b_v_2021
crime_b_v_2020 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy20-21.csv', year="2020/21", major_cat_filt = "Violence Against the Person") 
crime_b_v_2020
crime_b_v_2019 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy19-20.csv', year="2019/20", major_cat_filt = "Violence Against the Person") 
crime_b_v_2019
crime_b_v_2018 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy18-19.csv', year="2018/19", major_cat_filt = "Violence Against the Person") 
crime_b_v_2018

crime_b_v_time <- rbindlist(list(crime_b_v_2018, crime_b_v_2019, crime_b_v_2020, crime_b_v_2021))

# Make a graph of crime_b_v in ggplot2
crime_b_v_time_graph <- ggplot(crime_b_v_time, aes(y = `Number of offences per 1,000 people`, x = Year, colour = `Area name`,
            group = `Area name`, linetype=`Area name`,
            text= paste("Area name: ", `Area name`, "<br>",
               "Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
               "Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
            ))
) +
  geom_line()+#position = position_dodge2(width = 0.9))+#, reverse = T)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank())+#,
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
  #scale_y_continuous(labels = scales::comma) +
  expand_limits(y=0) +
  ylab("Violence against the person - Number of offences per 1,000 people")

crime_b_v_time_graph

# Hate crime ----------------------------------------

# Load data
crimeh <- fread(paste(c(dir_stub, '../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy21-22.csv'), collapse = '')) %>% data.table()


# Filter to Measure = Total and to Boroughs
crimeh <- crimeh[crimeh$Measure == "Offences" & crimeh$`Area Type`=="Borough",]

#Remove rows containing Aviation in crime_b$Borough_SNT
crimeh <- crimeh[!grepl("Aviation", Borough_SNT),] %>% as.data.table()


#Keep only hate crime
crimeh <- crimeh[`Crime Type` == "Hate crime",] %>% as.data.table()


crime_bh <- aggregate(Count ~ Borough_SNT + `Crime Subtype`, data = crimeh, 
          FUN = function(x) sum(x)) %>% as.data.table()

# Rename Borough_SNT to Area name
names(crime_bh)[1] = c("Area name")

# Join on pop_tot to crime_bh
#crime_bh <- merge(crime_bh, pop_tot, by.x = "Area name", by.y = "Area")

# Create crime_bh$Rate
#crime_bh[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,1)]

# ---
# Create df crime_lam
crime_lam <- crime_bh[crime_bh$`Area name` == "Lambeth",]

# Join on pop_tot to crime_lam
crime_lam <- merge(crime_lam, pop_tot, by.x = "Area name", by.y = "Area")

# Create crime_lam$Rate
crime_lam[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,1)]

# ---
# Create df crime_lon from sum of crime_bh
crime_lon <- crime_bh[,.(Count = sum(Count), `Area name` = "London"),by="Crime Subtype"]
  
#data.table(`Area name` = "London", Count = sum(crime_bh$Count))

# Join on pop_tot to crime_lon
crime_lon <- merge(crime_lon, pop_tot, by.x = "Area name", by.y = "Area")

# Create crime_lon$Rate
crime_lon[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,1)]

# ---
# Combine crime_lam and crime_lon
crime_bh <- rbind(crime_lam, crime_lon)

# Make a graph of crime_bh in ggplot2

crime_bh_graph <- ggplot(crime_bh, aes(y = `Number of offences per 1,000 people`, x =  `Crime Subtype`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
"Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
#geom_text(aes(label=`Number of offences per 1,000 people`, y=`Number of offences per 1,000 people`+ 0.05),
#          vjust = -0.5,position = position_dodge2(width = 0.9), show.legend =F) +
theme_lam() +
theme(axis.title.x = element_blank(),
axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
expand_limits(y=0) +
ylab("Hate crime - Number of offences per 1,000 people")

# Hate crime rate by time merged ----------------------------------------

# Each 'year' is financial year from start of April to end of March in the next year

crime_b_hate_2021 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy21-22.csv', year="2021/22", major_cat_filt = "Hate crime") 
crime_b_hate_2021
crime_b_hate_2020 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy20-21.csv', year="2020/21", major_cat_filt = "Hate crime") 
crime_b_hate_2020
crime_b_hate_2019 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy19-20.csv', year="2019/20", major_cat_filt = "Hate crime") 
crime_b_hate_2019
crime_b_hate_2018 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy18-19.csv', year="2018/19", major_cat_filt = "Hate crime") 
crime_b_hate_2018

crime_b_hate_time <- rbindlist(list(crime_b_hate_2018, crime_b_hate_2019, crime_b_hate_2020, crime_b_hate_2021))

# Make a graph of crime_b_hate in ggplot2
crime_b_hate_time_graph <- ggplot(crime_b_hate_time, aes(y = `Number of offences per 1,000 people`, x = Year, colour = `Area name`,
                                               group = `Area name`, linetype=`Area name`,
                                               text= paste("Area name: ", `Area name`, "<br>",
                                                           "Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
                                                           "Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
                                               ))
) +
  geom_line()+#position = position_dodge2(width = 0.9))+#, reverse = T)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank())+#,
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
  #scale_y_continuous(labels = scales::comma) +
  expand_limits(y=0) +
  ylab("Hate crime - Number of offences per 1,000 people")

crime_b_hate_time_graph

# Domestic abuse ----------------------------------------

# Load data
crimea <- fread(paste(c(dir_stub, '../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy21-22.csv'), collapse = '')) %>% data.table()

# Filter to Measure = Total and to Boroughs
crimea <- crimea[crimea$Measure == "Offences" & crimea$`Area Type`=="Borough",]

#Remove rows containing Aviation in crime_b$Borough_SNT
crimea <- crimea[!grepl("Aviation", Borough_SNT),] %>% as.data.table()

#Keep only domestic abuse
crimea <- crimea[`Crime Type` == "Domestic Abuse",] %>% as.data.table()

crime_ba <- aggregate(Count ~ Borough_SNT, data = crimea, 
          FUN = function(x) sum(x)) %>% as.data.table()

# Rename Borough_SNT to Area name
names(crime_ba)[1] = c("Area name")

# Join on pop_tot to crime_ba
#crime_ba <- merge(crime_ba, pop_tot, by.x = "Area name", by.y = "Area")

# Create crime_ba$Rate
#crime_ba[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,1)]

# ---

# Create df crime_lam
crime_lam <- crime_ba[crime_ba$`Area name` == "Lambeth",]

# Join on pop_tot to crime_lam

crime_lam <- merge(crime_lam, pop_tot, by.x = "Area name", by.y = "Area")

# Create crime_lam$Rate

crime_lam[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,1)]

# ---

# Create df crime_lon from sum of crime_ba

crime_lon <- data.table(`Area name` = "London", Count = sum(crime_ba$Count))

# Join on pop_tot to crime_lon

crime_lon <- merge(crime_lon, pop_tot, by.x = "Area name", by.y = "Area")

# Create crime_lon$Rate

crime_lon[, `Number of offences per 1,000 people` := round((Count / Population) * 1000,1)]

# ---
# Combine crime_lam and crime_lon
crime_ba <- rbind(crime_lam, crime_lon)

# Make a graph of crime_ba in ggplot2

crime_ba_graph <- ggplot(crime_ba, aes(y = `Number of offences per 1,000 people`, x =  `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
"Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
ylab("Domestic abuse - Number of offences per 1,000 people")

crime_ba_graph

# Domestic abuse crime rate by time merged ----------------------------------------

# Each 'year' is financial year from start of April to end of March in the next year

da_filter <- "Domestic Abuse"

crime_b_da_2021 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy21-22.csv', year="2021/22", major_cat_filt = da_filter) 
crime_b_da_2021
crime_b_da_2020 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy20-21.csv', year="2020/21", major_cat_filt = da_filter) 
crime_b_da_2020
crime_b_da_2019 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy19-20.csv', year="2019/20", major_cat_filt = da_filter) 
crime_b_da_2019
crime_b_da_2018 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy18-19.csv', year="2018/19", major_cat_filt = da_filter) 
crime_b_da_2018

crime_b_da_time <- rbindlist(list(crime_b_da_2018, crime_b_da_2019, crime_b_da_2020, crime_b_da_2021))

#summary(crime_b_da_time)

# Make a graph of crime_b_da in ggplot2
crime_b_da_time_graph <- ggplot(crime_b_da_time, aes(y = `Number of offences per 1,000 people`, x = Year, colour = `Area name`,
               group = `Area name`, linetype=`Area name`,
               text= paste("Area name: ", `Area name`, "<br>",
                  "Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
                  "Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
               ))
) +
  geom_line()+#position = position_dodge2(width = 0.9))+#, reverse = T)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank())+#,
  expand_limits(y=0) +
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
  #scale_y_continuous(labels = scales::comma) +
  ylab("Domestic abuse - Number of offences per 1,000 people")

crime_b_da_time_graph

# Knife crime rate by time merged ----------------------------------------

# Each 'year' is financial year from start of April to end of March in the next year

da_filter <- "Knife crime"

crime_knife_2021 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy21-22.csv', year="2021/22", major_cat_filt = da_filter) 
crime_knife_2021
crime_knife_2020 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy20-21.csv', year="2020/21", major_cat_filt = da_filter) 
crime_knife_2020
crime_knife_2019 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy19-20.csv', year="2019/20", major_cat_filt = da_filter) 
crime_knife_2019
crime_knife_2018 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy18-19.csv', year="2018/19", major_cat_filt = da_filter) 
crime_knife_2018

crime_knife_time <- rbindlist(list(crime_knife_2018, crime_knife_2019, crime_knife_2020, crime_knife_2021))

summary(crime_knife_time)

# Make a graph of crime_knife in ggplot2
crime_knife_time_graph <- ggplot(crime_knife_time, aes(y = `Number of offences per 1,000 people`,
                                                       x = Year, colour = `Area name`,
                                                       group = `Area name`, linetype=`Area name`,
              text= paste("Area name: ", `Area name`, "<br>",
                 "Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
                 "Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
              ))
) +
  geom_line()+#position = position_dodge2(width = 0.9))+#, reverse = T)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank())+#,
  expand_limits(y=0) +
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
  #scale_y_continuous(labels = scales::comma) +
  ylab("Knife crime - Number of offences per 1,000 people")

crime_knife_time_graph

# Knife crime rate victims 1-24 non DA by time merged ----------------------------------------

# Each 'year' is financial year from start of April to end of March in the next year

da_filter <- "Knife crime"
min_filter <- "Knife Injury Victims (non DA 1-24)"

undebug(format_crime_year)
crime_knife24_2021 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy21-22.csv', year="2021/22", major_cat_filt = da_filter, minor_cat_filt = min_filter, round_dig = 2) 
crime_knife24_2021
crime_knife24_2020 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy20-21.csv', year="2020/21", major_cat_filt = da_filter, minor_cat_filt = min_filter, round_dig = 2) 
crime_knife24_2020
crime_knife24_2019 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy19-20.csv', year="2019/20", major_cat_filt = da_filter, minor_cat_filt = min_filter, round_dig = 2) 
crime_knife24_2019
crime_knife24_2018 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_OtherCrimeDatafy18-19.csv', year="2018/19", major_cat_filt = da_filter, minor_cat_filt = min_filter, round_dig = 2) 
crime_knife24_2018

crime_knife24_time <- rbindlist(list(crime_knife24_2018, crime_knife24_2019, crime_knife24_2020, crime_knife24_2021))

#summary(crime_knife24_time)

# Make a graph of crime_knife24 in ggplot2
crime_knife24_time_graph <- ggplot(crime_knife24_time, aes(y = `Number of offences per 1,000 people`,
                                                       x = Year, colour = `Area name`,
                                                       group = `Area name`, linetype=`Area name`,
                                                       text= paste("Area name: ", `Area name`, "<br>",
                                                                   "Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
                                                                   "Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
                                                       ))
) +
  geom_line()+#position = position_dodge2(width = 0.9))+#, reverse = T)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank())+#,
  expand_limits(y=0) +
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
  #scale_y_continuous(labels = scales::comma) +
  ylab("Knife Injury Victims (non DA 1-24)\nNumber of offences per 1,000 people")

crime_knife24_time_graph

# Drug crime rate by time merged ----------------------------------------

# Each 'year' is financial year from start of April to end of March in the next year

crime_drug_2021 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy21-22.csv', year="2021/22", major_cat_filt = "Drug Offences") 
crime_drug_2021
crime_drug_2020 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy20-21.csv', year="2020/21", major_cat_filt = "Drug Offences") 
crime_drug_2020
crime_drug_2019 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy19-20.csv', year="2019/20", major_cat_filt = "Drug Offences") 
crime_drug_2019
crime_drug_2018 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy18-19.csv', year="2018/19", major_cat_filt = "Drug Offences") 
crime_drug_2018

crime_drug_time <- rbindlist(list(crime_drug_2018, crime_drug_2019, crime_drug_2020, crime_drug_2021))

# Make a graph of crime_drug in ggplot2
crime_drug_time_graph <- ggplot(crime_drug_time, aes(y = `Number of offences per 1,000 people`, x = Year, colour = `Area name`,
                                                   group = `Area name`, linetype=`Area name`,
                                                   text= paste("Area name: ", `Area name`, "<br>",
                                                               "Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
                                                               "Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
                                                   ))
) +
  geom_line()+#position = position_dodge2(width = 0.9))+#, reverse = T)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank())+#,
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
  #scale_y_continuous(labels = scales::comma) +
  expand_limits(y=0) +
  ylab("Drug Offences - Number of offences per 1,000 people")

#crime_drug_time_graph

#ggplotly(crime_drug_time_graph)

# Domestic burglary crime rate by time merged ----------------------------------------

# Each 'year' is financial year from start of April to end of March in the next year

crime_burglar_2021 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy21-22.csv', year="2021/22", major_cat_filt = "Burglary", minor_cat_filt = "Domestic Burglary") 
crime_burglar_2021
crime_burglar_2020 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy20-21.csv', year="2020/21", major_cat_filt = "Burglary", minor_cat_filt = "Domestic Burglary") 
crime_burglar_2020
crime_burglar_2019 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy19-20.csv', year="2019/20", major_cat_filt = "Burglary", minor_cat_filt = "Domestic Burglary") 
crime_burglar_2019
crime_burglar_2018 = format_crime_year('../data/Crime safety/Crime incidents/MPS_BoroughSNT_TNOCrimeDatafy18-19.csv', year="2018/19", major_cat_filt = "Burglary", minor_cat_filt = "Domestic Burglary") 
crime_burglar_2018

crime_burglar_time <- rbindlist(list(crime_burglar_2018, crime_burglar_2019, crime_burglar_2020, crime_burglar_2021))

# Make a graph of crime_burglar in ggplot2
crime_burglar_time_graph <- ggplot(crime_burglar_time, aes(y = `Number of offences per 1,000 people`, x = Year, colour = `Area name`,
                                                     group = `Area name`, linetype=`Area name`,
                                                     text= paste("Area name: ", `Area name`, "<br>",
                                                                 "Number of offences per 1,000 people: ",prettyNum(`Number of offences per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
                                                                 "Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
                                                     ))
) +
  geom_line()+#position = position_dodge2(width = 0.9))+#, reverse = T)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank())+#,
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
  #scale_y_continuous(labels = scales::comma) +
  expand_limits(y=0) +
  ylab("Drug Offences - Number of offences per 1,000 people")

#crime_burglar_time_graph

#ggplotly(crime_burglar_time_graph)

# Anti-social behaviour in 2022 -------------------------------------------
# Read the csv

asb <- fread(paste(c(dir_stub, '../data/Crime safety/Crime incidents/asb_lam_lon_201922.csv'), collapse = ''), header = T) %>% data.table()

asb_m <- melt(asb, id.vars = c("Area", "Crime"), variable.name = "Year", value.name = "Anti-Social Behaviour (rate per 1,000 people)")

asb_m[,Crime := NULL]

setnames(asb_m, "Area", "Area name")


asb_m_graph <- ggplot(asb_m, aes(y = `Anti-Social Behaviour (rate per 1,000 people)`, x =  Year, colour = `Area name`, linetype=`Area name`, group = `Area name`,
                                     text= paste("Area name: ", `Area name`, "<br>",
                                                 "Anti-Social Behaviour (rate per 1,000 people): ",prettyNum(`Anti-Social Behaviour (rate per 1,000 people)`,big.mark=",", preserve.width="none"), "<br>"
                                     ))
) +
  geom_line()+#position = position_dodge2(width = 0.9))+#, reverse = T)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank())+#,
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
  #scale_y_continuous(labels = scales::comma) +
  ylab("Anti-social behaviour - number of offences per 1,000 people")

asb_m_graph

# Reoffending rates -------------------------------------------
# Read the XLSX file
reoff <- read.xlsx(paste(c(dir_stub, '../data/Crime safety/Justice/Geographical_data_tool_Apr09_Mar21.xlsx'), collapse = ''), sheet = "Data") %>% data.table()

reoff

# Replace . in column names with " "
names(reoff) = gsub("\\.", " ", names(reoff))

reoff_f <- reoff[grep("total", Category)]
reoff_f <- reoff_f[(`Adult / Juvenile` == "Adult")]
reoff_f <- reoff_f[(grepl("^England", Geography) | grepl("^London", Geography) | grepl("Lambeth", Geography))]
reoff_f <- reoff_f[(Category != "UTLA total")]

reoff_f[, Year := str_extract(Cohort, "\\d\\d\\d\\d$")]
reoff_f <- reoff_f[, c("Geography", "Year", "offenders", "reoffenders")]
reoff_f[, `Percentage of offenders who reoffend`:= round(((reoffenders/offenders)*100),1)]

reoff_f_17 <- reoff_f[Year >= 2017]

#reoff_m <- melt(reoff, id.vars = c("Area", "Crime"), variable.name = "Year", value.name = "Anti-Social Behaviour (rate per 1,000 people)")

names(reoff_f_17) <- c("Area name", "Year", "Number of offenders", "Number of reoffenders", "Percentage of offenders who reoffended (%)")


# Make Area name an ordered factor with Lambeth, London, England
reoff_f_17[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England and Wales"))]


reoff_f_17_graph <- ggplot(reoff_f_17, aes(y = `Percentage of offenders who reoffended (%)`, x =  Year, fill = `Area name`, #linetype=`Area name`, group = `Area name`,
                                 text= paste("Area name: ", `Area name`, "<br>",
                                             "Percentage of offenders who reoffended (%): ",prettyNum(`Percentage of offenders who reoffended (%)`,big.mark=",", preserve.width="none"), "<br>"
                                 ))
) +
  geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
  #geom_point()+
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank())+#,
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
  #scale_y_continuous(labels = scales::comma) +
  expand_limits(y=0) +
  ylab("Percentage of offenders who reoffended (%)")

ggplotly(reoff_f_17_graph)

# Stop and search ----------------------------------------

# Load data
stops <- fread(paste(c(dir_stub, '../data/Crime safety/Stop and search/Stops_LDS_Extract_24MonthsToEnd_202212.csv'), collapse = '')) %>% data.table()

# Filter 'Airport' out of Borough of Stop
stops <- stops[!grepl("Airport", `Borough of Stop`),] %>% as.data.table()

# Filter Outcome to no further action - not used in the end
#stops <- stops[Outcome == "1 No further action",] %>% as.data.table()

# Filter Date to 2022
stops <- stops[year(Date) == 2022,]

max(stops$Date)
min(stops$Date)

# Filter out `Self-defined Ethnicity Code` rows that are blank

#grepl("^N", stops$`Self-defined Ethnicity Code`)

# Create column Ethnic minority from SDE Group not White
stops[, `Ethnic minority` := ifelse(
  `Self-defined Ethnicity Code` == "" | grepl("^N", `Self-defined Ethnicity Code`), "Not self-defined/unknown",
    ifelse(grepl("^W", `Self-defined Ethnicity Code`, fixed=F) , "No", "Yes"))]


table(stops$`Ethnic minority`)

# Aggregate to Borough
stops_b <- aggregate(Count ~ `Borough of Stop` + `Ethnic minority`, data = stops, 
          FUN = function(x) sum(x)) %>% as.data.table()
stops_b

# Rename Borough of Stop to Area name
names(stops_b)[1] = c("Area name")

# Create stops_lam from stops_b
stops_lam <- stops_b[`Area name` == "Lambeth",]
stops_lam

# Create stops_lon from sum of stops_b, grouped by Ethnic minority
stops_lon <- aggregate(Count ~ `Ethnic minority`, data = stops_b, 
          FUN = function(x) sum(x)) %>% as.data.table()
stops_lon

# Area name = London
stops_lon[, `Area name` := "London"]

# Combine stops_lam and stops_lon
stops_b_out <- rbind(stops_lam, stops_lon)

# Load in ethnicity population data

eth_d <- l_dat(paste(c(dir_stub, "../data/Key demographics/Census 2021/TS022_ethnic_group_census_2021.xlsx"), collapse = ""), 7)[[1]]

# rename Ethnic group (detailed) column to 'Ethnic group'
setnames(eth_d, "Ethnic group (detailed)", "Ethnic group")

eth_d

# Create column Ethnic minority for eth_d, White and not White
eth_d[, `Ethnic minority` := ifelse(`Ethnic group` != "White", "Yes", "No")]

eth_d_t <- aggregate(Population ~ `Ethnic minority` + `Area name`, data = eth_d, 
          FUN = function(x) sum(x)) %>% as.data.table()

# Join on eth_d_t to stops_b_out

stops_b_out_m = merge(stops_b_out, eth_d_t, by = c("Area name", "Ethnic minority"), all.x = T)

# Calculate stop and search rate
stops_b_out_m[, `Rate` := round((Count / Population) * 1000,1)]

stops_b_out_m

# Rename Rate to 'Number of 'stop and searches' per 1,000 people'
names(stops_b_out_m)[5] = c("Number of 'stop and searches' per 1,000 people")

stops_b_out_m

# Make a graph of stops_b_out in ggplot2

stops_b_out_graph <- ggplot(stops_b_out_m[`Ethnic minority` != "Not self-defined/unknown"], aes(y = `Number of 'stop and searches' per 1,000 people`, x =  `Ethnic minority`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Number of 'stop and searches' per 1,000 people: ",prettyNum(`Number of 'stop and searches' per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
"Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>"
))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
#theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
ylab("Number of 'stop and searches' per 1,000 people") +
xlab("From an ethnic minority background")

stops_b_out_graph

ggplotly(stops_b_out_graph)


# Traffic incidents casualties ----------------------------------------

# Load data
traffic <- fread(paste(c(dir_stub, '../data/Crime safety/Traffic incidents/jan-dec-2021-gla-data-extract-casualty.csv'), collapse = ''), skip=1) %>% data.table()

traffic

# Sum up _Casualty Count by Borough Name
traffic_b <- aggregate(`_Casualty Count` ~ `Borough Name`, data = traffic, 
          FUN = function(x) sum(x)) %>% as.data.table()

traffic_b

# Rename Borough Name to Area name
names(traffic_b)[1] = c("Area name")

traffic_b

# Filter to lambeth
traffic_lam <- traffic_b[`Area name` == "Lambeth",]

# Filter to London
traffic_lon <- traffic_b
traffic_lon <- traffic_lon[,`Area name` := "London"]

traffic_lon <- aggregate(`_Casualty Count` ~ `Area name`, data = traffic_b, 
          FUN = function(x) sum(x)) %>% as.data.table()

# Combine traffic_lam and traffic_lon
traffic_b_out <- rbind(traffic_lam, traffic_lon)


# Merge population data to traffic_b
traffic_b_out_m <- merge(traffic_b_out, pop_tot, by.x = c("Area name"), by.y = c("Area"), all.x = T)

traffic_b_out_m

# Calculate rate
traffic_b_out_m[, `Rate` := round((`_Casualty Count` / Population) * 1000,1)]

traffic_b_out_m

# Rename Rate to 'Number of traffic incidents casualties per 1,000 people'
names(traffic_b_out_m)[4] = c("Number of traffic incidents casualties per 1,000 people")

# Rename _Casualty Count to 'Number of traffic incidents casualties'
names(traffic_b_out_m)[2] = c("Number of traffic incidents casualties")

traffic_b_out_m

names(traffic_b_out_m)

# Make a graph of traffic_b_out_m in ggplot2

traffic_b_out_m_graph <- ggplot(traffic_b_out_m, aes(y = `Number of traffic incidents casualties per 1,000 people`, x =  `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Number of traffic incidents casualties per 1,000 people: ",prettyNum(`Number of traffic incidents casualties per 1,000 people`,big.mark=",", preserve.width="none"), "<br>",
"Population: ",prettyNum(`Population`,big.mark=",", preserve.width="none")#, "<br>"
))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
#theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
ylab("Traffic incidents - Number of traffic incidents casualties\nper 1,000 people") 

traffic_b_out_m_graph

# Public perception - Do the Police do a good job in the local area? ----------------------------------------

# Load data
gj <- fread(paste(c(dir_stub, '../data/Crime safety/MOPAC public perception/all_borough_police_good_job_june_2022.csv'), collapse = ''), header = T)#, skip=0)# %>% data.table()

names(gj)

# Remove second row
gj <- gj[-2,]

# melt with Measure as id var, Borough as measure var, and value as value var
gj_m <- melt(gj, id.vars = c("Measure"), variable.name = "Borough", value.name = "Value")

# Remove Borough == Date, Borough == V3, Borough == MPS
gj_m <- gj_m[!(Borough %in% c("Date", "V3", "MPS")),]

# Remove percentage signs from Value, then convert into integer
gj_m[, Value := as.numeric(gsub("%", "", Value))]

# Create Lambeth data
gj_lam <- gj_m[Borough == "Lambeth",]

# Create London data from average of all boroughs
gj_lon <- gj_m
gj_lon_g <- gj_lon[, .(Value = mean(Value))]
gj_lon_g <- gj_lon_g[, Borough := "London"]

gj_lon_g

# Combine gj_lam and gj_lon
gj_out <- rbindlist(list(gj_lam, gj_lon_g), use.names = T, fill = T)

gj_out 

# Rename Value to 'Percentage'
names(gj_out)[3] = c("Percentage")

# Rename Borough to 'Area name'
names(gj_out)[2] = c("Area name")

# Round percentage to 1 dp
gj_out[,`Percentage` := round(`Percentage`, 1)]


# Make a graph of gj_out in ggplot2

gj_out_graph <- ggplot(gj_out, aes(y = Percentage, x = `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Value: ",prettyNum(Percentage,big.mark=",", preserve.width="none"), "<br>"
))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
#theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
ylab("% Agreeing with the statement 'Police do a good job in the local area'")

gj_out_graph

# Load packages, initial variables ----------------------------------------

require(ggplot2)
require(plotly)
require(data.table)
require(rstudioapi)
#require(xlsx)
#library(readxl)
library(openxlsx)
#library(gglaplot)

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

  #print(df_m)

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

  #print(df_d)

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


# Some graph parameters
adj = 0.01
label_size = 6
graph_height = 3.5 # inches
graph_aspect_ratio = 0.75 # ratio between height/width

# Population NOT CENSUS --------------------------------------------------------------
pop <- read.xlsx(paste(c(dir_stub, "../data/Jobs/NOMIS/Total population simple 2020.xlsx"),collapse = ''),
       startRow = 2) %>% data.table()
pop

pop_df= pop[2:4]
pop_df

pop_df_m = melt(pop_df, id.vars = "X1") %>% as.data.table()
names(pop_df_m) = c("Sex", "Area name", "Population")

pop_tot <- pop_df_m[Sex=="All People"][, Sex:= NULL]

# Population CENSUS --------------------------------------------------------------

pop_cen <- read.xlsx(paste(c(dir_stub, "../data/Key demographics/Census 2021/TS008_sex_pop_census_2021.xlsx"),collapse = ''),
       startRow = 7) %>% data.table()

pop_cen

# remove first and last row
pop_cen <- pop_cen[-c(1, nrow(pop_cen)),]

# Remove anything before : in column names
names(pop_cen) = gsub(".*:", "", names(pop_cen))

# Rename columns with X in them as 'percentage' concatenated with the column name to the left

names(pop_cen)[3] = "Lambeth %"
names(pop_cen)[5] = "England %"
names(pop_cen)[7] = "London %"

# melt dataframe with sex as id variable
pop_cen_m <- melt(pop_cen, id.vars = "Sex", variable.name="Area name", value.name = "Value") %>% as.data.table()

# make a column for 'Area name' that contains a % sign
pop_cen_m[, Percentage := grepl("%", `Area name`)]

# replace ' %' with '' in Area name column
pop_cen_m[, `Area name` := gsub(" %", "", `Area name`)]

# dcast dataframe with Percentage as id variable
pop_cen_m_d <- dcast(pop_cen_m, `Area name` + Sex ~ Percentage, value.var = "Value")

# rename 'FALSE' to 'Population' and 'TRUE' to 'Percentage'
names(pop_cen_m_d)[3] = "Population"
names(pop_cen_m_d)[4] = "Percentage"

# make Area an ordered factor Lambeth, London, England
pop_cen_m_d[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England"))]

pop_cen_m_d_g = pop_cen_m_d[`Sex`!="All persons"]

pop_cen_m_d_g[, Percentage := as.numeric(Percentage)]


prettyNum(pop_cen_m_d_g$`Population`,big.mark=",", preserve.width="none")
format(pop_cen_m_d_g$`Population`,big.mark=",", trim=TRUE)

# Make a graph of population by sex and Area name by population with ggplot2
pop_cen_m_d_graph = ggplot(pop_cen_m_d_g, aes(x = `Area name`,
y = `Percentage`,
fill= Sex,
                        text= paste("Area name: ", `Area name`, "<br>",
                        "Sex: ", Sex, "<br>",
                        "Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
                        "Percentage: ", `Percentage`, "%"))
                        ) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank(),
  legend.position = "none") +
  expand_limits(y=0) +
  #coord_cartesian(ylim = c(0, 1)) +
  ylab("Population (%)")

pop_cen_m_d_graph

# Country of birth --------------------------------------------------------------
# Load data
cob <- read.xlsx(paste(c(dir_stub, "../data/Key demographics/Census 2021/TS004_country_of_birth_census_2021.xlsx"),collapse = ''),
       startRow = 7) %>% data.table()

# remove first and last row
cob <- cob[-c(1, nrow(cob)),]

# Remove anything before : in column names
names(cob) = gsub(".*:", "", names(cob))

# Replace . in column names with ' '
names(cob) = gsub("\\.", " ", names(cob))

# Rename columns with X in them as 'percentage' concatenated with the column name to the left
names(cob)[3] = "Lambeth %"
names(cob)[5] = "England %"
names(cob)[7] = "London %"

# melt dataframe with country of birth as id variable
cob_m <- melt(cob, id.vars = "Country of birth", variable.name="Area name", value.name = "Value") %>% as.data.table()

# make a column for 'Area name' that contains a % sign
cob_m[, Percentage := grepl("%", `Area name`)]

# replace ' %' with '' in Area name column
cob_m[, `Area name` := gsub(" %", "", `Area name`)]

# dcast dataframe with Percentage as id variable
cob_m_d <- dcast(cob_m, `Area name` + `Country of birth` ~ Percentage, value.var = "Value")

# rename 'FALSE' to 'Population' and 'TRUE' to 'Percentage'
names(cob_m_d)[3] = "Population"
names(cob_m_d)[4] = "Percentage"

# make Area an ordered factor Lambeth, London, England
cob_m_d[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England"))]

cob_m_d[, `Country of birth` := gsub("Europe: United Kingdom","United Kingdom",`Country of birth`)]

# Remove 'Total: All usual residents'
cob_m_d <- cob_m_d[`Country of birth`!="Total: All usual residents"]

unique(cob_m_d[, `Country of birth`])

cob_m_d_lam = cob_m_d[`Area name` == "Lambeth" ]

ctry_lvls = cob_m_d_lam[order(as.numeric(`Percentage`), decreasing = T), `Country of birth`]

# make country of birth an ordered factor
cob_m_d[, `Country of birth` := ordered(`Country of birth`, levels = ctry_lvls)]

# Make percentage numeric
cob_m_d[, `Percentage` := as.numeric(`Percentage`)]

# Order cob_m_d by Area name and Country of birth
cob_m_d <- cob_m_d[order(`Area name`, `Country of birth`)]

#cob_m_d[, `Country of birth`]

# Make a graph of population by country of birth and Area name by percentage with ggplot2
cob_m_d_graph = ggplot(cob_m_d, aes(x = `Country of birth`,
y = `Percentage`,
fill= `Area name`,
                        text= paste("Area name: ", `Area name`, "<br>",
                        "Country of birth: ", `Country of birth`, "<br>",
                        "Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
                        "Percentage: ", `Percentage`, "%"))
                        ) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  #coord_cartesian(ylim = c(0, 1)) +
  ylab("Population (%)")

cob_m_d_graph

# Passports held --------------------------------------------------------------------
# Load data
pass <- read.xlsx(paste(c(dir_stub, "../data/Key demographics/Census 2021/TS005_passports_census_2021.xlsx"),collapse = ''),
       startRow = 7) %>% data.table()

# remove first and last row
pass <- pass[-c(1, nrow(pass)),]

# Remove anything before : in column names
names(pass) = gsub(".*:", "", names(pass))

# Replace . in column names with ' '
names(pass) = gsub("\\.", " ", names(pass))

# Rename columns with X in them as 'percentage' concatenated with the column name to the left
names(pass)[3] = "Lambeth %"
names(pass)[5] = "England %"
names(pass)[7] = "London %"

# Replace anything before the last ':' in Passports held with ''
pass[, `Passports held` := gsub(".*:", "", `Passports held`)]

# melt dataframe with passports held as id variable
pass_m <- melt(pass, id.vars = "Passports held", variable.name="Area name", value.name = "Value") %>% as.data.table()

# make a column for 'Area name' that contains a % sign
pass_m[, Percentage := grepl("%", `Area name`)]

# replace ' %' with '' in Area name column
pass_m[, `Area name` := gsub(" %", "", `Area name`)]

# dcast dataframe with Percentage as id variable
pass_m_d <- dcast(pass_m, `Area name` + `Passports held` ~ Percentage, value.var = "Value")

# rename 'FALSE' to 'Population' and 'TRUE' to 'Percentage'
names(pass_m_d)[3] = "Population"
names(pass_m_d)[4] = "Percentage"

# make Area an ordered factor Lambeth, London, England
pass_m_d[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England"))]

# Make percentage numeric
pass_m_d[, `Percentage` := as.numeric(`Percentage`)]

# Create variable of descending Passports held for Lambeth by Percentage
pass_m_d_lam = pass_m_d[`Area name` == "Lambeth" ]



pass_lvls = pass_m_d_lam[order(as.numeric(`Percentage`), decreasing = T), `Passports held`]

# Keep top 15 levels
pass_lvls = pass_lvls[1:20]
pass_m_d <- pass_m_d[`Passports held` %in% pass_lvls]

# make Passports held an ordered factor
pass_m_d[, `Passports held` := ordered(`Passports held`, levels = pass_lvls)]

# Order pass_m_d by Area name and Passports held
pass_m_d <- pass_m_d[order(`Area name`, `Passports held`)]

# Make a graph of population by Passports held and Area name by percentage with ggplot2
pass_m_d_graph = ggplot(pass_m_d, aes(y = `Passports held`,
x = `Percentage`,
fill= `Area name`,
                        text= paste("Area name: ", `Area name`, "<br>",
                        "Passports held: ", `Passports held`, "<br>",
                        "Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
                        "Percentage: ", `Percentage`, "%"))
                        ) +

  geom_col(position = position_dodge2(width = 1, reverse = T)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.y = element_blank(),
  axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 50), limits = rev(levels(pass_m_d$`Passports held`))) +
  #coord_cartesian(ylim = c(0, 1)) +
  xlab("Population (%)")

pass_m_d_graph

# Sex by age ---------------------------------------------------------------------------------------------
# Load data
#sex_age <- read.xlsx(,startRow = 9) %>% data.table()

file_path <- paste(c(dir_stub, "../data/Key demographics/Census 2021/TS009_sex_pop_age_census_2021.xlsx"),collapse = '')


pull_tables <- function(path, start_row, area_name = NULL){
  # Load data
  df <- read.xlsx(path, startRow = start_row) %>% data.table()

  # cut table at first blank row
  df <- df[1:which(is.na(df[,2]))[1]-1,]
  df <- df[Age != "Total: All usual residents"]

  # Remove . from columns
  names(df) = gsub("\\.", " ", names(df))
  
  print(df)

  # Make columns with numbers numeric
  df[, `All persons` := as.numeric(`All persons`)]
  df[, `Male` := as.numeric(`Male`)]
  df[, `Female` := as.numeric(`Female`)]

  # Make percentage columns, Female and Male divided by All persons
  df[, `Percentage Female` := (`Female` / `All persons`)*100]
  df[, `Percentage Male` := (`Male` / `All persons`)*100]
  df[, `Percentage All persons` := (`All persons` / sum(`All persons`,na.rm=T))*100]
 
  df[, `Area name` := area_name]

  # Make an Age group column based on Age that contains just the numbers in the variable
  df[, `Age group` := as.character(stringr::str_extract_all(`Age`, "[0-9]+" ))]
  df[,`Age group` := gsub(", ", "-", `Age group`)]

  # Replace c( in Age group with ''
  df[, `Age group` := gsub("c\\(", "", `Age group`)]
  # replace quote marks in Age group with ''
  df[, `Age group` := gsub("\"", "", `Age group`)]
  # replace ) in Age group with ''
  df[, `Age group` := gsub("\\)", "", `Age group`)]

  df[, `Min age` := as.numeric(stringr::str_extract(`Age`, "[0-9]+" ))]

  # remove first row
  df <- df[-1,]

  # Sort df ascending by Min age
  df <- df[order(`Min age`)]

  # Replace 'Aged 4 and under' in Age with '0-4'
  df[, `Age` := gsub("Aged 4 years and under", "0-4", `Age`)]

  # Replace 'Aged' in Age with ''
  df[, `Age` := gsub("Aged ", "", `Age`)]

  # Replace 'years' in Age with ''
  df[, `Age` := gsub(" years", "", `Age`)]

  # Replace 'and over' in Age with '+'
  df[, `Age` := gsub(" and over", "+", `Age`)]

   # Replace ' to ' in Age with '-'
  df[, `Age` := gsub(" to ", "-", `Age`)]

  df[,Age := ordered(Age, levels = Age)]
  
}

lam <- pull_tables(file_path, 9, "Lambeth")
eng <- pull_tables(file_path, 35, "England")
lon <- pull_tables(file_path, 61, "London")

lam$`All persons`/sum(lam$`All persons`)

# combine the three tables
comb_age <- rbind(lam, eng, lon)

# Make Area name an ordered factor Lambeth, London, England
comb_age[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England"))]

# Create column total all persons as sum of All persons by Area name
comb_age[, `Total all persons` := sum(`All persons`, na.rm = T), by =.(`Area name`)]

# Create Percentage Female all persons as Percentage Female/Total all persons
comb_age[, `Percentage female all persons` := round((Female / `Total all persons`) * 100, 1)]
comb_age[, `Percentage male all persons` := round((Male / `Total all persons`) * 100, 1)]



# Make a graph of population by Age group and Area name by Female/Male with ggplot2
comb_graph <- ggplot(comb_age, aes(x = `Percentage female all persons`, y = `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",  "Age: ",
`Age`, "<br>", 
 "Female population: ", prettyNum(`Female`,big.mark=",", preserve.width="none"), "<br>",
 "Female %: ", round(`Percentage Female`,1), "<br>",
 #"Female % of all persons: ", round(`Percentage female all persons`,1), "<br>",
 "Male population: ", prettyNum(`Male`,big.mark=",", preserve.width="none"), "<br>",
 #"Male % of all persons: ", round(`Percentage male all persons`,1), "<br>",
 "Male %: ", round(`Percentage Male`,1) 
 )) ) +
geom_col(position = position_dodge2(width = 0.9, reverse=TRUE)) +
#geom_col(aes(-`Percentage male all persons`), position = position_dodge2(width = 0.9), reverse=TRUE) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  #theme_lam() +
  theme_lam() +
  theme(axis.title.y = element_blank(),
  legend.position="none",
  panel.margin=unit(.05, "lines"),
  panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5)#, 
  #strip.background = element_rect(color = "grey", size = 0.5)
  ) +
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(comb_age$`Area name`))) +
  scale_x_continuous(labels = scales::comma, name = "Female population") +
  #coord_cartesian(ylim = c(0, 1)) +
  facet_grid(rows = vars(`Age`)) #+
  #facet_wrap(~`Age`, scale = "free_x", nrow=3) #+
  #xlab("Male population (left), female population (right)")

  comb_graph



  # Make a graph of population by Age group and Area name by All persons with ggplot2
comb_all_graph <- ggplot(comb_age, aes(x = `Percentage All persons`, y =`Area name` , fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",  "Age: ",
`Age`, "<br>", 
 "All persons population: ", prettyNum(`All persons`,big.mark=",", preserve.width="none"), "<br>",
 "All persons percentage: ", round(`Percentage All persons`,1)
 ))) +
geom_col(position = position_dodge2(width = 0.9, reverse = TRUE)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  #theme_lam() +
  theme_lam() +
  theme(axis.title.y = element_blank(),
  legend.position="none") +
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(comb_age$`Area name`))) +
  scale_x_continuous(labels = scales::comma, name = "Age group population (% of total population)") +
  #coord_cartesian(ylim = c(0, 1)) +
  facet_grid(rows = vars(`Age`)) #+
  #xlab("Age group population (% of total population)")

  comb_all_graph


# Household composition ---------------------------------------------------------------------------------------------
# Load data
file_path <- paste(c(dir_stub, "../data/Key demographics/Census 2021/TS003_household_composition_census_2021.xlsx"),collapse = '')

household_comp <- read.xlsx(file_path,startRow = 7) %>% data.table()

# remove first row
household_comp <- household_comp[-1,]

# Remove . from columns
names(household_comp) = gsub("\\.", " ", names(household_comp))


# Remove everything before : in column names
names(household_comp) = gsub(".*:", "", names(household_comp))

# Remove rows where Lambeth is NA
household_comp <- household_comp[!is.na(Lambeth),]

# Replace column names with X in
names(household_comp) = gsub("X3", "Lambeth %", names(household_comp))
names(household_comp) = gsub("X5", "England %", names(household_comp))
names(household_comp) = gsub("X7", "London %", names(household_comp))

# Melt with Household composition as ID variable
pass_m <- melt(pass, id.vars = "Passports held", variable.name="Area name", value.name = "Value") %>% as.data.table()


household_comp_m <- melt(household_comp, id.vars = "Household composition") 

# Create percentage column - rows with variable containing %
household_comp_m[, `Percentage` := ifelse(grepl("%", variable), TRUE, FALSE)]

# Remove % from variable
household_comp_m[, `Area name` := gsub(" %", "", variable)]

# dcast on percentage column
household_comp_m_d <- dcast(household_comp_m, `Area name` + `Household composition` ~ Percentage, value.var = "value")

# rename columns false and true
names(household_comp_m_d) = gsub("FALSE", "Population", names(household_comp_m_d))
names(household_comp_m_d) = gsub("TRUE", "Percentage", names(household_comp_m_d))

# Make Population an integer aand Percentage a numeric
household_comp_m_d[, `Population` := as.integer(`Population`)]
household_comp_m_d[, `Percentage` := as.numeric(`Percentage`)]

# Make Area name an ordered factor Lambeth, London, England
household_comp_m_d[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England"))]

# Remove total row
household_comp_m_d <- household_comp_m_d[!grepl("Total", `Household composition`),]

# Keep rows without : in Household composition
household_comp_m_d_l1 <- household_comp_m_d[!grepl(":", `Household composition`),]

# Filter to Lambeth
household_comp_m_d_l1_lam <- household_comp_m_d_l1[grepl("Lambeth", `Area name`),]

# order descending by percentage
household_comp_m_d_l1_lam <- household_comp_m_d_l1_lam[order(-`Percentage`)]

l1_cats_order = household_comp_m_d_l1_lam$`Household composition`

# Make household_comp_m_d_l1 Household composition an ordered factor
household_comp_m_d_l1[, `Household composition` := ordered(`Household composition`, levels = l1_cats_order)]

# Keep rows with : in Household composition
household_comp_m_d_l2 <- household_comp_m_d[grepl(":", `Household composition`),]

# Filter to Lambeth
household_comp_m_d_l2_lam <- household_comp_m_d_l2[grepl("Lambeth", `Area name`),]

# order descending by percentage
household_comp_m_d_l2_lam <- household_comp_m_d_l2_lam[order(-`Percentage`)]

l2_cats_order = household_comp_m_d_l2_lam$`Household composition`

# In household_comp_m_d, make column 'Household composition level 1' from Household composition column as all before :
household_comp_m_d[, `Household composition level 1` := gsub(":.*", "", `Household composition`)]

# In household_comp_m_d, make column 'Household composition level 2' from Household composition column
household_comp_m_d[, `Household composition level 2` := gsub(".*: ", "", `Household composition`)]

# Make household_comp_m_d_l2 Household composition an ordered factor
household_comp_m_d_l2[, `Household composition` := ordered(`Household composition`, levels = l2_cats_order)]

# Make graph of Household composition by Area name with ggplot2
household_comp_l1_graph <- ggplot(household_comp_m_d_l1, aes(x = `Household composition`, y = `Percentage`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",  "Household composition: ",
`Household composition`, "<br>", 
 "Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
 "Percentage: ", round(`Percentage`,1)
 ))) +
geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  #theme_lam() +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  scale_y_continuous(labels = scales::comma, name = "Household composition population (% of total population)")# +
  #coord_cartesian(ylim = c(0, 1)) +
  #facet_grid(rows = vars(`Area name`)) #+
  #xlab("Household composition population (% of total population)")

  household_comp_l1_graph


  # Make graph of Household composition by Area name with ggplot2
household_comp_l2_graph <- ggplot(household_comp_m_d_l2, aes(y = `Household composition`, x = `Percentage`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",  "Household composition: ",
`Household composition`, "<br>", 
 "Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
 "Percentage: ", round(`Percentage`,1)
 ))) +
geom_col(position = position_dodge2(width = 1, reverse = T)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  #theme_lam() +
  theme_lam() +
  theme(axis.title.y = element_blank()) +
  #axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(household_comp_m_d_l2$`Household composition`))) +
  scale_x_continuous(labels = scales::comma, name = "Household composition population (% of total population)")# +
  #coord_cartesian(ylim = c(0, 1)) +
  #facet_grid(rows = vars(`Area name`)) #+
  #xlab("Household composition population (% of total population)")

  household_comp_l2_graph

# Ethnicity --------------------------------------------------------------------
# Load data


eth_d <- l_dat(paste(c(dir_stub, "../data/Key demographics/Census 2021/TS022_ethnic_group_census_2021.xlsx"), collapse = ""), 7)[[1]]

# rename Ethnic group (detailed) column to 'Ethnic group'
setnames(eth_d, "Ethnic group (detailed)", "Ethnic group")


# Make a graph of eth_d with ggplot2
eth_graph <- ggplot(eth_d, aes(y = `Ethnic group`, x = `Percentage`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Ethnic group: ", `Ethnic group`, "<br>",
"Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
"Percentage: ", `Percentage`, "%"))
) +
geom_col(position = position_dodge2(width = 0.9, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.y = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(eth_d$`Ethnic group`))) +
scale_x_continuous(labels = scales::comma) +
xlab("Percentage of population (%)")

eth_graph

# Religion --------------------------------------------------------------------
# Load data
rel_d <- l_dat(paste(c(dir_stub, "../data/Key demographics/Census 2021/TS031_religion_census_2021.xlsx"), collapse = ""), 7)[[1]]

# Rename Religion column to 'Religion'
setnames(rel_d, "Religion (detailed)", "Religion")

# Make a graph of rel_d with ggplot2
rel_graph <- ggplot(rel_d, aes(y = `Religion`, x = `Percentage`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Religion: ", `Religion`, "<br>",
"Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
"Percentage: ", `Percentage`, "%"))
) +
geom_col(position = position_dodge2(width = 0.9, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.y = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(rel_d$`Religion`))) +
scale_x_continuous(labels = scales::comma) +
xlab("Percentage of population (%)")

rel_graph

# Main language --------------------------------------------------------------------
# Load data
lang_d <- l_dat(paste(c(dir_stub, "../data/Key demographics/Census 2021/TS024_main_language_census_2021.xlsx"), collapse = ""), 7)[[1]]

names(lang_d)

# Rename Language column to 'Main language'
setnames(lang_d, "Main language (detailed)", "Main language")

# Make a graph of lang_d with ggplot2
lang_graph <- ggplot(lang_d, aes(y = `Main language`, x = `Percentage`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Main language: ", `Main language`, "<br>",
"Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
"Percentage: ", `Percentage`, "%"))
) +
geom_col(position = position_dodge2(width = 0.9, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.y = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(lang_d$`Main language`))) +
scale_x_continuous(labels = scales::comma) +
xlab("Percentage of population (%)")

lang_graph

# English language proficiency --------------------------------------------------------------------

# Load data
eng_out <- l_dat(paste(c(dir_stub, "../data/Key demographics/Census 2021/TS029_english_profic_census_2021.xlsx"), collapse = ""), 7)

eng_d <- eng_out[[1]]

eng_cats <- eng_out[[2]]
eng_cats

# Filter out 'Main language is not English (English or Welsh in Wales)' from eng_d
eng_d <- eng_d[eng_d$`Proficiency in English language` != "Main language is not English (English or Welsh in Wales)",]

# Remove "Main language is not English (English or Welsh in Wales)" from eng_cats
eng_cats <- eng_cats[! eng_cats %in% "Main language is not English (English or Welsh in Wales)"]
eng_cats

# Set eng_d$`Proficiency in English language` as an ordered factor with levels eng_cats
eng_d$`Proficiency in English language` <- factor(eng_d$`Proficiency in English language`, levels = eng_cats)


# Make a graph of eng_d with ggplot2
eng_graph <- ggplot(eng_d, aes(y = `Proficiency in English language`, x = `Percentage`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Proficiency in English language: ", `Proficiency in English language`, "<br>",
"Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
"Percentage: ", `Percentage`, "%"))
) +
geom_col(position = position_dodge2(width = 0.9, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.y = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(eng_d$`Proficiency in English language`))) +
scale_x_continuous(labels = scales::comma) +
xlab("Percentage of population (%)")

eng_graph

# Sexual orientation ------------------------------------------------

# Load data
ori_out <- l_dat(paste(c(dir_stub, "../data/Key demographics/Census 2021/TS077_sexual_orientation_census_2021.xlsx"), collapse = ""), 7)

ori_d <- ori_out[[1]]
ori_cats <- ori_out[[2]]

# Remove 'Straight or Heterosexual' from gender identity categories
ori_cats_short <- ori_cats[! ori_cats %in% "Straight or Heterosexual"]


# Set ori_d$`Sexual orientation` as an ordered factor with levels ori_cats
ori_d$`Sexual orientation` <- factor(ori_d$`Sexual orientation`, levels = ori_cats)


# Make a graph of ori_d with ggplot2
ori_graph <- ggplot(ori_d[! `Sexual orientation` %in% "Straight or Heterosexual"], aes(y = `Sexual orientation`, x = `Percentage`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Sexual orientation: ", `Sexual orientation`, "<br>",
"Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
"Percentage: ", `Percentage`, "%"))
) +
geom_col(position = position_dodge2(width = 0.9, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.y = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(ori_cats_short)) +
scale_x_continuous(limits=c(0,10)) + #labels = scales::comma) +
xlab("Percentage of respondents (%)")

ori_graph

# Gender identity ------------------------------------------------

# Load data
gen_out <- l_dat(paste(c(dir_stub, "../data/Key demographics/Census 2021/TS078_gender_identity_census_2021.xlsx"), collapse = ""), 7, percentage_cut_off = 0)

gen_d <- gen_out[[1]]

gen_cats <- gen_out[[2]]

# Remove 'Gender identity the same as sex registered at birth' from gender identity categories
gen_cats_short <- gen_cats[! gen_cats %in% "Gender identity the same as sex registered at birth"]

# Set gen_d$`Gender identity` as an ordered factor with levels gen_cats
gen_d$`Gender identity` <- factor(gen_d$`Gender identity`, levels = gen_cats)

# Make a graph of gen_d with ggplot2
gen_graph <- ggplot(gen_d[!`Gender identity` %in% "Gender identity the same as sex registered at birth"], aes(y = `Gender identity`, x = `Percentage`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Gender identity: ", `Gender identity`, "<br>",
"Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
"Percentage: ", `Percentage`, "%"))
) +
geom_col(position = position_dodge2(width = 0.9, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.y = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(gen_cats_short)) +
scale_x_continuous(limits=c(0,10)) + #labels = scales::comma) +
xlab("Percentage of respondents (%)")

gen_graph

# Number of residents ------------------------------------------------






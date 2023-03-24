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

# Inner London df ---------------------------------------------------------
## Create inner london df
inner_london <- fread(paste(c(dir_stub, "../data/inner_london_boroughs.csv"), collapse = "")) %>% data.table()

# Strip whitespace from Borough
inner_london[,`Borough` := trimws(`Borough`)]


# Population NOT CENSUS --------------------------------------------------------------
pop <- read.xlsx(paste(c(dir_stub, "../data/Jobs/NOMIS/Total population simple 2020.xlsx"),collapse = ''),
       startRow = 2) %>% data.table()
pop

pop_df= pop[2:4]
pop_df

pop_df_m = melt(pop_df, id.vars = "X1") %>% as.data.table()
names(pop_df_m) = c("Sex", "Area name", "Population")

pop_tot <- pop_df_m[Sex=="All People"][, Sex:= NULL]

# Tenure type -------------------------------------------------------------------------

tenure_out <- l_dat(paste(c(dir_stub, "../data/Housing infrastructure/Tenure/Tenure_census_2021_TS054.xlsx"), collapse = ""), 7, percentage_cut_off = 0)

tenure_d <- tenure_out[[1]]

tenure_cats <- tenure_out[[2]]

# Set gen_d$`Tenure of household` as an ordered factor with levels gen_cats
tenure_d$`Tenure of household` <- factor(tenure_d$`Tenure of household`, levels = tenure_cats)

# Make a graph
tenure_graph <- ggplot(tenure_d, aes(y = `Tenure of household`, x = `Percentage`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Tenure type: ", `Tenure of household`, "<br>",
"Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
"Percentage: ", `Percentage`, "%"))
) +
geom_col(position = position_dodge2(width = 0.9, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.y = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(tenure_cats)) +
#scale_x_continuous(limits=c(0,10)) + #labels = scales::comma) +
xlab("Percentage of population (%)")

tenure_graph

# Road traffic-------------------------------------------------------------------------

# Load data
road_traffic <- fread(paste(c(dir_stub, "../data/Housing infrastructure/Road traffic/local_authority_traffic.csv"),collapse = '')) %>%
                      data.table()

road_traffic

# Rename name to 'Area name'
setnames(road_traffic, "name", "Area name")

# Filter year to latest
road_traffic <- road_traffic[year == max(year),]

# Create df from Lambeth
road_traffic_lambeth <- road_traffic[`Area name` == "Lambeth",]

# Sum up link_length_km and all_motor_vehicles for Lambeth
road_traffic_lambeth <- road_traffic_lambeth[, .(`Area name` = "Lambeth",
                                               link_length_km = sum(link_length_km),
                                               all_motor_vehicles = sum(all_motor_vehicles))]

# Create variable for all motor vehicles per km per day in Lambeth
road_traffic_lambeth[,`Number of motor vehicles per km per day` := 
                                        round((all_motor_vehicles / link_length_km)/365.25,0)]




# Create London df from ONS_code starts with 'E09'
road_traffic_london <- road_traffic[grepl("^E09", ONS_code),]

# Sum up link_length_km and all_motor_vehicles for London
road_traffic_london_out <- road_traffic_london[, .(`Area name` = "London",
                                               link_length_km = sum(link_length_km),
                                               all_motor_vehicles = sum(all_motor_vehicles))]

# Create variable for all motor vehicles per km per day in London
road_traffic_london_out[,`Number of motor vehicles per km per day` := 
                                        round((all_motor_vehicles / link_length_km)/365.25,0)]


# Join inner_london to tree
road_traffic_london_inner <- merge(road_traffic, inner_london, by.x = "Area name", by.y = "Borough", all.x = FALSE)

# Remove 'City of London'
road_traffic_london_inner <- road_traffic_london_inner[!`Area name` == "City of London",]

# Sum up link_length_km and all_motor_vehicles for England
road_traffic_london_inner <- road_traffic_london_inner[, .(`Area name` = "Inner London",
                                               link_length_km = sum(link_length_km),
                                               all_motor_vehicles = sum(all_motor_vehicles))]

# Create variable for all motor vehicles per km per day in England
road_traffic_london_inner[,`Number of motor vehicles per km per day` := 
                                        round((all_motor_vehicles / link_length_km)/365.25,0)]


## Create England df from ONS_code starts with 'E'
road_traffic_england <- road_traffic[grepl("^E", ONS_code),]

# Sum up link_length_km and all_motor_vehicles for England
road_traffic_england <- road_traffic_england[, .(`Area name` = "England",
                                               link_length_km = sum(link_length_km),
                                               all_motor_vehicles = sum(all_motor_vehicles))]

# Create variable for all motor vehicles per km per day in England
road_traffic_england[,`Number of motor vehicles per km per day` := 
                                        round((all_motor_vehicles / link_length_km)/365.25,0)]


# Combine all dfs
road_traffic_out <- rbindlist(list(road_traffic_lambeth, road_traffic_london_inner, road_traffic_london_out, road_traffic_england))

# Make area name an ordered factor with order Lambeth, Inner London, London, England
road_traffic_out$`Area name` <- ordered(road_traffic_out$`Area name`, levels = c("Lambeth", "Inner London", "London", "England"))

# Rename link_length_km to 'Road length (km)' and all_motor_vehicles to 'Number of motor vehicles'
setnames(road_traffic_out, "link_length_km", "Road length (km)")
setnames(road_traffic_out, "all_motor_vehicles", "Number of motor vehicles")

# Round road length to 0 decimal places
road_traffic_out[,`Road length (km)` := round(`Road length (km)`,0)]

# Make a graph of Number of motor vehicles per km per day in ggplot2
road_traffic_graph <- ggplot(road_traffic_out, aes(x = `Area name`, y = `Number of motor vehicles per km per day`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Road length (km): ", prettyNum(`Road length (km)`,big.mark=",", preserve.width="none"), "<br>",
"Number of motor vehicles: ", prettyNum(`Number of motor vehicles`,big.mark=",", preserve.width="none"), "<br>",
"Number of motor vehicles per km per day: ", `Number of motor vehicles per km per day`))
) +
geom_col(position = position_dodge2(width = 0.9)) +#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30))+#, limits = rev(road_traffic_out$`Area name`)) +
scale_y_continuous(labels = scales::comma) +
#scale_x_continuous(limits=c(0,10)) + #labels = scales::comma) +
ylab("Number of motor vehicles per km per day")

road_traffic_graph 

# House prices-------------------------------------------------------------------------

# Load data
house_prices_reg <- read.xlsx(paste(c(dir_stub, "../data/Housing infrastructure/House price/hpssadataset9medianpricepaidforadministrativegeographies.xlsx"),collapse = ''), sheet = "1a", startRow = 4) %>%
                      data.table()

# Replace . in column names with " "
setnames(house_prices_reg, names(house_prices_reg), gsub("\\.", " ", names(house_prices_reg)))


# Keep columns Name and Year ending Jun 2022
house_prices_reg <- house_prices_reg[, .(`Name`, `Year ending Jun 2022`)]

# Filter to London and England
house_prices_reg <- house_prices_reg[grepl("London", `Name`) | grepl("^England$", `Name`),]

# Rename Name to Area name
setnames(house_prices_reg, "Name", "Area name")


#

house_prices_lam <- read.xlsx(paste(c(dir_stub, "../data/Housing infrastructure/House price/hpssadataset9medianpricepaidforadministrativegeographies.xlsx"),collapse = ''), sheet = "2a", startRow = 4) %>%
                      data.table()

# Replace . in column names with " "
setnames(house_prices_lam, names(house_prices_lam), gsub("\\.", " ", names(house_prices_lam)))

# Keep columns Name and Year ending Jun 2022
house_prices_lam <- house_prices_lam[, .(`Local authority name`, `Year ending Jun 2022`)]

# Filter to Lambeth
house_prices_lam <- house_prices_lam[grepl("Lambeth", `Local authority name`),]

# Rename Local authority name to Area name
setnames(house_prices_lam, "Local authority name", "Area name")

# Combine dfs
house_prices_out <- rbindlist(list(house_prices_lam, house_prices_reg))

# Make area name an ordered factor with order Lambeth, London, England
house_prices_out$`Area name` <- ordered(house_prices_out$`Area name`, levels = c("Lambeth", "London", "England"))

house_prices_out

# Rename Year ending Jun 2022 to 'Median house price (£)'
setnames(house_prices_out, "Year ending Jun 2022", "Median house price (£, 1000s)")

# Divide by 1000 to get thousands
house_prices_out[,`Median house price (£, 1000s)` := round(`Median house price (£, 1000s)`/1000,0)]


# Make a graph of Median house price (£) in ggplot2
house_prices_graph <- ggplot(house_prices_out, aes(x = `Area name`, y = `Median house price (£, 1000s)`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Median house price (£, 1000s): ", `Median house price (£, 1000s)`))
) +
geom_col(position = position_dodge2(width = 0.9)) +#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30))+#, limits = rev(road_traffic_out$`Area name`)) +
#scale_x_continuous(limits=c(0,10)) + #labels = scales::comma) +
ylab("Median house price (£, 1000s)")

house_prices_graph

# Number of households-------------------------------------------------------------------------

# Load data
#households_reg <- read.xlsx(paste(c(dir_stub, "../data/Housing infrastructure/Households/households.xlsx"),collapse = ''), sheet = "1a", startRow = 4) %>%
 #                     data.table()

# Housebuilding-------------------------------------------------------------------------

hb_sheet_name <- "2021-22"

# Load data
hb <- read.xlsx(paste(c(dir_stub, "../data/Housing infrastructure/House building/houses_started_completed_80_22.xlsx"),collapse = ''), sheet = hb_sheet_name, startRow = 4) %>%
                      data.table()

# Replace . in column names with " "
setnames(hb, names(hb), gsub("\\.", " ", names(hb)))

# Keep columns X1, X6, Dwellings completed, X12, X13, X14
hb <- hb[, .(`X1`, `X6`, `Dwellings completed`, `X12`, `X13`, `X14`)]

new_names = as.character(hb[1,])

# replace NA in nw names with 'Region'
new_names[is.na(new_names)] <- "Region"

# Replace \n in names with " "
new_names <- gsub("\\n", " ", new_names)

# Make first row column names
setnames(hb, names(hb), new_names)

# Remove first row
hb <- hb[-1,]

# Filter to Lambeth,  London and England
hb_f <- hb[grepl("Lambeth", `Lower and Single Tier Authority Data`) | grepl("London", `Region`) | grepl("England", `Region`),]

# Create Area name column from Region and Lower and Single Tier Authority Data
hb_f[, `Area name` := paste(`Region`, `Lower and Single Tier Authority Data`, sep = " ")]

# Remove 'NA' from Area name
hb_f[, `Area name` := gsub("NA ", "", `Area name`)]
hb_f[, `Area name` := gsub(" NA", "", `Area name`)]

# Replace ' Boroughs' in Area name with ''
hb_f[, `Area name` := gsub(" Boroughs", "", `Area name`)]

# Remove Region and Lower and Single Tier Authority Data columns
hb_f <- hb_f[, -c(1,2)]

# Strip whitespace from Area name
hb_f[, `Area name` := trimws(`Area name`)]

# Make Area name an ordered factor with order Lambeth, London, England
hb_f$`Area name` <- ordered(hb_f$`Area name`, levels = c("Lambeth", "London", "England"))

# Melt data on Area name
hb_f_m <- melt(hb_f, id.vars = "Area name", variable.name = "House type", value.name = "Number of houses")

# Join on total population
hb_f_m <- merge(hb_f_m, pop_tot, by = "Area name")

# # Filter to 'All'
hb_f_m <- hb_f_m[grepl("All", `House type`),]

# Make columns numeric
hb_f_m[, `Number of houses` := as.numeric(`Number of houses`)]
hb_f_m[, `Population` := as.numeric(`Population`)]

# Divide number of houses by total population/1000 to get proportion. Round to 0 dp
hb_f_m[, `Number of houses completed per 1000 people` := round(`Number of houses`/(`Population`/1000),2)]

# Make a column of year from hb_sheet_name
hb_f_m[, `Year` := hb_sheet_name]

# Make a graph of Number of houses completed per 1000 people in ggplot2
hb_graph <- ggplot(hb_f_m, aes(x = `Area name`, y = `Number of houses completed per 1000 people`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Year: ", `Year`, "<br>",
"Number of houses: ", prettyNum(`Number of houses`,big.mark=",", preserve.width="none"), "<br>",
"Population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
"Number of houses completed per 1000 people: ", `Number of houses completed per 1000 people`))
) +
geom_col(position = position_dodge2(width = 0.9)) +#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30))+#, limits = rev(road_traffic_out$`Area name`)) +
#scale_x_continuous(limits=c(0,10)) + #labels = scales::comma) +
ylab("Number of houses completed per 1000 people")

hb_graph

# Council tax bands-------------------------------------------------------------------------

# Load data
ctb <- read.xlsx(paste(c(dir_stub, "../data/Housing infrastructure/Council tax/CTSOP1.0_SUP.xlsx"),collapse = ''), sheet = "CTSOP1.0_SUP", startRow = 4) %>% data.table()

# Replace . in column names with " "
setnames(ctb, names(ctb), gsub("\\.", " ", names(ctb)))

names(ctb)

# Filter to Lambeth,  London and England
ctb_f <- ctb[grepl("Lambeth", `Area Name`,ignore.case=TRUE) | grepl("^London$", `Area Name`,ignore.case=TRUE) | grepl("^England$", `Area Name`,ignore.case=TRUE),]

# Sentence case Area Name
ctb_f[, `Area Name` := stringr::str_to_sentence(`Area Name`)]

# Remove first three columns
ctb_f <- ctb_f[, -c(1:3)]

# Remove column I
ctb_f <- ctb_f[, -c(10)]

num_names <- names(ctb_f)[2:10]

# Make all columns apart from area name numeric
ctb_f_n = ctb_f[, lapply(.SD, as.numeric), .SDcols = num_names]

ctb_f_n[,`Area name` := ctb_f$`Area Name`]

# Use dplyr to put Area Name at start of df
ctb_f_n <- ctb_f_n %>% select(`Area name`, everything())

names(ctb_f)

# Divide all numeric columns by All properties to get proportion
ctb_f_n[, `A` := round(`A`/`All properties`*100,1)]
ctb_f_n[, `B` := round(`B`/`All properties`*100,1)]
ctb_f_n[, `C` := round(`C`/`All properties`*100,1)]
ctb_f_n[, `D` := round(`D`/`All properties`*100,1)]
ctb_f_n[, `E` := round(`E`/`All properties`*100,1)]
ctb_f_n[, `F` := round(`F`/`All properties`*100,1)]
ctb_f_n[, `G` := round(`G`/`All properties`*100,1)]
ctb_f_n[, `H` := round(`H`/`All properties`*100,1)]

# Remove all properties column
ctb_f_n <- ctb_f_n[, -c(10)]

# Melt data on Area name
ctb_f_m <- melt(ctb_f_n, id.vars = "Area name", variable.name = "Council tax band", value.name = "Percentage of properties")

# Make Area name an ordered factor with order Lambeth, London, England
ctb_f_m$`Area name` <- ordered(ctb_f_m$`Area name`, levels = c("Lambeth", "London", "England"))

# Make a graph of Percentage of properties in ggplot2
ctb_graph <- ggplot(ctb_f_m, aes(x = `Council tax band`, y = `Percentage of properties`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Council tax band: ", `Council tax band`, "<br>",
"Percentage of properties: ", `Percentage of properties`, "%")
)) +
geom_col(position = position_dodge2(width = 0.9)) +#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30))+#, limits = rev(road_traffic_out$`Area name`)) +
#scale_x_continuous(limits=c(0,10)) + #labels = scales::comma) +
ylab("Percentage of properties") +
facet_wrap(~`Area name`)

ctb_graph

# Public transport accessibility levels (PTAL)-------------------------------------------------------------------------

# Load data
ptal <- fread(paste(c(dir_stub, "../data/Housing infrastructure/PTAL/Borough AvPTAI2015.csv"),collapse = '')) %>% data.table()

# Join on inner london boroughs
ptal_m <- merge(ptal, inner_london, by.x = "Borough Name", by.y = "Borough", all.x = TRUE)

# Remove rows where Position = city of london
ptal_m <- ptal_m[!grepl("City of London", Position,ignore.case=TRUE),]

# Replace NA in position with 'Outer London'
ptal_m[is.na(Position), Position := "Outer London"]


# Create ptal_lam for Lambeth
ptal_lam <- ptal_m[grepl("Lambeth", `Borough Name`,ignore.case=TRUE),]

# Rename column 'Borough Name' to 'Area name'
setnames(ptal_lam, "Borough Name", "Area name")

# Keep only columns 'Area name' and 'AvPTAL2015'
ptal_lam <- ptal_lam[, c("Area name", "AvPTAI2015")]

# Average of AvPTAL2015 for all Boroughs
ptal_lon <- ptal_m[, .(AvPTAI2015 = mean(`AvPTAI2015`)), by = .(Position)]

# Rename column 'Position' to 'Area name'
setnames(ptal_lon, "Position", "Area name")

# Combine ptal_lam and ptal_lon
ptal_out <- rbind(ptal_lam, ptal_lon)

# Make Area name an ordered factor with order Lambeth, Inner London, Outer London
ptal_out$`Area name` <- ordered(ptal_out$`Area name`, levels = c("Lambeth", "Inner London", "Outer London"))

# Rename column 'AvPTAL2015' to 'Average PTAL score per borough 2015'
setnames(ptal_out, "AvPTAI2015", "Average borough PTAL score 2015")

ptal_out[, `Average borough PTAL score 2015` := round(`Average borough PTAL score 2015`,1)]

# Make a graph of Average PTAL score per borough 2015 in ggplot2
ptal_graph <- ggplot(ptal_out, aes(x = `Area name`, y = `Average borough PTAL score 2015`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Average borough PTAL score 2015: ", `Average borough PTAL score 2015`, "<br>"))
) +
geom_col(position = position_dodge2(width = 0.9)) +#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30))+#, limits = rev(road_traffic_out$`Area name`)) +
#scale_x_continuous(limits=c(0,10)) + #labels = scales::comma) +
ylab("Average borough PTAL score 2015")

ptal_graph

# Dwelling types-------------------------------------------------------------------------

# Load data
dwelling_types <- read.xlsx(paste(c(dir_stub, "../data/Housing infrastructure/Households/LT_100_dwellings_tenure_district.xlsx"),collapse = ''), sheet = "2021", startRow = 4) %>% data.table()

# Replace . in column names with " "
setnames(dwelling_types, names(dwelling_types), gsub("\\.", " ", names(dwelling_types)))

# Filter X1 to England, London Boroughs, or NA
dwelling_types_c <- dwelling_types[grepl("England", X1, ignore.case = TRUE) | grepl("London Boroughs", X1, ignore.case = TRUE) | is.na(X1),]

# Filter Lower and Single Tier Authority Data to Lambeth or NA
dwelling_types_c <- dwelling_types_c[grepl("Lambeth", `Lower and Single Tier Authority Data`, ignore.case = TRUE) | is.na(`Lower and Single Tier Authority Data`),]

names(dwelling_types_c)

# Filter column Met and Shire County Totals to NA
dwelling_types_c <- dwelling_types_c[is.na(`Met and Shire County Totals`),]

dwelling_types_c[,`Area name` := c("England", "London", "Lambeth")]

names(dwelling_types_c)

# Keep only Area name and the numeric columns
dwelling_types_c <- dwelling_types_c[, c("Area name", "Local Authority (incl  owned by other LAs)",
"Private Registered Provider",
"Other public sector²",
"Private sector (P)1",
"Total (P)1")]

# Remove ' (P)1' from column names
setnames(dwelling_types_c, names(dwelling_types_c), gsub(" \\(P\\)1", "", names(dwelling_types_c)))

# Remove '²' from column names
setnames(dwelling_types_c, names(dwelling_types_c), gsub("²", "", names(dwelling_types_c)))

# Make all numeric columns into numeric
dwelling_types_c[, `:=`(
  `Local Authority (incl  owned by other LAs)` = as.numeric(`Local Authority (incl  owned by other LAs)`),
  `Private Registered Provider` = as.numeric(`Private Registered Provider`),
  `Other public sector` = as.numeric(`Other public sector`),
  `Private sector` = as.numeric(`Private sector`),
  `Total` = as.numeric(`Total`)
)]

# Divide numeric columns by Total and multiply by 100 to get percentages. Round to 1 dp
dwelling_types_c[, `:=`(
  `Local Authority (incl  owned by other LAs)` = round((`Local Authority (incl  owned by other LAs)`/`Total`)*100,1),
  `Private Registered Provider` = round((`Private Registered Provider`/`Total`)*100,1),
  `Other public sector` = round((`Other public sector`/`Total`)*100,1),
  `Private sector` = round((`Private sector`/`Total`)*100,1)
)]

# Round total to 0 dp
dwelling_types_c[, `Total` := round(`Total`,0)]

# Make Area name an ordered factor with order Lambeth, London, England
dwelling_types_c$`Area name` <- ordered(dwelling_types_c$`Area name`, levels = c("Lambeth", "London", "England"))

# Rename column 'Total' to 'Total number of households'
setnames(dwelling_types_c, "Total", "Total number of households")

# Melt data
dwelling_types_c_m <- melt(dwelling_types_c, id.vars = c("Area name", "Total number of households"), variable.name = "Dwelling type", value.name = "Percentage of households")

# Make a graph of Percentage of households by dwelling type in ggplot2
dwelling_types_graph <- ggplot(dwelling_types_c_m, aes(x = `Dwelling type`, y = `Percentage of households`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Dwelling type: ", `Dwelling type`, "<br>",
"Percentage of households: ", `Percentage of households`, "<br>"))
) +
geom_col(position = position_dodge2(width = 0.9)) +#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30))+#, limits = rev(road_traffic_out$`Area name`)) +
#scale_x_continuous(limits=c(0,10)) + #labels = scales::comma) +
ylab("Percentage of households")

dwelling_types_graph

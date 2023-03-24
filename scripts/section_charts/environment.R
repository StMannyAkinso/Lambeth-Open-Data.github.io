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

# Mode of transport to work --------------------------------------------------------------

# Load data
motw_out <- l_dat(paste(c(dir_stub, "../data/Environment/Travel to work/work_travel_method_TS061_Census_2021.xlsx"), collapse = ""), 7)

motw_d <- motw_out[[1]]

motw_cats <- motw_out[[2]]
motw_cats

# Make a graph of motw_d in ggplot2

motw_graph <- ggplot(motw_d, aes(y = `Method of travel to workplace`, x = `Percentage`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Method of travel to workplace: ", `Method of travel to workplace`, "<br>",
"Working population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
"Percentage: ", `Percentage`, "%"))
) +
geom_col(position = position_dodge2(width = 0.9, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.y = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(motw_d$`Method of travel to workplace`))) +
scale_x_continuous(labels = scales::comma) +
xlab("Percentage of working population (%)")

motw_graph

# Pollution --------------------------------------------------------------

load_pol <- function(sheet_name = "Population_NO2"){
  # Load data
  no2 <- read.xlsx(paste(c(dir_stub, "../data/Environment/pollutants/20211209_AQExceedingPop_RoadStats_LAEI2019.xlsx"), collapse = ""), 
                          startRow=6, rows=c(1:47), cols=c(7:13), sheet=sheet_name) %>% data.table()

  no2_d <- no2[!35:37] # Keep central, inner, outer London

  names(no2_d) = c("Area name", "Population 2016", "Population 2019", "Exceeding population 2016", "Exceeding population 2019", "Exceeding 2016 (%)", "Exceeding 2019 (%)")

  # Exceeding 2016 (%) and Exceeding 2019 (%) to numeric
  no2_d[, `Exceeding 2016 (%)` := as.numeric(`Exceeding 2016 (%)`)*100]
  no2_d[, `Exceeding 2019 (%)` := as.numeric(`Exceeding 2019 (%)`)*100]

  # Exceeding 2016 (%) and Exceeding 2019 (%) to two decimal places
  no2_d[, `Exceeding 2016 (%)` := round(`Exceeding 2016 (%)`, 1)]
  no2_d[, `Exceeding 2019 (%)` := round(`Exceeding 2019 (%)`, 1)]

  no2_d_out <- no2_d[`Area name` %in% c("Lambeth", "Central", "Inner", "Outer"), list(`Area name`, `Exceeding 2016 (%)`, `Exceeding 2019 (%)`)] %>% data.table()

  no2_d_out[,`Area name` := gsub("Inner", "Inner London", `Area name`)]
  no2_d_out[,`Area name` := gsub("Outer", "Outer London", `Area name`)]
  no2_d_out[,`Area name` := gsub("Central", "Central London", `Area name`)]

  no2_d_out[,`Area name` := ordered(`Area name`, levels = c("Lambeth", "Central London", "Inner London", "Outer London"))]

  no2_d_out_m <- melt(no2_d_out, id.vars = "Area name", variable.name = "Year", value.name="Exceeding (%)") %>% as.data.table()

  # Replace Exceeding in year column
  no2_d_out_m[, Year := gsub("Exceeding ", "", Year)]
  no2_d_out_m[, Year := gsub(" (%)", "", Year, fixed = TRUE)]


return (no2_d_out_m)

}

no2_d_out_m <- load_pol() # Lambeth experienced a 23% reduction in NO2 levels 2016-2019, resulting in a significant reduction in the population exposed to the WHO guideline value (40 ug/m3). This is still significantly above the new WHO guideline value of 10 ug/m3.
pm_d_out_m <- load_pol("Population_PM25") # Despite a 19% reduction 2016-2019, all Lambeth residents are still exposed to PM2.5 levels above the WHO guideline value (10 ug/m3)

# Make a graph of no2_d_out in ggplot2

no2_graph <- ggplot(no2_d_out_m, aes(y = `Exceeding (%)` , x = `Year`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Year: ", `Year`, "%", "<br>",
"Exceeding (%): ", `Exceeding (%)`, "%"
))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(no2_d_out_m$`Area name`))) +
scale_y_continuous(labels = scales::comma) +
ylab(bquote("Percentage of population exposed to "~NO[2]~" levels above WHO guideline value (%)"))

no2_graph

# Make a graph of pm_d_out in ggplot2

pm_graph <- ggplot(pm_d_out_m, aes(y = `Exceeding (%)` , x = `Year`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Year: ", `Year`, "%", "<br>",
"Exceeding (%): ", `Exceeding (%)`, "%"))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(pm_d_out_m$`Area name`))) +
scale_y_continuous(labels = scales::comma) +
ylab("Percentage of population exposed to PM2.5 levels above WHO guideline value (%)")

pm_graph

### Concentration of NO2 and PM2.5 in the air

conc_func= function(sheet_name = "Population_NO2"){

  no2_conc <- read.xlsx(paste(c(dir_stub, "../data/Environment/pollutants/20211209_AQExceedingPop_RoadStats_LAEI2019.xlsx"), collapse = ""), 
                          startRow=6, rows=c(1:47), cols=c(1:5), sheet=sheet_name) %>% data.table()
  no2_conc_d <- no2_conc[!35:37] # Keep central, inner, outer London

  names(no2_conc_d) = c("Area name", "Concentration 2016", "Concentration 2019", "Reduction (ug/m3)", "Reduction (%)")

  # All values to numeric
  no2_conc_d[, `Concentration 2016` := as.numeric(`Concentration 2016`)]
  no2_conc_d[, `Concentration 2019` := as.numeric(`Concentration 2019`)]
  no2_conc_d[, `Reduction (ug/m3)` := as.numeric(`Reduction (ug/m3)`)]
  no2_conc_d[, `Reduction (%)` := as.numeric(`Reduction (%)`)*100]

  # All values to one decimal place
  no2_conc_d[, `Concentration 2016` := round(`Concentration 2016`, 1)]
  no2_conc_d[, `Concentration 2019` := round(`Concentration 2019`, 1)]
  no2_conc_d[, `Reduction (ug/m3)` := round(`Reduction (ug/m3)`, 1)]
  no2_conc_d[, `Reduction (%)` := round(`Reduction (%)`, 1)]

  no2_conc_d_out <- no2_conc_d[`Area name` %in% c("Lambeth", "Central", "Inner", "Outer"), list(`Area name`, `Concentration 2016`, `Concentration 2019`, `Reduction (ug/m3)`, `Reduction (%)`)]

  no2_conc_d_out[,`Area name` := gsub("Inner", "Inner London", `Area name`)]
  no2_conc_d_out[,`Area name` := gsub("Outer", "Outer London", `Area name`)]

  no2_conc_d_out[,`Area name` := ordered(`Area name`, levels = c("Lambeth", "Inner London", "Outer London"))]

  no2_conc_d_out_m <- melt(no2_conc_d_out, id.vars = "Area name", 
  measure.vars = c("Concentration 2016", "Concentration 2019", "Reduction (ug/m3)", "Reduction (%)"),
  variable.name = "Year", value.name = "Concentration")

  no2_conc_d_out_m[, Year := gsub("Concentration ", "", Year)]

  return (no2_conc_d_out_m)

}

no2_conc_d_out <- conc_func() # Lambeth experienced a 23% reduction in NO2 levels 2016-2019, resulting in a significant reduction in the population exposed to the WHO guideline value (40 ug/m3). This is still significantly above the new WHO guideline value of 10 ug/m3.
pm_conc_d_out <- conc_func("Population_PM25") # Despite a 19% reduction 2016-2019, all Lambeth residents are still exposed to PM2.5 levels above the WHO guideline value (10 ug/m3)

no2_conc_d_out <- no2_conc_d_out[Year == "2016" | Year == "2019"]
pm_conc_d_out <- pm_conc_d_out[Year == "2016" | Year == "2019"]

# Make a graph of no2_conc_d_out in ggplot2

no2_conc_graph <- ggplot(no2_conc_d_out, aes(y = `Concentration`, x = `Year`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Concentration: ", `Concentration`, " ug/m3"))
) + 
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(no2_conc_d_out$`Year`))) +
#scale_y_continuous(labels = scales::comma) +
#ylab(expression("Average concentration of " ~NO[2]~ " in area (ug/"~m^3~")")) + # Expressions not supported in ggplotly
ylab("Average concentration of NO2 in area (ug/m3)") +
geom_hline(yintercept=40, linetype="dashed") + 
geom_hline(yintercept=10, linetype="dashed")

no2_conc_graph

# Make a graph of pm_conc_d_out in ggplot2

pm_conc_graph <- ggplot(pm_conc_d_out, aes(y = `Concentration`, x =  `Year`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Concentration: ", `Concentration`, " ug/m3"))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(pm_conc_d_out$`Year`))) +
#scale_y_continuous(labels = scales::comma) +
#ylab(expression("Average concentration of PM 2.5 (ug/"~m^3~")")) + # Expressions not supported in ggplotly
ylab("Average concentration of PM 2.5 (ug/m3)") +
geom_hline(yintercept=10, linetype="dashed")

pm_conc_graph

# Combine the two no2 data tables into one
no2_conc_d_out_dt <- merge(no2_conc_d_out, no2_d_out_m, by = c("Area name", "Year"))
pm_conc_d_out_dt <- merge(pm_conc_d_out, pm_d_out_m, by = c("Area name", "Year"))

# Green space access -----------------------------------------------------------


# Load data
gs_eng <- read.xlsx(paste(c(dir_stub, "../data/Environment/Green space/ospublicgreenspacereferencetables.xlsx"), collapse = ""), 
                          startRow=1, sheet="Country Parks and Playing Field") %>% data.table()

gs_eng

# replace . in column names
colnames(gs_eng) <- gsub("\\.", " ", colnames(gs_eng))

# Filter country name to England
gs_eng <- gs_eng[`Country name` == "England",]

# Melt with Country code and Country name as id vars
gs_eng_m <- melt(gs_eng, id.vars = c("Country code", "Country name"))

# Remove Country code
gs_eng_m <- gs_eng_m[,`Country code` := NULL]

# Rename Country name to Area name
setnames(gs_eng_m, "Country name", "Area name")


# Load region sheet from excel file
gs_reg <- read.xlsx(paste(c(dir_stub, "../data/Environment/Green space/ospublicgreenspacereferencetables.xlsx"), collapse = ""), 
                    startRow=1, sheet="Region Parks and Playing Fields") %>% data.table()

# Filter to London
gs_reg <- gs_reg[`Region.name` == "London",]

# replace . in column names
colnames(gs_reg) <- gsub("\\.", " ", colnames(gs_reg))

# Remove Country name, country code
gs_reg <- gs_reg[,`Country name` := NULL]
gs_reg <- gs_reg[,`Country code` := NULL]

# Melt with Region code and Region as id vars
gs_reg_m <- melt(gs_reg, id.vars = c("Region code", "Region name"))

# Remove Region code
gs_reg_m <- gs_reg_m[,`Region code` := NULL]

# Rename Region to Area name
setnames(gs_reg_m, "Region name", "Area name")


# Load local authority sheet from excel file
gs_bor <- read.xlsx(paste(c(dir_stub, "../data/Environment/Green space/ospublicgreenspacereferencetables.xlsx"), collapse = ""), 
                    startRow=1, sheet="LAD Parks and Playing Fields") %>% data.table()

# Filter to Lambeth
gs_bor <- gs_bor[`LAD.name` == "Lambeth",]

# replace . in column names
colnames(gs_bor) <- gsub("\\.", " ", colnames(gs_bor))

# Remove Country name, country code, Region name, Region code
gs_bor <- gs_bor[,`Country name` := NULL]
gs_bor <- gs_bor[,`Country code` := NULL]
gs_bor <- gs_bor[,`Region name` := NULL]
gs_bor <- gs_bor[,`Region code` := NULL]

# Melt with LAD code and LAD as id vars
gs_bor_m <- melt(gs_bor, id.vars = c("LAD code", "LAD name"))

# Remove LAD code
gs_bor_m <- gs_bor_m[,`LAD code` := NULL]

# Rename LAD to Area name
setnames(gs_bor_m, "LAD name", "Area name")

##
# Combine the three data tables into one
gs_d_out <- rbind(gs_eng_m, gs_reg_m, gs_bor_m)

# Make Area name an ordered factor with levels Lambeth, London, England
gs_d_out$`Area name` <- ordered(gs_d_out$`Area name`, levels = c("Lambeth", "London", "England"))

# Remove rows with no data
gs_d_out <- gs_d_out[!is.na(`value`),]

unique(gs_d_out$variable)

# Filter variable to Average number of...
gs_d_out_num <- gs_d_out[`variable` == "Average number of Parks, Public Gardens, or Playing Fields within 1,000 m radius"]

# Rename value to Number of public green spaces within 1km
setnames(gs_d_out_num, "value", "Number of public green spaces within 1km")

# remove variable
gs_d_out_num <- gs_d_out_num[,`variable` := NULL]

# Make a graph of gs_d_out_num in ggplot2
gs_graph <- ggplot(gs_d_out_num, aes(y = `Number of public green spaces within 1km`, x =  `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Number of public green spaces within 1km: ", `Number of public green spaces within 1km`))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(gs_d_out_num$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
#ylab(expression("Average concentration of PM 2.5 (ug/"~m^3~")")) + # Expressions not supported in ggplotly
ylab("Number of public green spaces within 1km") 

gs_graph

# GHG emissions ----------------------------------------------------------------

# Load data
ghg <- read.xlsx(paste(c(dir_stub, "../data/Environment/GHG emissions/UK-local-authority-ghg-emissions-2020.xlsx"), collapse = ""), 
                startRow=5, sheet="1_1") %>% data.table()

# Replace . in column names
colnames(ghg) <- gsub("\\.", " ", colnames(ghg))

# Filter to calendar year on or later than 2018 (latest data available)
ghg <- ghg[`Calendar Year` >= 2018,]

# Filter Local Authority to Lambeth, London Total, and England Total
ghg_f <- ghg[`Local Authority` %in% c("Lambeth", "London Total", "England Total"),]

#names(ghg_f)

# Keep only columns of interest
ghg_f <- ghg_f[,c("Local Authority", "Calendar Year", "Grand Total", "Per Capita Emissions (tCO2e)")]

# Convert Per Capita Emissions and Grand Total to numeric and Round to 2 decimal places
ghg_f$`Per Capita Emissions (tCO2e)` <- round(as.numeric(ghg_f$`Per Capita Emissions (tCO2e)`), 2)
ghg_f$`Grand Total` <- round(as.numeric(ghg_f$`Grand Total`), 0)

# Replace Total in Local Authority
ghg_f$`Local Authority` <- gsub(" Total", "", ghg_f$`Local Authority`)

# Rename Local Authority to Area name
setnames(ghg_f, "Local Authority", "Area name")

# Rename Calendar Year to Year
setnames(ghg_f, "Calendar Year", "Year")

ghg_f$Year <- as.character(ghg_f$Year)

# Make Area name an ordered factor with levels Lambeth, London, England
ghg_f$`Area name` <- ordered(ghg_f$`Area name`, levels = c("Lambeth", "London", "England"))

# Rename Grand Total to Total GHG emissions (ktCO2e)
setnames(ghg_f, "Grand Total", "Total GHG emissions (ktCO2e)")

# Make a graph of ghg_f in ggplot2
ghg_graph <- ggplot(ghg_f, aes(y = `Per Capita Emissions (tCO2e)`, x =  Year, colour = `Area name`,
                               linetype=`Area name`, group=`Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Year: ", Year, "<br>",
"Total GHG emissions (ktCO2e): ", prettyNum(`Total GHG emissions (ktCO2e)`,big.mark=",", preserve.width="none"), "<br>",
"Per Capita Emissions (tCO2e): ", `Per Capita Emissions (tCO2e)`))
) +
geom_line()+ #position = position_dodge2(width = 0.9))+#, reverse = T)) +
geom_point()+
scale_colour_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(ghg_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
#ylab(expression("Average concentration of PM 2.5 (ug/"~m^3~")")) + # Expressions not supported in ggplotly
expand_limits(y=0) +
ylab("Per Capita Emissions (tCO2e)")

ghg_graph

# Recycling ----------------------------------------------------------------

# Load data
rec <- read.xlsx(paste(c(dir_stub, "../data/Environment/Recycling/LA_and_Regional_Spreadsheet_2021_rev.xlsx"), collapse = ""), 
                startRow=4, sheet="Table_3") %>% data.table()

# Replace . in column names
colnames(rec) <- gsub("\\.", " ", colnames(rec))

#unique(rec$`Year`)

# Filter to financial year 2020
rec <- rec[`Year` == "2020-21",]

# Filter to local authority containing Lambeth
rec_lam <- rec[grepl("Lambeth",`Authority`) == TRUE]

names(rec_lam)

# Keep Year, Authority, Residual household waster per household, Percentage of household waste recycled, and Percentage of household waste composted

rec_lam <- rec_lam[,c("Year", "Authority", 
 "Percentage of household waste sent for reuse, recycling or composting (Ex NI192)")]

# Rename Authority to Area name
setnames(rec_lam, "Authority", "Area name")

# Change Lambeth LB to Lambeth
rec_lam$`Area name` <- gsub(" LB", "", rec_lam$`Area name`)

rec_lam[,`Percentage of household waste sent for reuse, recycling or composting (Ex NI192)` := round(`Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`*100, 1)]

# Load data
rec_reg <- read.xlsx(paste(c(dir_stub, "../data/Environment/Recycling/LA_and_Regional_Spreadsheet_2021_rev.xlsx"), collapse = ""), 
                startRow=5, sheet="Table_3a") %>% data.table()

#rec_reg

# Select columns 2020-21, Region
rec_reg <- rec_reg[c(1:10),c("Region","2020-21")]

# Filter to London, England
rec_reg <- rec_reg[`Region` %in% c("London", "England"),]

# Rename Region to Area name
setnames(rec_reg, "Region", "Area name")

# Rename 2020-21 to Percentage of household waste sent for reuse, recycling or composting (Ex NI192)
setnames(rec_reg, "2020-21", "Percentage of household waste sent for reuse, recycling or composting (Ex NI192)")

# Add column Year with value 2020-21
rec_reg$Year <- "2020-21"

# Combine rec_lam and rec_reg
rec_f <- rbind(rec_lam, rec_reg)

# Change Area name to ordered factor with levels Lambeth, London, England
rec_f$`Area name` <- ordered(rec_f$`Area name`, levels = c("Lambeth", "London", "England"))

# Change Percentage of household waste sent for reuse, recycling or composting (Ex NI192) to numeric
rec_f$`Percentage of household waste sent for reuse, recycling or composting (Ex NI192)` <- as.numeric(rec_f$`Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`)

# Make a graph of rec_f in ggplot2
rec_graph <- ggplot(rec_f, aes(y = `Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`, x =  `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Year: ", Year, "<br>",
"Percentage of household waste sent for reuse, recycling or composting (Ex NI192): ", `Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(rec_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
#ylab(expression("Average concentration of PM 2.5 (ug/"~m^3~")")) + # Expressions not supported in ggplotly
ylab("Percentage of household waste sent for reuse, recycling or composting (Ex NI192)")

rec_graph

# EPC ratings ----------------------------------------------------------------

# Load data
epc <- read.xlsx(paste(c(dir_stub, "../data/Environment/EPC/D1_-_Domestic_EPCs.xlsx"), collapse = ""), 
                startRow=4, sheet="D1_by_LA") %>% data.table()

# Replace . in column names
colnames(epc) <- gsub("\\.", " ", colnames(epc))

# Filter Local Authority to Lambeth
epc_lam <- epc[grepl("Lambeth",`Local Authority`) == TRUE]

# Keep Quarter 2019/4 and later
epc_lam <- epc_lam[`Quarter` >= "2019/4",]

# Rename Local Authority to Area name
setnames(epc_lam, "Local Authority", "Area name")

#str(epc_lam)

# Sum up all numeric columns by Local Authority
epc_lam <- epc_lam[,lapply(.SD, sum), by = `Area name`, .SDcols = c("Number of Lodgements", "A", "B", "C", "D", "E", "F", "G")]

# Create column for sum of A-C
epc_lam[,`A-C` := A + B + C]

# Create column for sum of D-G
epc_lam[,`D-G` := D + E + F + G]

# Create column for percentage of A-C
epc_lam[,`% A-C` := round((`A-C`/`Number of Lodgements`)*100, 1)]

# ---
# Load region data
epc_reg <- read.xlsx(paste(c(dir_stub, "../data/Environment/EPC/D1_-_Domestic_EPCs.xlsx"), collapse = ""), 
                startRow=4, sheet="D1_by_Region") %>% data.table()

# Replace . in column names
colnames(epc_reg) <- gsub("\\.", " ", colnames(epc_reg))

# Filter Region to London
epc_reg <- epc_reg[grepl("London",`Region`) == TRUE]

# Keep Quarter 2019/4 and later
epc_reg <- epc_reg[`Quarter` >= "2019/4",]

# Rename Region to Area name
setnames(epc_reg, "Region", "Area name")

# Sum up all numeric columns by Region
epc_reg <- epc_reg[,lapply(.SD, sum), by = `Area name`, .SDcols = c("Number of Lodgements", "A", "B", "C", "D", "E", "F", "G")]

# Create column for sum of A-C
epc_reg[,`A-C` := A + B + C]

# Create column for sum of D-G
epc_reg[,`D-G` := D + E + F + G]

# Create column for percentage of A-C
epc_reg[,`% A-C` := round((`A-C`/`Number of Lodgements`)*100, 1)]

# ---
# Load England data
epc_eng <- read.xlsx(paste(c(dir_stub, "../data/Environment/EPC/D1_-_Domestic_EPCs.xlsx"), collapse = ""), 
                startRow=4, sheet="D1_England_Only") %>% data.table()

# Replace . in column names
colnames(epc_eng) <- gsub("\\.", " ", colnames(epc_eng))

# Keep Quarter 2019/4 and later
epc_eng <- epc_eng[`Quarter` >= "2019/4",]

# Create column Area name = England
epc_eng[,`Area name` := "England"]

# Sum up all numeric columns by Region
epc_eng <- epc_eng[,lapply(.SD, sum), by = `Area name`, .SDcols = c("Number of Lodgements", "A", "B", "C", "D", "E", "F", "G")]

# Create column for sum of A-C
epc_eng[,`A-C` := A + B + C]

# Create column for sum of D-G
epc_eng[,`D-G` := D + E + F + G]

# Create column for percentage of A-C
epc_eng[,`% A-C` := round((`A-C`/`Number of Lodgements`)*100, 1)]

# ---

names(epc_lam)
names(epc_reg)

# Combine Lambeth, London and England data
epc_f <- rbindlist(list(epc_lam, epc_reg, epc_eng))

# Make area name an ordered factor
epc_f[,`Area name` := factor(`Area name`, levels = c("Lambeth", "London", "England"))]

# Change column names
colnames(epc_f) <- c("Area name", "Number of Lodgements", "A", "B", "C", "D", "E", "F", "G", "A-C", "D-G", "% A-C")

# Make a graph of epc_f in ggplot2
epc_graph <- ggplot(epc_f, aes(y = `% A-C`, x =  `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Number of Lodgements: ", prettyNum(`Number of Lodgements`,big.mark=",", preserve.width="none"), "<br>",
"Number rated A-C: ", prettyNum(`A-C`,big.mark=",", preserve.width="none"), "<br>",
"% A-C in latest three years: ", `% A-C`))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(epc_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
#ylab(expression("Average concentration of PM 2.5 (ug/"~m^3~")")) + # Expressions not supported in ggplotly
ylab("Percentage of EPC ratings rated as A-C in latest three years")

epc_graph

# Renewable energy -----------------------------------------------------------

# Load data
renew <- read.xlsx(paste(c(dir_stub, "../data/Environment/Renewable energy/Renewable_electricity_by_local_authority_2014-2021_Nov22update.xlsx"), collapse = ""), 
                startRow=6, sheet="LA - Sites 2021") %>% data.table()

# Replace . in column names
colnames(renew) <- gsub("\\.", " ", colnames(renew))

# Remove anything inside [ brackets ] from column names
colnames(renew) <- gsub("\\[.*\\]", "", colnames(renew))

# Remove spaces at end of column names
colnames(renew) <- gsub("\\s+$", "", colnames(renew))

# Create column Year = 2021
renew[,`Year` := 2021]

# Keep only Country, Region, Local Authority Name, Estimated number of households, Photovoltaics
renew <- renew[,c("Country", "Region", "Local Authority Name", "Year", "Estimated number of households", "Photovoltaics")]




# London ---
# Filter to London
renew_lon <- renew[grepl("London",`Region`) == TRUE]

# Make photovoltaics and Estimated number of households a numeric
renew_lon[,c("Photovoltaics", "Estimated number of households") := lapply(.SD, as.numeric), .SDcols = c("Photovoltaics", "Estimated number of households")]

# Sum up all numeric columns by Region
renew_lon <- renew_lon[,lapply(.SD, sum, na.rm=T), by = `Region`, .SDcols = c("Photovoltaics", "Estimated number of households")]

# Create column percentage of households with solar PV
renew_lon[,`Number of solar PV installations per 1k households` := round((`Photovoltaics`/(`Estimated number of households`/1000)), 2)]

# Rename column Region to Area name
renew_lon[,`Area name` := `Region`]


# England ---
# Filter to England
renew_eng <- renew[grepl("England",`Country`) == TRUE]

# Make photovoltaics and Estimated number of households a numeric
renew_eng[,c("Photovoltaics", "Estimated number of households") := lapply(.SD, as.numeric), .SDcols = c("Photovoltaics", "Estimated number of households")]

# Sum up all numeric columns by Country
renew_eng <- renew_eng[,lapply(.SD, sum, na.rm=T), by = `Country`, .SDcols = c("Photovoltaics", "Estimated number of households")]

# Create column percentage of households with solar PV
renew_eng[,`Number of solar PV installations per 1k households` := round((`Photovoltaics`/(`Estimated number of households`/1000)), 2)]

# Rename column Country to Area name
renew_eng[,`Area name` := `Country`]

# Lambeth ---
# Filter to Lambeth
renew_lam <- renew[grepl("Lambeth",`Local Authority Name`) == TRUE]


# Make photovoltaics and Estimated number of households a numeric
renew_lam[,c("Photovoltaics", "Estimated number of households") := lapply(.SD, as.numeric), .SDcols = c("Photovoltaics", "Estimated number of households")]

# Sum up all numeric columns by local authority
renew_lam <- renew_lam[,lapply(.SD, sum, na.rm=T), by = `Local Authority Name`, .SDcols = c("Photovoltaics", "Estimated number of households")]

# Create column percentage of households with solar PV
renew_lam[,`Number of solar PV installations per 1k households` := round((`Photovoltaics`/(`Estimated number of households`/1000)), 2)]

# Rename column Local Authority Name to Area name
renew_lam[,`Area name` := `Local Authority Name`]


# Combine Lambeth, London and England data
renew_f <- rbindlist(list(renew_lam, renew_lon, renew_eng), fill = TRUE)

# Make area name an ordered factor
renew_f[,`Area name` := factor(`Area name`, levels = c("Lambeth", "London", "England"))]

# Remove columns Local Authority Name, Region, Country
renew_f <- renew_f[,c("Area name", "Estimated number of households", "Photovoltaics", "Number of solar PV installations per 1k households")]

# Rename Photovoltaics to Total number of solar PV installations in area
setnames(renew_f, "Photovoltaics","Total number of solar PV installations in area")

# Year column
renew_f[,`Year` := 2021]

# Make a graph of renew_f in ggplot2

renew_graph <- ggplot(renew_f, aes(y = `Number of solar PV installations per 1k households`, x =  `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Total number of solar PV installations in area: ", prettyNum(`Total number of solar PV installations in area`,big.mark=",", preserve.width="none"), "<br>",
"Estimated number of households: ", prettyNum(`Estimated number of households`,big.mark=",", preserve.width="none"), "<br>",
"Number of solar PV installations per 1k households: ", `Number of solar PV installations per 1k households`))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(renew_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
ylab("Number of solar PV installations per 1k households")

renew_graph

# Tree canopy cover -----------------------------------------------------------

# Load data
tree <- fread(paste(c(dir_stub, "../data/Environment/Tree canopy/gla-canopy-lsoas.csv"), collapse = "")) %>% data.table()

unique(tree$la_nm)

# Get total sum of canopy_per
tree_lon <- tree[, .(lsoa_kmsq = sum(lsoa_kmsq), canopy_kmsq = sum(canopy_kmsq))]

# Create column canopy_per
tree_lon[,`canopy_per` := round((canopy_kmsq/lsoa_kmsq)*100, 2)]

# Create column Area name
tree_lon[,`Area name` := "London"]

## Create Inner London column
inner_london <- fread(paste(c(dir_stub, "../data/inner_london_boroughs.csv"), collapse = "")) %>% data.table()

# Strip whitespace from Borough
inner_london[,`Borough` := trimws(`Borough`)]

# Join inner_london to tree
tree_m <- merge(tree, inner_london, by.x = "la_nm", by.y = "Borough", all.x = TRUE)

table(tree_m$Position)

# Create inner london tree dataframe
tree_in <- tree_m[Position == "Inner London", .(lsoa_kmsq = sum(lsoa_kmsq), canopy_kmsq = sum(canopy_kmsq))]

# Create column canopy_per
tree_in[,`canopy_per` := round((canopy_kmsq/lsoa_kmsq)*100, 2)]

# Create column Area name
tree_in[,`Area name` := "Inner London"]


# Filter tree to Lambeth
tree_lam <- tree[la_nm == "Lambeth", .(lsoa_kmsq = sum(lsoa_kmsq), canopy_kmsq = sum(canopy_kmsq))]

# Create column canopy_per
tree_lam[,`canopy_per` := round((canopy_kmsq/lsoa_kmsq)*100, 2)]

# Create column Area name
tree_lam[,`Area name` := "Lambeth"]

# Combine Lambeth, Inner London and London data
tree_f <- rbindlist(list(tree_lam, tree_in, tree_lon), fill = TRUE)

# Make area name an ordered factor
tree_f[,`Area name` := factor(`Area name`, levels = c("Lambeth", "Inner London", "London"))]

# Rename canopy_per to Percentage of tree canopy cover
setnames(tree_f, "canopy_per","Percentage of tree canopy cover")

# Rename lsoa_kmsq to Total square km
setnames(tree_f, "lsoa_kmsq","Total square km")

# Rename canopy_kmsq to Total square km of tree canopy
setnames(tree_f, "canopy_kmsq","Total square km of tree canopy")

# Round Total square km and Total square km of tree canopy to 0 decimal places
tree_f[,c("Total square km", "Total square km of tree canopy") := lapply(.SD, round, digits = 0), .SDcols = c("Total square km", "Total square km of tree canopy")]

# Create graph of tree_f in ggplot2

tree_graph <- ggplot(tree_f, aes(y = `Percentage of tree canopy cover`, x =  `Area name`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Total square km: ",prettyNum(`Total square km`,big.mark=",", preserve.width="none"), "<br>",
"Total square km of tree canopy: ", prettyNum(`Total square km of tree canopy`,big.mark=",", preserve.width="none"), "<br>",
"Percentage of tree canopy cover: ", `Percentage of tree canopy cover`))
) +
geom_col(position = position_dodge2(width = 0.9))+#, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
#scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(tree_f$`Area name`))) +
#scale_y_continuous(labels = scales::comma) +
ylab("Percentage of tree canopy cover")

tree_graph

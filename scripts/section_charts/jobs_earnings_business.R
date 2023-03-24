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


l_dat_no_perc <- function(path, start_row = 2, percentage_cut_off = 0.5){
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
  names(df)[2] = "Lambeth"
  names(df)[3] = "England"
  names(df)[4] = "London"
  
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
  #names(df_d)[4] = "Percentage"
  
  # Create new df for Area name = Lambeth
  df_lambeth <- df_d[df_d$`Area name` == "Lambeth",]
  
  # Keep only categories of at least X% of the population (default 0.5%)
  #df_lambeth <- df_lambeth[df_lambeth$`Percentage` >= percentage_cut_off,]
  
  # Order df_lambeth descending by Percentage
  #df_lambeth <- df_lambeth[order(-df_lambeth$`Percentage`),]
  
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
  #df_d <- df_d[order(`Area name`, -Percentage),]
  
  out_stuff <- list(df_d, cat_order)
  
  
  return(out_stuff)
  
}



# The following should be true when testing this file. When knitting the report together and sourcing externally, set this to false
if (!exists("run_direct")){run_direct=T}

if (run_direct == F){dir_stub = ''
   }else if (run_direct == T){
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    dir_stub = '../'
    source(paste(c(dir_stub, 'colour_theme.r'),collapse = ''))
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

# Businesses --------------------------------------------------------------
# Types of businesses -----------------------------------------------------

bus <- read.xlsx(paste(c(dir_stub, "../data/Businesses/2022/local units type 2022 NOMIS.xlsx"),collapse = ''), startRow = 7) %>% as.data.table()
names(bus)

bus

row.names(bus)


S1 <- bus[, list(X = unlist(strsplit(Industry, ": "))), by = seq_len(nrow(bus))]
S1[, Industry := sequence(.N), by = seq_len]
industry_cats <- dcast.data.table(S1, seq_len ~ Industry, value.var="X")
industry_cats <- setnames(industry_cats, c("1","2"), c("Industry category no","Industry category")) 


bus_df <- merge(as.data.frame(bus), as.data.frame(industry_cats), by=0, all.x = T) %>% as.data.table()

bus_df_3 <- bus_df[nchar(`Industry category no`) == 3]

bus_df_3 <- setnames(bus_df_3, c("ladu:Lambeth","towncity:London","country:England"), c("Lambeth", "London", "England"))


bus_df_3[,`:=`("Lambeth %" = round((Lambeth/sum(Lambeth))*100,1),
               "London %" = round((London/sum(London))*100,1),
               "England %" = round((England/sum(England))*100,1)
)]

# Only interested in presenting industries that have a significant presence in Lambeth, so removing anything with less
# than 1% of workforce involved
bus_df_3_lam <- bus_df_3[`Lambeth %` >= 2][order(`Lambeth %`)]


# Keep only relevant columns
bus_df_3_lam_p <- bus_df_3_lam[, .(`Industry category`, `Lambeth %`, `London %`, `England %`)]

# 'Activities' is an unnecessary word throughout
bus_df_3_lam_p = bus_df_3_lam_p[,`Industry category`:=gsub(" activities", "", `Industry category`)]

#bus_df_3_lam[, .(`Industry category`)]

bus_df_3_lam_p_m <- melt(bus_df_3_lam_p, id.vars = "Industry category", variable.name = "Area", value.name = "% of total workforce")
bus_df_3_lam_p_m[,`:=`(`Industry category`=ordered(`Industry category`, levels = bus_df_3_lam_p$`Industry category`))]
bus_df_3_lam_p_m[,`:=`(`Area`=ordered(`Area`, levels = c( "Lambeth %",  "London %","England %")))]

bus_df_3_lam_p_m

bus_df_3_lam_p_m$`% of total workforce`

# Present on a ggplotly graph
bus_df_3_lam_p_m_graph = ggplot(bus_df_3_lam_p_m, aes(x = `% of total workforce`, y = `Industry category`, 
                                                      fill = Area,
                                                      text= paste("Area: ", Area, "<br>",
                                                                  "% of total workforce: ", `% of total workforce`, sep = "")
                                                      )) +
  geom_col(position = position_dodge2(width = 0.9, reverse = T)) + # aes(alpha=Area), 
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.y = element_blank()) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 55)) +
  scale_x_continuous(labels = function(x) sprintf("%.0f", x)) +
  xlab("Workers in industry as % of total workforce") #+
  #scale_alpha_manual(values=c(1,0.3,0.3))

bus_df_3_lam_p_m_graph

# Business creation graph -------------------------------------------------------------------------

#### creation

creation = fread(paste(c(dir_stub, "../data/Businesses/2022/rate-of-birth-enterprise-lginform-2004-21.csv"),collapse = ''),
              skip=5)

names(creation) = gsub("cal_","", names(creation))

# creation_df = creation.copy().iloc[3:,:]
# creation_df = creation_df.rename(columns={"area label":"Area"}).drop("area", axis=1)
#creation_df <- creation[2:length(creation)]
creation_df <- creation

#names(creation_df)

creation_df[,`:=`(area=NULL, `area long label`=NULL)]
creation_df <- setnames(creation_df, "area label", "Area")

#creation_df_m = creation_df.melt(id_vars="Area")
creation_df_m <- melt(creation_df, id.vars="Area")

creation_df_m = creation_df_m[!is.na(Area)]
creation_df_m = creation_df_m[variable != "area long label"]

#creation_df_m[,variable:=as.character(variable)]



# creation_df_m_d = creation_df_m_d.rename(columns={"value":"creation collected (GBP, 1000s)", 0:"Year", 1:"Quarter"})
creation_df_m_d = creation_df_m

setnames(creation_df_m_d, c("value", "variable"), c("Enterprise creation rate (per 10k people)", "Year"), skip_absent = T)

names(creation_df_m_d)

# creation_df_m_d["Year"] = creation_df_m_d["Year"].str.replace("/.*","", regex = True)
creation_df_m_d[,Year:=as.integer(gsub("/.*","",Year, fixed=F))]
creation_df_m_d[,Year]

# creation_df_m_d["creation collected (GBP, 1000s)"] = creation_df_m_d["creation collected (GBP, 1000s)"].str.replace(",","").astype(int)
creation_df_m_d[,`Enterprise creation rate (per 10k people)`:=  as.numeric(gsub(",","",`Enterprise creation rate (per 10k people)`))] 

## Group by year
# creation_df_m_d_yr = creation_df_m_d[["Area","creation collected (GBP, 1000s)","Year"]].groupby(by=["Area","Year"]).sum().reset_index()

creation_df_m_d_yr <- creation_df_m_d[,c("Area","Year", "Enterprise creation rate (per 10k people)"),with=F]
creation_df_m_d_yr <- creation_df_m_d_yr[,.(`Enterprise creation rate (per 10k people)`= sum(`Enterprise creation rate (per 10k people)`)), by=.(Area, Year)]
creation_df_m_d_yr <- creation_df_m_d_yr[!is.na(Area)]


## Later than 2018, earlier than 2022 (not complete year)
# creation_df_m_d_yr = creation_df_m_d_yr[creation_df_m_d_yr["Year"] >= 2018]

creation_df_m_d_yr_18 <- creation_df_m_d_yr[Year >= 2018 & Year < 2022]
creation_df_m_d_yr_18 
vals_2019 = creation_df_m_d_yr_18[Year == 2019]

##vals_2019 = setnames(vals_2019, "Enterprise creation rate (per 10k people)")


creation_df_m_d_yr_18_m = merge(creation_df_m_d_yr_18, vals_2019[,c("Area")], by = "Area", how = "left")
#creation_df_m_d_yr_18_m[,`creation collected as percentage of 2019 collection` := round((`Enterprise creation rate (per 10k people)`/`creation collected 2019 (GBP, millions)`)*100,2)]

#### For Lambeth
# creation_df_m_d_yr_lam = creation_df_m_d_yr[creation_df_m_d_yr['Area']=="Lambeth"]
creation_df_m_d_yr_18_lam <- creation_df_m_d_yr_18_m[`Area` == "Lambeth"]
creation_df_m_d_yr_18_lam

## For London only by year
creation_df_m_d_yr_18_lon <- creation_df_m_d_yr_18_m[,.(`Enterprise creation rate (per 10k people)`= mean(`Enterprise creation rate (per 10k people)`)), by=.(Year)]
creation_df_m_d_yr_18_lon[,Area:="London"]
creation_df_m_d_yr_18_lon


### Concat Lambeth and London
creation_df_m_d_yr_18_g = rbindlist(list(creation_df_m_d_yr_18_lam[,sort(names(creation_df_m_d_yr_18_lam)), with = F],
                                      creation_df_m_d_yr_18_lon[,sort(names(creation_df_m_d_yr_18_lon)), with = F]), use.names=F)

creation_df_m_d_yr_18_g[,`Enterprise creation rate (per 10k people)`:=round(`Enterprise creation rate (per 10k people)`, 1)]
creation_df_m_d_yr_18_g = creation_df_m_d_yr_18_g[order(Year, Area)]

# Present on a ggplotly graph
creation_comparison_graph = ggplot(creation_df_m_d_yr_18_g, aes(x = `Year`, y = `Enterprise creation rate (per 10k people)`, 
                                                          fill = Area,
                                                          text= paste("Area: ", Area, "<br>",
                                                                      "Enterprise creation rate (per 10k people): ", round(`Enterprise creation rate (per 10k people)`,0), sep = "")#,
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Enterprise creation rate\n(per 10k people)")

creation_comparison_graph


#ggplotly(creation_comparison_graph, height = 500, tooltip = c("text")) %>% layout(autosize = F,  margin = m, font=lam_ggplotly_font)


# Business closure graph -------------------------------------------------------------------------

#### closure

closure = fread(paste(c(dir_stub, "../data/Businesses/2022/rate-of-death-enterprise-lginform-2004-21.csv"),collapse = ''),
              skip=5)

names(closure) = gsub("cal_","", names(closure))

# closure_df = closure.copy().iloc[3:,:]
# closure_df = closure_df.rename(columns={"area label":"Area"}).drop("area", axis=1)
#closure_df <- closure[2:length(closure)]
closure_df <- closure

#names(closure_df)

closure_df[,`:=`(area=NULL, `area long label`=NULL)]
closure_df <- setnames(closure_df, "area label", "Area")

#closure_df_m = closure_df.melt(id_vars="Area")
closure_df_m <- melt(closure_df, id.vars="Area")

closure_df_m = closure_df_m[!is.na(Area)]
closure_df_m = closure_df_m[variable != "area long label"]



# closure_df_m_d = closure_df_m_d.rename(columns={"value":"closure collected (GBP, 1000s)", 0:"Year", 1:"Quarter"})
closure_df_m_d = closure_df_m

setnames(closure_df_m_d, c("value", "variable"), c("Enterprise closure rate (per 10k people)", "Year"), skip_absent = T)

names(closure_df_m_d)

# closure_df_m_d["Year"] = closure_df_m_d["Year"].str.replace("/.*","", regex = True)
closure_df_m_d[,Year:=as.integer(gsub("/.*","",Year, fixed=F))]
closure_df_m_d[,Year]

# closure_df_m_d["closure collected (GBP, 1000s)"] = closure_df_m_d["closure collected (GBP, 1000s)"].str.replace(",","").astype(int)
closure_df_m_d[,`Enterprise closure rate (per 10k people)`:=  as.numeric(gsub(",","",`Enterprise closure rate (per 10k people)`))] 

## Group by year
# closure_df_m_d_yr = closure_df_m_d[["Area","closure collected (GBP, 1000s)","Year"]].groupby(by=["Area","Year"]).sum().reset_index()

closure_df_m_d_yr <- closure_df_m_d[,c("Area","Year", "Enterprise closure rate (per 10k people)"),with=F]
closure_df_m_d_yr <- closure_df_m_d_yr[,.(`Enterprise closure rate (per 10k people)`= sum(`Enterprise closure rate (per 10k people)`)), by=.(Area, Year)]
closure_df_m_d_yr <- closure_df_m_d_yr[!is.na(Area)]


## Later than 2018, earlier than 2022 (not complete year)
# closure_df_m_d_yr = closure_df_m_d_yr[closure_df_m_d_yr["Year"] >= 2018]

closure_df_m_d_yr_18 <- closure_df_m_d_yr[Year >= 2018 & Year < 2022]
closure_df_m_d_yr_18 
#vals_2019 = closure_df_m_d_yr_18[Year == 2019]

##vals_2019 = setnames(vals_2019, "Enterprise closure rate (per 10k people)")


closure_df_m_d_yr_18_m = merge(closure_df_m_d_yr_18, vals_2019[,c("Area")], by = "Area", how = "left")
#closure_df_m_d_yr_18_m[,`closure collected as percentage of 2019 collection` := round((`Enterprise closure rate (per 10k people)`/`closure collected 2019 (GBP, millions)`)*100,2)]

#### For Lambeth
# closure_df_m_d_yr_lam = closure_df_m_d_yr[closure_df_m_d_yr['Area']=="Lambeth"]
closure_df_m_d_yr_18_lam <- closure_df_m_d_yr_18_m[`Area` == "Lambeth"]
closure_df_m_d_yr_18_lam

## For London only by year
closure_df_m_d_yr_18_lon <- closure_df_m_d_yr_18_m[,.(`Enterprise closure rate (per 10k people)`= mean(`Enterprise closure rate (per 10k people)`)), by=.(Year)]
closure_df_m_d_yr_18_lon[,Area:="London"]
closure_df_m_d_yr_18_lon


### Concat Lambeth and London
closure_df_m_d_yr_18_g = rbindlist(list(closure_df_m_d_yr_18_lam[,sort(names(closure_df_m_d_yr_18_lam)), with = F],
                                      closure_df_m_d_yr_18_lon[,sort(names(closure_df_m_d_yr_18_lon)), with = F]), use.names=F)

closure_df_m_d_yr_18_g[,`Enterprise closure rate (per 10k people)`:=round(`Enterprise closure rate (per 10k people)`, 1)]
closure_df_m_d_yr_18_g = closure_df_m_d_yr_18_g[order(Year, Area)]

# Present on a ggplotly graph
closure_comparison_graph = ggplot(closure_df_m_d_yr_18_g, aes(x = `Year`, y = `Enterprise closure rate (per 10k people)`, 
                                                          fill = Area,
                                                          text= paste("Area: ", Area, "<br>",
                                                                      "Enterprise closure rate (per 10k people): ", round(`Enterprise closure rate (per 10k people)`,0), sep = "")#,
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Enterprise closure rate\n(per 10k people)")

closure_comparison_graph


#ggplotly(closure_comparison_graph, tooltip = c("text")) %>% layout(autosize = F,  margin = m, font=lam_ggplotly_font) # , height = 500

# Combine the creation_df_m_d_yr_18_g and closure_df_m_d_yr_18_g dfs
creation_closure_df_m_d_yr_18_g = merge(creation_df_m_d_yr_18_g, closure_df_m_d_yr_18_g, by=c("Area", "Year"), how="outer")

# Create a Creation - closure rate column
creation_closure_df_m_d_yr_18_g[,`Creation - closure rate (per 10k people)`:=round(`Enterprise creation rate (per 10k people)` - `Enterprise closure rate (per 10k people)`, 1)]

creation_closure_df_m_d_yr_18_g_m = melt(creation_closure_df_m_d_yr_18_g, id.vars = c("Area", "Year"), 
measure.vars = c("Enterprise creation rate (per 10k people)", "Enterprise closure rate (per 10k people)",
 "Creation - closure rate (per 10k people)"), variable.name = "Metric", value.name = "Rate per 10k people")

# Replace ' (per 10k people)' with ''
creation_closure_df_m_d_yr_18_g_m[,`Metric`:=gsub(" \\(per 10k people\\)", "", `Metric`)]

# make Year an ordered factor
creation_closure_df_m_d_yr_18_g_m[,Year:=ordered(Year, levels = c(2018, 2019, 2020, 2021))]

creation_closure_df_m_d_yr_18_g_m[,Metric:=ordered(Metric, levels = c("Enterprise creation rate", "Enterprise closure rate", "Creation - closure rate"))]

# Present on a ggplotly graph
closure_creation_comparison_graph = ggplot(creation_closure_df_m_d_yr_18_g_m, aes(x = Year, y = `Rate per 10k people`, group=Area, linetype=Area,
                             colour = Area,
                             text= paste("Area: ", Area, "<br>",
                      "Metric: ", `Metric`, "<br>",
                      "Rate per 10k people: ", round(`Rate per 10k people`,0), "<br>"
))) +
  geom_line()+#position = position_dodge(width = 0.9)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~Metric) #+
  #ylab("Enterprise closure rate\n(per 10k people)")

closure_creation_comparison_graph

# NNDR graph -------------------------------------------------------------------------

#### NNDR

nndr = fread(paste(c(dir_stub, "../data/Businesses/2022/non-domestic rates collected lginform 2010-22.csv"),collapse = ''),
             skip=4)

names(nndr)

# nndr_df = nndr.copy().iloc[3:,:]
# nndr_df = nndr_df.rename(columns={"area label":"Area"}).drop("area", axis=1)
nndr_df <- nndr[2:length(nndr)]
nndr_df[,area:=NULL]
nndr_df <- setnames(nndr_df, "area label", "Area")

#nndr_df_m = nndr_df.melt(id_vars="Area")
nndr_df_m <- melt(nndr_df, id.vars="Area")

nndr_df_m = nndr_df_m[!is.na(Area)]
nndr_df_m = nndr_df_m[variable != "area long label"]

nndr_df_m[,variable:=as.character(variable)]

# nndr_df_m_dates = nndr_df_m["variable"].str.split(" ", expand=True)
## https://stackoverflow.com/questions/12946883/strsplit-by-row-and-distribute-results-by-column-in-data-frame
S1 <- nndr_df_m[, list(X = unlist(strsplit(variable, " "))), by = seq_len(nrow(nndr_df_m))]

S1[, Time := sequence(.N), by = seq_len]
nndr_df_m_dates = dcast.data.table(S1, seq_len ~ Time, value.var="X")

nndr_df_m_dates[,seq_len:=NULL]

# nndr_df_m_d = nndr_df_m.join(nndr_df_m_dates)
nndr_df_m_d = merge.data.frame(nndr_df_m, nndr_df_m_dates, by = "row.names") %>% data.table()
nndr_df_m_d

## Change metric to millions rather than thousands
nndr_df_m_d[, value:=round(as.numeric(gsub(",","",value))/1000,2)]

# nndr_df_m_d = nndr_df_m_d.rename(columns={"value":"NNDR collected (GBP, 1000s)", 0:"Year", 1:"Quarter"})
setnames(nndr_df_m_d, c("value", "1", "2"), c("NNDR collected (GBP, millions)", "Year", "Quarter"), skip_absent = T)

names(nndr_df_m_d)

# nndr_df_m_d["Year"] = nndr_df_m_d["Year"].str.replace("/.*","", regex = True)
nndr_df_m_d[,Year:=as.integer(gsub("/.*","",Year, fixed=F))]
nndr_df_m_d[,Year]

# nndr_df_m_d["NNDR collected (GBP, 1000s)"] = nndr_df_m_d["NNDR collected (GBP, 1000s)"].str.replace(",","").astype(int)
nndr_df_m_d[,`NNDR collected (GBP, millions)`:=  as.integer(gsub(",","",`NNDR collected (GBP, millions)`))] 

## Group by year
# nndr_df_m_d_yr = nndr_df_m_d[["Area","NNDR collected (GBP, 1000s)","Year"]].groupby(by=["Area","Year"]).sum().reset_index()

nndr_df_m_d_yr <- nndr_df_m_d[,c("Quarter","Area","Year", "NNDR collected (GBP, millions)"),with=F]
nndr_df_m_d_yr <- nndr_df_m_d_yr[,.(`NNDR collected (GBP, millions)`= sum(`NNDR collected (GBP, millions)`)), by=.(Area, Year)]
nndr_df_m_d_yr <- nndr_df_m_d_yr[!is.na(Area)]


## Later than 2018, earlier than 2022 (not complete year)
# nndr_df_m_d_yr = nndr_df_m_d_yr[nndr_df_m_d_yr["Year"] >= 2018]

nndr_df_m_d_yr_18 <- nndr_df_m_d_yr[Year >= 2018 & Year < 2022]
nndr_df_m_d_yr_18 
vals_2019 = nndr_df_m_d_yr_18[Year == 2019]

vals_2019 = setnames(vals_2019, "NNDR collected (GBP, millions)", "NNDR collected 2019 (GBP, millions)")


nndr_df_m_d_yr_18_m = merge(nndr_df_m_d_yr_18, vals_2019[,c("Area", "NNDR collected 2019 (GBP, millions)")], by = "Area", how = "left")

nndr_df_m_d_yr_18_m[,`NNDR collected as percentage of 2019 collection` := round((`NNDR collected (GBP, millions)`/`NNDR collected 2019 (GBP, millions)`)*100,2)]

#### For Lambeth
# nndr_df_m_d_yr_lam = nndr_df_m_d_yr[nndr_df_m_d_yr['Area']=="Lambeth"]
nndr_df_m_d_yr_18_lam <- nndr_df_m_d_yr_18_m[`Area` == "Lambeth"]
nndr_df_m_d_yr_18_lam

## For London only by year
nndr_df_m_d_yr_18_lon <- nndr_df_m_d_yr_18_m[,.(`NNDR collected (GBP, millions)`= sum(`NNDR collected (GBP, millions)`),
                        `NNDR collected 2019 (GBP, millions)`= sum(`NNDR collected 2019 (GBP, millions)`),
                        `NNDR collected as percentage of 2019 collection` = round(mean(`NNDR collected as percentage of 2019 collection`,na.rm=TRUE)                  
                                          ,2)), by=.(Year)]
nndr_df_m_d_yr_18_lon[,Area:="London"]

#nndr_df_m_d_yr_18_lon


### Concat Lambeth and London
nndr_df_m_d_yr_18_g = rbindlist(list(nndr_df_m_d_yr_18_lam[,sort(names(nndr_df_m_d_yr_18_lam)), with = F],
                                     nndr_df_m_d_yr_18_lon[,sort(names(nndr_df_m_d_yr_18_lon)), with = F]), use.names=F)


nndr_df_m_d_yr_18_g[,`:=`(`NNDR collected (GBP, millions)`=round(`NNDR collected (GBP, millions)`, 1),
                              `NNDR collected 2019 (GBP, millions)`=round(`NNDR collected 2019 (GBP, millions)`, 1),
                              `NNDR collected as percentage of 2019 collection` = round(`NNDR collected as percentage of 2019 collection`,1)
                              )]
#nndr_df_m_d_yr_18_g

nndr_df_m_d_yr_18_g = nndr_df_m_d_yr_18_g[order(Year, Area)]

# Present on a ggplotly graph
nndr_comparison_graph = ggplot(nndr_df_m_d_yr_18_g, aes(x = `Year`, y = `NNDR collected as percentage of 2019 collection`, colour = Area, group = Area, linetype= Area,
                          text= paste("Area: ", Area, "<br>",
                            "NNDR collected as % of 2019 collection: ", round(`NNDR collected as percentage of 2019 collection`,0), "%<br>", 
                           "NNDR collected (GBP, millions): ", round(`NNDR collected (GBP, millions)`,0), sep = "")#,
                                                        )) +
  geom_line()+#position = position_dodge(width = 0.9)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("NNDR collected as % of 2019 collection")

nndr_comparison_graph

ggplotly(nndr_comparison_graph, height = 500, tooltip = c("text")) %>% layout(autosize = F,  margin = m, font=lam_ggplotly_font)


# Trend in business rate value ----------------------------------------------------------------------

rate_val = fread(paste(c(dir_stub, "../data/Businesses/2022/Trend_business_rate_value_lginform_2001-2022_london.csv"),collapse = ''),
                         skip=4)


# rate_val_df = rate_val.copy().iloc[3:,:]
# rate_val_df = rate_val_df.rename(columns={"area label":"Area"}).drop("area", axis=1)
rate_val_df <- rate_val[2:length(rate_val)]
rate_val_df[,area:=NULL]
rate_val_df <- setnames(rate_val_df, "area label", "Area")

#rate_val_df_m = rate_val_df.melt(id_vars="Area")
rate_val_df_m <- melt(rate_val_df, id.vars="Area")

rate_val_df_m = rate_val_df_m[!is.na(Area)]
rate_val_df_m = rate_val_df_m[variable != "area long label"]

rate_val_df_m[,variable:=as.character(variable)]

# rate_val_df_m_dates = rate_val_df_m["variable"].str.split(" ", expand=True)
## https://stackoverflow.com/questions/12946883/strsplit-by-row-and-distribute-results-by-column-in-data-frame
S1 <- rate_val_df_m[, list(X = unlist(strsplit(variable, " "))), by = seq_len(nrow(rate_val_df_m))]

S1[, Time := sequence(.N), by = seq_len]
rate_val_df_m_dates = dcast.data.table(S1, seq_len ~ Time, value.var="X")

rate_val_df_m_dates[,seq_len:=NULL]

# rate_val_df_m_d = rate_val_df_m.join(rate_val_df_m_dates)
rate_val_df_m_d = merge.data.frame(rate_val_df_m, rate_val_df_m_dates, by = "row.names") %>% data.table()
rate_val_df_m_d


###

# rate_val_df_m_d = rate_val_df_m_d.rename(columns={"value":"rate_val collected (GBP, 1000s)", 0:"Year", 1:"Quarter"})
setnames(rate_val_df_m_d, c("value", "1", "2"), c("Rateable value index (compared to 2002)", "Year", "Quarter"), skip_absent = T)

names(rate_val_df_m_d)

# rate_val_df_m_d["Year"] = rate_val_df_m_d["Year"].str.replace("/.*","", regex = True)
rate_val_df_m_d[,Year:=as.integer(gsub("/.*","",Year, fixed=F))]
rate_val_df_m_d[,Year]

# rate_val_df_m_d["rate_val collected (GBP, 1000s)"] = rate_val_df_m_d["rate_val collected (GBP, 1000s)"].str.replace(",","").astype(int)
rate_val_df_m_d[,`Rateable value index (compared to 2002)`:=  as.numeric(gsub(",","",`Rateable value index (compared to 2002)`))] 

## Group by year
# rate_val_df_m_d_yr = rate_val_df_m_d[["Area","rate_val collected (GBP, 1000s)","Year"]].groupby(by=["Area","Year"]).sum().reset_index()

rate_val_df_m_d_yr <- rate_val_df_m_d[,c("Area","Year", "Rateable value index (compared to 2002)"),with=F]
rate_val_df_m_d_yr <- rate_val_df_m_d_yr[,.(`Rateable value index (compared to 2002)`= mean(`Rateable value index (compared to 2002)`)), by=.(Area, Year)]
rate_val_df_m_d_yr <- rate_val_df_m_d_yr[!is.na(Area)]

## Later than 2018, earlier than 2022 (not complete year)
# rate_val_df_m_d_yr = rate_val_df_m_d_yr[rate_val_df_m_d_yr["Year"] >= 2018]

rate_val_df_m_d_yr_18_m <- rate_val_df_m_d_yr[Year >= 2017 & Year < 2022]
rate_val_df_m_d_yr_18_m


#### For Lambeth
# rate_val_df_m_d_yr_lam = rate_val_df_m_d_yr[rate_val_df_m_d_yr['Area']=="Lambeth"]
rate_val_df_m_d_yr_18_lam <- rate_val_df_m_d_yr_18_m[`Area` == "Lambeth"]
rate_val_df_m_d_yr_18_lam

## For London only by year
rate_val_df_m_d_yr_18_lon <- rate_val_df_m_d_yr_18_m[,.(`Rateable value index (compared to 2002)`= mean(`Rateable value index (compared to 2002)`)),
                                                    by=.(Year)]
rate_val_df_m_d_yr_18_lon[,Area:="London"]

rate_val_df_m_d_yr_18_lon


### Concat Lambeth and London
rate_val_df_m_d_yr_18_g = rbindlist(list(rate_val_df_m_d_yr_18_lam[,sort(names(rate_val_df_m_d_yr_18_lam)), with = F],
                                     rate_val_df_m_d_yr_18_lon[,sort(names(rate_val_df_m_d_yr_18_lon)), with = F]), use.names=F)


rate_val_df_m_d_yr_18_g[,`Rateable value index (compared to 2002)`:=round(`Rateable value index (compared to 2002)`, 1)]
rate_val_df_m_d_yr_18_g = rate_val_df_m_d_yr_18_g[order(Year, Area)]

# Present on a ggplotly graph
rate_val_comparison_graph = ggplot(rate_val_df_m_d_yr_18_g, aes(x = `Year`, y = `Rateable value index (compared to 2002)`,
                                                                fill = Area,
                                                                text= paste("Area: ", Area, "<br>",
                                                                            "Rateable value index (compared to 2002): ", round(`Rateable value index (compared to 2002)`,0), sep = "")#,
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Rateable value index\n(compared to 2002)")


#rate_val_comparison_graph

#ggplotly(rate_val_comparison_graph, height = 500, tooltip = c("text")) %>% layout(autosize = F,  margin = m, font=lam_ggplotly_font)




# Jobs --------------------------------------------------------------------


# Working age population --------------------------------------------------

work_age_pop = read.xlsx(paste(c(dir_stub,"../data/Jobs/NOMIS/Pop simple 16-64 2020.xlsx"),collapse = ''), sheet = "Sheet1", startRow=2) %>% as.data.table()
work_age_pop_source <- "https://www.nomisweb.co.uk/reports/lmp/la/1946157253/report.aspx?c1=2013265927&c2=2092957699"
# Remove first ro

work_age_pop
work_age_pop_df <- work_age_pop[2:nrow(work_age_pop)]
names(work_age_pop_df)
#work_age_pop_df[, `Lambeth (Numbers)` := NULL]
work_age_pop_df[, 2:= NULL]

work_age_pop_df_m <- melt(work_age_pop_df, id.vars = "X1", variable.name = "Area", value.name = "Working age population 2020 (%)") %>% as.data.table()

# %%
work_age_pop_df_m[,`Working age population 2020 (%)` := round(as.numeric(`Working age population 2020 (%)`),1)]

setnames(work_age_pop_df_m,"X1","Sex")

work_age_pop_df_m[,Sex := gsub(" Aged 16-64", "", Sex)]
work_age_pop_df_m_s <- work_age_pop_df_m[Sex != "All People",]

work_age_pop_df_m_s

# Present on a ggplotly graph
work_age_pop_graph = ggplot(work_age_pop_df_m_s, aes(x = Area, y = `Working age population 2020 (%)`, 
                                                            fill = Sex,
                                                            text= paste("Area: ", Area, "<br>",
                                                                        "Sex: ", Sex, "<br>",
                                                                        "Working age population 2020 (%): ", `Working age population 2020 (%)`, sep = "")#,
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Working age population 2020 (%)")

work_age_pop_graph

# Employment by occupation ------------------------------------------------

emp_occ <- read.xlsx(paste(c(dir_stub, "../data/Jobs/2022/employment_by_occupation_2022.xlsx"),collapse = ''),
                   startRow = 1) %>% as.data.table()
names(emp_occ)
emp_occ_source <- "https://data.london.gov.uk/dataset/model-based-emp_occloyment-estimates"

# Get rid of first line, numbers column
emp_occ_df <- emp_occ[(2:nrow(emp_occ)),]
emp_occ_df <- emp_occ_df[,2:=NULL]
emp_occ_df <- setnames(emp_occ_df, "X1", "Employment category")

# Filter out the group summary rows:

emp_occ_df <- emp_occ_df[grepl("Soc 2020", `Employment category`)==F]



# Melt
emp_occ_latest_refined_m <- melt(emp_occ_df, id.vars="Employment category", variable.name = "Area", value.name = "Workforce in employment category (%)")
emp_occ_latest_refined_m[, `Workforce in employment category (%)`:= round(as.numeric(`Workforce in employment category (%)`), 1)]

# Re-order columns
emp_occ_latest_refined_m = emp_occ_latest_refined_m[order(`Employment category`, Area)]
emp_occ_latest_refined_m[, `Area`:= ordered(`Area`, levels=c("Lambeth", "London", "England"))]
emp_occ_latest_refined_m[, `Employment category`:= ordered(`Employment category`, levels=rev(sort(unique(`Employment category`))))]


# Present on a ggplotly graph
emp_occ_comparison_graph = ggplot(emp_occ_latest_refined_m, aes(x = `Workforce in employment category (%)`, y =  `Employment category`,
                    fill = Area,
                    text= paste("Area: ", Area, "<br>",
                    "Employment category: ", `Employment category`, "<br>",
                    "Workforce in employment category (%): ", `Workforce in employment category (%)`,
                    sep = "")#,
)) +
  geom_col(position = position_dodge2(width = 0.9,reverse = T)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  scale_y_discrete(labels = function(x) stringr::str_wrap(gsub("[0-9] ", "", x), width = 30)) +
  theme(axis.title.y = element_blank()) +
  xlab("Workforce in employment category (%)\nMissing values indicate too low a sample size to estimate")

emp_occ_comparison_graph


# Unemployment rate -------------------------------------------------------

unemp <- read.xlsx(paste(c(dir_stub, "../data/Jobs/2022/mb-unemployment-rates 04-21.xlsx"),collapse = ''),
                   startRow = 1, sheet = "Rates") %>% as.data.table()
names(unemp)
unemp_source <- "https://data.london.gov.uk/dataset/model-based-unemployment-estimates"


c("Area", (names(unemp)[grepl("^Jan",names(unemp)) == T]))

# Keep only years starting with January
unemp_df <- unemp[,c("Area", (names(unemp)[grepl("^Jan",names(unemp)) == T])), with=F]


new_names <- gsub(".*([0-9]{4}).*$", "\\1", names(unemp_df))

names(unemp_df) <- new_names

# Get rid of first line, city of london
unemp_df_c <- unemp_df[(2:nrow(unemp_df)),]
unemp_df_c <- unemp_df_c[Area != "City of London"]


# Keep only the last 5 years
names_latest <- as.character(c("Area", 2018:2021))
unemp_latest <- unemp_df_c[,(names_latest), with=F]



# Keep only Lambeth, London, England
unemp_latest_refined <- unemp_latest[grepl(c("Lambeth|London|England$"), `Area`)]

# Melt
unemp_latest_refined_m <- melt(unemp_latest_refined, id.vars="Area", variable.name = "Year", value.name = "Unemployment rate (%)")
unemp_latest_refined_m[, `Unemployment rate (%)`:= round(as.numeric(`Unemployment rate (%)`), 1)]

# Re-order columns
unemp_latest_refined_m = unemp_latest_refined_m[order(Year, Area)]
unemp_latest_refined_m[, `Area`:= ordered(`Area`, levels=c("Lambeth", "London", "England"))]


# Present on a ggplotly graph
unemp_comparison_graph = ggplot(unemp_latest_refined_m, aes(x = `Year`, y = `Unemployment rate (%)`, 
                             colour = Area, group = Area, linetype = Area,
                             text= paste("Area: ", Area, "<br>",
                                   "Unemployment rate (%): ", `Unemployment rate (%)`, sep = "")#,
)) +
  geom_line() +#position = position_dodge(width = 0.9)) +
  geom_point() +
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Unemployment rate (%)")

unemp_comparison_graph



# Claimant count -------------------------------------------------------

claim <- read.xlsx(paste(c(dir_stub, "../data/Jobs/2022/Claimant count by month.xlsx"),collapse = ''), startRow = 1) %>% as.data.table()
names(claim)
claim_source <- "https://www.nomisweb.co.uk/reports/lmp/la/1946157253/subreports/cc_time_series/report.aspx?"

claim_df = claim[2:(length(Date)-2)]

claim_df[,Date:= as.Date(as.numeric(Date), origin = "1899-12-30")]

claim_df <- claim_df[(Date > as.Date("2017-06-01"))]

claim_df <- claim_df[grep("-06-",as.character(Date))]

claim_df[,2:= NULL]

# Replace . in column names
names(claim_df) <- gsub("\\.", " ", names(claim_df))


# Melt
claim_latest_refined_m <- melt(claim_df, id.vars="Date", variable.name = "Area", value.name = "Claimant count rate (%)")
claim_latest_refined_m[, `Claimant count rate (%)`:= round(as.numeric(`Claimant count rate (%)`), 1)]

claim_latest_refined_m[,Date := year(Date)]

setnames(claim_latest_refined_m, "Date", "Year")

# Re-order columns
claim_latest_refined_m = claim_latest_refined_m[order(Year, Area)]
claim_latest_refined_m[, `Area`:= ordered(`Area`, levels=c("Lambeth", "London", "England"))]


# Present on a ggplotly graph
claim_comparison_graph = ggplot(claim_latest_refined_m, aes(x = `Year`, y = `Claimant count rate (%)`, 
               colour = Area, group=Area, linetype=Area,
               text= paste("Area: ", Area, "<br>",
                  "Claimant count rate (%): ", `Claimant count rate (%)`, sep = "")#,
)) +
  geom_line()+#position = position_dodge(width = 0.9)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Claimant count rate (%)")

claim_comparison_graph

#ggplotly(claim_comparison_graph)

# Claimant count by age -------------------------------------------------------

claim_age_func = function(file_name="../data/Jobs/2022/Claimant count 18-24 time NOMIS.xlsx", age_cat = "16-24"){

claim <- read.xlsx(paste(c(dir_stub, file_name),collapse = ''), startRow = 1) %>% as.data.table()
names(claim)
claim_source <- "https://www.nomisweb.co.uk/reports/lmp/la/1946157253/report.aspx"

claim


claim_df = claim[2:(length(Date)-2)]

claim_df[,Date:= as.Date(as.numeric(Date), origin = "1899-12-30")]

claim_df <- claim_df[(Date > as.Date("2017-06-01"))]

claim_df <- claim_df[grep("-06-",as.character(Date))]

claim_df[,2:= NULL]

# Replace . in column names
names(claim_df) <- gsub("\\.", " ", names(claim_df))


# Melt
claim_latest_refined_m <- melt(claim_df, id.vars="Date", variable.name = "Area", value.name = "Claimant count rate (%)")
claim_latest_refined_m[, `Claimant count rate (%)`:= round(as.numeric(`Claimant count rate (%)`), 1)]

claim_latest_refined_m[,Date := year(Date)]

setnames(claim_latest_refined_m, "Date", "Year")

# Re-order columns
claim_latest_refined_m = claim_latest_refined_m[order(Year, Area)]
claim_latest_refined_m[, `Area`:= ordered(`Area`, levels=c("Lambeth", "London", "England"))]

claim_latest_refined_m[, Age := age_cat]

}

claim_latest_1624 = claim_age_func()

## 25-49
claim_latest_2549 = claim_age_func(file_name="../data/Jobs/2022/Claimant count 25-49 time NOMIS.xlsx", age_cat = "25-49")

## 50+
claim_latest_50 = claim_age_func(file_name="../data/Jobs/2022/Claimant count 50 plus time NOMIS.xlsx", age_cat = "50+")

# Combined
claim_all_ages <- rbindlist(list(claim_latest_1624, claim_latest_2549, claim_latest_50))


# Present on a ggplotly graph
claim_comparison_age_graph = ggplot(claim_all_ages, aes(x = `Year`, y = `Claimant count rate (%)`, 
                                                            colour = Area, group=Area, linetype=Area,
                                                            text= paste("Area: ", Area, "<br>",
                                                                        "Claimant count rate (%): ", `Claimant count rate (%)`, sep = "")#,
)) +
  geom_line()+#position = position_dodge(width = 0.9)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Claimant count rate (%)") +
  facet_wrap(~Age)

claim_comparison_age_graph

#ggplotly(claim_comparison_graph)

# Unemployment by sex, ethnicity -------------------------------------------------------

unemp_m <- read.xlsx(paste(c(dir_stub, "../data/Jobs/2022/Unemployment sex minority 2022 NOMIS.xlsx"),collapse = ''), startRow = 8) %>% as.data.table()
names(unemp_m)
unemp_m_source <- "https://www.nomisweb.co.uk/query/construct/summary.asp?reset=yes&mode=construct&dataset=17&version=0&anal=5&initsel=geog:1946157253,2013265927,2092957699"


unemp_m_df <- unemp_m[1:(nrow(unemp_m)-3)]

percent_cols <- grepl("percent|X", names(unemp_m_df))
unemp_m_df <- unemp_m_df[, ..percent_cols]
names(unemp_m_df) <- c("Category", "Lambeth", "England", "London")


# Melt
unemp_m_latest_refined_m <- melt(unemp_m_df, id.vars="Category", variable.name = "Area", value.name = "Unemployment rate (%)")
unemp_m_latest_refined_m[, `Unemployment rate (%)`:= round(as.numeric(`Unemployment rate (%)`), 1)]

# Re-order columns
unemp_m_latest_refined_m = unemp_m_latest_refined_m[order(Category, Area)]
unemp_m_latest_refined_m[, `Area`:= ordered(`Area`, levels=c("Lambeth", "London", "England"))]

# Replace category names and order
unemp_m_latest_refined_m[, Category:= gsub("16+ unemployment rate - ethnic minority", "Ethnic minority", Category, fixed=T)]
unemp_m_latest_refined_m[Category=="Unemployment rate - aged 16+", Category:= "All"]
unemp_m_latest_refined_m[, Category:= gsub("Unemployment rate females - aged 16\\+", "Female", Category)]
unemp_m_latest_refined_m[, Category:= gsub("Unemployment rate males - aged 16\\+", "Male", Category)]


# Present on a ggplotly graph
unemp_m_comparison_graph = ggplot(unemp_m_latest_refined_m, aes(x = Category, y = `Unemployment rate (%)`, 
                                                            fill = Area,
                                                            text= paste("Area: ", Area, "<br>",
                                                                        "Category: ", Category, "<br>",
                                                                        "Unemployment rate (%): ", `Unemployment rate (%)`, sep = "")#,
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Unemployment rate (%)")

unemp_m_comparison_graph


### Create a disparity graph - i.e. the sub-categories - all

unemp_m_latest_refined_m_all <- unemp_m_latest_refined_m[Category == "All"]
setnames(unemp_m_latest_refined_m_all, "Unemployment rate (%)", "Unemployment rate - all (%)")
unemp_m_latest_refined_m_all[,Category := NULL]

unemp_m_latest_refined_m_disp <- merge(unemp_m_latest_refined_m, unemp_m_latest_refined_m_all, by ="Area", how = "left")
  
unemp_m_latest_refined_m_disp[, "Unemployment rate disparity (%)":= `Unemployment rate (%)` - `Unemployment rate - all (%)`]


# Present on a ggplotly graph
unemp_m_comparison_disparity_graph = ggplot(unemp_m_latest_refined_m_disp[Category != "All"], aes(x = Category, y = `Unemployment rate disparity (%)`, 
                                                                fill = Area,
                                                                text= paste("Area: ", Area, "<br>",
                                                                            "Category: ", Category, "<br>",
                                                                            "Unemployment rate disparity (%): ", `Unemployment rate disparity (%)`, sep = "")#,
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Unemployment rate disparity (%)")

unemp_m_comparison_disparity_graph



# Qualifications level

# Unemployment 16-24 -------------------------------------------------------

pop_by_age <- l_dat_no_perc(paste(c(dir_stub, "../data/Jobs/NOMIS/Population by age group 2021.xlsx"), collapse = ""), 7)[[1]]

#debug(l_dat)
unemp_16 <- l_dat_no_perc(paste(c(dir_stub, "../data/Jobs/2022/Claimant count age 2022 NOMIS.xlsx"), collapse = ""), 7)[[1]]

setnames(unemp_16, "Population", "Claimant count")

unemp_16_source <- "https://www.nomisweb.co.uk/query/construct/summary.asp?reset=yes&mode=construct&dataset=17&version=0&anal=5&initsel=geog:1946157253,2013265927,2092957699"

pop_by_age_f <- pop_by_age[Age %in% c("Aged 16 to 24", "Aged 25 to 49", "Aged 50 to 64")]
pop_by_age_f[, Age := gsub("Aged 16 to 24", "Aged 16-24", Age)]
pop_by_age_f[, Age := gsub("Aged 25 to 49", "Aged 25-49", Age)]
pop_by_age_f[, Age := gsub("Aged 50 to 64", "Aged 50+", Age)]

unemp_16_j <- merge(unemp_16, pop_by_age_f, by = c("Age", "Area name"), how = "left")


unemp_16_j[, `Claimant count %` := round((`Claimant count` / Population)*100, 1)]

# pop_by_age_relevant <- filter(pop_by_age, )


 # unemp_16_df <- unemp_16[1:(nrow(unemp_16)-3)]
 # 
 # percent_cols <- grepl("percent|X", names(unemp_16_df))
 # unemp_16_df <- unemp_16_df[, ..percent_cols]
 # names(unemp_16_df) <- c("Category", "Lambeth", "England", "London")
 # 
 # 
 # # Melt
 # unemp_16_latest_refined_m <- melt(unemp_16_df, id.vars="Category", variable.name = "Area", value.name = "Unemployment rate (%)")
 # unemp_16_latest_refined_m[, `Unemployment rate (%)`:= round(as.numeric(`Unemployment rate (%)`), 1)]
 # 
 # # Re-order columns
 # unemp_16_latest_refined_m = unemp_16_latest_refined_m[order(Category, Area)]
 # unemp_16_latest_refined_m[, `Area`:= ordered(`Area`, levels=c("Lambeth", "London", "England"))]
 # 
 # # Replace category names and order
 # unemp_16_latest_refined_m[, Category:= gsub("16+ unemployment rate - ethnic minority", "Ethnic minority", Category, fixed=T)]
 # unemp_16_latest_refined_m[Category=="Unemployment rate - aged 16+", Category:= "All"]
 # unemp_16_latest_refined_m[, Category:= gsub("Unemployment rate females - aged 16\\+", "Female", Category)]
 # unemp_16_latest_refined_m[, Category:= gsub("Unemployment rate males - aged 16\\+", "Male", Category)]
 # 
 # 
 # # Present on a ggplotly graph
 # unemp_16_comparison_graph = ggplot(unemp_16_latest_refined_m, aes(x = Category, y = `Unemployment rate (%)`,
 #                                                                 fill = Area,
 #                                                                 text= paste("Area: ", Area, "<br>",
 #                                                                             "Category: ", Category, "<br>",
 #                                                                             "Unemployment rate (%): ", `Unemployment rate (%)`, sep = ""),
 # )) +
 #   geom_col(position = position_dodge(width = 0.9)) +
 #   scale_fill_discrete(type=lambeth_palette_graph) +
 #   theme_lam() +
 #   theme(axis.title.x = element_blank()) +
 #   ylab("Unemployment rate (%)")
 # 
 # unemp_16_comparison_graph



# Qualifications level

# Earnings by sex ---------------------------------------------------------

earn <- read.xlsx(paste(c(dir_stub, "../data/Jobs/2022/Earnings by sex Nomis 2021.xlsx"),collapse = ''),
                   startRow = 9) %>% as.data.table()

earn

earn_df <- earn[2:(length(earn)-3)]

no_x <- !grepl("X", names(earn_df))

earn_df <- earn_df[, ..no_x]

names(earn_df) <- gsub(".", " ", names(earn_df), fixed=T)
names(earn_df) <- c("Area", "Male", "Female", "All")


## Split cols
S1 <- earn_df[, list(X = unlist(strsplit(Area, ":"))), by = seq_len(nrow(earn_df))]
S1[, Area := sequence(.N), by = seq_len]
area_cats <- dcast.data.table(S1, seq_len ~ Area, value.var="X")
area_cats <- setnames(area_cats, c("1","2"), c("area_type","area_name")) 


earn_df[,Area:=area_cats$area_name]


earn_df_m <- melt(earn_df, id.vars="Area", variable.name = "Sex", value.name = "Mean weekly earnings (GBP)")

# Make area an ordered factor
earn_df_m[, `Area` := ordered(`Area`, levels = c("Lambeth", "London", "England"))]
earn_df_m[, `Sex` := ordered(`Sex`, levels = c("Female", "All", "Male"))]


earn_df_m[,`Mean weekly earnings (GBP)`:= as.numeric(`Mean weekly earnings (GBP)`)]

summary(earn_df_m)

# Present on a ggplotly graph
earn_comparison_graph = ggplot(earn_df_m, aes(x = Area, y = `Mean weekly earnings (GBP)`, 
                                                            fill = Sex,
                                                            text= paste("Area: ", Area, "<br>",
                                                                        "Sex: ", Sex, "<br>",
                                                                        "Mean weekly earnings (GBP): ", `Mean weekly earnings (GBP)`, sep = "")#,
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Mean weekly earnings (GBP)")

earn_comparison_graph



# Earnings by sex - part-time ---------------------------------------------------------

earn_part <- read.xlsx(paste(c(dir_stub, "../data/Jobs/2022/Earnings by sex part time Nomis 2021.xlsx"),collapse = ''), startRow = 9) %>% as.data.table()

earn_part

earn_part_df <- earn_part[2:(length(earn_part)-3)]

no_x <- !grepl("X", names(earn_part_df))

earn_part_df <- earn_part_df[, ..no_x]

names(earn_part_df) <- gsub(".", " ", names(earn_part_df), fixed=T)
names(earn_part_df) <- c("Area", "Male", "Female", "All")


## Split cols
S1 <- earn_part_df[, list(X = unlist(strsplit(Area, ":"))), by = seq_len(nrow(earn_part_df))]
S1[, Area := sequence(.N), by = seq_len]
area_cats <- dcast.data.table(S1, seq_len ~ Area, value.var="X")
area_cats <- setnames(area_cats, c("1","2"), c("area_type","area_name")) 


earn_part_df[,Area:=area_cats$area_name]


earn_part_df_m <- melt(earn_part_df, id.vars="Area", variable.name = "Sex", value.name = "Mean weekly earnings (GBP)")

# Make area an ordered factor
earn_part_df_m[, `Area` := ordered(`Area`, levels = c("Lambeth", "London", "England"))]
earn_part_df_m[, `Sex` := ordered(`Sex`, levels = c("Female", "All", "Male"))]


earn_part_df_m[,`Mean weekly earnings (GBP)`:= as.numeric(`Mean weekly earnings (GBP)`)]

summary(earn_part_df_m)

# Present on a ggplotly graph
earn_part_comparison_graph = ggplot(earn_part_df_m, aes(x = Area, y = `Mean weekly earnings (GBP)`, 
                                              fill = Sex,
                                              text= paste("Area: ", Area, "<br>",
                                                          "Sex: ", Sex, "<br>",
                                                          "Mean weekly earnings (GBP): ", `Mean weekly earnings (GBP)`, sep = "")#,
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Mean weekly earnings (GBP)")

earn_part_comparison_graph

ggplotly(earn_part_comparison_graph)

# London living wage ------------------------------------------------------

llw <- read.xlsx(paste(c(dir_stub, "../data/Earnings/2022/PROV - Work Geography LWF Table 7 LWF.1a   lwfmgx 2022.xlsx"),collapse = ''), startRow = 4, sheet="All") %>% as.data.table()

llw

llw_df <- llw[grep("E09000022|E12000007|E92000001", X2)]
llw_df <- llw_df[!is.na(`Per.cent`), c(1,3,4)]
llw_df <- llw_df[!grep("City of London", X1), X1 := trimws(X1)]

names(llw_df) <- c("Area", "Employees earning under London Living Wage (thousands)", "Employees earning under London Living Wage (%)")

llw_df[,`:=`(`Employees earning under London Living Wage (thousands)`=as.numeric(`Employees earning under London Living Wage (thousands)`),
             `Employees earning under London Living Wage (%)`=as.numeric(`Employees earning under London Living Wage (%)`))]

summary(llw_df)

# London average
# llw_df_lon <- llw_df[,.(`Employees earning under London Living Wage (%)`=mean(`Employees earning under London Living Wage (%)`), `Employees earning under London Living Wage (thousands)`=sum(`Employees earning under London Living Wage (thousands)`), Area = "London")]
# 
# llw_df_lon <- llw_df[grep("London",Area)]

# Lambeth

# llw_df_lam <- llw_df[grep("Lambeth",Area)]
# llw_df_lam$Area = "Lambeth"

# Combine
llw_df_comb <- llw_df#rbind(llw_df_lon, llw_df_lam)

llw_df_comb$Year <- "2022"



# Present on a ggplotly graph
llw_comparison_graph = ggplot(llw_df_comb, aes(x = Area, y = `Employees earning under London Living Wage (%)`, 
              fill = Area,
              text= paste("Area: ", Area, "<br>",
                 "Year: ", Year, "<br>",
                 "Employees earning under London Living Wage (%): ", `Employees earning under London Living Wage (%)`, sep = "")#,
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Employees earning under London Living Wage (%)")

llw_comparison_graph

ggplotly(llw_comparison_graph)


# Distance to work ---------------------------------------------------------

# Load data
dist_out <- l_dat(paste(c(dir_stub, "../data/Jobs/2022/distance_to_work_census_2021.xlsx"), collapse = ""), 7)

dist_d <- dist_out[[1]]

dist_cats <- dist_out[[2]]
dist_cats

# Make a graph of dist_d in ggplot2

dist_graph <- ggplot(dist_d, aes(y = `Distance travelled to work`, x = `Percentage`, fill = `Area name`,
text= paste("Area name: ", `Area name`, "<br>",
"Distance travelled to work: ", `Distance travelled to work`, "<br>",
"Working population: ", prettyNum(`Population`,big.mark=",", preserve.width="none"), "<br>",
"Percentage: ", `Percentage`, "%"))
) +
geom_col(position = position_dodge2(width = 0.9, reverse = T)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.y = element_blank())+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30), limits = rev(levels(dist_d$`Distance travelled to work`))) +
scale_x_continuous(labels = scales::comma) +
xlab("Percentage of working population (%)")

dist_graph

# Qualifications -------------------------------------------------------

qual <- read.xlsx(paste(c(dir_stub, "../data/Jobs/2022/Qualifications NOMIS 2021.xlsx"),collapse = ''), startRow = 8) %>% as.data.table()

qual

qual_df <- qual[1:(length(qual$X1)-1)]

qual_df

no_x <- grepl("percent|X1", names(qual_df))

qual_df <- qual_df[, ..no_x]

names(qual_df) <- gsub(".", " ", names(qual_df), fixed=T)



#percent_cols <- grepl("percent|X", names(qual_df))
#qual_df <- qual_df[, ..percent_cols]
names(qual_df) <- c("Category", "Lambeth", "Great Britain", "England", "London")


qual_df[,`Great Britain`:=NULL]

qual_df
qual_df[,Category:=gsub(" - aged 16-64", "",Category)]
qual_df[,Category:=gsub("% with ", "",Category)]

cat_levels <- unique(qual_df$Category)


# Melt
qual_latest_refined_m <- melt(qual_df, id.vars="Category", variable.name = "Area", value.name = "Population with qualification (%)")
qual_latest_refined_m[, `Population with qualification (%)`:= round(as.numeric(`Population with qualification (%)`), 1)]

# Re-order columns
qual_latest_refined_m = qual_latest_refined_m[order(Category, Area)]
qual_latest_refined_m[, `Area`:= ordered(`Area`, levels=c("Lambeth", "London", "England"))]
qual_latest_refined_m[, `Category`:= ordered(`Category`, levels=cat_levels)]

# Present on a ggplotly graph
qual_comparison_graph = ggplot(qual_latest_refined_m, aes(x = Category, y = `Population with qualification (%)`, 
                fill = Area,
                text= paste("Area: ", Area, "<br>",
                   "Category: ", Category, "<br>",
                   "Population with qualification (%): ", `Population with qualification (%)`, sep = "")#,
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Population with qualification (%)")

qual_comparison_graph

ggplotly(qual_comparison_graph)

# Unemployment by sex, ethnicity -------------------------------------------------------

# unemp_m <- read.xlsx(paste(c(dir_stub, "../data/Jobs/2022/Unemployment sex minority 2022 NOMIS.xlsx"),collapse = ''), startRow = 8) %>% as.data.table()
# names(unemp_m)
# unemp_m_source <- "https://www.nomisweb.co.uk/query/construct/summary.asp?reset=yes&mode=construct&dataset=17&version=0&anal=5&initsel=geog:1946157253,2013265927,2092957699"
# 
# 
# unemp_m_df <- unemp_m[1:(nrow(unemp_m)-3)]
# 
# percent_cols <- grepl("percent|X", names(unemp_m_df))
# unemp_m_df <- unemp_m_df[, ..percent_cols]
# names(unemp_m_df) <- c("Category", "Lambeth", "England", "London")
# 
# 
# # Melt
# unemp_m_latest_refined_m <- melt(unemp_m_df, id.vars="Category", variable.name = "Area", value.name = "Unemployment rate (%)")
# unemp_m_latest_refined_m[, `Unemployment rate (%)`:= round(as.numeric(`Unemployment rate (%)`), 1)]
# 
# # Re-order columns
# unemp_m_latest_refined_m = unemp_m_latest_refined_m[order(Category, Area)]
# unemp_m_latest_refined_m[, `Area`:= ordered(`Area`, levels=c("Lambeth", "London", "England"))]
# 
# # Replace category names and order
# unemp_m_latest_refined_m[, Category:= gsub("16+ unemployment rate - ethnic minority", "Ethnic minority", Category, fixed=T)]
# unemp_m_latest_refined_m[Category=="Unemployment rate - aged 16+", Category:= "All"]
# unemp_m_latest_refined_m[, Category:= gsub("Unemployment rate females - aged 16\\+", "Female", Category)]
# unemp_m_latest_refined_m[, Category:= gsub("Unemployment rate males - aged 16\\+", "Male", Category)]
# 
# 
# # Present on a ggplotly graph
# unemp_m_comparison_graph = ggplot(unemp_m_latest_refined_m, aes(x = Category, y = `Unemployment rate (%)`, 
#                                                                 fill = Area,
#                                                                 text= paste("Area: ", Area, "<br>",
#                                                                             "Category: ", Category, "<br>",
#                                                                             "Unemployment rate (%): ", `Unemployment rate (%)`, sep = "")#,
# )) +
#   geom_col(position = position_dodge(width = 0.9)) +
#   scale_fill_discrete(type=lambeth_palette_graph) +
#   theme_lam() +
#   theme(axis.title.x = element_blank()) +
#   ylab("Unemployment rate (%)")
# 
# unemp_m_comparison_graph
# 
# 
# ### Create a disparity graph - i.e. the sub-categories - all
# 
# unemp_m_latest_refined_m_all <- unemp_m_latest_refined_m[Category == "All"]
# setnames(unemp_m_latest_refined_m_all, "Unemployment rate (%)", "Unemployment rate - all (%)")
# unemp_m_latest_refined_m_all[,Category := NULL]
# 
# unemp_m_latest_refined_m_disp <- merge(unemp_m_latest_refined_m, unemp_m_latest_refined_m_all, by ="Area", how = "left")
# 
# unemp_m_latest_refined_m_disp[, "Unemployment rate disparity (%)":= `Unemployment rate (%)` - `Unemployment rate - all (%)`]
# 
# 
# # Present on a ggplotly graph
# unemp_m_comparison_disparity_graph = ggplot(unemp_m_latest_refined_m_disp[Category != "All"], aes(x = Category, y = `Unemployment rate disparity (%)`, 
#                                                                                                   fill = Area,
#                                                                                                   text= paste("Area: ", Area, "<br>",
#                                                                                                               "Category: ", Category, "<br>",
#                                                                                                               "Unemployment rate disparity (%): ", `Unemployment rate disparity (%)`, sep = "")#,
# )) +
#   geom_col(position = position_dodge(width = 0.9)) +
#   scale_fill_discrete(type=lambeth_palette_graph) +
#   theme_lam() +
#   theme(axis.title.x = element_blank()) +
#   ylab("Unemployment rate disparity (%)")
# 
# unemp_m_comparison_disparity_graph



# Qualifications level

# Index of multiple deprivation ---------------------------------------------------------

# NOTES: (from xlsx) The deprivation gap for each local authority is calculated by subtracting the lowest 'Income Score (rate)' from the highest 'Income Score (rate)' within that local authority. 

# In Lambeth, 15.3% of the population was income-deprived in 2019. https://www.ons.gov.uk/visualisations/dvc1371/#/E09000022


# Load data
imd <- read.xlsx(paste(c(dir_stub, "../data/Jobs/2022/localincomedeprivationdata.xlsx"), collapse = ""), sheet = "Local authorities") %>% as.data.table()

imd


# Replace . in column names with ' '

names(imd) <- gsub(".", " ", names(imd), fixed=T)

names(imd)

# Make a df for Lambeth
imd_lambeth <- imd[grep("Lambeth", `Local Authority District name (2019)`),]

# rename "Local Authority District name (2019)" to "Area name"
setnames(imd_lambeth, "Local Authority District name (2019)", "Area name")

# Keep only the columns we need
imd_lambeth <- imd_lambeth[, .(`Income deprivation- Average score`, `Area name`)]

# Make a df for London where District code starts with E09
imd_london <- imd[grep("^E09", `Local Authority District code (2019)`),.(`Income deprivation- Average score` = mean(`Income deprivation- Average score`), `Area name`="London")]

# Make a df with average Income deprivation-Average score for all districts
imd_eng <- imd[, .(`Income deprivation- Average score` = mean(`Income deprivation- Average score`), `Area name`="England")]

# Combine the three dfs
imd_combined <- rbind(imd_lambeth, imd_london, imd_eng)

# Load in other data
imd_oth <- read.xlsx(paste(c(dir_stub, "../data/Jobs/2022/localincomedeprivationdata.xlsx"), collapse = ""), sheet = "Rankings for all indicators") %>% as.data.table()

# Replace . in column names with ' '
names(imd_oth) <- gsub(".", " ", names(imd_oth), fixed=T)

# Keep only "Local Authority District code (2019)", "Local Authority District name (2019)", "Deprivation gap (percentage points)", "Income deprivation rate"
imd_oth <- imd_oth[, .(`Deprivation gap (percentage points)`, `Income deprivation rate`,`Income deprivation rate ranking`, `Local Authority District name (2019)`, `Local Authority District code (2019)`)]

# Make a df for Lambeth
imd_oth_lambeth <- imd_oth[grep("Lambeth", `Local Authority District name (2019)`),]

# rename "Local Authority District name (2019)" to "Area name"
setnames(imd_oth_lambeth, "Local Authority District name (2019)", "Area name")

# Remove "Local Authority District code (2019)" column
imd_oth_lambeth[,"Local Authority District code (2019)":= NULL]

# Make a df for London where District code starts with E09
imd_oth_london <- imd_oth[grep("^E09", `Local Authority District code (2019)`),.(`Deprivation gap (percentage points)` = mean(`Deprivation gap (percentage points)`), `Income deprivation rate` = mean(`Income deprivation rate`),
`Income deprivation rate ranking` = mean(`Income deprivation rate ranking`),
 `Area name`="London")]

# Make a df with average Income deprivation-Average score for all districts
imd_oth_eng <- imd_oth[, .(`Deprivation gap (percentage points)` = mean(`Deprivation gap (percentage points)`), `Income deprivation rate` = mean(`Income deprivation rate`),
`Income deprivation rate ranking` = mean(`Income deprivation rate ranking`), `Area name`="England")]

# Combine the three dfs
imd_oth_combined <- rbind(imd_oth_lambeth, imd_oth_london, imd_oth_eng)

# Combine the two dfs
imd_combined <- imd_oth_combined %>% as.data.table()#merge(imd_combined, imd_oth_combined, by = "Area name")

# Remove column Income deprivation- Average score from imd_combined
#imd_combined[,`Income deprivation- Average score`:= NULL]

# Multiply numbers by 100 
imd_combined[,`:=`(`Deprivation gap (percentage points)`= round(`Deprivation gap (percentage points)`*100,1), 
                  `Income deprivation rate` = round(`Income deprivation rate`*100,1),
                  `Income deprivation rate ranking` = round(`Income deprivation rate ranking`,1))]


# Rename Depriavtion gap (percentage points) to Deprivation gap (%) and Income deprivation rate to Income deprivation rate (%)
setnames(imd_combined, "Deprivation gap (percentage points)", "Deprivation gap (%)")
setnames(imd_combined, "Income deprivation rate", "Income deprivation (%)")
setnames(imd_combined, "Income deprivation rate ranking", "Income deprivation rate ranking (out of 316 LAs)")

# Melt the df
imd_combined_m <- melt(imd_combined, id.vars = "Area name", variable.name = "Indicator", value.name = "Value (%)")

# Make area name an ordered factor with levels Lambeth, London, England
imd_combined_m$`Area name` <- ordered(imd_combined_m$`Area name`, levels = c("Lambeth", "London", "England"))

# Make a graph of imd_combined in ggplot2

imd_graph <- ggplot(imd_combined_m, aes(x = `Area name`, y = `Value (%)`, fill = `Area name`,
    text= paste("Area name: ", `Area name`, "<br>",
    "Indicator: ", `Indicator`, "<br>",
    "Value (%): ", `Value (%)`))) +

geom_col(position = position_dodge2(width = 0.9)) +
scale_fill_discrete(type=lambeth_palette_graph) +
theme_lam() +
theme(axis.title.x = element_blank(),
legend.position="none")+#,
#axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
scale_y_continuous(labels = scales::comma) +
xlab("Percentage (%)") +
facet_wrap(~Indicator, scales = "free_y", nrow = 1)

imd_graph



# Poverty rate --------------

pov <- fread(paste(c(dir_stub, "../data/Financial stability/Poverty rates by London borough (2019_20).csv"),collapse = ''), skip = 2) %>% as.data.table()

pov

#pov_df <- pov[grep("^E09", X2)]
pov_df <- pov[!is.na(`Number in poverty (AHC)`), c(1:6)]

names(pov_df)

pov_df <- pov_df[!grep("City of London", `London borough`)]
names(pov_df)[1] <- c("Area")

pov_df_nums <- pov_df

#pov_df <- gsub("%", "", pov_df)

pov_df_nums[] <- lapply(pov_df_nums, function(x) as.numeric(gsub("%", "", x)))

pov_df_nums$Area <- pov_df$Area

# Lambeth

pov_df_lam <- pov_df_nums[grep("Lambeth",Area)]
pov_df_lam$Area = "Lambeth"

# London average
# pov_df_lon <- pov_df_nums[,.(
#                         `Number in poverty (AHC)`=sum(as.numeric(`Number in poverty (AHC)`), na.rm=T),
#                         `Confidence Interval (+/-)`=mean(as.numeric(`Confidence Interval (+/-)`), na.rm=T),
#                         `Poverty rate (AHC)`=mean(as.numeric(`Poverty rate (AHC)`), na.rm=T),
#                         `Poverty rate (AHC) CI Lower`= mean(as.numeric(`Poverty rate (AHC) CI Lower`), na.rm=T),                             `Poverty rate (AHC) CI Upper`= mean(as.numeric(`Poverty rate (AHC) CI Upper`), na.rm=T), Area = "London")]


# To get England and London need to load in another data source

pov_all <- read.xlsx(paste(c(dir_stub, "../data/Financial stability/CBP7096-poverty-trends-by-country-and-region.xlsx"),collapse = ''), sheet = "Raw data", startRow = 6) %>% as.data.table()

# Replace . in column names with " "
names(pov_all) = gsub("\\.", " ", names(pov_all))

names(pov_all)

pov_eng_lon <- pov_all[Group == "Total" &	`Poverty measure`=="Relative"&	Quantity=="%"&	`Housing costs`=="AHC"& (	Region=="England" | Region=="London")] 

cols <- c("Region", "2017/18-2019/20")

pov_eng_lon <- pov_eng_lon[,..cols]
names(pov_eng_lon) <- c("Area", "Poverty rate (AHC)")


# Combine
pov_df_comb <- rbindlist(list(pov_eng_lon, pov_df_lam), use.names = T, fill = T)

pov_df_comb$Year <- "2017-2020"

pov_df_comb <- setnames(pov_df_comb, "Poverty rate (AHC)", "Poverty rate (AHC, %)")

# Make Area name an ordered factor with Lambeth, London, England
pov_df_comb[, Area := ordered(Area, levels = c("Lambeth", "London", "England"))]

pov_df_comb[,`Poverty rate (AHC, %)`:=as.numeric(`Poverty rate (AHC, %)`)]

# Present on a ggplotly graph
pov_comparison_graph = ggplot(pov_df_comb, aes(x = Area, y = `Poverty rate (AHC, %)`, 
                        fill = Area,
                        text= paste("Area: ", Area, "<br>",
                              "Year: ", Year, "<br>",
                              "Poverty rate (AHC, %): ", `Poverty rate (AHC, %)`, sep = ""))) +
 geom_col(position = position_dodge(width = 0.9)) +
 scale_fill_discrete(type=lambeth_palette_graph) +
 theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Poverty rate (AHC, %)") +
  expand_limits(y=0)

#pov_comparison_graph

#ggplotly(pov_comparison_graph)

# Children low income households ------------------------------------------

child_li <- read.xlsx(paste(c(dir_stub, "../data/Financial stability/children-in-low-income-families-local-area-statistics-2014-to-2021.xlsx"),collapse = ''), sheet = "1_Relative_Local_Authority", startRow = 10) %>% as.data.table()

# Replace . in column names with " "
names(child_li) = gsub("\\.", " ", names(child_li))

names(child_li)

cols <- c("Local Authority [note 2]",  "Area Code",
          "Number of children FYE 2021 [p]", "Percentage of children FYE 2021 (%) [p] [note 3]"
          )

child_li_df <- child_li[, ..cols]

names(child_li_df) <- c("Area", "Area Code", "Number of children in low income households", "Percentage of children in low income households (%)")

child_li_df <- child_li_df[,`Percentage of children in low income households (%)` := round(`Percentage of children in low income households (%)` * 100,1)]

child_li_df_lam <- child_li_df[Area=="Lambeth"]
child_li_df_lam <- child_li_df_lam[,`Area Code` := NULL]


# London
child_li_df_lon <- child_li_df[grep("^E09", `Area Code`)]
child_li_df_lon <- child_li_df_lon[Area != "City of London"]


child_li_df_lon_g <- child_li_df_lon[,.(`Number of children in low income households` = sum(`Number of children in low income households`),
                                        `Percentage of children in low income households (%)` = mean(`Percentage of children in low income households (%)`), 
                                         Area = "London"
                                        )]

# England
child_li_df_eng <- child_li_df[grep("^E", `Area Code`)]
#child_li_df_eng <- child_li_df_eng[Area != "City of London"]


child_li_df_eng_g <- child_li_df_eng[,.(`Number of children in low income households` = sum(`Number of children in low income households`),
                                        `Percentage of children in low income households (%)` = mean(`Percentage of children in low income households (%)`), 
                                        Area = "England"
)]

# Combine
child_li_df_comb <- rbindlist(list(child_li_df_lon_g,child_li_df_eng_g, child_li_df_lam), use.names = T, fill = T)

child_li_df_comb$Year <- "2021"

# Make Area name an ordered factor with Lambeth, London, England
child_li_df_comb[, Area := ordered(Area, levels = c("Lambeth", "London", "England"))]

child_li_df_comb[,`Percentage of children in low income households (%)`:=as.numeric(`Percentage of children in low income households (%)`)]

# Present on a ggplotly graph
child_li_comparison_graph = ggplot(child_li_df_comb, aes(x = Area, y = `Percentage of children in low income households (%)`, 
                                               fill = Area,
                                               text= paste("Area: ", Area, "<br>",
                                                           "Year: ", Year, "<br>",
                                                           "Percentage of children in low income households (%): ", `Percentage of children in low income households (%)`, sep = ""))) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Percentage of children in low income households (%)") +
  expand_limits(y=0)

#child_li_comparison_graph

#ggplotly(child_li_comparison_graph)

# Pensioners on pension credit ------------------------------------------

pension <- read.xlsx(paste(c(dir_stub, "../data/Financial stability/DWP statxplore pension credit aug 2018-2022.xlsx"),collapse = ''), sheet = "Data Sheet 0", startRow = 10) %>% as.data.table()

pop_by_age <- l_dat_no_perc(paste(c(dir_stub, "../data/Jobs/NOMIS/Population by age group 2021.xlsx"), collapse = ""), 7)[[1]]

# Replace . in column names with " "
names(pension) = gsub("\\.", " ", names(pension))

names(pension)

cols <- c("X2",  "Aug-18",  "Aug-19",  "Aug-20",  "Aug-21",  "Aug-22")

pension_df <- pension[2:4, ..cols]

pension_df <- melt(pension_df, id.vars= "X2", variable.name = "Year", value.name = "Number of people aged 65+ on pension credit")

names(pension_df)[1] <- c("Area")

# Join on population
pop_by_age_65 <- pop_by_age[Age=="Aged 65+"]

pension_df_m <- merge(pension_df, pop_by_age_65, by.x="Area", by.y="Area name", how = "left")

pension_df_m[,Age:=NULL]

pension_df_m[, `Percentage of people aged 65+ on pension credit (%)` := round((`Number of people aged 65+ on pension credit`/Population)*100,1)]

pension_df_m <- setcolorder(pension_df_m,c("Area","Number of people aged 65+ on pension credit","Percentage of people aged 65+ on pension credit (%)", "Population"))

pension_df_m$Year <- gsub(".*-", "", pension_df_m$Year)
pension_df_m$Year <- paste0("20", pension_df_m$Year)

# Make Area name an ordered factor with Lambeth, London, England
pension_df_m[, Area := ordered(Area, levels = c("Lambeth", "London", "England"))]

pension_df_m[,`Percentage of people aged 65+ on pension credit (%)`:=as.numeric(`Percentage of people aged 65+ on pension credit (%)`)]

# Present on a ggplotly graph
pension_comparison_graph = ggplot(pension_df_m, aes(x = Year, y = `Percentage of people aged 65+ on pension credit (%)`, linetype = Area, group = Area,
                             colour = Area,
                             text= paste("Area: ", Area, "<br>",
                                   "Year: ", Year, "<br>",
                                   "Percentage of people aged 65+ on pension credit (%): ", `Percentage of people aged 65+ on pension credit (%)`, sep = ""))) +
  geom_line()+#position = position_dodge(width = 0.9)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Percentage of people aged 65+ on pension credit (%)") +
  expand_limits(y=0)

#pension_comparison_graph

ggplotly(pension_comparison_graph)

# Load packages, initial variables ----------------------------------------

require(ggplot2)
require(plotly)
require(data.table)
require(rstudioapi)
#require(xlsx)
#library(readxl)
library(openxlsx)
#library(gglaplot)

# Not in function from here: https://stackoverflow.com/questions/5831794/opposite-of-in
'%!in%' <- function(x,y)!('%in%'(x,y))

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

# Some map parameters
uk_lat = 51.46593
uk_lng = -0.10652

# Make a leaflet map
require(leaflet)
require(leaflet.extras)
require(sf)
require(rgdal)
require(htmltools)
require(crosstalk)


# Loading some spatial projections 
wgs84 = '+proj=longlat +ellps=WGS84 +datum=WGS84'
bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894'
mrc = '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs'
# 
prj.wgs84 <- CRS(wgs84)
prj.bng <- CRS(bng)
prj.mrc <- CRS(mrc)

# Simplify ward outlines - only needed to do once
# require(rmapshaper)
# 
# wards_sf <- read_sf("new_lambeth_wards_wgs84_riverclipped.geojson")
# # proj4string(wards_ni_sp)
# 
# wards_sf_simple <- rmapshaper::ms_simplify(input = wards_sf, keep_shapes = T, keep = 0.01)
# 
# write_sf(wards_sf_simple, paste(c(dir_stub, "../data/Map/new_lambeth_wards_wgs84_riverclipped_simple.geojson"),collapse = ''), delete_dsn = T)

wards_sf <- read_sf(paste(c(dir_stub, "../data/Map/new_lambeth_wards_wgs84_riverclipped_simple.geojson"),collapse = ''))

wards_sf <- st_transform(wards_sf, prj.wgs84)


l_dat_wd <- function(path, start_row = 2, percentage_cut_off = 0.5, replace_colon = T){
  df <- read.xlsx(path, startRow = start_row) %>% data.table()
  
  # Remove rows with no data
  df <- df[!is.na(df[[1]]),]
  df <- df[!is.na(df[[2]]),]
  
  # Remove rows where column 1 contains Total
  df <- df[!grepl("Total", df[[1]]),]
  
  # Remove everything before : in column names
  
  if (replace_colon == T){
    names(df) = gsub(".*:", "", names(df))
  }
  
  
  #print(names(df))
  
  # Replace . in column names with " "
  names(df) = gsub("\\.", " ", names(df))
  
  # Set first name to 2022 ward
  names(df)[1] <- "2022 ward"
  
  # Remove anything before : in Area names
  df[, `Area code` := stringr::str_extract(`2022 ward`, ".*:")]
  df[, `Area code` := gsub(" :", "", `Area code`)]
  df[, `2022 ward` := gsub(".*:", "", `2022 ward`)]
  
  # Rename ward column
  #setnames(df, "2022 ward", "Area name")
  
  # Replace (Lambeth) in Area name and trim
  
  df[, `2022 ward` := (gsub(" (Lambeth)", "", `2022 ward`, fixed = T))]
  
  #stringr::str_trim
  
  # Rename 2022 ward
  setnames(df, "2022 ward", "Area name")
  
  # Trim whitespaces
  df[, `Area name` := stringr::str_trim(`Area name`)]
  
  
  # Add % sign to names of relevant columns
  names(df)[grep("^X", names(df))] <- paste(names(df)[grep("^X", names(df))-1], " %", sep = "")
  
  # Melt table with first column as id
  df_m <- melt(df, id.vars = c(names(df)[1], names(df)[ncol(df)]), variable.name = "Variable", value.name = "Value")
  
  # Trim whitespaces
  df_m[, `Variable` := stringr::str_trim(`Variable`)]
  
  #print(df_m)
  
  # Create percentage column from if 'Area name' contains %
  df_m[, `Percentage` := ifelse(grepl("%", `Variable`), TRUE, FALSE)]
  
  # Replace ' %' in Area name with ''
  df_m[, `Variable` := gsub(" %", "", `Variable`)]
  
  # Make Value a numeric column
  df_m[, `Value` := as.numeric(`Value`)]
  
  # Make Area name an ordered factor with Lambeth, London, England
  #df_m[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England"))]
  
  cat_col <- names(df)[1]
  
  dc_formula <- paste0("`",cat_col,"` + ","`Variable` + `Area code`", " ~ ", "Percentage")
  
  # dcast table with first column and Area name on left, percentage on right, Value as value column
  df_d <- dcast(df_m, dc_formula, value.var = "Value", fun.aggregate = mean)
  
  #print(df_d)
  
  # Rename column FALSE to Population
  names(df_d)[ncol(df_d)-1] = "Population"
  
  # Rename column TRUE to Percentage
  names(df_d)[ncol(df_d)] = "Percentage"
  
  # Create new df for Area name = Lambeth
  df_lambeth <- df_d#[df_d$`Area name` == "Lambeth",]
  
  # Keep only categories of at least X% of the population (default 0.5%)
  df_lambeth <- df_lambeth[df_lambeth$`Percentage` >= percentage_cut_off,]
  
  # Order df_lambeth descending by Percentage
  df_lambeth <- df_lambeth[order(df_lambeth$`Area name`),]
  
  # Create variable from first column
  
  cat_order <- unique(df_lambeth$`Area name`)
  
  #col <- names(df_lambeth)[1]
  #cat_order <- df_lambeth[,col, with=F]
  
  print(cat_order)
  
  # In df_d set column 1 as ordered factor with levels cat_order
  df_d[, (1) := lapply(.SD, ordered, levels=cat_order), .SDcols = 1]
  
  # filter df_d by column 1 for NAs
  df_d <- df_d[!is.na(df_d[[1]]),]
  
  # order df_d by column 1 and `Area name`
  df_d <- df_d[order(`Area name`, Variable),]
  
  out_stuff <- list(df_d, cat_order)
  
  
  return(out_stuff)
  
}


welcome_popup <- function(theme, year) {output <- paste(sep = "",
  "<b>Welcome to the interactive map for ",theme,".</b>","<br/><br/>",
  "Map layers are in the top right, and filters to add/remove different metrics to/from the map.","<br/><br/>",
  "Zoom buttons, a fullscreen button, a search location bar, and a reset button are in the top left.","<br/><br/>",
  "In the bottom left is a legend for the variables shown on the map.","<br/><br/>",
  "All data is from ", year,". Ward shapes have been simplified and so boundaries will not match exactly to their official locations."
)
return(output)
}

# Function to apply a metric to an area/variable
apply_metric_area <- function(map_sp, variable_name, value_name, prefix = "", suffix = "%", rounding = 1) {
  
  map_sp = map_sp[map_sp@data$Variable %in% variable_name,]
  
  map_sp@data$metric <- select(as.data.frame(map_sp@data), paste(value_name))[,1] %>% as.numeric()
  map_sp@data$variable <- select(as.data.frame(map_sp@data), Variable)[,1] %>% as.character()
  
  map_sp@data$labs <- with(map_sp@data, paste('<p><font size="+1"><b>', WARD_NAME,
                                              '</b></font></p><p><b>', "Variable", ':</b> ', map_sp@data$variable,
                                              '</p><p>',
                                              '</b></font></p><p><b>', get("value_name"), ':</b> ', prefix, round(map_sp@data$metric,rounding),suffix,
                                              sep = ""))
  
  map_sp@data$hover <- with(map_sp@data, paste('<p><b>', WARD_NAME,
                                               '</b></font></p><p><b>', "Variable", ':</b> ', map_sp@data$variable,
                                               '</p><p>',
                                               '</b></font></p><p><b>', get("value_name"), ':</b> ', prefix, round(map_sp@data$metric,rounding),suffix,
                                               sep = ""))
  
  ward_metric_numbers <- colorNumeric(palette = "viridis", domain = map_sp@data$metric, na.color = "#ffffff")
  
  
  ## Prepping data for crosstalk integration
  
  # Removing unnecessary columns
  map_sp_cut <- map_sp; map_sp_cut@data <- select(map_sp_cut@data, WARD_NAME, variable, metric, labs)
  
  # Preparing data for filters
  sd_ward <- SharedData$new(map_sp)
  sd_df_ward <- SharedData$new(map_sp@data, group = sd_ward$groupName())
  
  
  list_output <- list(map_sp, ward_metric_numbers, sd_ward,  sd_df_ward)
  
  
  return(list_output)
}

ward_map_simple <- function(map_df, variable_name = descending_sort_vars_not_all, pref = "", suff = "%", popup = welcome_popup_eng) {
  # Make sure you have the right version of crosstalk / leaflet to be compatible with one another. See here for the discussion: https://github.com/rstudio/crosstalk/issues/47 , need to install version: devtools::install_github("dmurdoch/leaflet@crosstalk4"). Note that this is an old version of leaflet
  
  # Here are the changes you need to make to leaflet package 2.0.0 to make it compatible with crosstalk: https://github.com/rstudio/leaflet/pull/499/commits/8de6d8307c47bdd2ba60164abec03dc1d19082d2 , based on conversation from here: https://github.com/rstudio/leaflet/pull/499
  
  map_df = map_df[map_df@data$variable %in% variable_name,]
  
  # Add map with filter
  map <- leaflet(width = "100%", height = 600) %>% 
    setView(lng = uk_lng, lat = uk_lat, zoom = 12) %>%
    
    # Register the cluster marker feature group plugin (this is not working)
    # reg_plug(feature_group_plugin) %>%
    
    # Base tiles - Here is the full list: http://leaflet-extras.github.io/leaflet-providers/preview/index.html
    # addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$CartoDB.Voyager, group = "Carto (default)") %>% # Need leaflet extras
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Grey and white") %>%
    
    # Following from Leaflet Extra
    addResetMapButton() %>%
    # http://rpubs.com/bhaskarvk/leaflet-heat
    
    addSearchOSM(options = searchOptions(collapsed = F, autoCollapse = F, textPlaceholder = "Search places in UK")) %>%
    addFullscreenControl() %>% # Need leaflet extra
    ### Add a drawing menu (leaflets extra). Code from here: http://rpubs.com/bhaskarvk/measure-path
    #addDrawToolbar(targetGroup = 'model',
    #               polygonOptions = F,
    #               rectangleOptions = F,
    #               markerOptions = F,
    #               circleMarkerOptions = F,
    #               editOptions = editToolbarOptions(edit = F,
    #                                                selectedPathOptions = selectedPathOptions())) %>%
    #addLayersControl(overlayGroups = c(variable_name), options =
    #                   layersControlOptions(collapsed=FALSE)) %>%
    #addMeasurePathToolbar(options =
  #                        measurePathOptions(imperial = F,
  #                                           minPixelDistance = 100,
  #                                          showDistances = T)) %>%
  
  # 
  # # Add a popup to introduce the map
  addPopups(uk_lng, uk_lat, popup, options = popupOptions(closeButton = F)) %>%
    # 
    # # Add panes to the map to make sure that points are always above polygons
    addMapPane("polys", zIndex = 410) #%>% # shown below circles
  #addMapPane("circles", zIndex = 420) #%>% # shown above polys
  
  
  #map_mark <- map %>%
  
  # add the base polygons to the map.
  
  for (x in 1:length(variable_name)){
    
    map_df_poly = map_df[map_df@data$variable %in% variable_name[x],]
    
    ward_metric_numbers_sub <- colorNumeric(palette = "viridis", domain = map_df_poly$metric, na.color = "#ffffff")
    
    map <- map %>% 
      addPolygons(data = map_df_poly, group = variable_name[x], color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  popup = ~lapply(labs, HTML), label = ~lapply(hover, HTML),
                  fillColor = ~ward_metric_numbers_sub(map_df_poly$metric),
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F),
                  options = pathOptions(pane = "polys")) %>%
      
      addLegend("bottomleft", values = map_df_poly$metric, pal = ward_metric_numbers_sub,
                title = paste(map_df_poly$variable[1],"<br>percentage"),
                opacity = 1, group = variable_name[x], labFormat = labelFormat(prefix = pref, suffix = suff)) #%>%
    
    
  }
  
  # add the markers to the map (separately). Guide here to layers - https://rstudio.github.io/leaflet/showhide.html . I am creating a filterable layer of markers by section. The downside of this is that it creates 5 x cluster schemes instead of one
  
  # Add a search for markers. To get this to work, need to do a hack with the leaflet.extras package files. see here https://github.com/bhaskarvk/leaflet.extras/issues/143, comment by TacticalFate:
  
  #   I figured out how to make the search work with the CircleMarkers (removing the path check), you have to go into your R library path
  # #R Library path#\leaflet.extras\htmlwidgets\build\lfx-search\
  # 
  # Open lfx-search-prod.js and search for "e instanceof t.Path ||" , and then delete it and save the file. Your CircleMarker search should work now
  
  #map_mark <- map %>%
  
  # addSearchFeatures( # Need leafletextras for this to work
  #  targetGroups = variable_name,
  #  options = searchFeaturesOptions(
  #  zoom=11, openPopup = T, firstTipSubmit = TRUE,
  # autoCollapse = T, hideMarkerOnCollapse = T, collapsed = F, textPlaceholder = "Search Wards" ))
  
  # Hide and show layers
  map_mark <- map %>%
    addLayersControl(
      baseGroups = c("Carto (default)", "Satellite", "Grey and white"),
      overlayGroups = c(variable_name), 
      options = layersControlOptions(collapsed = T))  %>%
    # Things hidden by default. Show only Wards by default
    hideGroup(variable_name[-1]) # , "LSOA", "Deprivation vs Reach"
  
  
  return(map_mark)
}

#undebug(l_dat_wd)


# Country of birth --------------------------------------------------------------
# Load data

cob <- l_dat_wd(paste(c(dir_stub, "../data/Ward level/TS004 Country of birth Lambeth ward census 2021.xlsx"),collapse = ''), start_row = 7)# %>% data.table()

cob_out_m <- cob[[1]]

cob_out_m$Theme <- "Country of birth"
cob_out_m$Year <- 2021

# Prepare spatial data 

cob_out_join <- cob_out_m[,c("Theme", "Year",  "Area name", "Variable", "Percentage"), with = F]# %>%

# vor_intersect_sf$pasted_location <- with(vor_intersect_sf, paste(longitude, latitude, sep = " "))

wards_sf_j_cob <- merge(wards_sf, cob_out_join, by.x = "WARD_NAME", by.y = "Area name", all.x = T)
wards_sf_j_sp_cob <-  as(wards_sf_j_cob, 'Spatial')

# Map variables to return to rmd

welcome_popup_cob <- welcome_popup(theme = unique(wards_sf_j_cob$Theme), year = unique(wards_sf_j_cob$Year))

# Sort variable by mean percentage, remove 'All usual residents'
descending_sort_vars_cob <- group_by(wards_sf_j_sp_cob@data, Variable) %>% summarise(Mean = mean(Percentage, na.rm=T)) %>% arrange(desc(Mean))
descending_sort_vars_not_all_cob <- descending_sort_vars_cob$Variable[-1]


list_out_cob <- apply_metric_area(wards_sf_j_sp_cob, variable_name = descending_sort_vars_not_all_cob, value_name = "Percentage")

wards_sf_j_sp_out_cob <- list_out_cob[[1]]
ward_metric_numbers_cob <- list_out_cob[[2]]
sd_ward_cob <- list_out_cob[[3]]
sd_df_ward_cob <- list_out_cob[[4]]

#ward_map_simple(map_df = wards_sf_j_sp_out, variable_name = descending_sort_vars_not_all, popup = welcome_popup_cob)
# English proficiency --------------------------------------------------------------
# Load data

eng <- l_dat_wd(paste(c(dir_stub, "../data/Ward level/TS029 Proficiency in English Lambeth ward census 2021.xlsx"),collapse = ''), start_row = 7)# %>% data.table()

eng_out_m <- eng[[1]]

eng_out_m$Theme <- "Proficiency in English"
eng_out_m$Year <- 2021

# Prepare spatial data 

eng_out_join <- eng_out_m[,c("Theme", "Year",  "Area name", "Variable", "Percentage"), with = F]# %>%

# vor_intersect_sf$pasted_location <- with(vor_intersect_sf, paste(longitude, latitude, sep = " "))

wards_sf_j_eng <- merge(wards_sf, eng_out_join, by.x = "WARD_NAME", by.y = "Area name", all.x = T)
wards_sf_j_sp_eng <-  as(wards_sf_j_eng, 'Spatial')

# Map variables to return to rmd

welcome_popup_eng <- welcome_popup(theme = unique(wards_sf_j_eng$Theme), year = unique(wards_sf_j_eng$Year))

# Sort variable by mean percentage, remove 'All usual residents'
descending_sort_vars_eng <- group_by(wards_sf_j_sp_eng@data, Variable) %>% summarise(Mean = mean(Percentage, na.rm=T)) %>% arrange(desc(Mean))
descending_sort_vars_not_all_eng <- descending_sort_vars_eng$Variable[-1]


list_out_eng <- apply_metric_area(wards_sf_j_sp_eng, variable_name = descending_sort_vars_not_all_eng, value_name = "Percentage")

wards_sf_j_sp_out_eng <- list_out_eng[[1]]
ward_metric_numbers_eng <- list_out_eng[[2]]
sd_ward_eng <- list_out_eng[[3]]
sd_df_ward_eng <- list_out_eng[[4]]

ward_map_simple(map_df = wards_sf_j_sp_out_eng, variable_name = descending_sort_vars_not_all_eng, popup = welcome_popup_eng)
# Ethnicity --------------------------------------------------------------
# Load data

undebug(l_dat_wd)
eth <- l_dat_wd(paste(c(dir_stub, "../data/Ward level/TS022 Ethnicity Lambeth ward census 2021.xlsx"),collapse = ''), start_row = 7)# %>% data.table()

eth_out_m <- eth[[1]]

eth_out_m$Theme <- "Ethnicity"
eth_out_m$Year <- 2021

# Prepare spatial data 

eth_out_join <- eth_out_m[,c("Theme", "Year",  "Area name", "Variable", "Percentage"), with = F]# %>%

# vor_intersect_sf$pasted_location <- with(vor_intersect_sf, paste(longitude, latitude, sep = " "))

wards_sf_j_eth <- merge(wards_sf, eth_out_join, by.x = "WARD_NAME", by.y = "Area name", all.x = T)
wards_sf_j_sp_eth <-  as(wards_sf_j_eth, 'Spatial')

# Map variables to return to rmd

welcome_popup_eth <- welcome_popup(theme = unique(wards_sf_j_eth$Theme), year = unique(wards_sf_j_eth$Year))

# Sort variable by mean percentage, remove 'All usual residents'
descending_sort_vars_eth <- group_by(wards_sf_j_sp_eth@data, Variable) %>% summarise(Mean = mean(Percentage, na.rm=T)) %>% arrange(desc(Mean))
descending_sort_vars_not_all_eth <- descending_sort_vars_eth$Variable[-1]


list_out_eth <- apply_metric_area(wards_sf_j_sp_eth, variable_name = descending_sort_vars_not_all_eth, value_name = "Percentage")

wards_sf_j_sp_out_eth <- list_out_eth[[1]]
ward_metric_numbers_eth <- list_out_eth[[2]]
sd_ward_eth <- list_out_eth[[3]]
sd_df_ward_eth <- list_out_eth[[4]]

#ward_map_simple(map_df = wards_sf_j_sp_out, variable_name = descending_sort_vars_not_all, popup = welcome_popup_eth)
# Religion --------------------------------------------------------------
# Load data

undebug(l_dat_wd)
rel <- l_dat_wd(paste(c(dir_stub, "../data/Ward level/TS031 Religion Lambeth ward census 2021.xlsx"),collapse = ''), start_row = 7)# %>% data.table()

rel_out_m <- rel[[1]]

rel_out_m$Theme <- "Religion"
rel_out_m$Year <- 2021

# Prepare spatial data 

rel_out_join <- rel_out_m[,c("Theme", "Year",  "Area name", "Variable", "Percentage"), with = F]# %>%

# vor_intersect_sf$pasted_location <- with(vor_intersect_sf, paste(longitude, latitude, sep = " "))

wards_sf_j_rel <- merge(wards_sf, rel_out_join, by.x = "WARD_NAME", by.y = "Area name", all.x = T)
wards_sf_j_sp_rel <-  as(wards_sf_j_rel, 'Spatial')

# Map variables to return to rmd

welcome_popup_rel <- welcome_popup(theme = unique(wards_sf_j_rel$Theme), year = unique(wards_sf_j_rel$Year))

# Sort variable by mean percentage, remove 'All usual residents'
descending_sort_vars_rel <- group_by(wards_sf_j_sp_rel@data, Variable) %>% summarise(Mean = mean(Percentage, na.rm=T)) %>% arrange(desc(Mean))
descending_sort_vars_not_all_rel <- descending_sort_vars_rel$Variable[-1]


list_out_rel <- apply_metric_area(wards_sf_j_sp_rel, variable_name = descending_sort_vars_not_all_rel, value_name = "Percentage")

wards_sf_j_sp_out_rel <- list_out_rel[[1]]
ward_metric_numbers_rel <- list_out_rel[[2]]
sd_ward_rel <- list_out_rel[[3]]
sd_df_ward_rel <- list_out_rel[[4]]

ward_map_simple(map_df = wards_sf_j_sp_out_rel, variable_name = descending_sort_vars_not_all_rel, popup = welcome_popup_rel)

# Age --------------------------------------------------------------
# Load data

undebug(l_dat_wd)
age <- l_dat_wd(paste(c(dir_stub, "../data/Ward level/TS007 Age Lambeth ward census 2021.xlsx"),collapse = ''), start_row = 7)# %>% data.table()

age_out_m <- age[[1]]

age_out_m$Theme <- "Age"
age_out_m$Year <- 2021

# Prepare spatial data 

age_out_join <- age_out_m[,c("Theme", "Year",  "Area name", "Variable", "Percentage"), with = F]# %>%

# vor_intersect_sf$pasted_location <- with(vor_intersect_sf, paste(longitude, latitude, sep = " "))

wards_sf_j_age <- merge(wards_sf, age_out_join, by.x = "WARD_NAME", by.y = "Area name", all.x = T)
wards_sf_j_sp_age <-  as(wards_sf_j_age, 'Spatial')

# Map variables to return to rmd

welcome_popup_age <- welcome_popup(theme = unique(wards_sf_j_age$Theme), year = unique(wards_sf_j_age$Year))

# Sort variable by mean percentage, remove 'All usual residents'
descending_sort_vars_age <- c("All usual residents","Aged 4 years and under",  "Aged 5 to 9 years" ,    
 "Aged 10 to 15 years" , "Aged 16 to 19 years" , "Aged 20 to 24 years", "Aged 25 to 34 years" ,"Aged 35 to 49 years", "Aged 50 to 64 years" , "Aged 65 to 74 years" ,   "Aged 75 to 84 years",  "Aged 85 years and over")

#descending_sort_vars_age <- group_by(wards_sf_j_sp_age@data, Variable) %>% summarise(Mean = mean(Percentage, na.rm=T)) %>% arrange(desc(Mean))
descending_sort_vars_not_all_age <- descending_sort_vars_age[descending_sort_vars_age!="All usual residents"]


list_out_age <- apply_metric_area(wards_sf_j_sp_age, variable_name = descending_sort_vars_not_all_age, value_name = "Percentage")

wards_sf_j_sp_out_age <- list_out_age[[1]]
ward_metric_numbers_age <- list_out_age[[2]]
sd_ward_age <- list_out_age[[3]]
sd_df_ward_age <- list_out_age[[4]]

#ward_map_simple(map_df = wards_sf_j_sp_out, variable_name = descending_sort_vars_not_all, popup = welcome_popup_age)
# Household composition --------------------------------------------------------------
# Load data

undebug(l_dat_wd)
hous <- l_dat_wd(paste(c(dir_stub, "../data/Ward level/TS003 Household composition Lambeth ward census 2021.xlsx"),collapse = ''), start_row = 7, replace_colon=F)# %>% data.table()

hous_out_m <- hous[[1]]

hous_out_m$Theme <- "Household composition"
hous_out_m$Year <- 2021

# Remove some overall categories
hous_out_m <- hous_out_m[hous_out_m$Variable %!in% c("One-person household",
                                                     "Single family household",
                                                     "Other household types")]

# Prepare spatial data 

hous_out_join <- hous_out_m[,c("Theme", "Year",  "Area name", "Variable", "Percentage"), with = F]# %>%

# vor_intersect_sf$pasted_location <- with(vor_intersect_sf, paste(longitude, latitude, sep = " "))

wards_sf_j_hous <- merge(wards_sf, hous_out_join, by.x = "WARD_NAME", by.y = "Area name", all.x = T)
wards_sf_j_sp_hous <-  as(wards_sf_j_hous, 'Spatial')

# Map variables to return to rmd

welcome_popup_hous <- welcome_popup(theme = unique(wards_sf_j_hous$Theme), year = unique(wards_sf_j_hous$Year))

# Sort variable by mean percentage, remove 'All usual residents'
descending_sort_vars_hous <- group_by(wards_sf_j_sp_hous@data, Variable) %>% summarise(Mean = mean(Percentage, na.rm=T)) %>% arrange(desc(Mean))
descending_sort_vars_not_all_hous <- descending_sort_vars_hous$Variable[-1]


list_out_hous <- apply_metric_area(wards_sf_j_sp_hous, variable_name = descending_sort_vars_not_all_hous, value_name = "Percentage")

wards_sf_j_sp_out_hous <- list_out_hous[[1]]
ward_metric_numbers_hous <- list_out_hous[[2]]
sd_ward_hous <- list_out_hous[[3]]
sd_df_ward_hous <- list_out_hous[[4]]

ward_map_simple(map_df = wards_sf_j_sp_out_hous, variable_name = descending_sort_vars_not_all_hous, popup = welcome_popup_hous)

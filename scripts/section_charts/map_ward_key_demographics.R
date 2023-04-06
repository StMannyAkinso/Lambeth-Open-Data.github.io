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

# Simplify borough outlines - only needed to do once
# require(rmapshaper)
# 
# borough_sf <- read_sf("LondonBoroughs.geojson")
# # proj4string(wards_ni_sp)
# 
# borough_sf_simple <- rmapshaper::ms_simplify(input = borough_sf, keep_shapes = T, keep = 0.01)
# 
# write_sf(borough_sf_simple, paste(c(dir_stub, "../data/Map/LondonBoroughs_simple.geojson"),collapse = ''), delete_dsn = T)

###

# Simplify ward outlines - only needed to do once
# require(rmapshaper)
# 
# wards_sf <- read_sf(paste(c(dir_stub, "../data/Map/Lambeth_2021_Lower_Super_Output_Areas.geojson"),collapse = ''))
# # proj4string(wards_ni_sp)
# 
# wards_sf_simple <- rmapshaper::ms_simplify(input = wards_sf, keep_shapes = T, keep = 0.01)
# 
# write_sf(wards_sf_simple, paste(c(dir_stub, "../data/Map/Lambeth_2021_Lower_Super_Output_Areas_simple.geojson"),collapse = ''), delete_dsn = T)

# Simplify lsoa outlines - only needed to do once
# require(rmapshaper)

#lsoa_sf <- read_sf(paste(c(dir_stub, "../data/Map/Lambeth_2021_Lower_Super_Output_Areas.geojson"),collapse = ''))
# # proj4string(wards_ni_sp)
# 
# lsoa_sf_simple <- rmapshaper::ms_simplify(input = lsoa_sf, keep_shapes = T, keep = 0.01)
# 
# write_sf(lsoa_sf_simple, paste(c(dir_stub, "../data/Map/Lambeth_2021_Lower_Super_Output_Areas_simple.geojson"),collapse = ''), delete_dsn = T)

# Simplify lsoa old outlines - only needed to do once
# require(rmapshaper)
# 
# lsoa_old_sf <- read_sf(paste(c(dir_stub, "../data/Map/lsoa_map_lambeth_old.geojson"),collapse = ''))
# 
# lsoa_old_sf_simple <- rmapshaper::ms_simplify(input = lsoa_old_sf, keep_shapes = T, keep = 0.01)
# 
# write_sf(lsoa_old_sf_simple, paste(c(dir_stub, "../data/Map/lsoa_map_lambeth_old_simple.geojson"),collapse = ''), delete_dsn = T) 


# ###
# 
wards_sf_old <- read_sf(paste(c(dir_stub, "../data/Map/lambeth_old_wards_wgs84.geojson"),collapse = ''))

wards_sf_old <- st_transform(wards_sf_old, prj.wgs84)
# 
# setnames(wards_sf_old, "WD13NM", "WARD_NAME")
# 
wards_sf <- read_sf(paste(c(dir_stub, "../data/Map/new_lambeth_wards_wgs84_riverclipped_simple.geojson"),collapse = ''))

wards_sf <- st_transform(wards_sf, prj.wgs84)

# Full size version
#wards_sf_big <- read_sf(paste(c(dir_stub, "../data/Map/WardBoundaries2022_wgs_riverclipped.json"),collapse = ''))
#wards_sf_big <- st_transform(wards_sf, prj.wgs84)

lsoamap = read_sf(paste(c(dir_stub, "../data/Map/Lambeth_2021_Lower_Super_Output_Areas_simple.geojson"),collapse = ''))
lsoamap <- st_transform(lsoamap, prj.wgs84)


lsoamap_old = read_sf(paste(c(dir_stub, "../data/Map/lsoa_map_lambeth_old.geojson"),collapse = ''))
lsoamap_old <- st_transform(lsoamap_old, prj.wgs84)



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

undebug(l_dat_wd)
cob <- l_dat_wd(paste(c(dir_stub, "../data/Ward level/TS004 Country of birth Lambeth ward census 2021.xlsx"),collapse = ''), start_row = 7)# %>% data.table()

cob_out_m <- cob[[1]]

cob_out_m$Theme <- "Country of birth"
cob_out_m$Year <- 2021

# Prepare spatial data 

cob_out_join <- cob_out_m[,c("Theme", "Year",  "Area name", "Variable", "Percentage"), with = F]# %>%

# vor_intersect_sf$pasted_location <- with(vor_intersect_sf, paste(longitude, latitude, sep = " "))

wards_sf_j_cob <- merge(wards_sf, cob_out_join, by.x = "WARD_NAME", by.y = "Area name", all.x = T)

# Write a new version of wards_sf with area code included, will be useful elsewhere
#write_sf(wards_sf_j_cob[,c("WARD_NAME", "NO_OF_COUN", "CURRENT_EL", "FORECAST_E", "Area code")], paste(c(dir_stub, "../data/Map/new_lambeth_wards_wgs84_riverclipped_simple.geojson"),collapse = ''), delete_dsn = T)

# Write a new version of wards_sf_big with area code included, will be useful elsewhere
#wards_sf_j_cob_big <- merge(wards_sf_big, cob_out_join, by.x = "WARD_NAME", by.y = "Area name", all.x = T)

#write_sf(wards_sf_j_cob_big[,c("WARD_NAME", "NO_OF_COUN", "CURRENT_EL", "FORECAST_E", "Area code")], paste(c(dir_stub, "../data/Map/new_lambeth_wards_wgs84_riverclipped.geojson"),collapse = ''), delete_dsn = T)


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
hous_out_m$`Area code`

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

# Children in low income households map --------------------------------------------------------------
# Load data

undebug(l_dat_wd)

child_li_wd <- read.xlsx(paste(c(dir_stub, "../data/Financial stability/children-in-low-income-families-local-area-statistics-2014-to-2022.xlsx"),collapse = ''),
                         sheet = "7_Relative_Ward", startRow = 11) %>% as.data.table()

names(child_li_wd)

# Replace . in column names with " "
names(child_li_wd) = gsub("\\.", " ", names(child_li_wd))

child_li_wd = child_li_wd[`Local Authority [note 2]` == "Lambeth"]

child_li_wd[, 2 := NULL]
child_li_wd[, `Local Authority [note 2]` := NULL]


cols = names(child_li_wd)#c("Ward [note 2]",	"Area Code", "Number of children FYE 2021 [p]", "Percentage of children FYE 2021 (%) [p] [note 3]")


child_li_wd_df <- child_li_wd[, ..cols]

child_li_wd_df <- melt(child_li_wd_df, id.vars = c("Ward [note 2]", "Area Code"))

names(child_li_wd_df) <- c("Area", "Area Code", "Variable", "Value")

#child_li_wd_df <- child_li_wd_df[,`:=`(`Percentage of children in low income households (%)` = round(as.numeric(`Percentage of children in low income households (%)`) * 100,1),
#                                       `Number of children in low income households` = as.numeric(`Number of children in low income households`)
#)]




#child_li_wd_df_lam <- child_li_wd_df[grep("^E050004", `Area Code`)]

#lam_wds <- unique(sort(child_li_wd_df_lam$`Area Code`))[1:21]

unique(wards_sf$`Area code`)
unique(child_li_wd_df[,`Area Code`])

#child_li_wd_df_lam <- child_li_wd_df[(`Area Code` %in% wards_sf$`Area code`)]

child_li_wd_df_lam <- child_li_wd_df[(`Area Code` %in% wards_sf_old$`WD13CD`)]
#child_li_wd_df_lam <- child_li_wd_df_lam[(grep("^E09", `Area Code`))]
#


child_li_wd_df_lam$Value <- as.numeric(child_li_wd_df_lam$Value)

#child_li_wd_df_lam$Variable = "Wards"

child_li_wd_df_lam <- child_li_wd_df_lam[grep("Percentage", Variable)]

child_li_wd_df_lam[, Value := round(Value * 100,1)]

child_li_wd_df_lam[, Variable := gsub(" \\(%\\).*", "", Variable, fixed=F)]

child_li_wd_df_lam[, Year:= as.integer(stringr::str_extract(Variable, "\\d\\d\\d\\d$"))]

#sort(unique(child_li_wd_df_lam$Variable))

child_li_wd_df_lam[, Variable := ordered(Variable, levels=sort(unique(Variable)))]

child_li_wd_df_lam[, Variable]


### Make a time graph for the above

##, aes(
#  text= paste("Area: ", Area, "<br>",
#              "Year: ", Year, "<br>",
#              "Percentage of children in low income households (%): ", `Value`, sep = ""))

#child_li_wd_df_lam[, Area := as.factor(Area)]

child_li_wd_df_lam_2022 <- child_li_wd_df_lam[Variable == "Percentage of children FYE 2022"]
ordered_areas <- child_li_wd_df_lam_2022[order(child_li_wd_df_lam_2022, -Value),Area] %>% unique()

child_li_wd_df_lam[, `:=`(Area = ordered(Area, levels=ordered_areas),
                      Value = as.numeric(Value)
)]

# Present on a ggplotly graph
child_li_wd_df_lam_graph = ggplot(child_li_wd_df_lam, aes(x = Year, y = `Value`, colour = Area,
                                                          group = Area, linetype= Area, shape = Area)) +
  geom_line()+#position = position_dodge(width = 0.9)) +
  geom_point() + 
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  #geom_point(aes(x = Year, y = `Value`)) +
  #geom_smooth(aes(x = Year, y = `Value`), method = "lm") +
  scale_shape_manual(values=1:nlevels(child_li_wd_df_lam$Area)) +
  scale_linetype_manual(values=1:nlevels(child_li_wd_df_lam$Area)) +
  ylab("Percentage of children in low income households (%)") +
  expand_limits(y=0)

child_li_wd_df_lam_graph

### Prepare spatial data 

# vor_intersect_sf$pasted_location <- with(vor_intersect_sf, paste(longitude, latitude, sep = " "))

setnames(wards_sf_old, "WD13NM", "WARD_NAME")

wards_sf_old_j_child <- merge(wards_sf_old, child_li_wd_df_lam, by.x = "WARD_NAME", by.y = "Area", all.x = T)
wards_sf_old_j_child

wards_sf_old_j_child$Theme = "Percentage of children in relative poverty"


wards_sf_old_j_child$`Area Code` = NULL
wards_sf_old_j_child$`Number of children in low income households` = NULL
wards_sf_old_j_sp_child <-  as(wards_sf_old_j_child, 'Spatial')

names(wards_sf_old_j_sp_child@data) = stringr::str_trim(gsub("\\.", " ", names(wards_sf_old_j_sp_child@data)))



# Map variables to return to rmd

welcome_popup_child <- welcome_popup(theme = unique(wards_sf_old_j_child$Theme), year = unique(wards_sf_old_j_child$Year))

# Sort variable by mean percentage, remove 'All usual residents'
descending_sort_vars_child <- group_by(wards_sf_old_j_sp_child@data, `Variable`) %>% summarise(Mean = mean(Value, na.rm=T)) %>% arrange(desc(Variable))
descending_sort_vars_not_all_child <- descending_sort_vars_child$Variable

undebug(apply_metric_area)
list_out_child <- apply_metric_area(wards_sf_old_j_sp_child, variable_name = descending_sort_vars_not_all_child, value_name = "Value")

wards_sf_old_j_sp_out_child <- list_out_child[[1]]
ward_metric_numbers_child <- list_out_child[[2]]
sd_ward_child <- list_out_child[[3]]
sd_df_ward_child <- list_out_child[[4]]

ward_map_simple(map_df = wards_sf_old_j_sp_out_child, variable_name = descending_sort_vars_not_all_child, popup = welcome_popup_child)




# Claimant count by ward map ----------------------------------------------

claim_func_wd = function(file_name="../data/Jobs/2022/Claimant count age ward 2023.xlsx", start_row = 4){
  
  claim <- read.xlsx(paste(c(dir_stub, file_name),collapse = ''), startRow = start_row) %>% as.data.table()
  names(claim)
  claim_source <- "https://www.nomisweb.co.uk/reports/lmp/la/1946157253/report.aspx"
  
  claim_age <- as.character(claim[1,2])
  claim_metric <- as.character(claim[2,2])
  
  ## Get rid of NAs
  claim_df <- claim[3:nrow(claim)]
  names(claim_df) <- (claim_df)[1,] %>% as.list() %>% unname() %>% unlist()
  
  claim_df = claim_df[3:which(is.na(`February 2023`))[1]-1]
  
  # Replace . in column names
  names(claim_df) <- gsub("\\.", " ", names(claim_df))
  
  
  # Remove everything before : in column names
  claim_df[, Area := gsub(".*:", "", Area)]
  claim_df[, Area := stringr::str_trim(Area)]
  
  
  claim_df_m <- melt(claim_df, id.vars="Area",variable.name= "Date", value.name = "Value")
  
  claim_df_m[,Date:= paste("01 ", as.character(Date), sep="")]
  claim_df_m[,Date:= as.Date(Date, format = "%d %B %Y")]
  
  claim_df_m[,Age := claim_age]
  claim_df_m[,Variable := claim_metric]
  
}

claimant_file_path = "../data/Jobs/2022/Claimant count age ward 2023.xlsx"

undebug(claim_func_wd)
claim_all_no <- claim_func_wd(file_name=claimant_file_path, start_row = 4)
claim_all_perc <- claim_func_wd(file_name=claimant_file_path, start_row = 45)

claim_all_no_16_24 <- claim_func_wd(file_name=claimant_file_path, start_row = 86)
claim_all_no_25_49 <- claim_func_wd(file_name=claimant_file_path, start_row = 169)
claim_all_no_50 <- claim_func_wd(file_name=claimant_file_path, start_row = 252)


# Combined
#claim_all_ages <- rbindlist(list(claim_latest_1624, claim_latest_2549, claim_latest_50))

setnames(claim_all_perc, "Value", "Claimant count rate (%)", skip_absent = T)


claim_all_perc_2022 <- claim_all_perc[Date > as.Date("2023-01-01")]
ordered_areas <- claim_all_perc_2022[order(claim_all_perc_2022, -`Claimant count rate (%)`),Area] %>% unique()

claim_all_perc[, `:=`(Area = ordered(Area, levels=ordered_areas),
                      `Claimant count rate (%)` = as.numeric(`Claimant count rate (%)`)
)]


# Present on a ggplotly graph
claim_all_perc_graph = ggplot(claim_all_perc[Area != "London" & Area != "England" & Area != "Lambeth"],
                              aes(x = Date, y = `Claimant count rate (%)`, 
                                  colour = Area, group=Area, linetype=Area, shape = Area,
                                  text= paste("Area: ", Area, "<br>",
                                              "Claimant count rate (%): ", `Claimant count rate (%)`, sep = "")#,
                              )) +
  geom_line()+#position = position_dodge(width = 0.9)) +
  geom_point()+
  scale_colour_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  scale_shape_manual(values=1:nlevels(claim_all_perc$Area)) +
  scale_linetype_manual(values=1:nlevels(claim_all_perc$Area)) +
  theme(axis.title.x = element_blank()) +
  ylab("Claimant count rate (%)") #+
#facet_wrap(~Age)

# IF YOU WANT TO ADD LAMBETH IN AS ITS OWN LINE
#claim_all_perc_graph_lam <- claim_all_perc_graph +
#  geom_line(data=claim_all_perc[Area == "Lambeth"], aes(x=Date, y = `Claimant count rate (%)`), linetype=2, size=2, alpha=0.5, inherit.aes = F) +
#  geom_point(data=claim_all_perc[Area == "Lambeth"], aes(x=Date, y = `Claimant count rate (%)`), alpha=0.5, inherit.aes = F)

claim_all_perc_graph

#ggplotly(claim_all_perc_graph_lam)



### Prepare spatial data 

# vor_intersect_sf$pasted_location <- with(vor_intersect_sf, paste(longitude, latitude, sep = " "))

wards_sf_old_j_claim <- merge(wards_sf_old, claim_all_perc, by.x = "WARD_NAME", by.y = "Area", all.x = T)
wards_sf_old_j_claim

wards_sf_old_j_claim$Theme = "Unemployment claimant count"

wards_sf_old_j_claim$`Area Code` = NULL

names(wards_sf_old_j_claim)[names(wards_sf_old_j_claim) == "Variable"] = "Variable name"
names(wards_sf_old_j_claim)[names(wards_sf_old_j_claim) == "Date"] = "Variable"
wards_sf_old_j_claim$Variable = as.factor(wards_sf_old_j_claim$Variable)

wards_sf_old_j_sp_claim <-  as(wards_sf_old_j_claim, 'Spatial')

names(wards_sf_old_j_sp_claim@data) = stringr::str_trim(gsub("\\.", " ", names(wards_sf_old_j_sp_claim@data)))



# Map variables to return to rmd

welcome_popup_claim <- welcome_popup(theme = unique(wards_sf_old_j_claim$Theme), year = unique(wards_sf_old_j_claim$Date))

# Sort variable by mean percentage, remove 'All usual residents'
descending_sort_vars_claim <- group_by(wards_sf_old_j_sp_claim@data, `Variable`) %>% summarise(Mean = mean(`Claimant count rate`, na.rm=T)) %>% arrange(desc(Variable))
descending_sort_vars_not_all_claim <- descending_sort_vars_claim$Variable

undebug(apply_metric_area)
list_out_claim <- apply_metric_area(wards_sf_old_j_sp_claim, variable_name = descending_sort_vars_not_all_claim, value_name = "Claimant count rate")

wards_sf_old_j_sp_out_claim <- list_out_claim[[1]]
ward_metric_numbers_claim <- list_out_claim[[2]]
sd_ward_claim <- list_out_claim[[3]]
sd_df_ward_claim <- list_out_claim[[4]]

ward_map_simple(map_df = wards_sf_old_j_sp_out_claim, variable_name = descending_sort_vars_not_all_claim, popup = welcome_popup_claim)



# Fuel poverty LSOA map ----------------------------------------------

fuel_all_perc_lsoa <- read.xlsx(paste(c(dir_stub, "../data/Financial stability/sub-regional-fuel-poverty-2022-tables.xlsx"),collapse = ''),
                         sheet = "Table 3", startRow = 3) %>% as.data.table()


# Replace . in column names with " "
names(fuel_all_perc_lsoa) = gsub("\\.", " ", names(fuel_all_perc_lsoa))


fuel_all_perc_lsoa_lam <- fuel_all_perc_lsoa[`LA Name` == "Lambeth"]

# Combined
#fuel_all_ages <- rbindlist(list(fuel_latest_1624, fuel_latest_2549, fuel_latest_50))

#setnames(fuel_all_perc_lsoa, "Value", "fuelant count rate (%)", skip_absent = T)


#fuel_all_perc_lsoa_2022 <- fuel_all_perc_lsoa[Date > as.Date("2023-01-01")]
#ordered_areas <- fuel_all_perc_lsoa_2022[order(fuel_all_perc_lsoa_2022, -`fuelant count rate (%)`),Area] %>% unique()

#fuel_all_perc_lsoa[, `:=`(Area = ordered(Area, levels=ordered_areas),
##                      `fuelant count rate (%)` = as.numeric(`fuelant count rate (%)`)
#)]

names(fuel_all_perc_lsoa_lam)

setnames(fuel_all_perc_lsoa_lam, "Proportion of households fuel poor (%)", "Proportion of households in fuel poverty (%)")

fuel_all_perc_lsoa_lam[, `:=`(`Proportion of households in fuel poverty (%)` = as.numeric(`Proportion of households in fuel poverty (%)`))]

### Summarise to Ward to make ward map
lsoa_to_country_lam <- fread(paste(c(dir_stub, "../data/Map/lsoa_to_country_2019_lambeth.csv"),collapse = ''))
lsoa_to_country_lam_l <- lsoa_to_country_lam[,c("lsoa11cd", "lsoa11nm", "wd19cd", "wd19nm")]

fuel_all_perc_wd_lam <- merge(fuel_all_perc_lsoa_lam, lsoa_to_country_lam_l, by.x = "LSOA Code", by.y= "lsoa11cd", how = "left")


fuel_all_perc_wd_lam <- group_by(fuel_all_perc_wd_lam, `wd19nm`) %>% summarise(`Proportion of households in fuel poverty (%)` = mean(`Proportion of households in fuel poverty (%)`, na.rm=T)) %>% as.data.table()


### For use with ward map

fuel_all_perc_wd_lam_2022 <- fuel_all_perc_wd_lam
ordered_areas <- fuel_all_perc_wd_lam_2022[order(fuel_all_perc_wd_lam_2022, -`Proportion of households in fuel poverty (%)`),`wd19nm`] %>% unique()


fuel_all_perc_wd_lam[, `:=`(`wd19nm` = ordered(`wd19nm`, levels=ordered_areas))]


# Present on a ggplotly graph
fuel_all_perc_wd_graph = ggplot(fuel_all_perc_wd_lam,
                                  aes(x = `wd19nm`, y = `Proportion of households in fuel poverty (%)`, 
                                      fill = `wd19nm`,
                                      text= paste("wd19nm: ", `wd19nm`, "<br>",
                                                  "Proportion of households in fuel poverty (%): ", `Proportion of households in fuel poverty (%)`, sep = "")#,
                                  )) +
  geom_col(position = position_dodge(width = 0.9)) +
  #geom_point()+
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Proportion of households in fuel poverty (%)") #+
#facet_wrap(~Age)

fuel_all_perc_wd_graph


### For use with LSOA map

fuel_all_perc_lsoa_lam_2022 <- fuel_all_perc_lsoa_lam
ordered_areas <- fuel_all_perc_lsoa_lam_2022[order(fuel_all_perc_lsoa_lam_2022, -`Proportion of households in fuel poverty (%)`),`LSOA Name`] %>% unique()


fuel_all_perc_lsoa_lam[, `:=`(`LSOA Name` = ordered(`LSOA Name`, levels=ordered_areas))]


# Present on a ggplotly graph
fuel_all_perc_lsoa_graph = ggplot(fuel_all_perc_lsoa_lam,
                              aes(x = `LSOA Name`, y = `Proportion of households in fuel poverty (%)`, 
                                  fill = `LSOA Name`,
                                  text= paste("LSOA Name: ", `LSOA Name`, "<br>",
                                              "Proportion of households in fuel poverty (%): ", `Proportion of households in fuel poverty (%)`, sep = "")#,
                              )) +
  geom_col(position = position_dodge(width = 0.9)) +
  #geom_point()+
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank(), legend.position = "None",
                                                                      axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Proportion of households in fuel poverty (%)") #+
#facet_wrap(~Age)

fuel_all_perc_lsoa_graph

# IF YOU WANT TO ADD LAMBETH IN AS ITS OWN LINE
#fuel_all_perc_lsoa_graph_lam <- fuel_all_perc_lsoa_graph +
#  geom_line(data=fuel_all_perc_lsoa[Area == "Lambeth"], aes(x=Date, y = `fuelant count rate (%)`), linetype=2, size=2, alpha=0.5, inherit.aes = F) +
#  geom_point(data=fuel_all_perc_lsoa[Area == "Lambeth"], aes(x=Date, y = `fuelant count rate (%)`), alpha=0.5, inherit.aes = F)


#ggplotly(fuel_all_perc_lsoa_graph_lam)

names(fuel_all_perc_lsoa)

fuel_all_perc_lsoa_lam$Variable = "Fuel poverty 2020"

### Prepare spatial data 

# vor_intersect_sf$pasted_location <- with(vor_intersect_sf, paste(longitude, latitude, sep = " "))

wards_sf_old_j_fuel <- merge(lsoamap_old, fuel_all_perc_lsoa_lam, by.x = "geo_code", by.y = "LSOA Code", all.x = T)
wards_sf_old_j_fuel

setnames(wards_sf_old_j_fuel, "geo_label", "WARD_NAME")

wards_sf_old_j_fuel$Theme = "Fuel poverty"

#wards_sf_old_j_fuel$`Area Code` = NULL

#names(wards_sf_old_j_fuel)[names(wards_sf_old_j_fuel) == "Variable"] = "Variable name"
#names(wards_sf_old_j_fuel)[names(wards_sf_old_j_fuel) == "Date"] = "Variable"
wards_sf_old_j_fuel$Variable = as.factor(wards_sf_old_j_fuel$Variable)

wards_sf_old_j_sp_fuel <-  as(wards_sf_old_j_fuel, 'Spatial')

names(wards_sf_old_j_sp_fuel@data) = stringr::str_trim(gsub("\\.", " ", names(wards_sf_old_j_sp_fuel@data)))



# Map variables to return to rmd

welcome_popup_fuel <- welcome_popup(theme = unique(wards_sf_old_j_fuel$Theme), year = "2021")

# Sort variable by mean percentage, remove 'All usual residents'
descending_sort_vars_fuel <- group_by(wards_sf_old_j_sp_fuel@data, `Variable`) %>% summarise(Mean = mean(`Proportion of households in fuel poverty`, na.rm=T)) %>% arrange(desc(Variable))
descending_sort_vars_not_all_fuel <- descending_sort_vars_fuel$Variable

undebug(apply_metric_area)
list_out_fuel <- apply_metric_area(wards_sf_old_j_sp_fuel, variable_name = descending_sort_vars_not_all_fuel, value_name = "Proportion of households in fuel poverty")

wards_sf_old_j_sp_out_fuel <- list_out_fuel[[1]]
ward_metric_numbers_fuel <- list_out_fuel[[2]]
sd_ward_fuel <- list_out_fuel[[3]]
sd_df_ward_fuel <- list_out_fuel[[4]]

ward_map_simple(map_df = wards_sf_old_j_sp_out_fuel, variable_name = descending_sort_vars_not_all_fuel, popup = welcome_popup_fuel)



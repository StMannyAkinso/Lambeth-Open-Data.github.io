library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(forcats)

library(leaflet)
library(leaflet.extras)
library(maptools)
library(data.table)
library(sf)
library(htmlwidgets)

source("jsCode.r")

#setwd("C:/Users/msinclair/OneDrive - Lambeth Council/Sandbox/CostOfLiving Data Pack/scripts")


# == Intro text =====================
census_text = "The dimensions of deprivation used to classify households are indicators based on four selected household characteristics. 

__Education__   

A household is classified as deprived in the education dimension if no one has at least level 2 education and no one aged 16 to 18 years is a full-time student.

__Employment__   

A household is classified as deprived in the employment dimension if any member, not a full-time student, is either unemployed or economically inactive due to long-term sickness or disability.

__Health__

A household is classified as deprived in the health dimension if any person in the household has general health that is bad or very bad or is identified as disabled.

People who have assessed their day-to-day activities as limited by long-term physical or mental health conditions or illnesses are considered disabled.  This definition of a disabled person meets the harmonised standard for measuring disability and is in line with the Equality Act (2010).

__Housing__

A household is classified as deprived in the housing dimension if the household's accommodation is either overcrowded, in a shared dwelling, or has no central heating."

# == Data =============================
LA <- read.csv("../data/census_Deprivation_London_LAs.csv")
Ward <- read.csv("../data/census_Deprivation_Lambeth_Wards.csv")
LSOA <- read.csv("../data/census_Deprivation_Lambeth_LSOAs.csv")

head(LA)


reCalcValues <- function(df){
  colnames(df)[2] <- "area"
  
  
  df %<>% select(area, Household.deprivation..6.categories., Observation) %>% 
    spread(Household.deprivation..6.categories., Observation)
  
  df %<>%mutate(totalHouhseholds = `Household is not deprived in any dimension` +  
                                      `Household is deprived in one dimension` + 
                                      `Household is deprived in two dimensions` + 
                                      `Household is deprived in three dimensions` +
                                      `Household is deprived in four dimensions`) %>% 
    mutate(`Household is not deprived in any dimension` = round(100 * (`Household is not deprived in any dimension`/totalHouhseholds), 1),
           `Household is deprived in one dimension` = round(100 * (`Household is deprived in one dimension`/totalHouhseholds), 1),
           `Household is deprived in two dimensions` = round(100 * (`Household is deprived in two dimensions`/totalHouhseholds), 1),
           `Household is deprived in three dimensions` = round(100 * (`Household is deprived in three dimensions`/totalHouhseholds), 1),
           `Household is deprived in four dimensions` = round(100 * (`Household is deprived in four dimensions`/totalHouhseholds), 1)) %>% 
    select(-totalHouhseholds) %>% 
    gather(key="HouseholdDeprivation", value="Percentage of Households", 2:7)

  return(df)
}

# == clean data and calculate percentages
LA <- reCalcValues(LA)
Ward <- reCalcValues(Ward)
LSOA <- reCalcValues(LSOA)

head(LA)

# == Plot boroughs =====
#import map geojsons
wardmap = read_sf("map_files/new_lambeth_wards_wgs84_riverclipped_simple.geojson", layer="new_lambeth_wards_wgs84_riverclipped_simple")
lsoamap = read_sf("map_files/Lambeth_2021_Lower_Super_Output_Areas_simple.geojson", layer="Lambeth_2021_Lower_Super_Output_Areas_simple")
lamap = read_sf("map_files/LondonBoroughs_simple.geojson", layer="LondonBoroughs_simple")
allmaps <- list(lsoamap, wardmap, lamap)
lsoamap
wardmap
lamap

alldata <- list(LSOA, Ward, LA)

variablenames<- c("Does not apply",
                  "Household is not deprived in any dimension",
                  "Household is deprived in one dimension",
                  "Household is deprived in two dimensions",
                  "Household is deprived in three dimensions",
                  "Household is deprived in four dimensions")
LA
totallist = vector('list', 3)
area <- c("london-borough", "lambeth-wards", "lambeth-lower-super-output-areas-lsoas")

for (i in c(1:3)) { #lsoa, ward, la
  
  if (i == 3) {
    map<-leaflet(data = allmaps[[i]], options = leafletOptions(minZoom = 9.3)) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(-0.116, 51.46, zoom = 9.3) %>% 
      setMaxBounds(lng1 = -0.5950, lat1= 52,
                   lng2=  0.5, lat2 = 51) #%>%

  } else{
    map<-leaflet(data = allmaps[[i]], options = leafletOptions(minZoom = 11.4)) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(-0.116, 51.46, zoom = 11.4) %>% 
      setMaxBounds(lng1 = -0.075, lat1= 51.52,
                   lng2=  -0.159, lat2 = 51.4) #%>%    

  }
  for (j in variablenames) {
    grouped <- alldata[[i]] %>% filter(HouseholdDeprivation == j)
    if (i == 1) {
      popup <- paste0("<strong>LSOA:  </strong>", allmaps[[i]]$LSOA21NM,
                      "<br><strong>Percentage of Households:  </strong>", grouped$`Percentage of Households`)
    }
    else if (i == 2) {
      popup <- paste0("<strong>Ward:  </strong>", allmaps[[i]]$WARD_NAME,
                      "<br><strong>Percentage of Households:  </strong>", grouped$`Percentage of Households`)
    }
    else {
      popup <- paste0("<strong>Local Authority:  </strong>", allmaps[[i]]$NAME,
                      "<br><strong>Percentage of Households:  </strong>", grouped$`Percentage of Households`)
    }
    
    pal = colorBin('RdPu', grouped$`Percentage of Households`, bins = 5, pretty = TRUE, na.color = "#808080")
    map <- map %>%
      addPolygons(group = j, fillOpacity = 0.7, fillColor = pal(grouped$`Percentage of Households`), 
                  color = "darkgrey", weight = 2, popup = popup) %>% 
      
      addLegend(position = c("bottomleft"), 
                pal = pal, 
                values = grouped$`Percentage of Households`, 
                opacity = 0.7, 
                group = j,
                layerId = j,
                title = '% of Households')
  }
  
  
  map_mark <- map %>%
    addResetMapButton() %>%
    addProviderTiles(providers$CartoDB.Voyager, group = "Carto (default)") %>% # Need leaflet extras
    
    addSearchOSM(options = searchOptions(collapsed = F, autoCollapse = F, textPlaceholder = "Search places in UK")) %>%
    addFullscreenControl() %>%
    addLayersControl(
      baseGroups = c(variablenames), 
      #overlayGroups = c(variablenames), 
      options = layersControlOptions(collapsed = F))  %>%
    hideGroup(group=variablenames[-1]) 
   if(i == 3) {
    map_mark <- map_mark %>% 
      htmlwidgets::onRender(jquery)
   } 
  
  totallist[[i]]<-map_mark
  
}

#totallist[[1]]


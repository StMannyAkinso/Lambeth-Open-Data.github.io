# Load packages, functions ------------------------------------------------

library(ggplot2)
library(dplyr)
library(plotly)
library(readxl)
library(tidyr)
library(ggthemes)
library(leaflet)
library(DT)
library(openxlsx)
library(data.table)

options(scipen=10000)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('colour_theme.r')

data_sources <- read.xlsx('Data sources.xlsx', sheet = "Sources") %>% as.data.table()
names(data_sources) <- gsub("\\.", " ", names(data_sources))

#data_sources_text <- read.xlsx('Data sources.xlsx', sheet = "Text") %>% as.data.table()
#names(data_sources_text) <- gsub("\\.", " ", names(data_sources_text))

#data_sources

m <- list(
  l = 5,#50,
  r = 5,#50,
  b = 10,#100,
  t = 10,#100,
  pad = 2#4
)

make_datatable <- function(dt, visual_name = "Population by sex", data_link_col = "Source data link"){
  
  # htmltools::em
  
  col_no <- ncol(dt) - 1
  
  dt_out <- datatable(dt, 
                      caption = htmltools::tags$caption(
                        style = 'caption-side: bottom; text-align: center;',
                        paste(c(data_sources[`Visual short name`== visual_name, Section], ":"),collapse=""),
                        htmltools::withTags(HTML(
                          paste(c(data_sources[`Visual short name`== visual_name,`Table commentary`], ". Latest data from ",
                                              data_sources[`Visual short name`== visual_name,`Year (latest year in data)`],". Underlying data sourced from ",
                                              data_sources[`Visual short name`== visual_name,`Source organisation`], " <a href=",
                                              data_sources[`Visual short name`== visual_name, ..data_link_col],' target="_blank">',
                                              "here</a>.")
                      ),collapse=""))),
                      rownames = FALSE,
                      extensions = c('Buttons'),#, 'Scroller'), 
                      options = list(
                        #dom = "ft",
                        pageLength = 500,
                        autoWidth=T,
                        columnDefs = list(list(width = '10%', targets = c(0:col_no))),
                        dom = 'Bfrti',
                        buttons = c('copy', 'csv', 'excel')#,
                        #deferRender = TRUE,
                        #scrollY = 200,
                        #scroller = T
                      ))
  
  return(dt_out)
}




make_theme_index <- function(data_sources_f = data_sources, section_name_f = section_name) {
  
  internal_link_col = "Internal link"
  
  theme_data <- data_sources_f[`Section`== section_name_f]

  source_out <- list("<ul>")
  
  for (i in 1:nrow(theme_data)){
    source <- 
    paste(c("<li><a href=",theme_data[i, ..internal_link_col],'>',theme_data[i, `Visual short name`],"</a></li>")
    ,collapse="")
    
    source_out[i+1] <- source
  }
  
  source_out[nrow(theme_data)+2] <- "</ul>"
  

  source_out_html = htmltools::withTags(htmltools::HTML(unlist(source_out)))
  
  return(source_out_html)
  
}

#names(data_sources)

undebug(make_theme_index)
make_theme_index(data_sources, "Key demographics")


#== Covid 19 pages data and charts =====================================================================================
#== Infections ==============

file <- "../data/Covid/Combined- Cases rate.xlsx"

infections <- readxl::read_xlsx(file) %>% 
                    gather("Area", "Rate", 2:4) %>% filter(!is.na(date))

infections$Area = ordered(infections$Area, c("Lambeth", "London", "England"))

covidInfectionsplot <- ggplot(infections, aes( x = date, y = Rate, group = Area, colour = Area)) + 
                          geom_line() + theme(axis.title.y = "Infection rate per 10,000 population") + 
                          scale_colour_discrete(type=lambeth_palette_graph) +
                          #theme_excel_new()
                          theme_lam()

#== Vaccinations ============


#== Sandbox examples =====================================================================================================

#== Mapping in Leaflet ====================
lon <- -0.11642
lat <- 51.46073

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=lon, lat=lat, popup="Lambeth Town Hall")
#m  # Print the map


#== Highcharts =============================
library(magrittr)
library(highcharter)
hplot <- hchart(mtcars, "scatter", hcaes(wt, mpg, z = drat, color = hp)) %>%
              hc_title(text = "Scatter chart with size and color")

#== Education Section ===================================================================================================


#== Early years =============



#== Attainment ==============



#== Exclusions ==============


#== Cost of Living Crisis Section ======================================================================================


#== Universal Credit / Job seeker allowance


#== Businesses section =====================================================================================

























# Load packages, initial variables ----------------------------------------
require(data.table)
require(rstudioapi)
# require(leaflet)
# require(leaflet.extras)
# require(maptools)
require(dplyr)
require(readxl)
# require(sf)
require(ggplot2)
require(viridis)
require(forcats)
require(openxlsx)


# The following should be true when testing this file. When knitting the report together and sourcing externally, set this to false
if (!exists("run_direct")){run_direct=T}

if (run_direct == F){dir_stub = ''
} else if (run_direct == T){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  dir_stub = '../'
  source(paste(c(dir_stub, 'colour_theme.r'),collapse = ''))
} 


# wards = read_sf(paste(c(dir_stub, "../data/new_lambeth_wards_simple.geojson"),collapse = ''), layer="new_lambeth_wards_wgs84_riverclipped_simple")
# 
# wards

################# Function to make breaks on axis label
# every_nth = function(n) {
#   return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
# }
#################

#Childhood vaccinations ---------------------------------------------
vax <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/Vax.csv"),collapse = '')) %>% data.table()
vax

vax <- vax %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
vax
vax[vax == "London region"] <- 'London'
vax[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]


vax_plot <- vax %>%
  ggplot(aes(x=Time.period, y= Value, group = AreaName, color = AreaName, linetype= AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Period: ", Time.period, "<br>",
                         "Prevalence: ", Value, '%'))
  ) +
  geom_line() + scale_linetype_manual(values=c('solid', "dashed", "dashed")) +
  geom_point(show.legend = FALSE) + expand_limits(x = 0, y = 0) + #scale_x_discrete(breaks = every_nth(n = 2)) +
  scale_color_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1), axis.title.x = element_blank()) +
  ylab('Percentage of Children Receiving MMR vaccine by Age 2 (%)') + labs(linetype = "Area name", color = "Area name")

vax_plot

#Healthy Weight Reception ---------------------------------------------
RecObesity <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/ReceptionObesity.csv"),collapse = '')) %>% data.table()
RecObesity

RecObesity <- RecObesity %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
RecObesity
RecObesity[RecObesity == "London region"] <- 'London'
RecObesity[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]


RecObesity_plot <- RecObesity %>%
  ggplot(aes(x=Time.period, y= Value, group = AreaName, color = AreaName, linetype= AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Period: ", Time.period, "<br>",
                         "Prevalence: ", Value, '%'))
  ) +
  geom_line() + scale_linetype_manual(values=c('solid', "dashed", "dashed")) +
  geom_point(show.legend = FALSE) + expand_limits(x = 0, y = 0) + #scale_x_discrete(breaks = every_nth(n = 2)) +
  scale_color_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1), axis.title.x = element_blank()) +
  ylab('Overweight and Obesity Prevalence at Reception (%)') + labs(linetype = "Area name", color = "Area name")

RecObesity_plot

#Healthy Weight Year 6 ---------------------------------------------
Yr6Obesity <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/Year6Obesity.csv"),collapse = '')) %>% data.table()
Yr6Obesity

Yr6Obesity <- Yr6Obesity %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
Yr6Obesity
Yr6Obesity[Yr6Obesity == "London region"] <- 'London'
Yr6Obesity[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]


Yr6Obesity_plot <- Yr6Obesity %>%
  ggplot(aes(x=Time.period, y= Value, group = AreaName, color = AreaName, linetype= AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Period: ", Time.period, "<br>",
                         "Prevalence: ", Value, '%'))
  ) +
  geom_line() + scale_linetype_manual(values=c('solid', "dashed", "dashed")) + 
  geom_point(show.legend = FALSE) + expand_limits(y = 0) + #scale_x_discrete(breaks = every_nth(n = 2)) +
  scale_color_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1), axis.title.x = element_blank()) +
  ylab('Overweight and Obesity Prevalence at Year 6 (%)') + labs(linetype = "Area name", color = "Area name")

Yr6Obesity_plot

#Children Income Deprivation ---------------------------------------------
ChildrenRelLowIncome <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/ChildrenRelLowIncome.csv"),collapse = '')) %>% data.table()
ChildrenRelLowIncome

ChildrenRelLowIncome <- ChildrenRelLowIncome %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
ChildrenRelLowIncome
ChildrenRelLowIncome[ChildrenRelLowIncome == "London region"] <- 'London'
ChildrenRelLowIncome[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]


ChildrenRelLowIncome_plot <- ChildrenRelLowIncome %>%
  ggplot(aes(x=Time.period, y= Value, group = AreaName, color = AreaName, linetype= AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Period: ", Time.period, "<br>",
                         "Prevalence: ", Value, '%'))
  ) + geom_line() + scale_linetype_manual(values=c('solid', "dashed", "dashed")) + 
  geom_point(show.legend = FALSE) + expand_limits(y = 0) + #scale_x_discrete(breaks = every_nth(n = 2)) +
  scale_color_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1), axis.title.x = element_blank()) +
  ylab('Children in Relative Low-Income Families (%)') + labs(linetype = "Area name", color = 'Area name')

ChildrenRelLowIncome_plot

#Good level of development age 5 ---------------------------------------------
dev <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/Development.csv"),collapse = '')) %>% data.table()
dev

dev <- dev %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
dev
dev[dev == "London region"] <- 'London'
dev[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]


dev_plot <- dev %>%
  ggplot(aes(x=AreaName, y= Value, fill = AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Period: ", Time.period, "<br>",
                         "Prevalence: ", Value, '%'))
  ) + geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.title.x = element_blank()) +
  ylab('% of children achieving a good level of development') + labs(fill = "Area name")

dev_plot

# Borough Happiness --------------------------------------------------------------
happiness <- read.xlsx(paste(c(dir_stub, "../data/Health and wellbeing/personal-well-being-borough.xlsx"),collapse = ''), 2, fillMergedCells = TRUE, colNames = FALSE) %>% data.table()

happiness
colnames(happiness) <- paste(happiness[1,], happiness[2,], sep = " ")
happiness <- happiness[-c(1,2),]

happiness

variable.names<-append(variable.names, list('happiness','lifesatis','worthwhile','anxiety'))

happiness <- happiness %>% 
  select(c(`NA Area`, `Life Satisfaction 2018/19`, `Worthwhile 2018/19`, `Happiness 2018/19`, `Anxiety 2018/19`))

happiness
names(happiness) <- c('Area', 'Life Satisfaction', 'Worthwhile','Happiness','Anxiety')
happiness <- happiness %>%
  mutate_at(vars(`Life Satisfaction`, Worthwhile, Happiness, Anxiety), as.numeric)
happiness_compare <- happiness %>%
  filter(Area %in% c('Lambeth', 'London', 'England'))
happiness_compare[, Area := ordered(Area, levels = c("Lambeth", "London", "England"))]

happiness_plot <- happiness_compare %>%
  ggplot(aes(x = Area, y=Happiness, fill = Area, text= paste("Area name: ", Area, "<br>",
                                                             "Average Happiness Score: ", Happiness))) + geom_bar(position="dodge", stat="identity") + 
  scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() + theme(axis.title.x = element_blank()) + labs(fill = "Area name")

happiness_plot


lifesat_plot <- happiness_compare %>%
  ggplot(aes(x = Area, y=`Life Satisfaction`, fill= Area, 
             text= paste("Area name: ", Area, "<br>",
                         "Average Life Satisfaction Score: ", `Life Satisfaction`))) + geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() + theme(axis.title.x = element_blank()) + labs(fill = "Area name")

lifesat_plot

worthwhile_plot <- happiness_compare %>%
  ggplot(aes(x = Area, y=Worthwhile, fill = Area, 
             text= paste("Area name: ", Area, "<br>",
                         "Average Worthwhile Score: ", Worthwhile))) + geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() + theme(axis.title.x = element_blank()) + labs(fill = "Area name")


worthwhile_plot

anxiety_plot <- happiness_compare %>%
  ggplot(aes(x = Area, y=Anxiety, fill= Area,
             text= paste("Area name: ", Area, "<br>",
                         "Average Anxiety Score: ", Anxiety))) + geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() + theme(axis.title.x = element_blank()) + labs(fill = "Area name")

anxiety_plot


#Sexual Health ---------------------------------------------
STI <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/STI.csv"),collapse = '')) %>% data.table()
STI

STI <- STI %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
STI
STI[STI == "London region"] <- 'London'
STI[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]


STI_plot <- STI %>%
  ggplot(aes(x=Time.period, y= Value, group = AreaName, color = AreaName, linetype= AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Period: ", Time.period, "<br>",
                         "New STI's per 100,000: ", Value))
  ) + geom_line() + scale_linetype_manual(values=c('solid', "dashed", "dashed")) + 
  geom_point(show.legend = FALSE) + expand_limits(y = 0) + #scale_x_discrete(breaks = every_nth(n = 3)) +
  scale_color_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1), axis.title.x = element_blank()) +
  ylab('All new STI diagnoses rate per 100,000') + labs(linetype = "Area name", color = "Area name")

STI_plot

#Physical Exercise ---------------------------------------------
physical <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/PhysicalActiv.csv"),collapse = '')) %>% data.table()
physical

physical <- physical %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
physical
physical[physical == "London region"] <- 'London'
physical[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]

physical_plot <- physical %>%
  ggplot(aes(x=Time.period, y= Value, group = AreaName, color = AreaName, linetype= AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Period: ", Time.period, "<br>",
                         "% Active: ", Value))
  ) + geom_line() + scale_linetype_manual(values=c('solid', "dashed", "dashed")) + 
  geom_point(show.legend = FALSE) + expand_limits(y = 0) + #scale_x_discrete(breaks = every_nth(n = 2)) +
  scale_color_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1), axis.title.x = element_blank()) +
  ylab('Percentage of physically active adults') + labs(linetype = "Area name", color = "Area name")

physical_plot

#Healthy Weight ---------------------------------------------
obesity <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/AdultObesity.csv"),collapse = '')) %>% data.table()
obesity

obesity <- obesity %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
obesity
obesity[obesity == "London region"] <- 'London'
obesity[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]

obesity_plot <- obesity %>%
  ggplot(aes(x=Time.period, y= Value, group = AreaName, color = AreaName, linetype= AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Period: ", Time.period, "<br>",
                         "Prevalence: ", Value, '%'))
  ) + geom_line() + scale_linetype_manual(values=c('solid', "dashed", "dashed")) + 
  geom_point(show.legend = FALSE) + expand_limits(y = 0) + #scale_x_discrete(breaks = every_nth(n = 2)) +
  scale_color_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1), axis.title.x = element_blank()) +
  ylab('Percentage of adults classified as Overweight/Obese') + labs(linetype = "Area name", color = "Area name")

obesity_plot

#Smoking ---------------------------------------------
smoke <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/Smoking.csv"),collapse = '')) %>% data.table()
smoke

smoke <- smoke %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
smoke
smoke[smoke == "London region"] <- 'London'
smoke[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]

smoke_plot <- smoke %>%
  ggplot(aes(x=Time.period, y= Value, group = AreaName, color = AreaName, linetype= AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Period: ", Time.period, "<br>",
                         "Prevalence: ", Value, '%'))
  ) + geom_line() + scale_linetype_manual(values=c('solid', "dashed", "dashed")) + 
  geom_point(show.legend = FALSE) + expand_limits(y = 0) + #scale_x_discrete(breaks = every_nth(n = 2)) +
  scale_color_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1), axis.title.x = element_blank()) +
  ylab('Smoking prevalence in adults (15+) - current smokers (%)') + labs(linetype = "Area name", color = "Area name")

smoke_plot

#High Blood Pressure ---------------------------------------------
htn <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/Hypertension.csv"),collapse = '')) %>% data.table()
htn

htn <- htn %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
htn
htn[htn == "London region"] <- 'London'
htn[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]

htn_plot <- htn %>%
  ggplot(aes(x=Time.period, y= Value, group = AreaName, color = AreaName, linetype= AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Period: ", Time.period, "<br>",
                         "Prevalence: ", Value, '%'))
  ) + geom_line() + scale_linetype_manual(values=c('solid', "dashed", "dashed")) + 
  geom_point(show.legend = FALSE) + expand_limits(y = 0) + #scale_x_discrete(breaks = every_nth(n = 2)) +
  scale_color_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1), axis.title.x = element_blank()) +
  ylab('Hypertension prevalence (%)') + labs(linetype = "Area name", color = "Area name")

htn_plot

#Drug / Alcohol Misuse ---------------------------------------------
drug <- read.xlsx(paste(c(dir_stub, "../data/Health and wellbeing/Drug offences.xlsx"),collapse = ''), colNames = TRUE) %>% data.table()
drug$X1 <- format(as.Date(drug$X1, origin = "1899-12-30"), "%Y-%m")
colnames(drug) <- c('Date','Number.of.offences', 'Rate/1000', 'yearly.change', 'percent.change.from.last.month')
drug
drug_plot <- drug %>%
  ggplot(aes(x=Date, y= `Rate/1000`, group = 1, 
             text= paste("Period: ", Date, "<br>",
                         "Prevalence: ", `Rate/1000`))
  ) + geom_line(color = "#ff9900") + 
  geom_point(color = "#ff9900") + expand_limits(y = 0) + #scale_x_discrete(breaks = every_nth(n = 2)) +
  scale_color_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1), axis.title.x = element_blank()) +
  ylab('Drug Offences per 1000') 

drug_plot

# Healthy Life Expectancy at birth by Sex --------------------------------------------------------------
LEMale <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/LE_Male.csv"),collapse = '')) %>% data.table()
LEFemale <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/LE_Female.csv"),collapse = '')) %>% data.table()

LEMale
LE<- Reduce(function(x, y) merge(x, y, all=TRUE), list(LEMale,LEFemale))

LE <- LE %>% select(c(AreaName, Sex, Time.period, Value, Lower.CI.95.0.limit, 
                        Upper.CI.95.0.limit))
LE[LE == "London region"] <- 'London'
LE[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]
LE
LE_plot <- LE %>%
  ggplot(aes(x=Time.period, y= Value, group = AreaName, fill = AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Period: ", Time.period, "<br>",
                         "Healthy Life expectancy: ", Value))
  ) + facet_wrap(~Sex) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.title.x = element_blank()) +
  ylab('Life Expectancy at Birth') + labs(fill = "Area name")

LE_plot

# Healthy Life Expectancy at birth by Sex --------------------------------------------------------------
HLEMale <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/HLEMale.csv"),collapse = '')) %>% data.table()
HLEFemale <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/HLEFemale.csv"),collapse = '')) %>% data.table()

HLEMale
HLE<- Reduce(function(x, y) merge(x, y, all=TRUE), list(HLEMale,HLEFemale))

HLE <- HLE %>% select(c(AreaName, Sex, Time.period, Value, Lower.CI.95.0.limit, 
                                Upper.CI.95.0.limit))
HLE[HLE == "London region"] <- 'London'
HLE[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]

HLE_plot <- HLE %>%
  ggplot(aes(x=Time.period, y= Value, group = AreaName, color = AreaName, linetype= AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Period: ", Time.period, "<br>",
                         "Healthy Life expectancy: ", Value))
         ) + facet_wrap(~Sex) +
  geom_line() + scale_linetype_manual(values=c('solid', "dashed", "dashed")) + 
  geom_point(show.legend = FALSE) + expand_limits(y = 0) + #scale_x_discrete(breaks = every_nth(n = 2)) +
  scale_color_discrete(type=lambeth_palette_graph) +  
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1), axis.title.x = element_blank())+# theme_lam() +
  ylab('Healthy Life Expectancy at Birth (Years)') + labs(linetype = "Area name", color = "Area name")

HLE_plot

# Deathrate under 75 (preventable)  --------------------------------------------------------------
deathunder75 <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/Deathsunder75preventable.csv"),collapse = '')) %>% data.table()
deathunder75
# variable.names <- append(variable.names, 'deathunder75')

deathunder75 <- deathunder75 %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
deathunder75
deathunder75[deathunder75 == "London region"] <- 'London'
deathunder75[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]


death75_plot <- deathunder75 %>%
  ggplot(aes(x = AreaName, y=Value, fill=AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Deaths under 75 per 100,000 (preventable): ", Value))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.title.x = element_blank()) +
  ylab('Deaths Under 75') + labs(fill = "Area name")

death75_plot

# COVID Mortality --------------------------------------------------------------
# Lamcovid <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/Lambeth Weekly COVID deaths.csv"),collapse = '')) %>% data.table()
# Loncovid <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/London Weekly COVID deaths.csv"),collapse = '')) %>% data.table()
# Engcovid <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/England Weekly COVID deaths.csv"),collapse = '')) %>% data.table()
# covid<- Reduce(function(x, y) merge(x, y, all=TRUE), list(Lamcovid, Loncovid, Engcovid))
# covid
# covid<- covid[,c(2,4,5,6)]
# names(covid) <- c('area', 'Date', 'newWeeklyDeaths','cumulDeaths')
# # variable.names <- append(variable.names, 'covid')
# 
# covid$Date <- as.Date(covid$Date)
# covid <- filter(covid, format(covid$Date,'%Y') %in% c("2022","2023"))
# 
# population <- data.frame(area=c('Lambeth','London','England'), 
#                          population=c(317674, 8799728, 560490048))
# merged <- merge(covid, population, by='area')
# 
# 
# covid$'New Weekly Deaths Per Capita' <- round(merged$newWeeklyDeaths / merged$population * 100000, 3)
# covid
# covid[, area := ordered(area, levels = c("Lambeth", "London", "England"))]
# 
# covidplot <- covid %>%
#   ggplot( aes(x=Date, y=`New Weekly Deaths Per Capita`, group=area, color=area,
#               text= paste("Area name: ", area, "<br>",
#                           "Weekly Deaths: ", newWeeklyDeaths, "<br>",
#                           "Weekly Deaths Per Capita: ", `New Weekly Deaths Per Capita`))) +
#   geom_line() + scale_x_date(date_labels="%b-%y", breaks = "3 month", limits = c(min(covid$Date), max = max(covid$Date)),
#                              expand=c(0,0)) + #scale_x_date(date_labels="%b-%y",date_breaks  ="3 month") +
#   scale_color_discrete(type=lambeth_palette_graph) + theme_lam() + theme(axis.title.x = element_blank()) +
#   labs(color = "Area name")
# covidplot
# 
covid <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/covid.csv"),collapse = '')) %>% data.table()
covid

covid <- covid %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
covid
covid[covid == "London region"] <- 'London'
covid[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]


covidplot <- covid %>%
  ggplot(aes(x = AreaName, y=Value, fill=AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "COVID mortality: ", Value))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() + 
  theme(axis.title.x = element_blank()) +
  ylab('COVID Mortality per 100,000') + labs(fill = "Area name")

covidplot
# 
# # Cancer Mortality (preventable)  --------------------------------------------------------------
cancer <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/CancerMortality.csv"),collapse = '')) %>% data.table()
cancer

cancer <- cancer %>%
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
cancer
cancer[cancer == "London region"] <- 'London'
cancer[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]


cancer_plot <- cancer %>%
  ggplot(aes(x = AreaName, y=Value, fill=AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Cancer mortality: ", Value))) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() + theme(axis.title.x = element_blank()) +
  ylab('Cancer Mortality per 100,000') + labs(fill = "Area name")

cancer_plot


# Cardiovascular Disease Mortality (preventable)  --------------------------------------------------------------
cvd <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/CVDMortality.csv"),collapse = '')) %>% data.table()
cvd

cvd <- cvd %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
cvd
cvd[cvd == "London region"] <- 'London'
cvd[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]


cvd_plot <- cvd %>%
  ggplot(aes(x = AreaName, y=Value, fill=AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Cardiovascular Disease mortality: ", Value))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() + theme(axis.title.x = element_blank()) +
  ylab('Cardiovascular Disease Mortality per 100,000') + labs(fill = "Area name")

cvd_plot

# Liver Disease Mortality (preventable)  --------------------------------------------------------------
liver <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/LiverMortality.csv"),collapse = '')) %>% data.table()
liver

liver <- liver %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
liver
liver[liver == "London region"] <- 'London'
liver[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]


liver_plot <- liver %>%
  ggplot(aes(x = AreaName, y=Value, fill=AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Liver Disease mortality: ", Value))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() + theme(axis.title.x = element_blank()) +
  ylab('Liver Disease Mortality per 100,000') + labs(fill = "Area name")

liver_plot

# Respiratory Disease Mortality (preventable)  --------------------------------------------------------------
resp <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/RespiratoryMortality.csv"),collapse = '')) %>% data.table()
resp

resp <- resp %>% 
  select(c(AreaName, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
resp
resp[resp == "London region"] <- 'London'
resp[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]


resp_plot <- resp %>%
  ggplot(aes(x = AreaName, y=Value, fill=AreaName,
             text= paste("Area name: ", AreaName, "<br>",
                         "Respiratory Disease mortality: ", Value))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() + theme(axis.title.x = element_blank()) +
  ylab('Respiratory Disease Mortality per 100,000') + labs(fill = "Area name")

resp_plot

# EFDI Score  --------------------------------------------------------------

# efdi <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/efdi_score_20200101.csv"),collapse = '')) %>% data.table()
# 
# efdi
# efdi <- efdi %>% 
#   rename(
#     Area = Dataset.name,
#     EFDI_Score = 'E.food.Desert.Index..EFDI..score'
#   )
# # keep last three rows
# efdi_compare <- efdi[c(26, 27, 28),]
# efdi_compare[, Area := ordered(Area, levels = c("Lambeth", "London", "England"))]
# 
# 
# efdi_plot <- efdi_compare %>%
#   ggplot(aes(x = Area, y=EFDI_Score, fill=Area,
#              text= paste("Area name: ", Area, "<br>",
#                          "EFDI Score: ", EFDI_Score))) +
#   geom_bar(position="dodge", stat="identity") +
#   scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() +
#   theme_lam() + theme(axis.title.x = element_blank()) + ylab('EFDI Score') + labs(fill = "Area name")
# 
# 
# 
# efdi_plot
# variable.names<- append(variable.names, 'efdi')
# pal = colorBin('RdPu', efdi$EFDI_Score, bins = 5, pretty = TRUE, na.color = "#808080")
# 
# 
# wards_popup <- paste0("<strong>Ward:  </strong>", wards$WARD_NAME,
#                       "<br><strong>EFDI Score (2020):  </strong>", efdi$EFDI_Score)
# 
# map <- map %>%
#   addPolygons(group= 'efdi', fillOpacity = 0.7,
#               fillColor = ~pal(efdi$EFDI_Score), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~efdi$EFDI_Score, opacity = 0.7, 
#             group = 'efdi', title = 'E-food Desert Index')
# 
# # Food Vulnerability Score  --------------------------------------------------------------
# 
# foodvul <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/food_vulnerability_score_20200601.csv"),collapse = '')) %>% data.table()
# foodvul
# foodvul <- foodvul %>% 
#   rename(
#     Area = Dataset.name,
#     Food_vulnerability_Score = 'Food.vulnerability.score'
#   )
# foodvul_compare <- foodvul[c(26, 27, 28),]
# foodvul_compare[, Ward := ordered(Area, levels = c("Lambeth", "London", "England"))]
# 
# 
# foodvul_plot <- foodvul_compare %>%
#   ggplot(aes(x = Area, y=Food_vulnerability_Score, fill = Area,
#              text= paste("Area name: ", Area, "<br>",
#                          "Food Vulnerability Score: ", Food_vulnerability_Score))) + geom_bar(position="dodge", stat="identity") +
#   scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() +
#   theme(axis.title.x = element_blank()) + ylab('Food Vulnerability Score') + labs(fill = "Area name")
# 
# foodvul_plot
# 
# variable.names <- append(variable.names, 'foodvul')
# 
# pal = colorBin('RdPu', foodvul$Food_vulnerability_Score, bins = 5, pretty = TRUE, na.color = "#808080")
# 
# 
# wards_popup <- paste0("<strong>Ward:  </strong>", wards$WARD_NAME,
#                       "<br><strong>Food Vulnerability Score (2020):  </strong>", foodvul$Food_vulnerability_Score)
# 
# map <- map %>%
#   addPolygons(group = 'foodvul', fillOpacity = 0.7,
#               fillColor = ~pal(foodvul$Food_vulnerability_Score), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~foodvul$Food_vulnerability_Score, opacity = 0.7, 
#             group = 'foodvul', title = 'Food Vulnerability Score')

# Alcohol Hospitalisations  --------------------------------------------------------------

# alcohol <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/alcoholrelatedadmissions.csv"),collapse = '')) %>% data.table()
# 
# alcohol
# alcohol <- alcohol %>% 
#   select(c(AreaName, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit))
# alcohol[alcohol == "London region"] <- 'London'
# 
# alcohol[, AreaName := ordered(AreaName, levels = c("Lambeth", "London", "England"))]
# 
# alcohol_plot <- alcohol %>%
#   ggplot(aes(x = AreaName, y=Value, fill = AreaName,
#              text= paste("Area name: ", AreaName, "<br>",
#                          "Alcohol-related hospitalisations: ", Value))) + geom_bar(position="dodge", stat="identity")+
#   scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() +
#   theme(axis.title.x = element_blank()) + ylab('Alcohol Related Hospitalisations') + labs(fill = "Area name")
# 
# alcohol_plot
# 
# variable.names<-append(variable.names, 'alcohol')
# 
# pal = colorBin('RdPu', alcohol$Value, bins = 5, pretty = TRUE, na.color = "#808080")
# 
# 
# wards_popup <- paste0("<strong>Ward:  </strong>", wards$WARD_NAME,
#                       "<br><strong>Alcohol Related Hospitalisations (2020):  </strong>", alcohol$Value)
# 
# map <- map %>%
#   addPolygons(group = 'alcohol', fillOpacity = 0.7,
#               fillColor = ~pal(alcohol$Value), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~alcohol$Value, opacity = 0.7,
#             group = 'alcohol', title = 'Alcohol Related Hospitalisations')

# Previous Cancer  --------------------------------------------------------------
# 
# cancer <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/prev_cancer_20190401.csv"),collapse = '')) %>% data.table()
# 
# cancer
# 
# cancer <- cancer %>% 
#   rename(
#     Area = Dataset.name,
#     cancer_prevalence = 'Cancer'
#   )
# cancer_compare <- cancer[c(26, 27, 28),]
# cancer_compare[, Area := ordered(Area, levels = c("Lambeth", "London", "England"))]
# 
# 
# cancer_plot <- cancer_compare %>%
#   # mutate(Ward = Ward %>% factor(levels = rankings$Ward)) %>%
#   ggplot(aes(x = Area, y=cancer_prevalence, fill=Area,
#              text= paste("Area name: ", Area, "<br>",
#                          "Cancer Prevalence: ", cancer_prevalence))) + geom_bar(position="dodge", stat="identity") +
#   scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() +
#   theme(axis.title.x = element_blank()) + ylab('Cancer Prevalence') + labs(fill = "Area name")
# 
# 
# cancer_plot
# variable.names<-append(variable.names, 'cancer')
# 
# pal = colorBin('RdPu', cancer$cancer_prevalence, bins = 5, pretty = TRUE, na.color = "#808080")
# 
# 
# wards_popup <- paste0("<strong>Ward:  </strong>", wards$WARD_NAME,
#                       "<br><strong>Cancer Prevalence (2021):  </strong>", cancer$cancer_prevalence)
# 
# map <- map %>%
#   addPolygons(group = 'cancer', fillOpacity = 0.7,
#               fillColor = ~pal(cancer$cancer_prevalence), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~cancer$cancer_prevalence, opacity = 0.7, 
#             group = 'cancer', title = 'Previous Cancer')

# Previous Cardiovascular Disease  --------------------------------------------------------------
# 
# cvd <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/prev_cardio_vasc_20190401.csv"),collapse = '')) %>% data.table()
# cvd
# 
# cvd <- cvd %>% 
#   rename(
#     Area = Dataset.name,
#     cardiovascular_disease_prevalence = 'Cardiovascular.Disease'
#   )
# cvd_compare <- cvd[c(26, 27, 28),]
# cvd_compare[, Area := ordered(Area, levels = c("Lambeth", "London", "England"))]
# 
# 
# cvd_plot <- cvd_compare %>%
#   ggplot(aes(x = Area, y=cardiovascular_disease_prevalence, fill = Area,
#              text= paste("Area name: ", Area, "<br>",
#                          "Cardiovascular Disease Prevalence: ", cardiovascular_disease_prevalence))) + geom_bar(position="dodge", stat="identity")+
#   scale_fill_discrete(type=lambeth_palette_graph) + theme_lam() +
#   theme(axis.title.x = element_blank()) + ylab('Cardiovascular Disease Prevalence') + labs(fill = "Area name")
# 
# cvd_plot

# variable.names = append(variable.names, 'cvd')
# 
# pal = colorBin('RdPu', cvd$cardiovascular_disease_prevalence, bins = 5, pretty = TRUE, na.color = "#808080")
# 
# 
# wards_popup <- paste0("<strong>Ward:  </strong>", wards$WARD_NAME,
#                       "<br><strong>CVD Prevalence (2021):  </strong>", cvd$cardiovascular_disease_prevalence)

# map <- map %>%
#   addPolygons(group = 'cvd', fillOpacity = 0.7,
#               fillColor = ~pal(cvd$cardiovascular_disease_prevalence), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~cvd$cardiovascular_disease_prevalence, opacity = 0.7, 
#             group = 'cvd', title = 'Previous Cardiovascular Disease')
# 
# Previous Depression  --------------------------------------------------------------

# depression <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/prev_depress_20190401.csv"),collapse = '')) %>% data.table()
# 
# depression
# 
# depression <- depression %>% 
#   rename(
#     Area = Dataset.name,
#     Depression_prevalence = 'Depression'
#   )
# depression_compare <- depression[c(26, 27, 28),]
# depression_compare[, Area := ordered(Area, levels = c("Lambeth", "London", "England"))]
# 
# 
# depression_plot <- depression_compare %>%
#   ggplot(aes(x = Area, y=Depression_prevalence, fill = Area,
#              text= paste("Area name: ", Area, "<br>",
#                          "Depression Prevalence: ", Depression_prevalence))) + geom_bar(position="dodge", stat="identity") +
#   scale_fill_discrete(type=lambeth_palette_graph) + theme_lam()  +
#   theme(axis.title.x = element_blank()) + ylab('Depression Prevalence')
# 
# 
# depression_plot
# 
# variable.names <- append(variable.names, 'depression')
# pal = colorBin('RdPu', depression$Depression_prevalence, bins = 5, pretty = TRUE, na.color = "#808080")
# 
# 
# wards_popup <- paste0("<strong>Ward:  </strong>", wards$WARD_NAME,
#                       "<br><strong>Depression Prevalence (2021):  </strong>", depression$Depression_prevalence)
# 
# map <- map %>%
#   addPolygons(group = 'depression', fillOpacity = 0.7,
#               fillColor = ~pal(depression$Depression_prevalence), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~depression$Depression_prevalence, opacity = 0.7, 
#             group = 'depression', title = 'Previous Depression')

# Mental Health Index  --------------------------------------------------------------

# mental <- read.csv(paste(c(dir_stub, "../data/Health and wellbeing/samhi_index_20190101.csv"),collapse = '')) %>% data.table()
# 
# mental
# 
# mental <- mental %>% 
#   rename(
#     Area = Dataset.name,
#     Mental_health_index = 'Mental.Health.Index'
#   )
# mental_compare <- mental[c(26, 27, 28),]
# mental_compare[, Area := ordered(Area, levels = c("Lambeth", "London", "England"))]
# 
# 
# mental_plot <- mental_compare %>%
#   ggplot(aes(x = Area, y=Mental_health_index, fill = Area,
#              text= paste("Area name: ", Area, "<br>",
#                          "Mental Health Index: ", Mental_health_index))) + geom_bar(position="dodge", stat="identity") +
#   scale_fill_discrete(type=lambeth_palette_graph) + theme_lam()  +
#   theme(axis.title.x = element_blank()) + ylab('Mental Health Index')
# 
# mental_plot
# 
# variable.names<-append(variable.names, 'mental')
# 
#Map--------------------------------------
# variable.names <- c()
# variable.names <- append(variable.names, 'life_exp')
# pal = colorBin('RdPu', long_LE$avg.life.expectancy, bins = 5, pretty = TRUE, na.color = "#808080")
# 
# 
# wards_popup <- paste0("<strong>Ward:  </strong>", wards$WARD_NAME,
#                       "<br><strong>Female Life Expectancy (2016):  </strong>", long_LE$Female.life.expectancy,
#                       "<br><strong>Male Life Expectancy (2016):  </strong>", long_LE$Male.Life.expectancy)
# 
# map<-leaflet(data = wards, options = leafletOptions(minZoom = 11.4)) %>% 
#   addProviderTiles("CartoDB.Positron") %>% 
#   setView(-0.116, 51.46, zoom = 11.4) %>% 
#   setMaxBounds(lng1 = -0.075, lat1= 51.52,
#                lng2=  -0.159, lat2 = 51.4) %>% 
#   addPolygons(group = 'life_exp', fillOpacity = 0.7,
#               fillColor = ~pal(long_LE$`Healthy life expectancy (HLE)`), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~long_LE$`Healthy life expectancy (HLE)`, opacity = 0.7, 
#             group = 'life_exp', title = 'Average Life Expectancy')
#   
# map
# pal = colorBin('RdPu', happiness$Happiness, bins = 5, pretty = TRUE, na.color = "#808080")
# 
# 
# wards_popup <- paste0("<strong>Ward:  </strong>", wards$WARD_NAME,
#                       "<br><strong>Happiness (2018-19):  </strong>", happiness$Happiness)
# 
# map <- map %>%
#   addPolygons(group= 'happiness', fillOpacity = 0.7,
#               fillColor = ~pal(happiness$Happiness), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~happiness$Happiness, opacity = 0.7, 
#             group = 'happiness', title = 'Happiness Index') %>%
#   
#   addPolygons(group= 'lifesatis', fillOpacity = 0.7,
#               fillColor = ~pal(happiness$'Life Satisfaction'), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~happiness$'Life Satisfaction', opacity = 0.7,
#             group = 'lifesatis', title = 'Life Satisfaction Index') %>%
#   
#   addPolygons(group= 'worthwhile', fillOpacity = 0.7,
#               fillColor = ~pal(happiness$Worthwhile), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~happiness$Worthwhile, opacity = 0.7, 
#             group = 'worthwhile', title = 'Worthwhile Index') %>%
#   
#   addPolygons(group= 'anxiety', fillOpacity = 0.7,
#               fillColor = ~pal(happiness$Anxiety), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~happiness$Anxiety, opacity = 0.7, 
#             group = 'anxiety', title = 'Anxiety Index')

# pal = colorBin('RdPu', deathunder75$'Deaths from all cancer,  under 75 years', bins = 5, pretty = TRUE, na.color = "#808080")
# 
# wards_popup <- paste0("<strong>Ward:  </strong>", wards$WARD_NAME,
#                       "<br><strong>Death rate (2021):  </strong>", deathunder75$'Deaths from all cancer,  under 75 years')
# 
# map <- map %>%
#   addPolygons(group= 'deathunder75', fillOpacity = 0.7,
#               fillColor = ~pal(deathunder75$'Deaths from all cancer,  under 75 years'), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~deathunder75$'Deaths from all cancer,  under 75 years', opacity = 0.7,
#             group = 'deathunder75', title = 'Deaths from all cancer, under 75 years')

# pal = colorBin('RdPu', mental$Mental_health_index, bins = 5, pretty = TRUE, na.color = "#808080")
# 
# 
# wards_popup <- paste0("<strong>Ward:  </strong>", wards$WARD_NAME,
#                       "<br><strong>Mental Health Index (2022):  </strong>", mental$Mental_health_index)
# 
# map <- map %>%
#   addPolygons(group = 'mental', fillOpacity = 0.7,
#               fillColor = ~pal(mental$Mental_health_index), color = "darkgrey", weight = 2, popup = wards_popup) %>%
#   addLegend(position = c("bottomleft"), pal = pal, values = ~mental$Mental_health_index, opacity = 0.7, 
#             group = 'mental', title = 'Mental Health Index')

# map_mark <- map %>%
#   addResetMapButton() %>%
#   # http://rpubs.com/bhaskarvk/leaflet-heat
#   addProviderTiles(providers$CartoDB.Voyager, group = "Carto (default)") %>% # Need leaflet extras
#   addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
#   addProviderTiles(providers$Stamen.TonerLite, group = "Grey and white") %>%
#   
#   addSearchOSM(options = searchOptions(collapsed = F, autoCollapse = F, textPlaceholder = "Search places in UK")) %>%
#   addFullscreenControl() %>%
#   addLayersControl(
#     baseGroups = c("Carto (default)", "Satellite", "Grey and white"),
#     overlayGroups = c(variable.names), 
#     options = layersControlOptions(collapsed = F))  %>%
#   # Things hidden by default. Show only Wards by default
#   hideGroup(variable.names[-1]) # , "LSOA", "Deprivation vs Reach"
# map_mark
# return(map_mark)

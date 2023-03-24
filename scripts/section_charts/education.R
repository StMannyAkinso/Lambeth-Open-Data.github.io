# Load packages, initial variables ----------------------------------------

require(ggplot2)
require(plotly)
require(data.table)
require(rstudioapi)
#require(xlsx)
#library(readxl)
library(openxlsx)
#library(gglaplot)



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


# Population --------------------------------------------------------------
pop <- read.xlsx(paste(c(dir_stub, "../data/Jobs/NOMIS/Total population simple 2020.xlsx"),collapse = ''),
       startRow = 2) %>% data.table()
pop

pop_df= pop[2:4]
pop_df

pop_df_m = melt(pop_df, id.vars = "X1") %>% as.data.table()
names(pop_df_m) = c("Sex", "Area", "Population")

pop_tot <- pop_df_m[Sex=="All People"][, Sex:= NULL]

# Population by age -----------------------------------------------------

file_path <- paste(c(dir_stub, "../data/Key demographics/Census 2021/TS009_sex_pop_age_census_2021.xlsx"),collapse = '')


pull_tables <- function(path, start_row, area_name = NULL){
  # Load data
  df <- read.xlsx(path, startRow = start_row) %>% data.table()
  
  # cut table at first blank row
  df <- df[1:which(is.na(df[,2]))[1]-1,]
  
  # Remove . from columns
  names(df) = gsub("\\.", " ", names(df))
  
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

# combine the three tables
comb <- rbind(lam, eng, lon)

# Make Area name an ordered factor Lambeth, London, England
comb[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England"))]

# Create column total all persons as sum of All persons by Area name
comb[, `Total all persons` := sum(`All persons`, na.rm = T), by =.(`Area name`)]

# Create Percentage Female all persons as Percentage Female/Total all persons
comb[, `Percentage female all persons` := round((Female / `Total all persons`) * 100, 1)]
comb[, `Percentage male all persons` := round((Male / `Total all persons`) * 100, 1)]

comb

comb_4 <- comb[`Age group` == 4]


# Number of providers -----------------------------------------------------

# Number of Providers by Provider Type and Children's Age (2018 to 2021)
# 
# Filename: 3_early_years_provision_provider_type_2018_to_2021.csv
# Geographic levels: National; Local Authority; Regional
# Time period: 2018 to 2021
# Content summary: Number of providers of funded entitlements by provider type (e.g., childminders).
# 
# Variable names and descriptions for this file are provided below:
# 
# Variable name    |  Variable description

# entitlement_type   |  Type of entitlement: funded or extended - Select type of entitlement: funded or extended
# number_providers_2 |  Number of providers delivering education for 2 year olds
# number_providers_34  |  Number of providers delivering education for 3 and 4 year olds
# provider_type    |  Type of provider - Select type of provider: for example Childminders
# 
# Footnotes:
# 
# 1. “Maintained Nursery and State-funded primary schools" are a combination of the "Primary schools with nursery classes", "Primary schools without nursery classes", "Nursery and infant classes in primary schools" and "Maintained Nursery schools" sub-groups.
# 2. 2021 is the first year of data covering COVID-19, collected during January's national lockdown. Open providers, or providers closed due to government guidance, made census returns relating to registered children's "expected attendance" in census week. Further information is available in the methodology. 




ey = fread(paste(c(dir_stub, "../data/Education/2022/EY/3b_early_years_provision_provider_type_2018_2022.csv"),
       collapse = ''))

##


#ey <- gsub("\\.", 0, fixed = T)

summary(ey)

# change to numeric
ey[, number_providers_2:=as.integer(number_providers_2)]
ey[, number_providers_34:=as.integer(number_providers_34)]

# [markdown]
# Local authority London level df


ey_df = ey[(time_period == 2022) & (geographic_level == "Local authority") & (provider_type == "All providers") &
     ((region_name == "Outer London") | (region_name == "Inner London"))]


#ey_df.to_csv("test.csv")

# sum up by LA
cols_to_keep <- c("la_name","number_providers_2","number_providers_34")

ey_df_c <- ey_df[, ..cols_to_keep]

ey_df_all <- ey_df_c[,`:=`(number_providers_2 = sum(number_providers_2, na.rm = T),
               number_providers_34 = sum(number_providers_34, na.rm=T)), by = c("la_name")][
               !duplicated(ey_df_c)
               ]


# London total - no LA named London, so I will need to sum up
ey_df_lon <- ey_df_all[, .(number_providers_2 = sum(number_providers_2, na.rm = T),
          number_providers_34 = sum(number_providers_34, na.rm=T))]


ey_df_lon$la_name = "London"

# Lambeth level


ey_df_lam = ey_df_all[la_name == "Lambeth"]


ey_df_lam

# [markdown]
# England level df

ey_df_eng = ey[(time_period == 2022) & (geographic_level == "National") & (provider_type == "All providers") &
     ((country_name == "England"))]


ey_df_eng_df <- ey_df_eng[, ..cols_to_keep]

ey_df_eng_all <- ey_df_eng_df[,`:=`(number_providers_2 = sum(number_providers_2, na.rm = T),
         number_providers_34 = sum(number_providers_34, na.rm=T)), by = c("la_name")][
           !duplicated(ey_df_eng_df)
         ]


ey_df_eng_all$la_name = "England"

# [markdown]
# Combine all together


ey_df_comb = rbindlist(list(ey_df_lam, ey_df_lon, ey_df_eng_all), use.names = T)


ey_df_comb


ey_df_comb[,total_providers := number_providers_2 + number_providers_34]


setnames(ey_df_comb, c("la_name", "number_providers_2", "number_providers_34", "total_providers"), c("Area", "Number of providers for aged 2", "Number of providers for ages 3-4", "Total providers for ages 2-4"))


comb_4_j <- setnames(comb_4, c("Area name", "All persons"), c("Area", "Population"))

cols <- c("Area", "Population")
ey_df_comb = merge(ey_df_comb, comb_4[,..cols], by = "Area", all.x = T)
ey_df_comb[,Population:= as.integer(Population)]
#
#summary(ey_df_comb)


ey_df_comb[, "Providers per 1000 children 0-4":= round((`Total providers for ages 2-4` / Population)*1000,1)]

ey_df_comb[, Area := ordered(Area, c("Lambeth", "London", "England"))]


ey_df_comb <- ey_df_comb[order(Area)]
## Graph

# Present on a ggplotly graph
ey_comparison_graph = ggplot(ey_df_comb, aes(x=Area, y = `Providers per 1000 children 0-4`, 
                    fill = Area,
                    text= paste("Area: ", Area, "<br>",
                        "Providers per 1000 children 0-4: ", `Providers per 1000 children 0-4`, sep = "")#, x = `Year`, 
)) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Providers per 1000 children 0-4")

ey_comparison_graph


# Quality of providers -----------------------------------------------------
# # Early years OFSTED ratings


ey_of = fread(paste(c(dir_stub, "../data/Education/2022/EY/4_early_years_provision_ofsted_ratings_2018_2022.csv"), collapse = ''))

 #[markdown]
# Filename: 4_early_years_provision_ofsted_ratings_2018_to_2021.csv
# Geographic levels: National; Local Authority; Regional
# Time period: 2018 to 2021
# Content summary: The number of children benefiting from some funded early education by Ofsted inspection rating.
# 
# Variable names and descriptions for this file are provided below:
# 
# Variable name   |  Variable description

# percentage_of_2_year_olds   |  Percent of two year olds registered
# percentage_of_3_4_year_olds   |  Per cent of three and four year olds registered
# entitlement_type  |  Type of entitlement: funded or extended - Select type of entitlement: funded or extended
# number_2_year_olds  |  Number of two year olds registered
# number_3_4_year_olds  |  Number of three and four year olds registered
# ofsted_inspection   |  Ofsted inspection rating - Select Ofsted inspection rating: for example Ofsted inspection rating - Good
# 
# Footnotes:
# 
# 1. Percentages are based upon those where the provider has a matched and identified Ofsted rating (i.e., excludes "No Match"). The four available ratings sum to 100% (may not be exact due to rounding).
# 2. "No Match" includes registered independent schools who are not required to register with Ofsted and academy converters who have not yet been inspected under their new status. These are included as a proportion of the "Total" numbers, based on the total number of children receiving funding.
# 3. 2021 is the first year of data covering COVID-19, collected during January's national lockdown. Open providers, or providers closed due to government guidance, made census returns relating to registered children's "expected attendance" in census week. Further information is available in the methodology. 
# 4. Statutory guidance advises local authorities not to fund 'Requires Improvement' or 'Inadequate' provision for eligible two-year-olds unless sufficient, accessible ‘Good’ or ‘Outstanding’ provision is not available. 
# 
# 
# Percentage and Number of Providers and Children Registered with Graduate-Level Staff (2018 to 2021)
# 




#ey_of = gsub(".","", ey_of, fixed=T)


ey_of[,`number_of_2_year_olds` := as.integer(`number_of_2_year_olds`)]
ey_of[,`number_of_3_4_year_olds` := as.integer(`number_of_3_4_year_olds`)]

### I need to reconstruct the percentages myself
ey_of[, `:=`(percentage_of_2_year_olds = NULL,
    percentage_of_3_4_year_olds = NULL
    )]


# [markdown]
# Local authority London level df


#unique(ey_of$entitlement_type)

cols = c("la_name","number_of_2_year_olds","number_of_3_4_year_olds")

# Melt on the 'number' columns
ey_of_m <- data.table::melt(ey_of, measure.vars = c("number_of_2_year_olds","number_of_3_4_year_olds"), variable.name= "child_age_category", value.name = "number_of_children")


## Pivot out Ofsted inspection

ey_of_d <- data.table::dcast(ey_of_m, geographic_level + country_name + region_name + la_name + time_period ~ ofsted_inspection, value.var = "number_of_children", fun.aggregate = sum, na.rm = T)[time_period == 2022]

ey_of_d[, percentage_of_children := (Good + Outstanding)/`Matched Ofsted judgement`]


ey_of_d_out <- ey_of_d[(la_name=="Lambeth") | 
         (region_name == "London" & geographic_level == "Regional") |
         (geographic_level == "National" & country_name == "England")
        ]
ey_of_d_out[, `Area name` := c("Lambeth", "England" , "London")]

#names(ey_of_d_out)

cols_out = c("Area name", "time_period", "Outstanding", "Good", "Matched Ofsted judgement", "percentage_of_children")

ey_of_d_out <- ey_of_d_out[, cols_out, with=F]

# set area name as an ordered factor with order Lambeth, London, England
ey_of_d_out[, `Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England"))]

# I would like to rename the columns of ey_of_d_out to be more descriptive
setnames(ey_of_d_out, c("Area name", "time_period", "Outstanding", "Good", "Matched Ofsted judgement", "percentage_of_children"), c("Area name", "Year", "Outstanding", "Good", "Total", "Children in good or outstanding institutions (%)"))



#  multiply percentage_of_children by 100 to get a percentage and round to 1 decimal place
ey_of_d_out[, `Children in good or outstanding institutions (%)` := round(`Children in good or outstanding institutions (%)` * 100, 1)]




names(ey_of_d_out)

# Make a bar chart of ey_of_d_out using ggplot
ey_ofsted_graph <- ggplot(ey_of_d_out, aes(x = `Area name`, y = `Children in good or outstanding institutions (%)`, fill = `Area name`,
                    text= paste("Area: ", `Area name`, "<br>",
                        "Children in good or outstanding institutions (%): ", `Children in good or outstanding institutions (%)`, sep = ""))) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Children in good or outstanding institutions (%)")
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #labs(title = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
 #  subtitle = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
  # caption = "Source: Department for Education",
   #  x = "Area",
  # y = "Percentage of children")

ey_ofsted_graph



# Areas of learning performance -----------------------------------------------------

## No additional data YET for 2021

#ey_of = fread(paste(c(dir_stub, "../data/Education/2022/EY/#4_early_years_provision_ofsted_ratings_2018_2022.csv"), collapse = ''))

eyfs_aol = fread(paste(c(dir_stub, "../data/Education/Early years/eyfs-2018-19/data/areas_of_learning_2013_2019.csv"), collapse = ''))



eyfs_aol_head <- head(eyfs_aol)
eyfs_aol <- replace(eyfs_aol, ".", 0)
eyfs_aol_columns <- colnames(eyfs_aol)
eyfs_aol$at_least_expected_percent <- as.numeric(eyfs_aol$at_least_expected_percent)
eyfs_aol$exceeded_percent <- as.numeric(eyfs_aol$exceeded_percent)
eyfs_aol_la_name_isin_lambeth <- eyfs_aol[eyfs_aol$la_name %in% "Lambeth"]

eyfs_aol <- eyfs_aol %>% as.data.table()

eyfs_aol_df <- eyfs_aol[  eyfs_aol$geographic_level == "Local authority" &
  eyfs_aol$gender == "Total" &
  eyfs_aol$area_of_learning == "All Prime Areas" &
  (eyfs_aol$region_name == "Outer London" | eyfs_aol$region_name == "Inner London") &
  (eyfs_aol$time_period == 201819 | eyfs_aol$time_period == "201819")
]
eyfs_aol_df_head <- head(eyfs_aol_df)
eyfs_aol_df_all <- eyfs_aol_df[,c("la_name","at_least_expected_percent")] %>%
  group_by(la_name) %>%
  summarise(at_least_expected_percent = sum(at_least_expected_percent)) %>% as.data.table()

eyfs_aol_df_lam <- eyfs_aol_df_all[eyfs_aol_df_all$la_name == "Lambeth"]


eyfs_aol_df_lon <- eyfs_aol[gender == "Total" &
  area_of_learning == "All Prime Areas" & geographic_level == "Regional" & (region_name == "Outer London" | region_name == "Inner London") & (time_period == 201819 | time_period == "201819")]

eyfs_aol_df_lon$la_name <- "London"
eyfs_aol_df_lon <- eyfs_aol_df_lon[,c("la_name","at_least_expected_percent")] %>%
  group_by(la_name) %>%
  summarise(at_least_expected_percent = mean(at_least_expected_percent))
eyfs_aol_df_lon




eyfs_aol_df_eng <- eyfs_aol[geographic_level == "National" &
  gender == "Total" &
  area_of_learning == "All Prime Areas" &
  (time_period == 201819 | time_period == "201819") &
  country_name == "England"]

eyfs_aol_df_eng$la_name <- "England"

eyfs_aol_df_eng <- eyfs_aol_df_eng[,c("la_name","at_least_expected_percent")] %>%
  group_by(la_name) %>%
  summarise(at_least_expected_percent = mean(at_least_expected_percent))
eyfs_aol_df_eng

# combine the three datasets I made above into one

eyfs_aol_df_all <- rbind(eyfs_aol_df_lam, eyfs_aol_df_lon, eyfs_aol_df_eng)

# rename at_least_expected_percent to "Children achieving expected level in all prime areas (%)"

eyfs_aol_df_all <- rename(eyfs_aol_df_all, "Children achieving expected level in all prime areas (%)" = `at_least_expected_percent`)

# Rename la_name to Area name

eyfs_aol_df_all <- rename(eyfs_aol_df_all, "Area name" = `la_name`)

# Reorder area name

eyfs_aol_df_all[,`Area name` := ordered(`Area name`, levels = c("Lambeth", "London", "England"))]


# Make a bar chart of eyfs_aol_df_all using ggplot
eyfs_aol_graph <- ggplot(eyfs_aol_df_all, aes(x = `Area name`, y = `Children achieving expected level in all prime areas (%)`, fill = `Area name`,
                    text= paste("Area: ", `Area name`, "<br>",
                        "Children achieving expected level in all prime areas (%): ", `Children achieving expected level in all prime areas (%)`, sep = ""))) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Children achieving expected level in all prime areas (%)")
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #labs(title = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
 #  subtitle = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
  # caption = "Source: Department for Education",
   #  x = "Area",
  # y = "Percentage of children")
eyfs_aol_graph



# School funding -----------------------------------------------------
yth_spnd = fread(paste(c(dir_stub, "../data/Education/2022/Schools/expenditure/data/cfr_expenditure_la_regional_national2.csv"), collapse = ''))

names(yth_spnd)

yth_spnd <- yth_spnd %>% as.data.table()

# I want to know how many unique values there are in each column in yth_spnd
table(yth_spnd$expenditure_type)
table(yth_spnd$expenditure_description)


summary(yth_spnd$expenditure_type)

yth_spnd_all <- yth_spnd[((la_name == "Lambeth" & geographic_level == "Local authority") |
 (grepl("London", region_name) & geographic_level == "Regional") |
  (country_name == "England" & geographic_level == "National")) &
 (time_period == 202122 | time_period == "202122") &
  (expenditure_type == "Total expenditure" & expenditure_description == "Total net expenditure")
 ]

  yth_spnd_all

  # I want la_name for region_name == "London" to be "London"
  yth_spnd_all$la_name <- ifelse(grepl("Inner London", yth_spnd_all$region_name) == T &  yth_spnd_all$la_name == "", "Inner London", yth_spnd_all$la_name)

  yth_spnd_all$la_name <- ifelse(grepl("Outer London", yth_spnd_all$region_name) == T &  yth_spnd_all$la_name == "", "Outer London", yth_spnd_all$la_name)
  
# I want la_name for country_name == "England" to be "England"
yth_spnd_all$la_name <- ifelse(yth_spnd_all$country_name == "England" &
  yth_spnd_all$geographic_level == "National", "England", yth_spnd_all$la_name)

table(yth_spnd_all$la_name)

# I want to filter school_phase to "All maintained schools"
yth_spnd_all <- yth_spnd_all[yth_spnd_all$school_phase == "All LA maintained schools"]

# I want to sum expenditure_per_pupil for each la_name. Make a new table with la_name and expenditure_per_pupil

yth_spnd_all[,expenditure_per_pupil := as.numeric(expenditure_per_pupil)]

yth_spnd_all_out <- yth_spnd_all[,c("la_name","expenditure_per_pupil")] %>%
  group_by(la_name) %>%
  summarise(expenditure_per_pupil = mean(expenditure_per_pupil)) %>% as.data.table()

yth_spnd_all_out

# Rename la_name to "Area name" and expenditure_per_pupil to "Expenditure per pupil"
names(yth_spnd_all_out) <- c("Area name", "Expenditure per pupil (GBP)")

# Make an ordered factor for Area name with order Lambeth, Lono, England
yth_spnd_all_out$`Area name` <- ordered(yth_spnd_all_out$`Area name`, levels = c("Lambeth", "Inner London", "Outer London", "England"))

# Make a bar chart of yth_spnd_all_out using ggplot
yth_spnd_graph <- ggplot(yth_spnd_all_out, aes(x = `Area name`, y = `Expenditure per pupil (GBP)`, fill = `Area name`,
                        text= paste("Area: ", `Area name`, "<br>",
                        "Expenditure per pupil (GBP): ", prettyNum(`Expenditure per pupil (GBP)`,big.mark=",", preserve.width="none"), sep = ""))) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  scale_y_continuous(label = scales::label_dollar(prefix = "£")) +
  theme(axis.title.x = element_blank()) +
  ylab("Expenditure per pupil (GBP)")
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #labs(title = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
 #  subtitle = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
  # caption = "Source: Department for Education",
   #  x = "Area",
  # y = "Percentage of children")
yth_spnd_graph


# KS2 results -----------------------------------------------------

# I want to read in the KS2 results data
#ks2_results_eng = fread(paste(c(dir_stub, "../data/Education/2022/Schools/ks2/data/ks2_national_pupil_characteristics_2016_to_2022_provisional.csv"), collapse = ''))


###
ks2_results = fread(paste(c(dir_stub, "../data/Education/2022/Schools/ks2/data/ks2_regional_and_local_authority_2016_to_2022_provisional.csv"), collapse = ''))

# Do the same transformations as above
ks2_results <- ks2_results %>% as.data.table()

ks2_results <- ks2_results[time_period == 202122 | time_period == "202122"]

# filter Gender to "Total"
ks2_results <- ks2_results[gender == "Total"]

# England

ks2_results_eng <- ks2_results[geographic_level == "National" & country_name == "England"]
head(ks2_results_eng)

ks2_results_eng$`Area name` = "England"

#pt_rwm_met_expected_standard           |  Percentage of pupils meeting the expected standard in reading, writing and maths (combined)
#pt_rwm_met_higher_standard             |  Percentage of pupils reaching the higher standard in reading, writing and maths (combined)
# 
# Keep only columns Area name, Area code, pt_rwm_met_expected_standard, pt_rwm_met_higher_standard
ks2_results_eng <- ks2_results_eng[,c("Area name", "pt_rwm_met_expected_standard", "pt_rwm_met_higher_standard")]


# rename pt_rwm_met_expected_standard to "Pupils reaching the expected standard in RWM (%)"

names(ks2_results_eng) <- c("Area name", "Pupils reaching the expected standard in RWM (%)", "Pupils reaching the higher standard in RWM (%)")




# Lambeth
# Filter to la_name == "Lambeth"
ks2_results_lam <- ks2_results[la_name == "Lambeth"]

ks2_results_lam$`Area name` = ifelse(ks2_results_lam$`la_name` == "Lambeth", "Lambeth", ks2_results_lam$`la_name`)


ks2_results_lam <- ks2_results_lam[,c("Area name", "pt_rwm_met_expected_standard", "pt_rwm_met_higher_standard")]


# rename pt_rwm_met_expected_standard to "Pupils reaching the expected standard in RWM (%)"

names(ks2_results_lam) <- c("Area name", "Pupils reaching the expected standard in RWM (%)", "Pupils reaching the higher standard in RWM (%)")

ks2_results_lam


# Filter ks2_results to London and rename to ks2_results_london
ks2_results_london <- ks2_results[region_name == "Outer London" | region_name == "Inner London"]

ks2_results_london$`Area name` = "London"

# filter Gender to "Total"
ks2_results_london <- ks2_results_london[,c("Area name", "pt_rwm_met_expected_standard", "pt_rwm_met_higher_standard")]


# rename pt_rwm_met_expected_standard to "Pupils reaching the expected standard in RWM (%)"

names(ks2_results_london) <- c("Area name", "Pupils reaching the expected standard in RWM (%)", "Pupils reaching the higher standard in RWM (%)")

# convert columns to numeric
ks2_results_london$`Pupils reaching the expected standard in RWM (%)` <- as.numeric(ks2_results_london$`Pupils reaching the expected standard in RWM (%)`)
ks2_results_london$`Pupils reaching the higher standard in RWM (%)` <- as.numeric(ks2_results_london$`Pupils reaching the higher standard in RWM (%)`)

ks2_results_london <- ks2_results_london %>% as.data.table()

# Find the average of all variables for each Area name for ks2_results_london
ks2_results_london_out <- ks2_results_london %>%
  group_by(`Area name`) %>%
  summarise(`Pupils reaching the expected standard in RWM (%)` = mean(`Pupils reaching the expected standard in RWM (%)`),
    `Pupils reaching the higher standard in RWM (%)` = mean(`Pupils reaching the higher standard in RWM (%)`)) %>% as.data.table()

ks2_results_london_out

# Join the three datasets together
ks2_results_all <- rbindlist(list(ks2_results_lam, ks2_results_london_out, ks2_results_eng))

ks2_results_all

# Change columns to numeric
ks2_results_all[, `:=`(`Pupils reaching the expected standard in RWM (%)` = as.numeric(`Pupils reaching the expected standard in RWM (%)`),
                       `Pupils reaching the higher standard in RWM (%)` = as.numeric(`Pupils reaching the higher standard in RWM (%)`)
                       )]

# Make Area name an ordered factor with order Lambeth, London, England
ks2_results_all$`Area name` <- ordered(ks2_results_all$`Area name`, levels = c("Lambeth", "London", "England"))

# Make a bar chart of yth_spnd_all_out using ggplot
ks2_results_graph <- ggplot(ks2_results_all, aes(x = `Area name`, y = `Pupils reaching the expected standard in RWM (%)`, fill = `Area name`, 
                        text= paste("Area: ", `Area name`, "<br>",
                        "Pupils reaching the expected standard in RWM (%) (%): ", `Pupils reaching the expected standard in RWM (%)`, sep = ""))) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Pupils reaching the expected standard in RWM (%)")
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #labs(title = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
 #  subtitle = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
  # caption = "Source: Department for Education",
   #  x = "Area",
  # y = "Percentage of children")

ks2_results_graph

# KS4 results --------------------------------------------------------------------
ks4_results = fread(paste(c(dir_stub, "../data/Education/2022/Schools/ks4/data/2122_la_data.csv"), collapse = ''))

names(ks4_results)[1:20]

# filter to time_period == 202122
ks4_results <- ks4_results[time_period == 202122 | time_period == "202122"]

table(ks4_results$school_characteristic)
table(ks4_results$characteristic_gender)

# Filter characteristic_gender to "Total"
ks4_results <- ks4_results[characteristic_gender == "Total"]

ks4_results

# England
ks4_results_eng <- ks4_results[geographic_level == "National" & country_name == "England"]
head(ks4_results_eng)

ks4_results_eng$`Area name` = "England"


# Keep only columns Area name, avg_att8
ks4_results_eng <- ks4_results_eng[,c("Area name", "avg_att8")]


# rename pt_rwm_met_expected_standard to "Pupils reaching the expected standard in RWM (%)"

names(ks4_results_eng) <- c("Area name", "Average attainment 8 score")



# Lambeth
# Filter to la_name == "Lambeth"
ks4_results_lam <- ks4_results[la_name == "Lambeth"]

ks4_results_lam$`Area name` = ifelse(ks4_results_lam$`la_name` == "Lambeth", "Lambeth", ks4_results_lam$`la_name`)


ks4_results_lam <- ks4_results_lam[,c("Area name", "avg_att8")]


# rename pt_rwm_met_expected_standard to "Pupils reaching the expected standard in RWM (%)"

names(ks4_results_lam) <- c("Area name", "Average attainment 8 score")
ks4_results_lam

##

table(ks4_results$geographic_level)

# Filter ks4_results to London and rename to ks4_results_london
ks4_results_london <- ks4_results[region_name == "London" & geographic_level == "Regional"]

ks4_results_london$`Area name` = "London"

# filter Gender to "Total"
ks4_results_london <- ks4_results_london[,c("Area name", "avg_att8")]


# rename pt_rwm_met_expected_standard to "Pupils reaching the expected standard in RWM (%)"

names(ks4_results_london) <- c("Area name", "Average attainment 8 score")

# convert average_attainment_8_score to numeric
ks4_results_london$`Average attainment 8 score` <- as.numeric(ks4_results_london$`Average attainment 8 score`)

ks4_results_london <- ks4_results_london %>% as.data.table()

#ks4_results_london

# Find the average of all variables for each Area name for ks4_results_london
#ks4_results_london_out <- ks4_results_london %>%
#  group_by(`Area name`) %>%
#  summarise(`Average attainment 8 score` = mean(`Average attainment 8 score`)) %>% as.data.table()

#ks4_results_london_out

# Join the three datasets together
ks4_results_all <- rbindlist(list(ks4_results_lam, ks4_results_london, ks4_results_eng))

# Make Area name an ordered factor with order Lambeth, London, England
ks4_results_all$`Area name` <- ordered(ks4_results_all$`Area name`, levels = c("Lambeth", "London", "England"))

# Change columns to numeric
ks4_results_all[, `:=`(`Average attainment 8 score` = as.numeric(`Average attainment 8 score`))]

# Make a bar chart of yth_spnd_all_out using ggplot
ks4_results_graph <- ggplot(ks4_results_all, aes(x = `Area name`, y = `Average attainment 8 score`, fill = `Area name`, 
                        text= paste("Area: ", `Area name`, "<br>",
                        "Average attainment 8 score: ", `Average attainment 8 score`, sep = ""))) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Average attainment 8 score")
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #labs(title = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
 #  subtitle = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
  # caption = "Source: Department for Education",
   #  x = "Area",
  # y = "Percentage of children")
ks4_results_graph

# KS4 results ethnicity --------------------------------------------------------------------
ks4_results_eth = fread(paste(c(dir_stub, "../data/Education/2022/Schools/ks4/data/2122_lachar_data.csv"), collapse = ''))

names(ks4_results_eth)[1:20]

# filter to time_period == 202122
ks4_results_eth <- ks4_results_eth[time_period == 202122 | time_period == "202122"]

table(ks4_results_eth$school_characteristic)
table(ks4_results_eth$characteristic_gender)
table(ks4_results_eth$characteristic_ethnic_major)

# Filter characteristic_gender to "Total"
ks4_results_eth <- ks4_results_eth[characteristic_gender == "Total"]
ks4_results_eth <- ks4_results_eth[characteristic_ethnic_major != "Total" &
                                   characteristic_ethnic_major != "Unclassified"]


ks4_results_eth

# England
ks4_results_eth_eng <- ks4_results_eth[geographic_level == "National" & country_name == "England"]
head(ks4_results_eth_eng)

ks4_results_eth_eng$`Area name` = "England"


# Keep only columns Area name, avg_att8
ks4_results_eth_eng <- ks4_results_eth_eng[,c("Area name", "characteristic_ethnic_major", "avg_att8")]


# rename pt_rwm_met_expected_standard to "Pupils reaching the expected standard in RWM (%)"

names(ks4_results_eth_eng) <- c("Area name", "Ethnicity", "Average attainment 8 score")



# Lambeth
# Filter to la_name == "Lambeth"
ks4_results_eth_lam <- ks4_results_eth[la_name == "Lambeth"]

ks4_results_eth_lam$`Area name` = ifelse(ks4_results_eth_lam$`la_name` == "Lambeth", "Lambeth", ks4_results_eth_lam$`la_name`)


ks4_results_eth_lam <- ks4_results_eth_lam[,c("Area name", "characteristic_ethnic_major", "avg_att8")]


# rename pt_rwm_met_expected_standard to "Pupils reaching the expected standard in RWM (%)"

names(ks4_results_eth_lam) <- c("Area name", "Ethnicity", "Average attainment 8 score")
ks4_results_eth_lam

##

table(ks4_results_eth$geographic_level)

# Filter ks4_results_eth to London and rename to ks4_results_eth_london
ks4_results_eth_london <- ks4_results_eth[(region_name == "London" & geographic_level == "Regional")]

ks4_results_eth_london$`Area name` = "London"

# filter Gender to "Total"
ks4_results_eth_london <- ks4_results_eth_london[,c("Area name", "characteristic_ethnic_major", "avg_att8")]


# rename pt_rwm_met_expected_standard to "Pupils reaching the expected standard in RWM (%)"

names(ks4_results_eth_london) <- c("Area name", "Ethnicity", "Average attainment 8 score")

# convert average_attainment_8_score to numeric
ks4_results_eth_london$`Average attainment 8 score` <- as.numeric(ks4_results_eth_london$`Average attainment 8 score`)

ks4_results_eth_london <- ks4_results_eth_london %>% as.data.table()

#ks4_results_eth_london

# Find the average of all variables for each Area name for ks4_results_eth_london
#ks4_results_eth_london_out <- ks4_results_eth_london %>%
#  group_by(`Area name`) %>%
#  summarise(`Average attainment 8 score` = mean(`Average attainment 8 score`)) %>% as.data.table()

#ks4_results_eth_london_out

# Join the three datasets together
ks4_results_eth_all <- rbindlist(list(ks4_results_eth_lam, ks4_results_eth_london, ks4_results_eth_eng))

# Make Area name an ordered factor with order Lambeth, London, England
ks4_results_eth_all$`Area name` <- ordered(ks4_results_eth_all$`Area name`, levels = c("Lambeth", "London", "England"))

# Change columns to numeric
ks4_results_eth_all[, `:=`(`Average attainment 8 score` = as.numeric(`Average attainment 8 score`))]

summary(ks4_results_eth_all)

# Make a bar chart of yth_spnd_all_out using ggplot
ks4_results_eth_graph <- ggplot(ks4_results_eth_all, aes(x = Ethnicity, y = `Average attainment 8 score`, fill = `Area name`, 
                                                 text= paste("Area: ", `Area name`, "<br>",
                                                             "Average attainment 8 score: ", `Average attainment 8 score`, sep = ""))) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Average attainment 8 score")
#theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#labs(title = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
#  subtitle = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
# caption = "Source: Department for Education",
#  x = "Area",
# y = "Percentage of children")
ks4_results_eth_graph

ggplotly(ks4_results_eth_graph)

# Progression to higher education -----------------------------------------

# Read in the data
progression_to_he = fread(paste(c(dir_stub, "../data/Education/2022/Progression/16-18-dest/data/1618_dm_ud_202021_la_prov.csv"), collapse = ''))

#names(progression_to_he)

# Filter to time_period == 202021
#progression_to_he <- progression_to_he[time_period == 202021 | time_period == "202021"]


#
#prog_1618_df = prog_1618.copy()[
 #       (prog_1618["geographic_level"] == "Local authority") &\
  #      (prog_1618['cohort_level_group'] == "Total") &\
 #     (prog_1618['characteristic_group'] == "Total") &\
  #      (prog_1618["data_type"] == "Percentage") &\
   #     (prog_1618["region_name"] == "London") &\
  #    #(prog_1618["institution_group"] == "All schools") &\
   #     ((prog_1618["time_period"] == 201920) | (prog_1618["time_period"] == "201920"))
    #    ]

# filter progression_to_he to cohort level group == "Total", characteristic_group == "Total", data_type == "Percentage", time_period == 202021
progression_to_he <- progression_to_he[cohort_level_group == "Total" & characteristic_group == "Total" &
 data_type == "Percentage" & time_period == 202021 & institution_group == "State-funded mainstream schools & colleges"]

# keep columns geographic_level, region_name, la_name, overall, country_name
progression_to_he <- progression_to_he[,c("geographic_level", "region_name", "la_name", "overall", "country_name")]

# make overall column numeric
progression_to_he$overall <- as.numeric(progression_to_he$overall)


# create london data table - geographic level = "Regional", region_name == "London"
progression_to_he_london <- progression_to_he[geographic_level == "Regional" & region_name == "London"]

# Area name = "London"
progression_to_he_london$`Area name` = "London"

# get average for columns overall (averages over )
progression_to_he_london <- progression_to_he_london %>% as.data.table() %>% 
  group_by(`Area name`) %>% 
  summarise(overall = mean(overall)) %>% as.data.table()

# create lambeth data table - geographic level = "Local authority", la_name == "Lambeth"
progression_to_he_lam <- progression_to_he[geographic_level == "Local authority" & la_name == "Lambeth"]

# Area name = "Lambeth"
progression_to_he_lam$`Area name` = "Lambeth"

# get average for columns overall
progression_to_he_lam <- progression_to_he_lam %>% as.data.table() %>%
  group_by(`Area name`) %>%
  summarise(overall = mean(overall)) %>% as.data.table()


# create england data table - geographic level = "National", region_name == "England"
table(progression_to_he$geographic_level)

# No national data here, loading separate file
progression_to_he_eng = fread(paste(c(dir_stub, "../data/Education/2022/Progression/16-18-dest/data/1618_dm_ud_202021_nat_prov.csv"),
 collapse = ''))

#names(progression_to_he_eng)

#table(progression_to_he_eng$version_no)

# rename column version to version_no
names(progression_to_he_eng)[names(progression_to_he_eng) == "version"] <- "version_no"

# filter progression_to_he to cohort level group == "Total", characteristic_group == "Total", data_type == "Percentage", time_period == 202021
progression_to_he_eng <- progression_to_he_eng[
 cohort_level_group == "Total" &
 characteristic_group == "Total" &
 data_type == "Percentage" &
 (time_period == 202021 | time_period == "202021")&
 institution_group == "State-funded mainstream schools & colleges" 
  ]

progression_to_he_eng

#progression_to_he_eng <- progression_to_he_eng[geographic_level == "National" & country_name == "England"]

names(progression_to_he_eng)

# Area name = "England"
progression_to_he_eng$`Area name` = "England"


# get average for columns overall
progression_to_he_eng <- progression_to_he_eng %>% as.data.table() %>%
  group_by(`Area name`) %>%
  summarise(overall = mean(overall)) %>% as.data.table()

progression_to_he_eng

# Combine lambeth, london and england data tables, keep only Area name and overall columns
progression_to_he_all <- rbindlist(list(progression_to_he_lam, progression_to_he_london, progression_to_he_eng))
progression_to_he_all

# Make Area name an ordered factor with order Lambeth, London, England
progression_to_he_all$`Area name` <- ordered(progression_to_he_all$`Area name`, levels = c("Lambeth", "London", "England"))

# Rename overall to  16-18 year olds progressed to education, apprenticeship or employment (%)
names(progression_to_he_all)[names(progression_to_he_all) == "overall"] <- "16-18 year olds progressed to education, apprenticeship or employment (%)"

# Keep only Area name and overall columns
progression_to_he_all <- progression_to_he_all[,c("Area name", "16-18 year olds progressed to education, apprenticeship or employment (%)")]

# round overall to 1 decimal place
progression_to_he_all$`16-18 year olds progressed to education, apprenticeship or employment (%)` <-
 round(progression_to_he_all$`16-18 year olds progressed to education, apprenticeship or employment (%)`, 1)
#progression_to_he_all

# Make a bar chart of progression_to_he_all using ggplot
progression_to_he_graph = ggplot(progression_to_he_all, aes(x = `Area name`,
y = `16-18 year olds progressed to education, apprenticeship or employment (%)`,
fill=`Area name`, 
                        text= paste("Area: ", `Area name`, "<br>",
                        "16-18 year olds progressed to education, apprenticeship or employment (%): ", `16-18 year olds progressed to education, apprenticeship or employment (%)`, sep = ""))) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("16-18 year olds progressed\nto education, apprenticeship or employment (%)")

progression_to_he_graph

# NEET results --------------------------------------------------------------------
neet_results = fread(paste(c(dir_stub, "../data/Education/2022/Schools/neets_by_la/data/ud_neet_characteristics.csv"), collapse = ''))


# filter to time_period == 202122
neet_results <- neet_results[time_period == 2022 | time_period == "2022"]

# Filter characteristic_gender to "Total"
neet_results <- neet_results[Characteristic == "Total"]
neet_results <- neet_results[Age == "16-17"]

neet_results

# England
neet_results_eng <- neet_results[geographic_level == "National" & country_name == "England"]
head(neet_results_eng)

neet_results_eng$`Area name` = "England"


# Keep only columns Area name, NEETNKprop
neet_results_eng <- neet_results_eng[,c("Area name", "NEETNKprop")]


# rename pt_rwm_met_expected_standard to "Pupils reaching the expected standard in RWM (%)"

names(neet_results_eng) <- c("Area name", "NEET or not known (%)")



# Lambeth
# Filter to la_name == "Lambeth"
neet_results_lam <- neet_results[la_name == "Lambeth"]

neet_results_lam$`Area name` = ifelse(neet_results_lam$`la_name` == "Lambeth", "Lambeth", neet_results_lam$`la_name`)


neet_results_lam <- neet_results_lam[,c("Area name", "NEETNKprop")]


# rename pt_rwm_met_expected_standard to "Pupils reaching the expected standard in RWM (%)"

names(neet_results_lam) <- c("Area name", "NEET or not known (%)")
neet_results_lam

##

table(neet_results$geographic_level)

# Filter neet_results to London and rename to neet_results_london
neet_results_london <- neet_results[region_name == "London" & geographic_level == "Regional"]

neet_results_london$`Area name` = "London"

# filter Gender to "Total"
neet_results_london <- neet_results_london[,c("Area name", "NEETNKprop")]


# rename pt_rwm_met_expected_standard to "Pupils reaching the expected standard in RWM (%)"

names(neet_results_london) <- c("Area name", "NEET or not known (%)")

# convert average_attainment_8_score to numeric
neet_results_london$`NEET or not known (%)` <- as.numeric(neet_results_london$`NEET or not known (%)`)

neet_results_london <- neet_results_london %>% as.data.table()

#neet_results_london

# Find the average of all variables for each Area name for neet_results_london
#neet_results_london_out <- neet_results_london %>%
#  group_by(`Area name`) %>%
#  summarise(`NEET or not known (%)` = mean(`NEET or not known (%)`)) %>% as.data.table()

#neet_results_london_out

# Join the three datasets together
neet_results_all <- rbindlist(list(neet_results_lam, neet_results_london, neet_results_eng))

# Make Area name an ordered factor with order Lambeth, London, England
neet_results_all$`Area name` <- ordered(neet_results_all$`Area name`, levels = c("Lambeth", "London", "England"))

# Change columns to numeric
neet_results_all[, `:=`(`NEET or not known (%)` = as.numeric(`NEET or not known (%)`))]

# Make a bar chart of yth_spnd_all_out using ggplot
neet_results_graph <- ggplot(neet_results_all, aes(x = `Area name`, y = `NEET or not known (%)`, fill = `Area name`,
                                                 text= paste("Area: ", `Area name`, "<br>",
                                                             "NEET or not known (%): ", `NEET or not known (%)`, sep = ""))) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("NEET or not known (%)")
#theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#labs(title = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
#  subtitle = "Percentage of children in Lambeth with a good or outstanding Ofsted rating",
# caption = "Source: Department for Education",
#  x = "Area",
# y = "Percentage of children")
neet_results_graph

# Children in need ---------------------------------------------------
#read in children in need data
cin <- fread(paste(c(dir_stub, "../data/Education/2022/Social care/CLA/data/b1_children_in_need_2013_to_2022.csv"),
 collapse = ''))

# filter cin to year == 2022
cin <- cin[time_period == 2022]

# create a new table, filter to lambeth
cin_lam <- cin[la_name == "Lambeth"]

# create a new table, filter region_name to london and geographic_level to Regional

cin_london <- cin[region_name == "London" & geographic_level == "Regional"]

# create a new table, filter country_name to england and geographic_level to National

cin_eng <- cin[country_name == "England" & geographic_level == "National"]
# combine lambeth, london and england data tables
cin_all <- rbindlist(list(cin_lam, cin_london, cin_eng))
cin_all    

# Create a new column, Area name, with values Lambeth, London, England
cin_all$`Area name` <- c("Lambeth", "London", "England")

# Make Area name an ordered factor with order Lambeth, London, England
cin_all$`Area name` <- ordered(cin_all$`Area name`, levels = c("Lambeth", "London", "England"))

# keep only columns Area name and Started_child_rate
cin_all <- cin_all[,c("Area name", "Started_child_rate")]

cin_all[, Started_child_rate := as.numeric(Started_child_rate)]

# rename column Started_child_rate to Rate of children becoming Children in Need (per 10,000 children)
names(cin_all)[names(cin_all) == "Started_child_rate"] <- "Rate of children becoming Children in Need (per 10,000 children)"

cin_all

# make a bar chart of cin_all using ggplot
cin_graph = ggplot(cin_all, aes(x = `Area name`,
y = `Rate of children becoming Children in Need (per 10,000 children)`,
fill=`Area name`, 
                        text= paste("Area: ", `Area name`, "<br>",
                        "Rate of children becoming children in need (per 10,000 children): ", `Rate of children becoming Children in Need (per 10,000 children)`, sep = ""))) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Rate of children becoming children in need (per 10,000 children)")

cin_graph


# School disadvantage status -------------------------------------------------------------

# Use 2122_lachar_data.csv or 2122_lad_sl_fsm_dis_data.csv for free school meal eligibility, disadvantaged status

# Can use 2122_lad_sl_fsm_dis_data.csv to calc differences in performance between those on fsm and those disadvantaged

ks4_dis <- fread(paste(c(dir_stub, "../data/Education/2022/Schools/ks4/data/2122_lad_sl_fsm_dis_data.csv"), collapse = ''))

ks4_dis

names(ks4_dis)[1:20]

# filter to latest time_period
ks4_dis <- ks4_dis[time_period == 202122]

table(ks4_dis$geographic_level)

# filter to lad_name == Lambeth and geographic_level == Local authority OR region_name == London and geographic_level == Regional OR country_name == England and geographic_level == National
ks4_dis_lam <- ks4_dis[lad_name == "Lambeth" & geographic_level == "Local authority district"]
ks4_dis_lam$`Area name` <- "Lambeth"

ks4_dis_london <- ks4_dis[(region_name == "Inner London" | region_name == "Outer London") &
                           geographic_level == "Regional"]
ks4_dis_london$`Area name` <- "London"

ks4_dis_eng <- ks4_dis[country_name == "England" & geographic_level == "National"]
ks4_dis_eng$`Area name` <- "England"

#ks4_dis_lam

#ks4_dis_london

# combine lambeth, london and england data tables
ks4_dis_all <- rbindlist(list(ks4_dis_lam, ks4_dis_london, ks4_dis_eng))         
ks4_dis_all

# Make Area name an ordered factor with order Lambeth, London, England
ks4_dis_all$`Area name` <- ordered(ks4_dis_all$`Area name`, levels = c("Lambeth", "London", "England"))

# keep only columns Area name, characteristic_disadvantage, characteristic_fsm, avg_att8
ks4_dis_all <- ks4_dis_all[,c("Area name", "characteristic_disadvantage", "characteristic_fsm", "avg_att8")]

# rename column avg_att8 to Average Attainment 8 score
names(ks4_dis_all)[names(ks4_dis_all) == "avg_att8"] <- "Average Attainment 8 score"

# rename column characteristic_disadvantage to Disadvantaged
names(ks4_dis_all)[names(ks4_dis_all) == "characteristic_disadvantage"] <- "Disadvantaged"

# rename column characteristic_fsm to Free school meal eligible
names(ks4_dis_all)[names(ks4_dis_all) == "characteristic_fsm"] <- "Free school meal eligible"

table(ks4_dis_all$`Disadvantaged`)

table(ks4_dis_all$`Free school meal eligible`)

ks4_dis_all

# Filter out rows with 'Disadvantaged all other' or 'FSM all other'
ks4_dis_all <- ks4_dis_all[!(Disadvantaged == "Disadvantaged all other" | `Free school meal eligible` == "FSM all other"),]
ks4_dis_all

# Get average of Average Attainment 8 score for each Area name, Disadvantaged, and Free school meal eligible
ks4_dis_all_g <- ks4_dis_all[, list(`Average Attainment 8 score` = mean(`Average Attainment 8 score`)),
                 by = c("Area name", "Disadvantaged", "Free school meal eligible")]

ks4_dis_all_g

# dcast ks4_dis_all_g to wide format
ks4_dis_all_g_wide <- dcast(ks4_dis_all_g, `Area name` ~ Disadvantaged + `Free school meal eligible`, value.var = "Average Attainment 8 score")
#ks4_dis_all_g_wide

# Remove string 'Total' and '_' from column names
names(ks4_dis_all_g_wide) <- gsub("_Total", "", names(ks4_dis_all_g_wide))
names(ks4_dis_all_g_wide) <- gsub("Total_", "", names(ks4_dis_all_g_wide))
#ks4_dis_all_g_wide

ks4_dis_all_g_wide[,`:=`(`Disadvantaged gap` = Total - Disadvantaged, `FSM gap` = Total - `FSM`)]

# make a bar chart of ks4_dis_all_g_wide using ggplot for disadvantaged pupils
ks4_dis_all_g_wide_graph_dis = ggplot(ks4_dis_all_g_wide, aes(x = `Area name`,
y = `Disadvantaged gap`,
fill=`Area name`, 
                        text= paste("Area: ", `Area name`, "<br>",
                        "Gap in KS4 attainment 8 scores with disadvantaged pupils: ", `Disadvantaged gap`, sep = ""))) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Disadvantaged gap\n(average KS4 attainment 8 score)")

ks4_dis_all_g_wide_graph_dis

# make a bar chart of ks4_dis_all_g_wide using ggplot for FSM pupils
ks4_dis_all_g_wide_graph_fsm = ggplot(ks4_dis_all_g_wide, aes(x = `Area name`,
y = `FSM gap`,
fill=`Area name`, 
                        text= paste("Area: ", `Area name`, "<br>",
                        "Gap in KS4 attainment 8 scores with Free School Meal eligible pupils: ", `FSM gap`, sep = ""))) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_discrete(type=lambeth_palette_graph) +
  theme_lam() +
  theme(axis.title.x = element_blank()) +
  ylab("Free school meal-eligible gap\n(average KS4 attainment 8 score)")

ks4_dis_all_g_wide_graph_fsm


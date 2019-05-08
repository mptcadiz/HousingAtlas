# Library -----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(sf)
library(scales)
library(plotly)
library(Hmisc)
library(extrafont)
library(gridExtra)
#library(lettercase)
library(ggiraph)
library(geojsonio)
#library(rmapshaper)
library(sp)
loadfonts()
options(scipen=999)


# Prepare ggplot themes ---------------------------------------------------

## These change the ggplot themes to more directly match CRPD's style

theme_bar <- theme_bw() +
  theme(text = element_text(family = "Helvetica Neue,Helvetica,Arial,sans-serif", size = 16),
        panel.grid.major = element_line(color = "grey70", size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_blank())

theme_line <- theme_bw() +
  theme(legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey70", size = 0.1),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        text = element_text(family = "Helvetica Neue,Helvetica,Arial,sans-serif", size = 16),
        plot.caption = element_text(family = "Helvetica Neue,Helvetica,Arial,sans-serif", size = 13, hjust = 0.5, face = "italic"))

theme_sf <- theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16, family = "Helvetica Neue,Helvetica,Arial,sans-serif"),
        legend.title = element_blank())

# Objects ---------------------------------------------------------

# Objects - Shapefiles ----------------------------------------------------
mn_counties <- st_read("Shapefiles/MNCounties_MNDOT.shp", quiet = TRUE) %>%
  rename(countyFIPS = FIPS_CODE) %>%
  st_simplify(dTolerance = 1000) %>%
  mutate(
    COUNTY_NAM = ifelse(COUNTY_NAM == "Saint Louis", "St Louis", as.character(COUNTY_NAM)),
    COUNTY_NAM = ifelse(COUNTY_NAM == "Lake of the Woods", "Lake Of The Woods", as.character(COUNTY_NAM))
  ) %>%
  rename(countyName = COUNTY_NAM)

mn_school_districts <- st_read("Shapefiles/school_district_boundaries.shp", quiet = TRUE) %>%
  select(UNI_TYP, UNI_MAJ,UNI_NAM,geometry) %>%
  rename(districtType=UNI_TYP,
         districtNumber=UNI_MAJ,
         districtName=UNI_NAM) %>%
  st_simplify(dTolerance = 1000) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName = ifelse(districtName=="Minneapolis-St. Paul Int'l Airport", "Minneapolis-St. Paul International Airport", as.character(districtName))
  ) %>%
  mutate(districtName = str_to_title(districtName))
#  ms_simplify()


#Objects - Housing --------------------------------------------------------
med.home.val.1990_2010 <- read_csv("Housing/Median Home Value/med_home_val_1990_2010.csv") %>%
  full_join(mn_counties, med.home.val.1990_2010, by = c("countyName")) %>%
  mutate(bins = cut(homeValue,
                    breaks = c(0, 59999,999999,139999,179999,219999,259999,300000),
                    labels = c("$20,000 - $59,999", "$60,000 - $999,999", "$100,000 - $139,999", "$140,000 - $179,999", "$180,000 - $219,999", "$220,000 - $259,999", "$260,000 - $300,000")))

med.year.built.1990_2017 <- read_csv("Housing/Median Year Built/med_year_built_1990_2017.csv") %>%
  full_join(mn_counties, med.year.built.1990_2017, by = c("countyName")) %>%
  mutate(bins = cut(yearBuilt,
                    breaks = c(0, 1946,1954,1962,1970,1978,1986,1994),
                    labels = c("1939 - 1946", "1947 - 1954", "1955 - 1962", "1963 - 1970", "1971 - 1978", "1979 - 1986", "1987 - 1994")))

tidymortgage.status.1990_2010 <- read_csv("Housing/Mortgage Status/mortgage_status_1990_2010.csv") %>%
  full_join(mn_counties, tidymortgage.status.1990_2010, by = c("countyFIPS")) %>%
  rename(countyName=countyName.x) %>%
  select(-countyName.y) %>%
  mutate(bins = cut(percentFree,
                    breaks = c(0, 0.19, 0.39, 0.59, 0.79, 1),
                    labels = c("0 - 19%", "20% - 39%", "40% - 59%", "60 - 79%", "80% - 100 %")))


county.list <- med.home.val.1990_2010 %>%
  select(countyName) %>%
  distinct(countyName)

# Objects - Education --------------------------------------------------------

# Objects - Enrollment (total) ----------------------------------------------------
enrolled.2018_2000.tidy <- read_csv("Education/Enrollment/enrolled_total_2000_2018.csv")

enrolled.2018_2000.map <- enrolled.2018_2000.tidy %>%
  full_join(mn_school_districts,  enrolled.2018_2000.tidy, by = c("districtNumber","districtType")) %>%
  drop_na(districtName.y) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(totalStudents=as.numeric(as.character(totalStudents)),
         year=as.numeric(as.character(year))
         ) %>%
  mutate(bins = cut(totalStudents,
                    breaks = c(0, 499, 999, 2499, 4999, 9999, 29999, 50000),
                    labels = c("1 - 499", "500 - 999", "1,000 - 2,499", "2,500 - 4,999", "5,000 - 9,999", "10,000 - 29,999", "30,000 - 50,000")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins,"1 - 499", "500 - 999", "1,000 - 2,499", "2,500 - 4,999", "5,000 - 9,999", "10,000 - 29,999", "30,000 - 50,000")
         )

enrollment.change.2008_2012 <- read_csv("Education/Enrollment/enrollment_change_2000_2018.csv")

enrollment.change.2008_2012.map <- enrollment.change.2008_2012 %>%
  full_join(mn_school_districts,  enrollment.change.2008_2012, by = c("districtNumber","districtType")) %>%
  drop_na(districtName.y) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(change=as.numeric(as.character(change)),
         year=as.numeric(as.character(year))
  ) %>%
  mutate(bins = cut(change,
                    breaks = c(-3, -0.04999999, -0.02499999, 0, 0.02499999, 0.04999999, 0.8),
                    labels = c("-300% to -5%", "-5% to -2.5%", "-2.5% to 0%", "0% to 2.5%", "2.5% to 5%", "5% to 76%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins,"-300% to -5%", "-5% to -2.5%", "-2.5% to 0%", "0% to 2.5%", "2.5% to 5%", "5% to 76%")
  )

# Objects - Enrollment (students of color) ----------------------------------------------------
enrolled.ethnicity.2000_2018.tidy <- read_csv("Education/Enrollment/enrolled_ethnicity_2000_2018.csv") %>%
  rename(districtNumber = DistrictNumber)

enrolled.ethnicity.2000_2018.map <- enrolled.ethnicity.2000_2018.tidy %>%
  full_join(mn_school_districts,  enrolled.2018_2000.tidy, by = c("districtNumber","districtType")) %>%
  drop_na(districtName.y) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(percentMinority=as.numeric(as.character(percentMinority)),
         year=as.numeric(as.character(year))
  ) %>%
  mutate(bins = cut(percentMinority,
                    breaks = c(-1, 0.04999999, 0.09999999, 0.14999999, 0.19999999, 0.24999999, 1),
                    labels = c("0% - 5%", "5% - 10%", "10% - 15%", "15% - 20%", "20% - 25%", "25% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 5%", "5% - 10%", "10% - 15%", "15% - 20%", "20% - 25%", "25% - 100%")
         )

ethnicity.district.list <- enrolled.ethnicity.2000_2018.tidy %>%
  select(districtName) %>%
  distinct(districtName)

enrollment.district.list <- enrolled.2018_2000.tidy %>%
  select(districtName) %>%
  distinct(districtName)

# Objects - Graduation and Dropout Rate (total) ----------------------------------------------------
grad.2012_2018.tidy <- read_csv("Education/Graduation and Dropout Rate/grad_rate_2012_2018.csv") %>%
  mutate(gradRate=gradRate/100)
  
grad.2012_2018.tidy.map <- grad.2012_2018.tidy %>%
  full_join(mn_school_districts,  grad.2012_2018.tidy, by = c("districtNumber","districtType")) %>%
  rename(districtName = districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(gradRate,
                    breaks = c(-1, 0.74999999, 0.84999999, 0.94999999, 1),
                    labels = c("0% - 75%", "75% - 85%", "85% - 95%", "95% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 75%", "75% - 85%", "85% - 95%", "95% - 100%")
  )
  

grad.rate.list <- grad.2012_2018.tidy %>%
  select(districtName) %>%
  distinct(districtName)

drop.2012_2018.tidy <- read_csv("Education/Graduation and Dropout Rate/drop_rate_2012_2018.csv") %>%
  mutate(dropRate=dropRate/100)

drop.2012_2018.tidy.map <- drop.2012_2018.tidy %>%
  full_join(mn_school_districts,  drop.2012_2018.tidy, by = c("districtNumber","districtType")) %>%
  rename(districtName = districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(dropRate,
                    breaks = c(-1, 0.02499999, 0.04999999, 0.07499999, 0.09999999, 1),
                    labels = c("0% - 2.5%", "2.5% - 5.5%", "5.5% - 7.5%", "7.5% - 10%", "10% to 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 2.5%", "2.5% - 5.5%", "5.5% - 7.5%", "7.5% - 10%", "10% to 100%")
  )

# Objects - Graduation and Dropout Rate (students of color) ----------------------------------------------------
grad.ethnicity.2012_2017 <- read_csv("Education/Graduation and Dropout Rate/grad_rate_ethnicity_2012_2017.csv") 

grad.ethnicity.2012_2017.map <- grad.ethnicity.2012_2017 %>%
  full_join(mn_school_districts,  grad.ethnicity.2012_2017, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(gradRate,
                    breaks = c(-1, 0.49999999, 0.74999999, 0.84999999, 1),
                    labels = c("0% - 50%", "50% - 75%", "75% - 85%", "85% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 50%", "50% - 75%", "75% - 85%", "85% - 100%")
  )

drop.ethnicity.2012_2017 <- read_csv("Education/Graduation and Dropout Rate/drop_rate_ethnicity_2012_2017.csv") 

drop.ethnicity.2012_2017.map <- drop.ethnicity.2012_2017 %>%
  full_join(mn_school_districts,  drop.ethnicity.2012_2017, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(dropRate,
                    breaks = c(-1, 0.04999999, 0.09999999, 0.19999999, 1),
                    labels = c("0% - 5%", "5% - 10%", "10% - 20%","20% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 5%", "5% - 10%", "10% - 20%","20% - 100%")
  )

grad.ethnicity.list <- grad.ethnicity.2012_2017 %>%
  select(districtName) %>%
  distinct(districtName)

grad.ethnicity.breakdown.2012_2017 <- read_csv("Education/Graduation and Dropout Rate/grad_rate_ethnicity_breakdown_2012_2017.csv") 

grad.ethnicity.breakdown.2012_2017.map <- grad.ethnicity.breakdown.2012_2017 %>%
  full_join(mn_school_districts,  grad.ethnicity.breakdown.2012_2017, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(gradRate,
                    breaks = c(-1, 0.49999999, 0.74999999, 0.84999999, 1),
                    labels = c("0% - 50%", "50% - 75%", "75% - 85%","85% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 50%", "50% - 75%", "75% - 85%","85% - 100%")
  )

grad.asian.2012_2017.map <- read_csv("Education/Graduation and Dropout Rate/grad_rate_asian_2012_2017.csv") %>%
  full_join(mn_school_districts,  grad.asian.2012_2017.map, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(gradRate,
                    breaks = c(-1, 0.84999999, 0.89999999, 0.94999999, 1),
                    labels = c("0% - 85%", "85% - 90%","90% - 95%", "95% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 85%", "85% - 90%","90% - 95%", "95% - 100%")
  )

grad.black.2012_2017.map <- read_csv("Education/Graduation and Dropout Rate/grad_rate_black_2012_2017.csv") %>%
  full_join(mn_school_districts,  grad.black.2012_2017.map, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(gradRate,
                    breaks = c(-1, 0.59999999, 0.69999999, 0.79999999, 1),
                    labels = c("0% - 60%", "60% - 70%", "70% - 80%","80% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 60%", "60% - 70%", "70% - 80%","80% - 100%")
  )

grad.hispanic.2012_2017.map <- read_csv("Education/Graduation and Dropout Rate/grad_rate_hispanic_2012_2017.csv") %>%
  full_join(mn_school_districts,  grad.hispanic.2012_2017.map, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(gradRate,
                    breaks = c(-1, 0.59999999, 0.69999999, 0.79999999, 1),
                    labels = c("0% - 60%", "60% - 70%", "70% - 80%","80% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 60%", "60% - 70%", "70% - 80%","80% - 100%")
  )

grad.native.2012_2017.map <- read_csv("Education/Graduation and Dropout Rate/grad_rate_native_2012_2017.csv") %>%
  full_join(mn_school_districts,  grad.native.2012_2017.map, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(gradRate,
                    breaks = c(-1, 0.49999999, 0.59999999, 0.74999999, 1),
                    labels = c("0% - 50%", "50% - 60%", "60% - 75%","75% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 50%", "50% - 60%", "60% - 75%","75% - 100%")
  )

grad.mixed.2012_2017.map <- read_csv("Education/Graduation and Dropout Rate/grad_rate_mixed_2012_2017.csv") %>%
  full_join(mn_school_districts,  grad.mixed.2012_2017.map, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(gradRate,
                    breaks = c(-1, 0.64999999, 0.74999999, 0.84999999, 1),
                    labels = c("0% - 65%", "65% - 75%", "75% - 85%","85% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 65%", "65% - 75%", "75% - 85%","85% - 100%")
  )

drop.ethnicity.breakdown.2012_2017 <- read_csv("Education/Graduation and Dropout Rate/drop_rate_ethnicity_breakdown_2012_2017.csv") 

drop.ethnicity.breakdown.2012_2017.map <- drop.ethnicity.breakdown.2012_2017 %>%
  full_join(mn_school_districts,  drop.ethnicity.breakdown.2012_2017, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(dropRate,
                    breaks = c(-1, 0.04999999, 0.09999999, 0.19999999, 1),
                    labels = c("0% - 5%", "5% - 10%", "10% - 20%","20% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 5%", "5% - 10%", "10% - 20%","20% - 100%")
  )

grad.ethnicity.breakdown.district.list <- grad.ethnicity.breakdown.2012_2017 %>%
  select(districtName) %>%
  distinct(districtName)

grad.ethnicity.breakdown.ethnicity.list <- grad.ethnicity.breakdown.2012_2017 %>%
  select(description) %>%
  distinct(description)

# Objects - Testing (GRAD and ACT) ----------------------------------------------------
grad.math.2009_2013_total <- read_csv("Education/GRAD scores/grad_math_2009_2013.csv") %>%
  mutate(percentPassed=percentPassed/100)

grad.math.list <- grad.math.2009_2013_total %>%
  select(districtName) %>%
  distinct(districtName)

act.scores.2008_2018 <- read_csv("Education/ACT Scores/act_scores_2008_2018.csv")

act.scores.list <- act.scores.2008_2018 %>%
  distinct(districtName)

act.takers.2008_2018 <- read_csv("Education/ACT Scores/act_percent_2008_2018.csv")

act.takers.2008_2018.map <- act.takers.2008_2018 %>%
  full_join(mn_school_districts,  act.takers.2008_2018, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(percentTakers,
                    breaks = c(-1,0.49999999, 0.74999999, 0.84999999, 0.94999999, 1.5),
                    labels = c("0 - 50%","50% - 75%", "75% - 85%", "85% - 95%", "95%+")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0 - 50%","50% - 75%", "75% - 85%", "85% - 95%", "95%+")
  )

act.takers.list <- act.takers.2008_2018 %>%
  distinct(districtName)

grad.reading.2008_2012_total <- read_csv("Education/GRAD scores/grad_reading_2008_2012.csv") %>%
  mutate(percentPassed=percentPassed/100)

grad.reading.list <- grad.reading.2008_2012_total %>%
  select(districtName) %>%
  distinct(districtName)

# Objects - Free/Reduced Lunch ----------------------------------------------------
tidy.free.red.lunch.2006_2018 <- read_csv("Education/Free Reduced Lunch/free_red_lunch_2006_2018.csv") %>%
  rename(districtNumber = DistrictNumber,
         districtType = DistrictType)

tidy.free.red.lunch.2006_2018.map <- tidy.free.red.lunch.2006_2018 %>%
  full_join(mn_school_districts,  act.takers.2008_2018, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(
    percentLunch=as.numeric(as.character(percentLunch)),
    year=as.numeric(as.character(year)),
    bins = cut(percentLunch,
                    breaks = c(-1,0.24999999, 0.34999999, 0.49999999, 0.74999999, 1),
                    labels = c("0% - 25%", "25% - 35%", "35% - 50%", "50% - 75%", "75% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 25%", "25% - 35%", "35% - 50%", "50% - 75%", "75% - 100%")
  )

# Objects - English Proficiency ----------------------------------------------------
english.2010_2019 <- read_csv("Education/English Proficiency/english_identified_2010_2019.csv")

english.2010_2019.map <- english.2010_2019 %>%
  full_join(mn_school_districts,  english.2010_2019.map, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(percentIdentified,
                    breaks = c(-1,0.02499999, 0.04999999, 0.07499999, 0.14999999, 1),
                    labels = c("0% - 2.5%", "2.5% - 5%", "5% - 7.5%", "7.5% - 15%", "15% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 2.5%", "2.5% - 5%", "5% - 7.5%", "7.5% - 15%", "15% - 100%")
  )

english.list <- english.2010_2019 %>%
  select(districtName) %>%
  distinct(districtName)

# Objects - Home Language ----------------------------------------------------
home_lang_2008_2018 <- read_csv("Education/Home Language/home_lang_2008_2018.csv")

home_lang_2008_2018.map <- home_lang_2008_2018 %>%
  full_join(mn_school_districts,  home_lang_2008_2018, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(percentEng,
                    breaks = c(-1,0.79999999, 0.93999999, 0.95999999, 0.97999999, 1),
                    labels = c("0% - 80%", "80% - 94%", "94% - 96%", "96% - 98%", "98% - 100%")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0% - 80%", "80% - 94%", "94% - 96%", "96% - 98%", "98% - 100%")
  )

home.lang.list <- home_lang_2008_2018 %>%
  select(districtName) %>%
  distinct(districtName)

# Objects - Student-Teacher Ratio ----------------------------------------------------
student.teacher.ratio.2008_2018 <- read_csv("Education/Student Teacher Ratio/student_teacher_ratio_2000_2018.csv")

student.teacher.ratio.2008_2018.map <- student.teacher.ratio.2008_2018 %>%
  full_join(mn_school_districts,  student.teacher.ratio.2008_2018, by = c("districtNumber","districtType")) %>%
  rename(districtName=districtName.y) %>%
  select(-districtName.x) %>%
  drop_na(districtName) %>%
  mutate(bins = cut(studentTeacherRatio,
                    breaks = c(-1,9.99999999, 11.999999, 14.999999, 16.999999, 23),
                    labels = c("0 - 10", "10 - 12", "12 - 15", "15 - 17", "17+")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins, "0 - 10", "10 - 12", "12 - 15", "15 - 17", "17+")
  )

teacher.district.list <- student.teacher.ratio.2008_2018 %>%
  distinct(districtName)

# Objects - Cost of Living ----------------------------------------------------
yearly.cost.2016_2018 <- read_csv("Cost of Living/yearly_cost_2016_2018.csv")

yearly.cost.2016_2018.map <- full_join(mn_counties,  yearly.cost.2016_2018, by = c("countyFIPS")) %>%
  drop_na(countyName.y) %>%
  rename(countyName=countyName.x) %>%
  select(-countyName.y) %>%
  mutate(bins = cut(yearlyCost,
                    breaks = c(20000, 34999, 44999, 54999, 64999, 115000),
                    labels = c("$20,000 - $35,000", "$35,000 - $45,000", "$45,000 - $55,000", "$55,000 - $65,000", "$65,000 - $115,000")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins,"$20,000 - $35,000", "$35,000 - $45,000", "$45,000 - $55,000", "$55,000 - $65,000", "$65,000 - $115,000")
  )

hourly.wage.2016_2018 <- read_csv("Cost of Living/hourly_wage_2016_2018.csv")

hourly.wage.2016_2018.map <- full_join(mn_counties,  hourly.wage.2016_2018, by = c("countyFIPS")) %>%
  drop_na(countyName.y) %>%
  rename(countyName=countyName.x) %>%
  select(-countyName.y) %>%
  mutate(bins = cut(hourlyWage,
                    breaks = c(7, 14.99, 19.99, 29.99, 55),
                    labels = c("$7 - $15", "$15 - $20", "$20 - $30", "$30 - $55")),
         bins = ifelse(is.na(bins), "NA", as.character(bins)),
         bins = fct_relevel(bins,"$7 - $15", "$15 - $20", "$20 - $30", "$30 - $55")
  )

ggplot(filter(hourly.wage.2016_2018.map, year==2018),aes(hourlyWage)) +
  geom_histogram()

col.county.name.list <-yearly.cost.2016_2018 %>%
  select(countyName) %>%
  distinct(countyName) %>%
  arrange(countyName)

col.county.workers.list <- c("Single","Partnered - 1 full-time worker", "Partnered - 1 full-time, 1 part-time worker","Partnered - 2 full-time workers") %>% 
  as.list()

col.county.children.list <- yearly.cost.2016_2018 %>%
  select(numberChildren) %>%
  distinct(numberChildren) %>%
  arrange(numberChildren)

# Objects - Visualization types --------------------------------------------------------
vis.list <- c("Trend lines","Data points", "Both visualizations") %>% 
  as.list()


# UI - Title Panel --------------------------------------------------------

##Setting up the theme, logo, etc....

ui <- fluidPage(

navbarPage("",
           #UI: Housing --------------------------------------------------------
           tabPanel("Housing Data",
                navbarMenu("",
                    navlistPanel("Housing",
                                 #UI: Home values by county over time --------------------------------------------------------
                                 tabPanel("Median Home Value",
                                          mainPanel(
                                            tabsetPanel(
                                              tabPanel("Median Home Value - Chart",
                                                       
                                                       selectizeInput(inputId = "home.val.county",
                                                                      label = "Choose a county",
                                                                      choices = county.list,
                                                                      multiple = TRUE),
                                                       
                                                       radioButtons(inputId="home.val.vis.list",
                                                                    label="How would you like to visualize the data?",
                                                                    choices = vis.list,
                                                                    inline=TRUE),
                                                       
                                                       ggiraphOutput("homevalcountygraph")
                                                       ),
                                              
                                              tabPanel("Median Home Value - Map",
                                                       
                                                       selectInput(inputId = "year.home.val",
                                                                   label = "Choose a year",
                                                                   choices = list(1990,2000,2010,2017),
                                                                   multiple = FALSE),
                                                       
                                                       ggiraphOutput("homevalmap")
                                              )
                                              
                                              ))),
                                 
                                 #UI: Median year built by county over time --------------------------------------------------------
                                 tabPanel("Median Year Built",
                                          mainPanel(
                                            tabsetPanel(
                                              tabPanel("Median Year Built - Chart",
                                                       selectizeInput(inputId = "year.built.county.chart",
                                                                      label = "Choose a county",
                                                                      choices = county.list,
                                                                      multiple = TRUE),
                                                       
                                                       radioButtons(inputId="year.built.vis.list",
                                                                    label="How would you like to visualize the data?",
                                                                    choices = vis.list,
                                                                    inline=TRUE),
                                                       
                                              tabPanel("Median Year Built - Map",
                                                       selectInput(inputId = "year.built.county",
                                                                      label = "Choose a year",
                                                                      choices = list(1990,2000,2010,2017),
                                                                      multiple = FALSE),
                                                       
                                                       ggiraphOutput("yearbuiltcountygraph")
                                              ),
                                              
                                                       ggiraphOutput("yearbuiltcountychart")
                                              )
                                              ))),
                                 
                                 #UI: Mortgage status --------------------------------------------------------
                                 tabPanel("Mortgage Status",
                                          mainPanel(
                                            tabsetPanel(
                                              tabPanel("Mortgage Status - Chart",
                                                       selectizeInput(inputId = "mortgage.status.county",
                                                                      label = "Choose a county",
                                                                      choices = county.list,
                                                                      multiple = TRUE),
                                                       
                                                       radioButtons(inputId="mortgage.status.vis.list",
                                                                    label="How would you like to visualize the data?",
                                                                    choices = vis.list,
                                                                    inline=TRUE),
                                                       
                                                       ggiraphOutput("mortgagestatuscountygraph")
                                              ),
                                              
                                              tabPanel("Mortgage Status - Map",
                                                       selectInput(inputId = "mortgage.status.county.map",
                                                                   label = "Choose a year",
                                                                   choices = list(1990,2000,2010),
                                                                   multiple = FALSE),
                                                       
                                                       ggiraphOutput("mortgagestatuscountymap")
                                              )
                                              )))
                                 )
                          )
                    ),
           #UI: Education --------------------------------------------------------
           tabPanel("Education Data",
                    navbarMenu("",
                               navlistPanel("Education",
                                            #UI: Enrollment --------------------------------------------------------
                                            tabPanel("Enrollment",
                                                     mainPanel(
                                                       tabsetPanel(
                                                         tabPanel("Total Number of Students Enrolled - Chart",
                                                                  
                                                                  selectizeInput(inputId = "student.enrollment.district",
                                                                                 label = "Choose a district",
                                                                                 choices = ethnicity.district.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="student.enrollment.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("studentenrollmentgraph")
                                                         ),
                                                         
                                                         tabPanel("Total Number of Students Enrolled - Map",
                                                                  selectInput(inputId = "student.enrollment.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("studentenrollmentmap")
                                                         ),
                                                         
                                                         tabPanel("Change In Student Enrollment - Map",
                                                                  selectInput(inputId = "enrollment.change.map",
                                                                              label = "Choose a year to compare to 2018",
                                                                              choices = list(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("enrollmentchangemap")
                                                         ),
                                                         
                                                          tabPanel("Percent of Students of Color Enrolled - Chart",
                                                                               
                                                                    selectizeInput(inputId = "student.ethnicity.district",
                                                                              label = "Choose a district",
                                                                              choices = ethnicity.district.list,
                                                                              multiple = TRUE),
                                                                               
                                                                    radioButtons(inputId="student.ethnicity.vis.list",
                                                                              label="How would you like to visualize the data?",
                                                                              choices = vis.list,
                                                                              inline=TRUE),
                                                                               
                                                                    ggiraphOutput("studentethnicitygraph")
                                                                      ),
                                                         
                                                         tabPanel("Percent of Students of Color Enrolled - Map",
                                                                  selectInput(inputId = "student.ethnicity.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("studentethnicitymap")
                                                         )
                                                         ))),
                                            
                                            #UI: Graduation and Dropout Rates --------------------------------------------------------
                                            tabPanel("Graduation and Dropout Rates",
                                                     mainPanel(
                                                       tabsetPanel(
                                                         tabPanel("Total Graduation Rate - Chart",
                                                                  
                                                                  selectizeInput(inputId = "grad.rate",
                                                                                 label = "Choose a district",
                                                                                 choices = grad.rate.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="grad.rate.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("gradrategraph")
                                                         ),
                                                         
                                                         tabPanel("Total Graduation Rate - Map",
                                                                  selectInput(inputId = "grad.rate.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2012,2013,2014,2015,2016,2017),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("gradratemap")
                                                         ),
                                                         
                                                         tabPanel("Total Dropout Rate - Chart",
                                                                  
                                                                  selectizeInput(inputId = "drop.rate",
                                                                                 label = "Choose a district",
                                                                                 choices = grad.rate.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="drop.rate.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("droprategraph")
                                                         ),
                                                         
                                                         tabPanel("Total Dropout Rate - Map",
                                                                  selectInput(inputId = "drop.rate.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2012,2013,2014,2015,2016,2017),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("dropratemap")
                                                         )
                                                         ))),
                                            #UI: Graduation and Dropout Rates - Students of Color --------------------------------------------------------
                                            tabPanel("Graduation and Dropout Rates - Students of Color",
                                                     mainPanel(
                                                       tabsetPanel(
                                                         tabPanel("Students of Color Graduation Rate - Chart",
                                                                  
                                                                  selectizeInput(inputId = "ethnicity.grad.rate",
                                                                                 label = "Choose a district",
                                                                                 choices = grad.ethnicity.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="ethnicity.grad.rate.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("ethnicitygradrategraph")
                                                         ),
                                                         
                                                         tabPanel("Students of Color Graduation Rate - Map",
                                                                  selectInput(inputId = "ethnicity.grad.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2012,2013,2014,2015,2016,2017),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("ethnicitygradratemap")
                                                         ),
                                                         
                                                         tabPanel("Students of Color Dropout Rate - Chart",
                                                                  
                                                                  selectizeInput(inputId = "ethnicity.drop.rate",
                                                                                 label = "Choose a district",
                                                                                 choices = grad.ethnicity.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="ethnicity.drop.rate.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("ethnicitydroprategraph")
                                                         ),
                                                         
                                                         tabPanel("Students of Color Dropout Rate - Map",
                                                                  selectInput(inputId = "ethnicity.drop.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2012,2013,2014,2015,2016,2017),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("ethnicitydropratemap")
                                                         ),
                                                         
                                                         tabPanel("Students of Color Graduation Rate - By Ethnicity - Chart",
                                                                  
                                                                  selectizeInput(inputId = "ethnicity.breakdown.grad.rate.district",
                                                                                 label = "Choose a district",
                                                                                 choices = grad.ethnicity.breakdown.district.list,
                                                                                 multiple = FALSE),
                                                                  
                                                                  selectizeInput(inputId = "ethnicity.breakdown.grad.rate.ethnicity",
                                                                                 label = "Choose an ethnicity",
                                                                                 choices = grad.ethnicity.breakdown.ethnicity.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="ethnicity.breakdown.grad.rate.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("ethnicitybreakdowngradrategraph")
                                                         ),
                                                         
                                                         tabPanel("Students of Color Graduation Rate - By Ethnicity - Map",
                                                                  
                                                                  selectizeInput(inputId = "ethnicity.breakdown.grad.ethnicity.map",
                                                                                 label = "Choose an ethnicity",
                                                                                 choices = grad.ethnicity.breakdown.ethnicity.list,
                                                                                 multiple = FALSE),
                                                                  
                                                                  selectInput(inputId = "ethnicity.breakdown.grad.year.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2012,2013,2014,2015,2016,2017),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("ethnicitybreakdowngradmap")
                                                                  
                                                         ),
                                                         
                                                         tabPanel("Students of Color Dropout Rate - By Ethnicity - Chart",
                                                                  
                                                                  selectizeInput(inputId = "ethnicity.breakdown.drop.rate.district",
                                                                                 label = "Choose a district",
                                                                                 choices = grad.ethnicity.breakdown.district.list,
                                                                                 multiple = FALSE),
                                                                  
                                                                  selectizeInput(inputId = "ethnicity.breakdown.drop.rate.ethnicity",
                                                                                 label = "Choose an ethnicity",
                                                                                 choices = grad.ethnicity.breakdown.ethnicity.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="ethnicity.breakdown.drop.rate.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("ethnicitybreakdowndroprategraph")
                                                         ),
                                                         
                                                         tabPanel("Students of Color Dropout Rate - By Ethnicity - Map",
                                                                  
                                                                  selectizeInput(inputId = "ethnicity.breakdown.drop.ethnicity.map",
                                                                                 label = "Choose an ethnicity",
                                                                                 choices = grad.ethnicity.breakdown.ethnicity.list,
                                                                                 multiple = FALSE),
                                                                  
                                                                  selectInput(inputId = "ethnicity.breakdown.drop.year.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2012,2013,2014,2015,2016,2017),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("ethnicitybreakdowndropmap")
                                                                  
                                                         )
                                                         
                                                         
                                                       ))),
                                            
                                            #UI: Test Scores --------------------------------------------------------
                                            tabPanel("Test Scores",
                                                     mainPanel(
                                                       tabsetPanel(
                                                         tabPanel("GRAD: Math Average Score and Percent Takers - Chart",
                                                                  
                                                                  #UI: Average GRAD: Math scores  --------------------------------------------------------
                                                                  selectizeInput(inputId = "grad.math.score",
                                                                                 label = "Choose a district",
                                                                                 choices = grad.math.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="grad.math.score.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("gradmathscoregraph"),
                                                                  
                                                                  #UI: Percent of GRAD: Math passers --------------------------------------------------------
                                                                  selectizeInput(inputId = "grad.math.pass",
                                                                                 label = "Choose a district",
                                                                                 choices = grad.math.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="grad.math.pass.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("gradmathpassgraph")
                                                         ),
                                                         
                                                         tabPanel("GRAD: Reading Average Score and Percent Takers - Chart",
                                                                  
                                                                  #UI: Average GRAD: Reading scores  --------------------------------------------------------
                                                                  selectizeInput(inputId = "grad.reading.score",
                                                                                 label = "Choose a district",
                                                                                 choices = grad.reading.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="grad.reading.score.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("gradreadingscoregraph"),
                                                                  
                                                                  #UI: Percent of GRAD: Reading passers --------------------------------------------------------
                                                                  selectizeInput(inputId = "grad.reading.pass",
                                                                                 label = "Choose a district",
                                                                                 choices = grad.reading.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="grad.reading.pass.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("gradreadingpassgraph")
                                                         ),
                                                         
                                                         tabPanel("ACT Scores and Percent of Takers - Chart",
                                                                  #UI: Average ACT Score --------------------------------------------------------
                                                                  selectizeInput(inputId = "act.scores",
                                                                                 label = "Choose a district",
                                                                                 choices = act.scores.list,
                                                                                 multiple = TRUE),

                                                                  radioButtons(inputId="act.scores.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),

                                                                  ggiraphOutput("actscoresgraph"),
                                                                  
                                                                  #UI: Percent of ACT Takers --------------------------------------------------------
                                                                  selectizeInput(inputId = "act.takers",
                                                                                 label = "Choose a district",
                                                                                 choices = act.takers.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="act.takers.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("acttakersgraph")
                                                         ),
                                                         
                                                         tabPanel("Percent of ACT Takers - Map",
                                                                  selectInput(inputId = "act.takers.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("acttakersmap")
                                                         )
                                                         
                                                         
                                                       ))),
                                            
                                            #UI: Free/Reduced Lunch --------------------------------------------------------
                                            tabPanel("Free/Reduced Lunch",
                                                     mainPanel(
                                                       tabsetPanel(
                                                         tabPanel("Percent of Students with Free/Reduced Lunch - Chart",
                                                                  
                                                                  selectizeInput(inputId = "lunch.district",
                                                                                 label = "Choose a district",
                                                                                 choices = ethnicity.district.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="lunch.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("lunchgraph")
                                                         ),
                                                         
                                                         tabPanel("Percent of Students with Free/Reduced Lunch - Map",
                                                                  
                                                                  selectInput(inputId = "lunch.district.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2006, 2007, 2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("lunchmap")
                                                         )
                                                       ))),
                                            
                                            #UI: English Proficiency --------------------------------------------------------
                                            tabPanel("English Proficiency",
                                                     mainPanel(
                                                       tabsetPanel(
                                                         tabPanel("Percent of Students Not Proficient in English Identified - Chart",

                                                                 selectizeInput(inputId = "english.prof",
                                                                                 label = "Choose a district",
                                                                                 choices = english.list,
                                                                                 multiple = TRUE),

                                                                  radioButtons(inputId="english.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),

                                                                  ggiraphOutput("englishprofgraph")
                                                         ),
                                                         
                                                         tabPanel("Percent of Students Not Proficient in English Identified - Map",
                                                                  
                                                                  selectInput(inputId = "english.prof.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("englishprofmap")
                                                         )
                                                       ))
                                                     ),
                                            
                                            #UI: Home Language --------------------------------------------------------
                                            tabPanel("Home Language",
                                                     mainPanel(
                                                       tabsetPanel(
                                                         tabPanel("Percent of Students With English As Home Language - Chart",

                                                                  selectizeInput(inputId = "home.lang",
                                                                                 label = "Choose a district",
                                                                                 choices = home.lang.list,
                                                                                 multiple = TRUE),

                                                                  radioButtons(inputId="home.lang.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),

                                                                  ggiraphOutput("homelanggraph")
                                                         ),
                                                         
                                                         tabPanel("Percent of Students With English As Home Language - Map",
                                                                  
                                                                  selectInput(inputId = "home.lang.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("homelangmap")
                                                         )
                                                       ))
                                                     ),
                                            #UI: Student-Teacher Ratio --------------------------------------------------------
                                            tabPanel("Student-Teacher Ratio",
                                                     mainPanel(
                                                       tabsetPanel(
                                                         tabPanel("Number of Teachers (Full-Time Equivalent Teachers) - Chart",
                                                                  
                                                                  selectizeInput(inputId = "teacher.district",
                                                                                 label = "Choose a district",
                                                                                 choices = teacher.district.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="teacher.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("teachergraph")
                                                         ),
                                                         tabPanel("Student-Teacher Ratio - Chart",
                                                                  
                                                                  selectizeInput(inputId = "student.teacher.ratio",
                                                                                 label = "Choose a district",
                                                                                 choices = teacher.district.list,
                                                                                 multiple = TRUE),
                                                                  
                                                                  radioButtons(inputId="student.teacher.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=TRUE),
                                                                  
                                                                  ggiraphOutput("studentteachegraph")
                                                         ),
                                                         
                                                         tabPanel("Student-Teacher Ratio - Map",
                                                                  
                                                                  selectInput(inputId = "student.teacher.ratio.map",
                                                                              label = "Choose a year",
                                                                              choices = list(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018),
                                                                              multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("studentteachermap")
                                                         )
                                                       ))
                                            )
                                            
                                            
                                            

                               )
                    )
           ),
           
           #UI: Cost of Living --------------------------------------------------------
           tabPanel("Cost of Living Data",
                    navbarMenu("",
                               navlistPanel("Cost of Living",
                                             tabPanel("Yearly Cost",
                                                     mainPanel(
                                                       tabsetPanel(
                                                         tabPanel("Yearly Cost - Chart",
                                                                  
                                                                  selectizeInput(inputId = "col.county",
                                                                                 label = "Choose a county",
                                                                                 choices = col.county.name.list ,
                                                                                 multiple = TRUE),
                                                                  
                                                                  selectizeInput(inputId = "col.county.workers",
                                                                                 label = "I am:",
                                                                                 choices = col.county.workers.list,
                                                                                 multiple = FALSE),
                                                                  
                                                                  selectizeInput(inputId = "col.county.children",
                                                                                 label = "How many children?",
                                                                                 choices = col.county.children.list,
                                                                                 multiple = FALSE),
                                                                  
                                                                  radioButtons(inputId="col.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=FALSE),
                                                                  
                                                                  ggiraphOutput("colcountygraph")
                                                         ),
                                                         
                                                         tabPanel("Yearly Cost - Map",
                                                                  
                                                                  selectInput(inputId = "year.col.county",
                                                                              label = "Choose a year",
                                                                              choices = list(2016,2017,2018),
                                                                              multiple = FALSE),
                                                                  
                                                                  selectizeInput(inputId = "col.county.workers.map",
                                                                                 label = "I am:",
                                                                                 choices = col.county.workers.list,
                                                                                 multiple = FALSE),
                                                                  
                                                                  selectizeInput(inputId = "col.county.children.map",
                                                                                 label = "How many children?",
                                                                                 choices = col.county.children.list,
                                                                                 multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("colcountymap")
                                                         )
                                                         
                                                       ))),
                                            
                                            tabPanel("Hourly Wage",
                                                     mainPanel(
                                                       tabsetPanel(
                                                         tabPanel("Hourly Wage - Chart",
                                                                  
                                                                  selectizeInput(inputId = "hourly.wage.county",
                                                                                 label = "Choose a county",
                                                                                 choices = col.county.name.list ,
                                                                                 multiple = TRUE),
                                                                  
                                                                  selectizeInput(inputId = "hourly.wage.workers",
                                                                                 label = "I am:",
                                                                                 choices = col.county.workers.list,
                                                                                 multiple = FALSE),
                                                                  
                                                                  selectizeInput(inputId = "hourly.wage.children",
                                                                                 label = "How many children?",
                                                                                 choices = col.county.children.list,
                                                                                 multiple = FALSE),
                                                                  
                                                                  radioButtons(inputId="hourly.wage.vis.list",
                                                                               label="How would you like to visualize the data?",
                                                                               choices = vis.list,
                                                                               inline=FALSE),
                                                                  
                                                                  ggiraphOutput("hourlywagegraph")
                                                         ),
                                                         
                                                         tabPanel("Hourly Wage - Map",
                                                                  
                                                                  selectInput(inputId = "year.hourly.wage.county",
                                                                              label = "Choose a year",
                                                                              choices = list(2016,2017,2018),
                                                                              multiple = FALSE),
                                                                  
                                                                  selectizeInput(inputId = "hourly.wage.county.workers.map",
                                                                                 label = "I am:",
                                                                                 choices = col.county.workers.list,
                                                                                 multiple = FALSE),
                                                                  
                                                                  selectizeInput(inputId = "hourly.wage.county.children.map",
                                                                                 label = "How many children?",
                                                                                 choices = col.county.children.list,
                                                                                 multiple = FALSE),
                                                                  
                                                                  ggiraphOutput("hourlywagemap")
                                                         )
                                                         
                                                       )))

                               )
                    )
           )
           

           
           
           
           
           )
)
  



# Server
server <- function(input, output, session) {
  
# Server - Median Home Value visualization --------------------------------------------------------
  output$homevalcountygraph <- renderggiraph({
    
    home.val.county.plot <- ggplot(filter(med.home.val.1990_2010, countyName %in% input$home.val.county), aes(color=countyName, x=as.numeric(year), y=as.numeric(homeValue))) +
      scale_x_continuous(breaks=c(1990,2000,2010,2017))+
      scale_y_continuous(labels=scales::dollar)+
      labs(x="Year", y="Home Value")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="bottom")+
      scale_color_discrete(guide = guide_legend(ncol = 3))
    
    if (input$home.val.vis.list == "Data points"){
      home.val.county.plot <- home.val.county.plot + geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Median Home Value: $",comma(homeValue, digits = 0))))
    }
    else if (input$home.val.vis.list == "Trend lines"){
      home.val.county.plot <- home.val.county.plot + geom_line() + scale_fill_manual(guide = guide_legend(ncol = 3))
    }
    else if (input$home.val.vis.list == "Both visualizations"){
      home.val.county.plot <- home.val.county.plot + geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Median Home Value: $",comma(homeValue, digits = 0)))) +
        geom_line()
    }
    ggiraph(code=print(home.val.county.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })
  
  output$homevalmap <- renderggiraph({
    home.val.map.plot <- ggplot(filter(med.home.val.1990_2010, year == as.numeric(input$year.home.val))) +
      geom_sf_interactive(aes(fill = bins, tooltip = paste(countyName, "\n", "Median Home Value: $", comma(homeValue, digits = 0), sep = "")), color = "black") + 
      scale_color_manual(guide = guide_legend(ncol = 3)) +
      theme_sf +
      #theme(legend.position="bottom") + 
      scale_fill_manual(values = c("white", "#E7F5D9", "#C7EF99", "#90E033", "#5CA81F", "#076324", "black"))

      ggiraph(code = print(home.val.map.plot), selection_type = "none")
    
  })


# Server - Median Year Built Visualization --------------------------------------------------------
  output$yearbuiltcountychart <- renderggiraph({

    year.built.county.chart.plot <- ggplot(filter(med.year.built.1990_2017, countyName %in% input$year.built.county.chart), aes(color=countyName, x=as.numeric(year), y=as.numeric(yearBuilt))) +
      scale_x_continuous(breaks=c(2000,2010,2017))+
      labs(x="Year", y="Year Built")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="bottom")+
      scale_color_discrete(guide = guide_legend(ncol = 3))

    if (input$year.built.vis.list == "Data points"){
      year.built.county.chart.plot <- year.built.county.chart.plot + geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Median Year Built: ",yearBuilt)))
    }
    else if (input$year.built.vis.list == "Trend lines"){
      year.built.county.chart.plot <- year.built.county.chart.plot + geom_line()
    }
    else if (input$year.built.vis.list == "Both visualizations"){
      year.built.county.chart.plot <- year.built.county.chart.plot + geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Median Year Built: ",yearBuilt))) +
        geom_line()
    }
    ggiraph(code=print(year.built.county.chart.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })
  
  output$yearbuiltcountygraph <- renderggiraph({
    
    year.built.county.plot <- ggplot(filter(med.year.built.1990_2017, year == as.numeric(input$year.built.county))) +
      geom_sf_interactive(aes(fill = bins, tooltip = paste(countyName, "\n", "Median Year Built: ", yearBuilt, sep = "")), color = "black") + 
      theme_sf +
      theme(legend.position="bottom") +
      scale_fill_manual(values = c("white", "#E7F5D9", "#C7EF99", "#90E033", "#5CA81F", "#076324", "black"))
    
    ggiraph(code = print(year.built.county.plot), selection_type = "none")
    
  })

  
# Server - Mortgage Status Visualization --------------------------------------------------------
  output$mortgagestatuscountygraph <- renderggiraph({

    mortgage.status.county.plot <- ggplot(filter(tidymortgage.status.1990_2010, countyName %in% input$mortgage.status.county), aes(color=countyName, x=as.numeric(year), y=as.numeric(percentFree))) +
      scale_x_continuous(breaks=c(1990,2000,2010))+
      scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Percent of houses without mortgages")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="bottom") +
      scale_color_discrete(guide = guide_legend(ncol = 3))

    if (input$mortgage.status.vis.list == "Data points"){
      mortgage.status.county.plot <- mortgage.status.county.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Percent of homes without mortgages: ",percent(percentFree))))
    }
    else if (input$mortgage.status.vis.list == "Trend lines"){
      mortgage.status.county.plot <- mortgage.status.county.plot +
        geom_line()
    }
    else if (input$mortgage.status.vis.list == "Both visualizations"){
      mortgage.status.county.plot <- mortgage.status.county.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Percent of homes without mortgages: ",percent(percentFree)))) +
        geom_line()
    }
    ggiraph(code=print(mortgage.status.county.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })

  output$mortgagestatuscountymap <- renderggiraph({

    mortgage.status.county.map.plot <- ggplot(filter(tidymortgage.status.1990_2010, year == as.numeric(input$mortgage.status.county.map))) +
      geom_sf_interactive(aes(fill = bins, tooltip = paste(countyName, "\n", "Percent Of Mortgage Free Homes: ", percent(percentFree), sep = "")), color = "black") +
      theme_sf +
      theme(legend.position="bottom") + 
      scale_fill_manual(values = c("white", "#E7F5D9", "#C7EF99", "#90E033", "#5CA81F", "black"))

    ggiraph(code = print(mortgage.status.county.map.plot), selection_type = "none")

  })

# Server - Students Enrolled Visualization --------------------------------------------------------
  output$studentenrollmentgraph <- renderggiraph({

    student.enrollment.district.plot <- ggplot(filter(enrolled.2018_2000.tidy, districtName %in% input$student.enrollment.district), aes(color=districtName, x=as.numeric(year), y=as.numeric(totalStudents))) +
      scale_x_continuous(breaks=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
      scale_y_continuous(label=comma) +
      labs(x="Year", y="Number of students enrolled")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="bottom") +
      scale_color_discrete(guide = guide_legend(ncol = 3))

    if (input$student.enrollment.vis.list == "Data points"){
      student.enrollment.district.plot <- student.enrollment.district.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Total number of students enrolled: ",comma(totalStudents, digits=0))))
    }
    else if (input$student.enrollment.vis.list == "Trend lines"){
      student.enrollment.district.plot <- student.enrollment.district.plot +
        geom_line()
    }
    else if (input$student.enrollment.vis.list == "Both visualizations"){
      student.enrollment.district.plot <- student.enrollment.district.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Total number of students enrolled: ",comma(totalStudents, digits=0)))) +
        geom_line()
    }
    ggiraph(code=print(student.enrollment.district.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })
  
  output$studentenrollmentmap <- renderggiraph({
    
    student.enrollment.map.plot <- ggplot(filter(enrolled.2018_2000.map, year == as.numeric(input$student.enrollment.map))) +
      geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", "Number of Students Enrolled: ", comma(totalStudents), sep = "")), color = "black") +
      theme_sf +
      theme(legend.position="bottom") + 
      scale_fill_manual(values=c("1 - 499"="white", "500 - 999" = "#E7F5D9", "1,000 - 2,499" = "#C7EF99", "2,500 - 4,999" = "#90E033", "5,000 - 9,999" = "#5CA81F", "10,000 - 29,999"="#076324", "30,000 - 50,000"="black","NA"= "#c6c6c6")) 
      
    ggiraph(code = print(student.enrollment.map.plot), selection_type = "none")
    
  })
  
  output$enrollmentchangemap <- renderggiraph({
    
    enrollment.change.map.plot <- ggplot(filter(enrollment.change.2008_2012.map, year == as.numeric(input$enrollment.change.map))) +
      geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", "Change In Student Enrollment from", year, "to 2018: ", percent(change), sep = "")), color = "black") +
      theme_sf +
      theme(legend.position="bottom") + 
      #scale_fill_manual(values=c("-300% to -5%", "-5% to -2.5%", "-2.5% to 0%", "0% to 2.5%", "2.5% to 5%", "5% to 76%")) +
      scale_fill_manual(values=c("-300% to -5%"="white", "-5% to -2.5%" = "#E7F5D9", "-2.5% to 0%" = "#C7EF99", "0% to 2.5%" = "#90E033", "2.5% to 5%" = "#5CA81F", "5% to 76%"="#076324", "NA"= "#c6c6c6")) 
    
    ggiraph(code = print(enrollment.change.map.plot), selection_type = "none")
    
  })
  
# Server - Ethnicity of Students Enrolled Visualization --------------------------------------------------------
  output$studentethnicitygraph <- renderggiraph({
    
    student.ethnicity.district.plot <- ggplot(filter(enrolled.ethnicity.2000_2018.tidy, districtName %in% input$student.ethnicity.district), aes(color=districtName, x=as.numeric(year), y=as.numeric(percentMinority))) +
      scale_x_continuous(breaks=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
      scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Percent of students of color enrolled")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="bottom") +
      scale_color_discrete(guide = guide_legend(ncol = 3))
    
    if (input$student.ethnicity.vis.list == "Data points"){
      student.ethnicity.district.plot <- student.ethnicity.district.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of students of color enrolled: ",percent(percentMinority))))
    }
    else if (input$student.ethnicity.vis.list == "Trend lines"){
      student.ethnicity.district.plot <- student.ethnicity.district.plot +
        geom_line()
    }
    else if (input$student.ethnicity.vis.list == "Both visualizations"){
      student.ethnicity.district.plot <- student.ethnicity.district.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of students of color enrolled: ",percent(percentMinority)))) +
        geom_line()
    }
    ggiraph(code=print(student.ethnicity.district.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })
  
  output$studentethnicitymap <- renderggiraph({
    
    student.ethnicity.map.plot <- ggplot(filter(enrolled.ethnicity.2000_2018.map, year == as.numeric(input$student.ethnicity.map))) +
      geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", "Percent of Students of Color Enrolled: ", percent(percentMinority), sep = "")), color = "black") +
      theme_sf +
      theme(legend.position="bottom") + 
      scale_fill_manual(values=c("0% - 5%"="white", "5% - 10%"="#E7F5D9", "10% - 15%"="#C7EF99", "15% - 20%"="#90E033", "20% - 25%"="#5CA81F", "25% - 100%"="#076324", "NA"="#c6c6c6")) 
    
    #scale_fill_manual(values = c("white", "#E7F5D9", "#C7EF99", "#90E033", "#5CA81F", "#076324", "black", "#c6c6c6"))
    
    ggiraph(code = print(student.ethnicity.map.plot), selection_type = "none")
    
  })
  
# Server - Overall Graduation Rates Visualization --------------------------------------------------------
    output$gradrategraph <- renderggiraph({

      grad.rate.plot <- ggplot(filter(grad.2012_2018.tidy, districtName %in% input$grad.rate), aes(color=districtName, x=as.numeric(year), y=as.numeric(gradRate))) +
        scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017))+
        scale_y_continuous(labels=scales::percent)+
        labs(x="Year", y="Graduation Rate")+
        theme_bar+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="bottom") + 
        scale_color_discrete(guide = guide_legend(ncol = 3))

      if (input$grad.rate.vis.list == "Data points"){
        grad.rate.plot <- grad.rate.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Total graduation rate: ",percent(gradRate))))
      }
      else if (input$grad.rate.vis.list == "Trend lines"){
        grad.rate.plot <- grad.rate.plot +
          geom_line()
      }
      else if (input$grad.rate.vis.list == "Both visualizations"){
        grad.rate.plot <- grad.rate.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Total graduation rate: ",percent(gradRate)))) +
          geom_line()
      }
      ggiraph(code=print(grad.rate.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
    })
  
  output$gradratemap <- renderggiraph({
    grad.rate.map.plot <- ggplot(filter(grad.2012_2018.tidy.map, year == as.numeric(input$grad.rate.map))) +
      geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", "Total Graduation Rate: ", percent(gradRate), sep = "")), color = "black") +
      scale_color_manual(guide = guide_legend(ncol = 3)) +
      theme_sf +
      #theme(legend.position="bottom") +
      scale_fill_manual(values=c("0% - 75%"="white", "75% - 85%"="#C7EF99", "85% - 95%"="#90E033", "95% - 100%"="#076324", "NA"= "#c6c6c6"))

    ggiraph(code = print(grad.rate.map.plot), selection_type = "none")

  })
    
# Server - Overall Dropout Rates Visualization --------------------------------------------------------
    output$droprategraph <- renderggiraph({
      
      drop.rate.plot <- ggplot(filter(drop.2012_2018.tidy, districtName %in% input$drop.rate), aes(color=districtName, x=as.numeric(year), y=as.numeric(dropRate))) +
        scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017))+
        scale_y_continuous(labels=scales::percent)+
        labs(x="Year", y="Dropout Rate")+
        theme_bar+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="bottom") +
        scale_color_discrete(guide = guide_legend(ncol = 3))
      
      if (input$drop.rate.vis.list == "Data points"){
        drop.rate.plot <- drop.rate.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Total dropout rate: ",percent(dropRate))))
      }
      else if (input$drop.rate.vis.list == "Trend lines"){
        drop.rate.plot <- drop.rate.plot +
          geom_line()
      }
      else if (input$drop.rate.vis.list == "Both visualizations"){
        drop.rate.plot <- drop.rate.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Total dropout rate: ",percent(dropRate)))) +
          geom_line()
      }
      ggiraph(code=print(drop.rate.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
    })
  
output$dropratemap <- renderggiraph({
  drop.rate.map.plot <- ggplot(filter(drop.2012_2018.tidy.map, year == as.numeric(input$drop.rate.map))) +
      geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", "Total Dropout Rate: ", percent(dropRate), sep = "")), color = "black") +
      scale_color_manual(guide = guide_legend(ncol = 3)) +
      theme_sf +
      #theme(legend.position="bottom") +
      scale_fill_manual(values=c("0% - 2.5%"="white", "2.5% - 5.5%"="#C7EF99", "5.5% - 7.5%"="#90E033", "7.5% - 10%"="#076324","10% to 100%"="black", "NA"= "#c6c6c6"))
    
    ggiraph(code = print(drop.rate.map.plot), selection_type = "none")
    
  })

# Server - Graduation Rates Visualization - Students of Color --------------------------------------------------------
    output$ethnicitygradrategraph <- renderggiraph({
      
      ethnicity.grad.rate.plot <- ggplot(filter(grad.ethnicity.2012_2017, districtName %in% input$ethnicity.grad.rate), aes(color=districtName, x=as.numeric(year), y=as.numeric(gradRate))) +
        scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017))+
        scale_y_continuous(labels=scales::percent)+
        labs(x="Year", y="Graduation Rate of Students of Color")+
        theme_bar+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="bottom") +
        scale_color_discrete(guide = guide_legend(ncol = 3))
      
      if (input$ethnicity.grad.rate.vis.list == "Data points"){
        ethnicity.grad.rate.plot <- ethnicity.grad.rate.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Graduation rate of students of color: ",percent(gradRate))))
      }
      else if (input$ethnicity.grad.rate.vis.list == "Trend lines"){
        ethnicity.grad.rate.plot <- ethnicity.grad.rate.plot +
          geom_line()
      }
      else if (input$ethnicity.grad.rate.vis.list == "Both visualizations"){
        ethnicity.grad.rate.plot <- ethnicity.grad.rate.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Graduation rate of students of color: ",percent(gradRate)))) +
          geom_line()
      }
      ggiraph(code=print(ethnicity.grad.rate.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
    })

output$ethnicitygradratemap <- renderggiraph({
  ethnicity.grad.rate.map.plot <- ggplot(filter(grad.ethnicity.2012_2017.map, year == as.numeric(input$ethnicity.grad.map))) +
    geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", "Total Students of Color Graduation Rate: ", percent(gradRate), sep = "")), color = "black") +
    scale_color_manual(guide = guide_legend(ncol = 3)) +
    theme_sf +
    #theme(legend.position="bottom") +
    scale_fill_manual(values=c("0% - 50%" = "white", "50% - 75%" = "#C7EF99", "75% - 85%" = "#90E033", "85% - 100%" = "#076324", "NA"= "#c6c6c6"))
  
  ggiraph(code = print(ethnicity.grad.rate.map.plot), selection_type = "none")
  
})
    
# Server - Graduation Rates Visualization - Students of Color - Breakdown--------------------------------------------------------
    output$ethnicitybreakdowngradrategraph <- renderggiraph({

      ethnicity.grad.rate.breakdown.plot <- ggplot(filter(grad.ethnicity.breakdown.2012_2017, (description %in% input$ethnicity.breakdown.grad.rate.ethnicity) & (districtName %in% input$ethnicity.breakdown.grad.rate.district)), aes(color=description, x=as.numeric(year), y=as.numeric(gradRate))) +
        scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017))+
        scale_y_continuous(labels=scales::percent)+
        labs(x="Year", y="Graduation Rate")+
        theme_bar+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="bottom") + 
        scale_color_discrete(guide = guide_legend(ncol = 3))

      if (input$ethnicity.breakdown.grad.rate.vis.list == "Data points"){
        ethnicity.grad.rate.breakdown.plot <- ethnicity.grad.rate.breakdown.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Graduation rate - ",description," : ",percent(gradRate))))
      }
      else if (input$ethnicity.breakdown.grad.rate.vis.list == "Trend lines"){
        ethnicity.grad.rate.breakdown.plot <- ethnicity.grad.rate.breakdown.plot +
          geom_line()
      }
      else if (input$ethnicity.breakdown.grad.rate.vis.list == "Both visualizations"){
        ethnicity.grad.rate.breakdown.plot <- ethnicity.grad.rate.breakdown.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Graduation rate - ",description," : ",percent(gradRate)))) +
          geom_line()
      }
      ggiraph(code=print(ethnicity.grad.rate.breakdown.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
    })

output$ethnicitybreakdowngradmap <- renderggiraph({
  ethnicity.breakdown.grad.map.plot <- ggplot(filter(grad.ethnicity.breakdown.2012_2017.map, (description %in% input$ethnicity.breakdown.grad.ethnicity.map) & (year == as.numeric(input$ethnicity.breakdown.grad.year.map)))) +
    geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", description, " Graduation Rate: ", percent(gradRate), sep = "")), color = "black") +
    scale_color_manual(guide = guide_legend(ncol = 3)) +
    theme_sf +
    #theme(legend.position="bottom") +
    scale_fill_manual(values=c("0% - 50%" = "white", "50% - 75%" = "#C7EF99", "75% - 85%" = "#90E033", "85% - 100%" = "#076324", "NA"= "#c6c6c6"))
  
  ggiraph(code = print(ethnicity.breakdown.grad.map.plot), selection_type = "none")
  
})

# Server - Dropout Rates Visualization - Students of Color --------------------------------------------------------
    output$ethnicitydroprategraph <- renderggiraph({
      
      ethnicity.drop.rate.plot <- ggplot(filter(drop.ethnicity.2012_2017, districtName %in% input$ethnicity.drop.rate), aes(color=districtName, x=as.numeric(year), y=as.numeric(dropRate))) +
        scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017))+
        scale_y_continuous(labels=scales::percent)+
        labs(x="Year", y="Dropout Rate of Students of Color")+
        theme_bar+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="bottom") +
        scale_color_discrete(guide = guide_legend(ncol = 3))
      
      if (input$ethnicity.drop.rate.vis.list == "Data points"){
        ethnicity.drop.rate.plot <- ethnicity.drop.rate.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Dropout rate of students of color: ",percent(dropRate))))
      }
      else if (input$ethnicity.drop.rate.vis.list == "Trend lines"){
        ethnicity.drop.rate.plot <- ethnicity.drop.rate.plot +
          geom_line()
      }
      else if (input$ethnicity.drop.rate.vis.list == "Both visualizations"){
        ethnicity.drop.rate.plot <- ethnicity.drop.rate.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Dropout rate of students of color: ",percent(dropRate)))) +
          geom_line()
      }
      ggiraph(code=print(ethnicity.drop.rate.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
    })

output$ethnicitydropratemap <- renderggiraph({
  ethnicity.drop.rate.map.plot <- ggplot(filter(drop.ethnicity.2012_2017.map, year == as.numeric(input$ethnicity.drop.map))) +
    geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", "Total Students of Color Dropout Rate: ", percent(dropRate), sep = "")), color = "black") +
    scale_color_manual(guide = guide_legend(ncol = 3)) +
    theme_sf +
    #theme(legend.position="bottom") +
    scale_fill_manual(values=c("0% - 5%" = "white", "5% - 10%" = "#C7EF99", "10% - 20%" = "#90E033","20% - 100%" = "#076324", "NA"= "#c6c6c6"))
  
  ggiraph(code = print(ethnicity.drop.rate.map.plot), selection_type = "none")
  
})

# Server - Dropout Rates Visualization - Students of Color - Breakdown--------------------------------------------------------
    output$ethnicitybreakdowndroprategraph <- renderggiraph({
      
      ethnicity.drop.rate.breakdown.plot <- ggplot(filter(drop.ethnicity.breakdown.2012_2017, (description %in% input$ethnicity.breakdown.drop.rate.ethnicity) & (districtName %in% input$ethnicity.breakdown.drop.rate.district)), aes(color=description, x=as.numeric(year), y=as.numeric(dropRate))) +
        scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017))+
        scale_y_continuous(labels=scales::percent)+
        labs(x="Year", y="Dropout Rate")+
        theme_bar+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="bottom") +
        scale_color_discrete(guide = guide_legend(ncol = 3))
      
      if (input$ethnicity.breakdown.drop.rate.vis.list == "Data points"){
        ethnicity.drop.rate.breakdown.plot <- ethnicity.drop.rate.breakdown.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Dropout rate - ",description," : ",percent(dropRate))))
      }
      else if (input$ethnicity.breakdown.drop.rate.vis.list == "Trend lines"){
        ethnicity.drop.rate.breakdown.plot <- ethnicity.drop.rate.breakdown.plot +
          geom_line()
      }
      else if (input$ethnicity.breakdown.drop.rate.vis.list == "Both visualizations"){
        ethnicity.drop.rate.breakdown.plot <- ethnicity.drop.rate.breakdown.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Dropout rate - ",description," : ",percent(dropRate)))) +
          geom_line()
      }
      ggiraph(code=print(ethnicity.drop.rate.breakdown.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
    })
    
# Server - GRAD: Math Score Visualization --------------------------------------------------------
    output$gradmathscoregraph <- renderggiraph({
      
      grad.math.score.plot <- ggplot(filter(grad.math.2009_2013_total, districtName %in% input$grad.math.score), aes(color=districtName, x=as.numeric(year), y=as.numeric(averageScore))) +
        scale_x_continuous(breaks=c(2009,2010,2011,2012,2013))+
        #scale_y_continuous(labels=scales::percent)+
        labs(x="Year", y="Average GRAD: Math Score")+
        theme_bar+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="bottom") +
        scale_color_discrete(guide = guide_legend(ncol = 3))
      
      if (input$grad.math.score.vis.list == "Data points"){
        grad.math.score.plot <- grad.math.score.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Average GRAD: Math Score: ",averageScore)))
      }
      else if (input$grad.math.score.vis.list == "Trend lines"){
        grad.math.score.plot <- grad.math.score.plot +
          geom_line()
      }
      else if (input$grad.math.score.vis.list == "Both visualizations"){
        grad.math.score.plot <- grad.math.score.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Average GRAD: Math Score: ",averageScore))) +
          geom_line()
      }
      ggiraph(code=print(grad.math.score.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
    })
    
# Server - GRAD: Math Passing Visualization --------------------------------------------------------
    output$gradmathpassgraph <- renderggiraph({
      
      grad.math.pass.plot <- ggplot(filter(grad.math.2009_2013_total, districtName %in% input$grad.math.pass), aes(color=districtName, x=as.numeric(year), y=as.numeric(percentPassed))) +
        scale_x_continuous(breaks=c(2009,2010,2011,2012,2013))+
        scale_y_continuous(labels=scales::percent)+
        labs(x="Year", y="GRAD: Math Percent Passed")+
        theme_bar+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="bottom") + 
        scale_color_discrete(guide = guide_legend(ncol = 3))
      
      if (input$grad.math.pass.vis.list == "Data points"){
        grad.math.pass.plot <- grad.math.pass.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n GRAD: Math Percent Passed: ",percent(percentPassed))))
      }
      else if (input$grad.math.pass.vis.list == "Trend lines"){
        grad.math.pass.plot <- grad.math.pass.plot +
          geom_line()
      }
      else if (input$grad.math.pass.vis.list == "Both visualizations"){
        grad.math.pass.plot <- grad.math.pass.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n GRAD: Math Percent Passed: ",percent(percentPassed)))) +
          geom_line()
      }
      ggiraph(code=print(grad.math.pass.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
    })
    
# Server - GRAD: Reading Score Visualization --------------------------------------------------------
    output$gradreadingscoregraph <- renderggiraph({
      
      grad.reading.score.plot <- ggplot(filter(grad.reading.2008_2012_total, districtName %in% input$grad.reading.score), aes(color=districtName, x=as.numeric(year), y=as.numeric(averageScore))) +
        scale_x_continuous(breaks=c(2008, 2009,2010,2011,2012))+
        #scale_y_continuous(labels=scales::percent)+
        labs(x="Year", y="Average GRAD: Reading Score")+
        theme_bar+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="bottom") +
        scale_color_discrete(guide = guide_legend(ncol = 3))
      
      if (input$grad.reading.score.vis.list == "Data points"){
        grad.reading.score.plot <- grad.reading.score.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Average GRAD: Reading Score: ",averageScore)))
      }
      else if (input$grad.reading.score.vis.list == "Trend lines"){
        grad.reading.score.plot <- grad.reading.score.plot +
          geom_line()
      }
      else if (input$grad.reading.score.vis.list == "Both visualizations"){
        grad.reading.score.plot <- grad.reading.score.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Average GRAD: Reading Score: ",averageScore))) +
          geom_line()
      }
      ggiraph(code=print(grad.reading.score.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
    })
    
# Server - GRAD: Reading Passing Visualization --------------------------------------------------------
    output$gradreadingpassgraph <- renderggiraph({
      
      grad.reading.pass.plot <- ggplot(filter(grad.reading.2008_2012_total, districtName %in% input$grad.reading.pass), aes(color=districtName, x=as.numeric(year), y=as.numeric(percentPassed))) +
        scale_x_continuous(breaks=c(2008,2009, 2010,2011,2012))+
        scale_y_continuous(labels=scales::percent)+
        labs(x="Year", y="GRAD: Reading Percent Passed")+
        theme_bar+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="bottom") +
        scale_color_discrete(guide = guide_legend(ncol = 3))
      
      if (input$grad.reading.pass.vis.list == "Data points"){
        grad.reading.pass.plot <- grad.reading.pass.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n GRAD: Reading Percent Passed: ",percent(percentPassed))))
      }
      else if (input$grad.reading.pass.vis.list == "Trend lines"){
        grad.reading.pass.plot <- grad.reading.pass.plot +
          geom_line()
      }
      else if (input$grad.reading.pass.vis.list == "Both visualizations"){
        grad.reading.pass.plot <- grad.reading.pass.plot +
          geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n GRAD: Reading Percent Passed: ",percent(percentPassed)))) +
          geom_line()
      }
      ggiraph(code=print(grad.reading.pass.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
    })

# Server - ACT Scores Visualization --------------------------------------------------------
  output$actscoresgraph <- renderggiraph({
    
    act.scores.plot <- ggplot(filter(act.scores.2008_2018, districtName %in% input$act.scores), aes(color=districtName, x=as.numeric(year), y=as.numeric(score))) +
      scale_x_continuous(breaks=c(2008,2009, 2010,2011,2012, 2013, 2014, 2015, 2016, 2017, 2018))+
      #scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Average ACT Score")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="bottom") +
      scale_color_discrete(guide = guide_legend(ncol = 3))
    
    if (input$act.scores.vis.list == "Data points"){
      act.scores.plot <- act.scores.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Average ACT Score: ",score)))
    }
    else if (input$act.scores.vis.list == "Trend lines"){
      act.scores.plot <- act.scores.plot +
        geom_line()
    }
    else if (input$act.scores.vis.list == "Both visualizations"){
      act.scores.plot <- act.scores.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Average ACT Score: ",score))) +
        geom_line()
    }
    ggiraph(code=print(act.scores.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })
  
# Server - ACT Percent Takers --------------------------------------------------------
  output$acttakersgraph <- renderggiraph({
    
    act.takers.plot <- ggplot(filter(act.takers.2008_2018, districtName %in% input$act.takers), aes(color=districtName, x=as.numeric(year), y=as.numeric(percentTakers))) +
      scale_x_continuous(breaks=c(2008,2009, 2010,2011,2012, 2013, 2014, 2015, 2016, 2017, 2018))+
      scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Percent of 12th Graders Who Take the ACT")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="bottom") +
      scale_color_discrete(guide = guide_legend(ncol = 3))
    
    if (input$act.takers.vis.list == "Data points"){
      act.takers.plot <- act.takers.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of 12th Graders Who Took the ACT: ",percent(percentTakers))))
    }
    else if (input$act.takers.vis.list == "Trend lines"){
      act.takers.plot <- act.takers.plot +
        geom_line()
    }
    else if (input$act.takers.vis.list == "Both visualizations"){
      act.takers.plot <- act.takers.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of 12th Graders Who Took the ACT: ",percent(percentTakers)))) +
        geom_line()
    }
    ggiraph(code=print(act.takers.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })

output$acttakersmap <- renderggiraph({
  act.takers.map.plot <- ggplot(filter(act.takers.2008_2018.map, year == as.numeric(input$act.takers.map))) +
    geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", "Percent of 12th Graders Who Took the ACT: ", percent(percentTakers), sep = "")), color = "black") +
    scale_color_manual(guide = guide_legend(ncol = 3)) +
    theme_sf +
    #theme(legend.position="bottom") +
    scale_fill_manual(values=c("0 - 50%"="white", "50% - 75%"="#C7EF99", "75% - 85%" ="#90E033", "85% - 95%"="#076324", "95%+" = "#076324", "NA"= "#c6c6c6")) 
  
  ggiraph(code = print(act.takers.map.plot), selection_type = "none")
  
})
  
# Server - Students With Free/Reduced Lunch Visualization --------------------------------------------------------
  output$lunchgraph <- renderggiraph({

    lunch.district.plot <- ggplot(filter(tidy.free.red.lunch.2006_2018, districtName %in% input$lunch.district), aes(color=districtName, x=as.numeric(year), y=as.numeric(percentLunch))) +
      scale_x_continuous(breaks=c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
      scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Percent of students with free/reduced lunch")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="bottom") +
      scale_color_discrete(guide = guide_legend(ncol = 3))

    if (input$lunch.vis.list == "Data points"){
      lunch.district.plot <- lunch.district.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of students with free/reduced lunch: ",percent(percentLunch))))
    }
    else if (input$lunch.vis.list == "Trend lines"){
      lunch.district.plot <- lunch.district.plot +
        geom_line()
    }
    else if (input$lunch.vis.list == "Both visualizations"){
      lunch.district.plot <- lunch.district.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of students with free/reduced lunch: ",percent(percentLunch)))) +
        geom_line()
    }
    ggiraph(code=print(lunch.district.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })

output$lunchmap <- renderggiraph({
  lunch.map.plot <- ggplot(filter(tidy.free.red.lunch.2006_2018.map, year == as.numeric(input$lunch.district.map))) +
    geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", "Percent of students with free/reduced lunch: ", percent(percentLunch), sep = "")), color = "black") +
    scale_color_manual(guide = guide_legend(ncol = 3)) +
    theme_sf +
    #theme(legend.position="bottom") +
    scale_fill_manual(values=c("0% - 25%"="white", "25% - 35%" = "#C7EF99", "35% - 50%" = "#90E033", "50% - 75%"="#076324", "75% - 100%" = "#076324", "NA"= "#c6c6c6")) 
  
  ggiraph(code = print(lunch.map.plot), selection_type = "none")
  
})

# Server - Students Not Proficient in English - Identified --------------------------------------------------------
  output$englishprofgraph <- renderggiraph({
    
    english.plot <- ggplot(filter(english.2010_2019, districtName %in% input$english.prof), aes(color=districtName, x=as.numeric(year), y=as.numeric(percentIdentified))) +
      scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
      scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Percent of students not proficient in English")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="bottom") +
      scale_color_discrete(guide = guide_legend(ncol = 3))
    
    if (input$english.vis.list == "Data points"){
      english.plot <- english.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of students not proficient in English: ",percent(percentIdentified))))
    }
    else if (input$english.vis.list == "Trend lines"){
      english.plot <- english.plot +
        geom_line()
    }
    else if (input$english.vis.list == "Both visualizations"){
      english.plot <- english.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of students not proficient in English: ",percent(percentIdentified)))) +
        geom_line()
    }
    ggiraph(code=print(english.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })  

output$englishprofmap <- renderggiraph({
  english.map.plot <- ggplot(filter(english.2010_2019.map, year == as.numeric(input$english.prof.map))) +
    geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", "Percent of students not proficient in English: ", percent(percentIdentified), sep = "")), color = "black") +
    scale_color_manual(guide = guide_legend(ncol = 3)) +
    theme_sf +
    #theme(legend.position="bottom") +
    scale_fill_manual(values=c("0% - 2.5%"="white", "2.5% - 5%"="#C7EF99", "5% - 7.5%"="#90E033", "7.5% - 15%"="#076324", "15% - 100%"="#076324", "NA"= "#c6c6c6"))
  
  ggiraph(code = print(english.map.plot), selection_type = "none")
  
})

# Server - Students With English As Home Language --------------------------------------------------------
  output$homelanggraph <- renderggiraph({
    
    home.lang.plot <- ggplot(filter(home_lang_2008_2018, districtName %in% input$home.lang), aes(color=districtName, x=as.numeric(year), y=as.numeric(percentEng))) +
      scale_x_continuous(breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
      scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Percent of students with English as home language")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="bottom") +
      scale_color_discrete(guide = guide_legend(ncol = 3))
    
    if (input$home.lang.vis.list == "Data points"){
      home.lang.plot <- home.lang.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of students with English as home language: ",percent(percentEng))))
    }
    else if (input$home.lang.vis.list == "Trend lines"){
      home.lang.plot <- home.lang.plot +
        geom_line()
    }
    else if (input$home.lang.vis.list == "Both visualizations"){
      home.lang.plot <- home.lang.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Percent of students with English as home language: ",percent(percentEng)))) +
        geom_line()
    }
    ggiraph(code=print(home.lang.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })  

output$homelangmap <- renderggiraph({
  home.lang.map.plot <- ggplot(filter(home_lang_2008_2018.map, year == as.numeric(input$home.lang.map))) +
    geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", "Percent of students with English as home language: ", percent(percentEng), sep = "")), color = "black") +
    scale_color_manual(guide = guide_legend(ncol = 3)) +
    theme_sf +
    #theme(legend.position="bottom") +
    scale_fill_manual(values=c("0% - 80%"="white", "80% - 94%"="#C7EF99", "94% - 96%"="#90E033", "96% - 98%"="#076324", "98% - 100%"="#033d15", "NA"= "#c6c6c6"))
  
  ggiraph(code = print(home.lang.map.plot), selection_type = "none")
  
})

# Server - Number of Teachers --------------------------------------------------------
  output$teachergraph <- renderggiraph({
    
    teacher.plot <- ggplot(filter(student.teacher.ratio.2008_2018, districtName %in% input$teacher.district), aes(color=districtName, x=as.numeric(year), y=as.numeric(totalTeachers))) +
      scale_x_continuous(breaks=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
      scale_y_continuous(label=comma) +
      labs(x="Year", y="Number of teachers (full-time equivalents)")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="bottom") +
      scale_color_discrete(guide = guide_legend(ncol = 3))
    
    if (input$teacher.vis.list == "Data points"){
      teacher.plot <- teacher.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Number of teachers (FTEs): ",totalTeachers)))
    }
    else if (input$teacher.vis.list == "Trend lines"){
      teacher.plot <- teacher.plot +
        geom_line()
    }
    else if (input$teacher.vis.list == "Both visualizations"){
      teacher.plot <- teacher.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Number of teachers (FTEs): ",totalTeachers))) +
        geom_line()
    }
    ggiraph(code=print(teacher.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })
  
# Server - Student-Teacher Ratio --------------------------------------------------------
  output$studentteachegraph <- renderggiraph({
    
    student.teacher.plot <- ggplot(filter(student.teacher.ratio.2008_2018, districtName %in% input$student.teacher.ratio), aes(color=districtName, x=as.numeric(year), y=as.numeric(studentTeacherRatio))) +
      scale_x_continuous(breaks=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
      scale_y_continuous(label=comma) +
      labs(x="Year", y="Student-Teacher Ratio")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="bottom") +
      scale_color_discrete(guide = guide_legend(ncol = 3))
    
    if (input$student.teacher.vis.list == "Data points"){
      student.teacher.plot <- student.teacher.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Student-Teacher Ratio: ",studentTeacherRatio)))
    }
    else if (input$student.teacher.vis.list == "Trend lines"){
      student.teacher.plot <- student.teacher.plot +
        geom_line()
    }
    else if (input$student.teacher.vis.list == "Both visualizations"){
      student.teacher.plot <- student.teacher.plot +
        geom_point_interactive(size=3,aes(tooltip=paste(districtName, year,"\n Student-Teacher Ratio ",studentTeacherRatio))) +
        geom_line()
    }
    ggiraph(code=print(student.teacher.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })

output$studentteachermap <- renderggiraph({
  student.teacher.map.plot <- ggplot(filter(student.teacher.ratio.2008_2018.map,
                                            year == as.numeric(input$student.teacher.ratio.map)
                                            )
                                     ) +
    geom_sf_interactive(aes(fill = bins, tooltip = paste(districtName, "\n", "Student-Teacher Ratio: ", studentTeacherRatio, sep = "")), color = "black") +
    scale_color_manual(guide = guide_legend(ncol = 3)) +
    theme_sf +
    #theme(legend.position="bottom") +
    #scale_fill_manual(values=c("0% - 80%"="white", "80% - 94%"="#C7EF99", "94% - 96%"="#90E033", "96% - 98%"="#076324", "98% - 100%"="#033d15", "NA"= "#c6c6c6")) + 
    scale_fill_manual(values=c("0 - 10"="white", "10 - 12"="#C7EF99", "12 - 15"="#90E033", "15 - 17"="#076324", "17+"="#033d15", "NA"= "#c6c6c6"))
  
  ggiraph(code = print(student.teacher.map.plot), selection_type = "none")
  
})

# Server - Cost of Living - Yearly Cost --------------------------------------------------------
output$colcountygraph <- renderggiraph({
  col.plot <- ggplot(filter(yearly.cost.2016_2018,
                            (countyName %in% input$col.county) &
                            
                            (if (input$col.county.workers == "Single") {
                              numberAdults == 1 & adultAge=="19-50"
                            }
                            
                            else if (input$col.county.workers == "Partnered - 1 full-time worker") {
                              numberAdults == 2 & numberWorkers == 1 & adultAge=="19-50"
                            }
                            
                            else if (input$col.county.workers == "Partnered - 1 full-time, 1 part-time worker") {
                              numberAdults == 2 & numberWorkers == 1.5 & adultAge=="19-50"
                            }
                            
                            else if (input$col.county.workers == "Partnered - 2 full-time workers") {
                              numberAdults == 2 & numberWorkers == 2 & adultAge=="19-50"
                            }) &
                          
                          (numberChildren == as.numeric(input$col.county.children))
                              
                            ),
                     
                     aes(color=countyName, x=as.numeric(year), y=as.numeric(yearlyCost))) +
    scale_x_continuous(breaks=c(2016,2017,2018))+
    scale_y_continuous(label=comma) +
    labs(x="Year", y="Cost of Living")+
    theme_bar+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position="bottom") +
    scale_color_discrete(guide = guide_legend(ncol = 3))

  if (input$col.vis.list == "Data points"){
    col.plot <- col.plot +
      geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Cost of Living: $",comma(yearlyCost))))
  }
  else if (input$col.vis.list == "Trend lines"){
    col.plot <- col.plot +
      geom_line()
  }
  else if (input$col.vis.list == "Both visualizations"){
    col.plot <- col.plot +
      geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Cost of Living: $",comma(yearlyCost)))) +
      geom_line()
  }
  ggiraph(code=print(col.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
})

output$colcountymap <- renderggiraph({
  col.county.map.plot <- ggplot(filter(yearly.cost.2016_2018.map,
                                            year == as.numeric(input$year.col.county) &
                                              
                                              (if (input$col.county.workers.map == "Single") {
                                                numberAdults == 1 & adultAge=="19-50"
                                              }
                                              
                                              else if (input$col.county.workers.map == "Partnered - 1 full-time worker") {
                                                numberAdults == 2 & numberWorkers == 1 & adultAge=="19-50"
                                              }
                                              
                                              else if (input$col.county.workers.map == "Partnered - 1 full-time, 1 part-time worker") {
                                                numberAdults == 2 & numberWorkers == 1.5 & adultAge=="19-50"
                                              }
                                              
                                              else if (input$col.county.workers.map == "Partnered - 2 full-time workers") {
                                                numberAdults == 2 & numberWorkers == 2 & adultAge=="19-50"
                                              }) &
                                              
                                              (numberChildren == as.numeric(input$col.county.children.map))
  )
  ) +
    geom_sf_interactive(aes(fill = bins, tooltip = paste(countyName, "\n", "Cost of Living: $",comma(yearlyCost), sep = "")), color = "black") +
    scale_color_manual(guide = guide_legend(ncol = 3)) +
    theme_sf +
    #theme(legend.position="bottom") +
    scale_fill_manual(values=c("$20,000 - $35,000"="white", "$35,000 - $45,000"="#C7EF99", "$45,000 - $55,000"="#90E033", "$55,000 - $65,000"="#076324", "$65,000 - $115,000"="#033d15", "NA"= "#c6c6c6"))
  
  ggiraph(code = print(col.county.map.plot), selection_type = "none")
  
})

# Server - Cost of Living - Hourly Wage --------------------------------------------------------

output$hourlywagegraph <- renderggiraph({
  hourly.wage.plot <- ggplot(filter(hourly.wage.2016_2018,
                            (countyName %in% input$hourly.wage.county) &
                              
                              (if (input$hourly.wage.workers == "Single") {
                                numberAdults == 1 & adultAge=="19-50"
                              }
                              
                              else if (input$hourly.wage.workers == "Partnered - 1 full-time worker") {
                                numberAdults == 2 & numberWorkers == 1 & adultAge=="19-50"
                              }
                              
                              else if (input$hourly.wage.workers == "Partnered - 1 full-time, 1 part-time worker") {
                                numberAdults == 2 & numberWorkers == 1.5 & adultAge=="19-50"
                              }
                              
                              else if (input$hourly.wage.workers == "Partnered - 2 full-time workers") {
                                numberAdults == 2 & numberWorkers == 2 & adultAge=="19-50"
                              }) &
                              
                              (numberChildren == as.numeric(input$hourly.wage.children))
                            
  ),
  
  aes(color=countyName, x=as.numeric(year), y=as.numeric(hourlyWage))) +
    scale_x_continuous(breaks=c(2016,2017,2018))+
    scale_y_continuous(label=comma) +
    labs(x="Year", y="Hourly Wage")+
    theme_bar+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position="bottom") +
    scale_color_discrete(guide = guide_legend(ncol = 3))
  
  if (input$hourly.wage.vis.list == "Data points"){
    hourly.wage.plot <- hourly.wage.plot +
      geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Hourly Wage: $",comma(hourlyWage))))
  }
  else if (input$hourly.wage.vis.list == "Trend lines"){
    hourly.wage.plot <- hourly.wage.plot +
      geom_line()
  }
  else if (input$hourly.wage.vis.list == "Both visualizations"){
    hourly.wage.plot <- hourly.wage.plot +
      geom_point_interactive(size=3,aes(tooltip=paste(countyName, year,"\n Hourly Wage: $",comma(hourlyWage)))) +
      geom_line()
  }
  ggiraph(code=print(hourly.wage.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
})

output$hourlywagemap <- renderggiraph({
  hourly.wage.map.plot <- ggplot(filter(hourly.wage.2016_2018.map,
                                       year == as.numeric(input$year.hourly.wage.county) &
                                         
                                         (if (input$hourly.wage.county.workers.map == "Single") {
                                           numberAdults == 1 & adultAge=="19-50"
                                         }
                                         
                                         else if (input$hourly.wage.county.workers.map == "Partnered - 1 full-time worker") {
                                           numberAdults == 2 & numberWorkers == 1 & adultAge=="19-50"
                                         }
                                         
                                         else if (input$hourly.wage.county.workers.map == "Partnered - 1 full-time, 1 part-time worker") {
                                           numberAdults == 2 & numberWorkers == 1.5 & adultAge=="19-50"
                                         }
                                         
                                         else if (input$hourly.wage.county.workers.map == "Partnered - 2 full-time workers") {
                                           numberAdults == 2 & numberWorkers == 2 & adultAge=="19-50"
                                         }) &
                                         
                                         (numberChildren == as.numeric(input$hourly.wage.county.children.map))
  )
  ) +
    geom_sf_interactive(aes(fill = bins, tooltip = paste(countyName, "\n", "Hourly Wage: $",comma(hourlyWage), sep = "")), color = "black") +
    scale_color_manual(guide = guide_legend(ncol = 3)) +
    theme_sf +
    #theme(legend.position="bottom") +
    scale_fill_manual(values=c("$7 - $15"="white", "$15 - $20"="#C7EF99", "$20 - $30"="#90E033", "$30 - $55"="#076324", "NA"= "#c6c6c6"))
  
  ggiraph(code = print(hourly.wage.map.plot), selection_type = "none")
  
})

}

# Run the application 
shinyApp(ui = ui, server = server)
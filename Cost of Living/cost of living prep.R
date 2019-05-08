library(tidyverse)

mn_counties <- read_csv("Cost of Living/mn_counties.csv") %>%
  mutate(countyName = ifelse(countyName == "St Louis", "Saint Louis", as.character(countyName)),
         countyName = ifelse(countyName == "Lake Of The Woods", "Lake of the Woods", as.character(countyName)),
         countyName = ifelse(countyName == "Lac Qui Parle", "Lac qui Parle", as.character(countyName))
         )

# Cost of living by county ---------------------------------

col.2018 <- read_csv("Cost of Living/cost_of_living_2018.csv") %>%
  select("Area", "Family Size", "Number of Adults", "Number of Workers", "Number of Children", "Age of Adults", "Yearly Cost", "Hourly Wage") %>%
  rename(
    countyName=Area,
    familySize = "Family Size",
    numberAdults = "Number of Adults",
    numberWorkers = "Number of Workers",
    numberChildren = "Number of Children",
    adultAge = "Age of Adults",
    yearlyCost = "Yearly Cost",
    hourlyWage = "Hourly Wage"
  ) %>%
  filter(str_detect(countyName, "County") == TRUE,
         str_detect(countyName, "EDR") == FALSE,
         str_detect(countyName, "Seven County Mpls-St Paul, MN") == FALSE
         ) %>%
  mutate(
    countyName = str_replace(countyName, " County", "")
  ) %>%
  full_join(mn_counties, col.2018, by = c("countyName")) %>%
  select(countyName, countyFIPS, familySize, numberAdults, numberWorkers, numberChildren, adultAge, yearlyCost, hourlyWage) %>%
  rename(yearlyCost2018 = yearlyCost,
         hourlyWage2018 = hourlyWage)

col.2017 <- read_csv("Cost of Living/cost_of_living_2017.csv") %>%
  select("Area", "Family Size", "Number of Adults", "Number of Workers", "Number of Children", "Age of Adults", "Yearly Cost", "Hourly Wage") %>%
  rename(
    countyName=Area,
    familySize = "Family Size",
    numberAdults = "Number of Adults",
    numberWorkers = "Number of Workers",
    numberChildren = "Number of Children",
    adultAge = "Age of Adults",
    yearlyCost = "Yearly Cost",
    hourlyWage = "Hourly Wage"
  ) %>%
  filter(str_detect(countyName, "County") == TRUE,
         str_detect(countyName, "EDR") == FALSE,
         str_detect(countyName, "Seven County Mpls-St Paul, MN") == FALSE
  ) %>%
  mutate(
    countyName = str_replace(countyName, " County", "")
  ) %>%
  mutate(
    countyName = str_replace(countyName, " County", "")
  ) %>%
  full_join(mn_counties, col.2018, by = c("countyName")) %>%
  select(countyName, countyFIPS, familySize, numberAdults, numberWorkers, numberChildren, adultAge, yearlyCost, hourlyWage) %>%
  rename(yearlyCost2017 = yearlyCost,
         hourlyWage2017 = hourlyWage)

col.2016 <- read_csv("Cost of Living/cost_of_living_2016.csv") %>%
  select(vintage,"Area", "Family Size", "Number of Adults", "Number of Workers", "Number of Children", "Age of Adults", "Yearly Cost", "Hourly Wage") %>%
  rename(
    countyName=Area,
    year=vintage,
    familySize = "Family Size",
    numberAdults = "Number of Adults",
    numberWorkers = "Number of Workers",
    numberChildren = "Number of Children",
    adultAge = "Age of Adults",
    yearlyCost = "Yearly Cost",
    hourlyWage = "Hourly Wage"
  ) %>%
  filter(year==2016) %>%
  select(-year) %>%
  filter(str_detect(countyName, "County") == TRUE,
         str_detect(countyName, "EDR") == FALSE,
         str_detect(countyName, "Seven County Mpls-St Paul, MN") == FALSE
  ) %>%
  mutate(
    countyName = str_replace(countyName, " County", "")
  ) %>%
  mutate(
    countyName = str_replace(countyName, " County", "")
  ) %>%
  full_join(mn_counties, col.2018, by = c("countyName")) %>%
  select(countyName, countyFIPS, familySize, numberAdults, numberWorkers, numberChildren, adultAge, yearlyCost, hourlyWage) %>%
  rename(yearlyCost2016 = yearlyCost,
         hourlyWage2016 = hourlyWage)

col.2018_2016 <- full_join(col.2018, col.2017, by = c("countyFIPS","familySize", "numberAdults", "numberWorkers", "numberChildren", "adultAge")) %>%
  select(-countyName.y) %>%
  rename(countyName=countyName.x) %>%
  full_join(col.2016, col.2018_2016, by = c("countyFIPS","familySize", "numberAdults", "numberWorkers", "numberChildren", "adultAge")) %>%
  select(-countyName.y) %>%
  rename(countyName=countyName.x)

yearly.cost.2018_2016 <- col.2018_2016 %>%
  select(-hourlyWage2018) %>%
  select(-hourlyWage2017) %>%
  select(-hourlyWage2016) %>%
  gather(year,yearlyCost,yearlyCost2018:yearlyCost2016) %>%
  mutate(
    year = replace(year, year =="yearlyCost2016" ,"2016"),
    year = replace(year, year =="yearlyCost2017" ,"2017"),
    year = replace(year, year =="yearlyCost2018" ,"2018"),
    countyFIPS = formatC(countyFIPS, width = 3, flag = "0")
  ) %>%
  write_csv("Cost of Living/yearly_cost_2016_2018.csv",append=FALSE)

hourly.wage.2018_2016 <- col.2018_2016 %>%
  select(-yearlyCost2018) %>%
  select(-yearlyCost2017) %>%
  select(-yearlyCost2016) %>%
  gather(year,hourlyWage,hourlyWage2018:hourlyWage2016) %>%
  mutate(
    year = replace(year, year =="hourlyWage2016" ,"2016"),
    year = replace(year, year =="hourlyWage2017" ,"2017"),
    year = replace(year, year =="hourlyWage2018" ,"2018"),
    countyFIPS = formatC(countyFIPS, width = 3, flag = "0")
  ) %>%
  write_csv("Cost of Living/hourly_wage_2016_2018.csv",append=FALSE)

col.2018_2016.tidy <- full_join(yearly.cost.2018_2016, hourly.wage.2018_2016, by = c("countyFIPS","year","familySize", "numberAdults", "numberWorkers", "numberChildren", "adultAge")) %>%
  select(-countyName.y) %>%
  rename(countyName=countyName.x) %>%
  write_csv("Cost of Living/cost_of_living_2016_2018.csv",append=FALSE)
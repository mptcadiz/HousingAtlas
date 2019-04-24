library(tidyverse)


# Cost of living by county ---------------------------------

col.2017 <- read_csv("Cost of Living/cost_of_living_2017.csv") %>%
  select("Area", "Family Size", "Number of Adults", "Number of Workers", "Number of Children", "Age of Adults", "Yearly Cost", "Hourly Wage") %>%
  rename(
    districtName=Area,
    familySize = "Family Size",
    numberAdults = "Number of Adults",
    numberWorkers = "Number of Workers",
    numberChildren = "Number of Children",
    adultAge = "Age of Adults",
    yearlyCost = "Yearly Cost",
    hourlyWage = "Hourly Wage"
  )

col.2016_2018 <- read_csv("Cost of Living/cost_of_living_2016_2018.csv") %>%
  select(vintage,"Area", "Family Size", "Number of Adults", "Number of Workers", "Number of Children", "Age of Adults", "Yearly Cost", "Hourly Wage") %>%
  rename(
    districtName=Area,
    year=vintage,
    familySize = "Family Size",
    numberAdults = "Number of Adults",
    numberWorkers = "Number of Workers",
    numberChildren = "Number of Children",
    adultAge = "Age of Adults",
    yearlyCost = "Yearly Cost",
    hourlyWage = "Hourly Wage"
  )
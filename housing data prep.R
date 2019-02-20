library(tidyverse)


# Median home value ---------------------------------

med.home.val.1990 <- read_csv("1990_median_home_val.csv") %>%
  select(YEAR, COUNTY,EST001) %>%
  rename(countyName = COUNTY,
         value1990=EST001,
         year=YEAR)

med.home.val.2000 <- read_csv("2000_median_home_val.csv") %>%
  select(YEAR, COUNTY,"Median Value") %>%
  rename(countyName = COUNTY,
         year=YEAR,
         value2000="Median Value")

med.home.val.2010 <- read_csv("2010_median_home_val.csv") %>%
  select(YEAR, COUNTY,JTIE001) %>%
  rename(countyName = COUNTY,
         year=YEAR,
         value2010=JTIE001) %>%
  mutate(
    countyName = str_replace(countyName, " County", ""),
    countyName = str_replace(countyName,"Lake of the Woods","Lake Of The Woods"),
    countyName = str_replace(countyName,"Lac qui Parle","Lac Qui Parle")
  )

med.home.val.1990_2000 <- full_join(med.home.val.1990, med.home.val.2000, by = c("countyName")) %>%
  select(-year.y, -year.x)

med.home.val.1990_2010 <- full_join(med.home.val.1990_2000, med.home.val.2010, by = c("countyName")) %>%
  select(-year) %>%
  gather(year,value,value1990:value2010) %>%
  mutate(
    year = replace(year, year =="value1990" ,"1990"),
    year = replace(year, year =="value2000" ,"2000"),
    year = replace(year, year =="value2010" ,"2010")) %>%
  rename(homeValue = value)

# Median year built ---------------------------------
med.year.built.1990 <- read_csv("1990_median_year_built.csv") %>%
  select (YEAR,COUNTY,EX8001) %>%
  rename(countyName = COUNTY,
         value1990=EX8001,
         year=YEAR) %>%
  mutate (countyName = as.factor(countyName))

med.year.built.2000 <- read_csv("2000_median_year_built.csv") %>%
  select (YEAR,COUNTY,GAK001) %>%
  rename(countyName = COUNTY,
         value2000=GAK001,
         year=YEAR) %>%
  mutate (countyName = as.factor(countyName))

med.year.built.2010 <- read_csv("2010_median_year_built.csv") %>%
  select (COUNTY,JSEE001) %>%
  mutate(
    COUNTY = str_replace(COUNTY, " County", ""),
    COUNTY = str_replace(COUNTY,"Lake of the Woods","Lake Of The Woods"),
    COUNTY = str_replace(COUNTY,"Lac qui Parle","Lac Qui Parle")
         ) %>%
  rename(countyName = COUNTY,
         value2010=JSEE001)

med.year.built.1990_2000 <- full_join(med.year.built.1990, med.year.built.2000, by = c("countyName")) %>%
  select(-year.y, -year.x) %>%
  mutate (
    countyName = str_replace(countyName,"St Louis","St. Louis")
  )

med.year.built.1990_2010 <- full_join(med.year.built.1990_2000, med.year.built.2010, by = c("countyName")) %>%
  gather(year,value,value1990:value2010) %>%
  mutate(
    year = replace(year, year =="value1990" ,"1990"),
    year = replace(year, year =="value2000" ,"2000"),
    year = replace(year, year =="value2010" ,"2010")) %>%
  rename(yearBuilt = value)

# Mortgage status  ---------------------------------

total.housing.units.1970_2010 <- read_csv("1970_2010_total_housing_units.csv") %>%
  select (COUNTY, A41AA1970, A41AA1980, A41AA1990, A41AA2000, A41AA2010) %>%
  mutate(
    COUNTY = str_replace(COUNTY, " County", ""),
    COUNTY = str_replace(COUNTY,"Lake of the Woods","Lake Of The Woods"),
    COUNTY = str_replace(COUNTY,"Lac qui Parle","Lac Qui Parle")
  ) %>%
  rename(countyName = COUNTY,
         "1970units"=A41AA1970,
         "1980units"=A41AA1980,
         "1990units"=A41AA1990,
         "2000units"=A41AA2000,
         "2010units"=A41AA2010)

mortgage.status.1990_2010 <- read_csv("1990_2010_mortgage_status.csv") %>%
  select (COUNTY, B80AA1990, B80AA2000, B80AA2010, B80AB1990, B80AB2000, B80AB2010, B80AC1990, B80AC2000, B80AC2010) %>%
  mutate(
    COUNTY = str_replace(COUNTY, " County", ""),
    COUNTY = str_replace(COUNTY,"Lake of the Woods","Lake Of The Woods"),
    COUNTY = str_replace(COUNTY,"Lac qui Parle","Lac Qui Parle")
  ) %>%
  rename(countyName = COUNTY,
         "1990_mortgage_owner"=B80AA1990,
         "2000_mortgage_owner"=B80AA2000,
         "2010_mortgage_owner"=B80AA2010,
         "1990_free_owner"=B80AB1990,
         "2000_free_owner"=B80AB2000,
         "2010_free"=B80AB2010,
         "1990_mortgage_renter"=B80AC1990,
         "2000_mortgage_renter"=B80AC2000,
         "2010_mortgage_renter"=B80AC2010)
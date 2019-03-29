library(tidyverse)


# Median year built ---------------------------------
med.year.built.1990 <- read_csv("Housing/Median Year Built/1990_median_year_built.csv") %>%
  mutate(COUNTYA = formatC(COUNTYA, width = 3, flag = "0")) %>%
  select (YEAR,COUNTY,COUNTYA,EX8001) %>%
  rename(countyName = COUNTY,
         value1990=EX8001,
         year=YEAR,
         countyFIPS=COUNTYA) %>%
  mutate (countyName = as.factor(countyName))

med.year.built.2000 <- read_csv("Housing/Median Year Built/2000_median_year_built.csv") %>%
  mutate(COUNTYA = formatC(COUNTYA, width = 3, flag = "0")) %>%
  select (YEAR,COUNTY,COUNTYA,GAK001) %>%
  rename(countyName = COUNTY,
         value2000=GAK001,
         year=YEAR,
         countyFIPS=COUNTYA) %>%
  mutate (countyName = as.factor(countyName))

med.year.built.2010 <- read_csv("Housing/Median Year Built/2010_median_year_built.csv") %>%
  mutate(COUNTYA = formatC(COUNTYA, width = 3, flag = "0")) %>%
  select (COUNTY,COUNTYA,JSEE001) %>%
  rename(countyName = COUNTY,
         value2010=JSEE001,
         countyFIPS=COUNTYA)

med.year.built.2017 <- read_csv("Housing/Median Year Built/2017_median_year_built.csv") %>%
  mutate(COUNTYA = formatC(COUNTYA, width = 3, flag = "0")) %>%
  select(COUNTY,COUNTYA, AH40E001) %>%
  rename(countyName = COUNTY,
         value2017=AH40E001,
         countyFIPS=COUNTYA)

med.year.built.1990_2000 <- full_join(med.year.built.1990, med.year.built.2000, by = c("countyFIPS")) %>%
  select(-year.y, -year.x, -countyName.y)

med.year.built.1990_2017 <- full_join(med.year.built.1990_2000, med.year.built.2010, by = c("countyFIPS")) %>%
  select(-countyName) %>%
  rename(countyName=countyName.x) %>%
  full_join(med.year.built.2017, med.year.built.1990_2017, by = c("countyFIPS")) %>%
  select(-countyName.y, -countyFIPS) %>%
  rename(countyName=countyName.x) %>%
  gather(year,yearBuilt,value1990:value2017) %>%
  mutate(
    year = replace(year, year =="value1990" ,"1990"),
    year = replace(year, year =="value2000" ,"2000"),
    year = replace(year, year =="value2010" ,"2010"),
    year = replace(year, year =="value2017" ,"2017")) %>%
    write_csv("Housing/Median Year Built/med_year_built_1990_2017.csv",append=FALSE)
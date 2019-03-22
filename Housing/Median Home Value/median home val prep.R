library(tidyverse)


# Median home value ---------------------------------

med.home.val.1990 <- read_csv("1990_median_home_val.csv") %>%
  mutate(COUNTYA = formatC(COUNTYA, width = 3, flag = "0")) %>%
  select(COUNTY,COUNTYA,EST001) %>%
  rename(countyName = COUNTY,
         value1990=EST001,
         countyFIPS=COUNTYA)

med.home.val.2000 <- read_csv("2000_median_home_val.csv") %>%
  mutate(COUNTYA = formatC(COUNTYA, width = 3, flag = "0")) %>%
  select(COUNTY,COUNTYA, "Median Value") %>%
  rename(countyName = COUNTY,
         value2000="Median Value",
         countyFIPS=COUNTYA)

med.home.val.2010 <- read_csv("2010_median_home_val.csv") %>%
  mutate(COUNTYA = formatC(COUNTYA, width = 3, flag = "0")) %>%
  select(COUNTY,COUNTYA, JTIE001) %>%
  rename(countyName = COUNTY,
         value2010=JTIE001,
         countyFIPS=COUNTYA)

med.home.val.2017 <- read_csv("2017_median_home_val.csv") %>%
  mutate(COUNTYA = formatC(COUNTYA, width = 3, flag = "0")) %>%
  select(COUNTY,COUNTYA, AH53E001) %>%
  rename(countyName = COUNTY,
         value2017=AH53E001,
         countyFIPS=COUNTYA)

#med.home.val.1990_2000 <- full_join(med.home.val.1990, med.home.val.2000, by = c("countyFIPS")) %>%
#  select(-year.y, -year.x, -countyName.y)

med.home.val.1990_2010 <- full_join(med.home.val.1990, med.home.val.2000, by = c("countyFIPS")) %>%
  select(-countyName.y) %>%
  rename (countyName=countyName.x) %>%
  full_join(med.home.val.2010,med.home.val.1990_2000, by = c("countyFIPS")) %>%
  select(-countyName.y) %>%
  rename (countyName=countyName.x) %>%
  full_join(med.home.val.2017, med.home.val.1990_2010, by = c("countyFIPS")) %>%
  select(-countyName.y) %>%
  rename(countyName=countyName.x) %>%
  gather(year,homeValue,value1990:value2017) %>%
  mutate(
    year = replace(year, year =="value1990" ,"1990"),
    year = replace(year, year =="value2000" ,"2000"),
    year = replace(year, year =="value2010" ,"2010"),
    year = replace(year, year =="value2017" ,"2017"))
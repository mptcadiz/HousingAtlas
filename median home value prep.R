library(tidyverse)


# Prep city and township data 1900 - 2000 ---------------------------------

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

med.home.val.1990_2000 <- full_join(med.home.val.1990, med.home.val.2000, by = c("countyName")) %>%
  select(-year.y, -year.x) %>%
  rename ("1990"=value1990,
          "2000"=value2000)
  gather(year,1990:2000)
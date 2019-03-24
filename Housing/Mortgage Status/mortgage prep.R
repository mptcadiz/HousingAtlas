library(tidyverse)


# Mortgage status  ---------------------------------

mortgage.status.1990_2010 <- read_csv("1990_2010_mortgage_status.csv") %>%
  mutate(COUNTYFP = formatC(COUNTYFP, width = 3, flag = "0")) %>%
  select (COUNTY, COUNTYFP, B80AA1990, B80AA2000, B80AA2010, B80AB1990, B80AB2000, B80AB2010) %>%
  rename(countyName = COUNTY,
         countyFIPS = COUNTYFP,
         mortgage_owner_1990=B80AA1990,
         mortgage_owner_2000=B80AA2000,
         mortgage_owner_2010=B80AA2010,
         free_owner_1990=B80AB1990,
         free_owner_2000=B80AB2000,
         free_owner_2010=B80AB2010) %>%
  mutate(mortgage_owner_1990 = as.numeric(mortgage_owner_1990),
         mortgage_owner_2000 = as.numeric(mortgage_owner_2000),
         mortgage_owner_2010 = as.numeric(mortgage_owner_2010)
         ) %>%
  mutate(
    total1990 = mortgage_owner_1990 + free_owner_1990,
    total2000 = mortgage_owner_2000 + free_owner_2000,
    total2010 = mortgage_owner_2010 + free_owner_2010
  ) %>%
  mutate (
    percentFree1990=free_owner_1990/total1990,
    percentFree2000=free_owner_2000/total2000,  
    percentFree2010=free_owner_2010/total2010
  )
  
mortgages.1990_2010 <- mortgage.status.1990_2010 %>%
  select(countyName,mortgage_owner_1990:mortgage_owner_2010) %>%
  gather(year,mortgage,mortgage_owner_1990:mortgage_owner_2010) %>%
  mutate(
      year = replace(year, year == "mortgage_owner_1990","1990"),
      year = replace(year, year =="mortgage_owner_2000","2000"),
      year = replace(year, year =="mortgage_owner_2010","2010"))
  
free.1990_2010 <- mortgage.status.1990_2010 %>%
  select(countyName,free_owner_1990:free_owner_2010) %>%
  gather(year,free,free_owner_1990:free_owner_2010) %>%
  mutate(
    year = replace(year, year == "free_owner_1990","1990"),
    year = replace(year, year =="free_owner_2000","2000"),
    year = replace(year, year =="free_owner_2010","2010"))

total.1990_2010 <- mortgage.status.1990_2010 %>%
  select(countyName,total1990:total2010) %>%
  gather(year,total,total1990:total2010) %>%
  mutate(
    year = replace(year, year == "total1990","1990"),
    year = replace(year, year =="total2000","2000"),
    year = replace(year, year =="total2010","2010"))

percentfree.1990_2010 <- mortgage.status.1990_2010 %>%
  select(countyName,percentFree1990:percentFree2010) %>%
  gather(year,percentFree,percentFree1990:percentFree2010) %>%
  mutate(
    year = replace(year, year == "percentFree1990","1990"),
    year = replace(year, year =="percentFree2000","2000"),
    year = replace(year, year =="percentFree2010","2010"))

tidymortgage.status.1990_2010 <- full_join(mortgages.1990_2010, free.1990_2010, by = c("countyName", "year")) %>%
  full_join(total.1990_2010, tidymortgage.status.1990_2010, by = c("countyName","year")) %>%
  full_join(percentfree.1990_2010, tidymortgage.status.1990_2010, by = c("countyName", "year"))
library(tidyverse)


# Graduation rates by district ---------------------------------

drop.2017 <- read_csv("2017_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    dropRate = "Four Year Percent",
    endStatus = "Ending Status"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Dropout") %>%
  #drop_na(dropRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(dropRate=mean(dropRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  rename(dropRate2017 = dropRate) %>%
  drop_na(districtName)




grad.2012_2018 <- full_join(grad.2017, grad.2016, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.2015, grad.2012_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.2014, grad.2012_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.2013, grad.2012_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.2012, grad.2012_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName)

grad.2012_2018.tidy <- grad.2012_2018 %>%
  gather(year,dropRate,dropRate2012:dropRate2017) %>%
  mutate(
    year = replace(year, year =="dropRate2012" ,"2012"),
    year = replace(year, year =="dropRate2013" ,"2013"),
    year = replace(year, year =="dropRate2014" ,"2014"),
    year = replace(year, year =="dropRate2015" ,"2015"),
    year = replace(year, year =="dropRate2016" ,"2016"),
    year = replace(year, year =="dropRate2017" ,"2017"))
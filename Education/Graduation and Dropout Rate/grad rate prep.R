library(tidyverse)


# Graduation rates by district ---------------------------------

grad.2017 <- read_csv("2017_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    gradRate = "Four Year Percent",
    endStatus = "Ending Status"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  #drop_na(gradRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(gradRate=mean(gradRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  rename(gradRate2017 = gradRate) %>%
  drop_na(districtName)

grad.2016 <- read_csv("2016_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    gradRate = "Four Year Percent",
    endStatus = "Ending Status"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  #drop_na(gradRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(gradRate=mean(gradRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  rename(gradRate2016 = gradRate) %>%
  drop_na(districtName)

grad.2015 <- read_csv("2015_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    gradRate = "Four Year Percent",
    endStatus = "Ending Status"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  #drop_na(gradRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(gradRate=mean(gradRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  rename(gradRate2015 = gradRate) %>%
  drop_na(districtName)

grad.2014 <- read_csv("2014_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    gradRate = "Four Year Percent",
    endStatus = "Ending Status"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  #drop_na(gradRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(gradRate=mean(gradRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  rename(gradRate2014 = gradRate) %>%
  drop_na(districtName)

grad.2013 <- read_csv("2013_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    gradRate = "Four Year Percent",
    endStatus = "Ending Status"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  #drop_na(gradRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(gradRate=mean(gradRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  rename(gradRate2013 = gradRate) %>%
  drop_na(districtName)

grad.2012 <- read_csv("2012_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    gradRate = "Four Year Percent",
    endStatus = "Ending Status"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  #drop_na(gradRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(gradRate=mean(gradRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  rename(gradRate2012 = gradRate) %>%
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
  gather(year,gradRate,gradRate2012:gradRate2017) %>%
  mutate(
    year = replace(year, year =="gradRate2012" ,"2012"),
    year = replace(year, year =="gradRate2013" ,"2013"),
    year = replace(year, year =="gradRate2014" ,"2014"),
    year = replace(year, year =="gradRate2015" ,"2015"),
    year = replace(year, year =="gradRate2016" ,"2016"),
    year = replace(year, year =="gradRate2017" ,"2017"))
library(tidyverse)


# Dropout rates by district ---------------------------------

drop.2017 <- read_csv("Education/Graduation and Dropout Rate/2017_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent", "Demographic Category") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    dropRate = "Four Year Percent",
    endStatus = "Ending Status",
    category = "Demographic Category"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(
    endStatus=="Dropout",
    category=="A"
         ) %>%
  drop_na(dropRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(dropRate=mean(dropRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  rename(dropRate2017 = dropRate) %>%
  drop_na(districtName)

drop.2016 <- read_csv("Education/Graduation and Dropout Rate/2016_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent", "Demographic Category") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    dropRate = "Four Year Percent",
    endStatus = "Ending Status",
    category = "Demographic Category"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(
    endStatus=="Dropout",
    category=="A"
  ) %>%
  drop_na(dropRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(dropRate=mean(dropRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  rename(dropRate2016 = dropRate) %>%
  drop_na(districtName)

drop.2015 <- read_csv("Education/Graduation and Dropout Rate/2015_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent", "Demographic Category") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    dropRate = "Four Year Percent",
    endStatus = "Ending Status",
    category = "Demographic Category"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(
    endStatus=="Dropout",
    category=="A"
  ) %>%
  drop_na(dropRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(dropRate=mean(dropRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  rename(dropRate2015 = dropRate) %>%
  drop_na(districtName)

drop.2014 <- read_csv("Education/Graduation and Dropout Rate/2014_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent", "Demographic Category") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    dropRate = "Four Year Percent",
    endStatus = "Ending Status",
    category = "Demographic Category"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(
    endStatus=="Dropout",
    category=="A"
  ) %>%
  drop_na(dropRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(dropRate=mean(dropRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  rename(dropRate2014 = dropRate) %>%
  drop_na(districtName)

drop.2013 <- read_csv("Education/Graduation and Dropout Rate/2013_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent", "Demographic Category") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    dropRate = "Four Year Percent",
    endStatus = "Ending Status",
    category = "Demographic Category"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(
    endStatus=="Dropout",
    category=="A"
  ) %>%
  drop_na(dropRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(dropRate=mean(dropRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  ) %>%
  rename(dropRate2013 = dropRate) %>%
  drop_na(districtName)

drop.2012 <- read_csv("Education/Graduation and Dropout Rate/2012_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent", "Demographic Category") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    dropRate = "Four Year Percent",
    endStatus = "Ending Status",
    category = "Demographic Category"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(
    endStatus=="Dropout",
    category=="A"
  ) %>%
  drop_na(dropRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(dropRate=mean(dropRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  ) %>%
  rename(dropRate2012 = dropRate) %>%
  drop_na(districtName)

drop.2012_2018 <- full_join(drop.2017, drop.2016, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(drop.2015, drop.2012_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(drop.2014, drop.2012_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(drop.2013, drop.2012_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(drop.2012, drop.2012_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName)  %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0323", "02", "Franconia", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0025", "01", "Pine Point", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0264", "01", "Herman-Norcross", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0402", "01", "Hendricks", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0403", "01", "Ivanhoe", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0404", "01", "Lake Benton", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0415", "01", "Lynd", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0635", "01", "Milroy", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0707", "01", "Nett Lake", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0801", "01", "Browns Valley", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0852", "01", "Campbell-Tintah", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2907", "01", "Round Lake-Brewster", NA, NA, NA, NA, NA, NA))

drop.2012_2018.tidy <- drop.2012_2018 %>%
  gather(year,dropRate,dropRate2012:dropRate2017) %>%
  mutate(
    year = replace(year, year =="dropRate2012" ,"2012"),
    year = replace(year, year =="dropRate2013" ,"2013"),
    year = replace(year, year =="dropRate2014" ,"2014"),
    year = replace(year, year =="dropRate2015" ,"2015"),
    year = replace(year, year =="dropRate2016" ,"2016"),
    year = replace(year, year =="dropRate2017" ,"2017")) %>%
    mutate(districtName = str_to_title(districtName)) %>%
  write_csv("Education/Graduation and Dropout Rate/drop_rate_2012_2018.csv",append=FALSE)

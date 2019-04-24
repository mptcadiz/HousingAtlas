library(tidyverse)


# Dropout rates by district ---------------------------------

drop.ethnicity.2017 <- read_csv("Education/Graduation and Dropout Rate/2017_grad.csv") %>%
  select ("District Number","District Type", "District Name","Demographic Category", "Demographic Description",  "Ending Status", "Four Year Count", "Four Year Total") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    endStatus = "Ending Status",
    category = "Demographic Category",
    description = "Demographic Description",
    dropCount = "Four Year Count",
    totalStudents = "Four Year Total"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Dropout") %>%
  filter(category=="B" | category=="C" | category=="E" | category=="F" | category=="H") %>%
  select(districtNumber, districtType, districtName, dropCount, totalStudents) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    dropCount=sum(dropCount, na.rm=TRUE),
    totalStudents=sum(totalStudents, na.rm=TRUE)
    ) %>%
  mutate(dropRate=dropCount/totalStudents) %>%
  drop_na(dropRate) %>%
  mutate(districtName=toupper(districtName)) %>%
  select(districtNumber, districtType, districtName, dropRate) %>%
  rename(dropRate2017 = dropRate) %>%
  drop_na(districtName)

drop.ethnicity.2016 <- read_csv("Education/Graduation and Dropout Rate/2016_grad.csv") %>%
  select ("District Number","District Type", "District Name","Demographic Category", "Demographic Description",  "Ending Status", "Four Year Count", "Four Year Total") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    endStatus = "Ending Status",
    category = "Demographic Category",
    description = "Demographic Description",
    dropCount = "Four Year Count",
    totalStudents = "Four Year Total"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Dropout") %>%
  filter(category=="B" | category=="C" | category=="E" | category=="F" | category=="H") %>%
  select(districtNumber, districtType, districtName, dropCount, totalStudents) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    dropCount=sum(dropCount, na.rm=TRUE),
    totalStudents=sum(totalStudents, na.rm=TRUE)
  ) %>%
  mutate(dropRate=dropCount/totalStudents) %>%
  drop_na(dropRate) %>%
  mutate(districtName=toupper(districtName)) %>%
  select(districtNumber, districtType, districtName, dropRate) %>%
  rename(dropRate2016 = dropRate) %>%
  drop_na(districtName)

drop.ethnicity.2015 <- read_csv("Education/Graduation and Dropout Rate/2015_grad.csv") %>%
  select ("District Number","District Type", "District Name","Demographic Category", "Demographic Description",  "Ending Status", "Four Year Count", "Four Year Total") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    endStatus = "Ending Status",
    category = "Demographic Category",
    description = "Demographic Description",
    dropCount = "Four Year Count",
    totalStudents = "Four Year Total"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Dropout") %>%
  filter(category=="B" | category=="C" | category=="E" | category=="F" | category=="H") %>%
  select(districtNumber, districtType, districtName, dropCount, totalStudents) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    dropCount=sum(dropCount, na.rm=TRUE),
    totalStudents=sum(totalStudents, na.rm=TRUE)
  ) %>%
  mutate(dropRate=dropCount/totalStudents) %>%
  drop_na(dropRate) %>%
  mutate(districtName=toupper(districtName)) %>%
  select(districtNumber, districtType, districtName, dropRate) %>%
  rename(dropRate2015 = dropRate) %>%
  drop_na(districtName)

drop.ethnicity.2014 <- read_csv("Education/Graduation and Dropout Rate/2014_grad.csv") %>%
  select ("District Number","District Type", "District Name","Demographic Category", "Demographic Description",  "Ending Status", "Four Year Count", "Four Year Total") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    endStatus = "Ending Status",
    category = "Demographic Category",
    description = "Demographic Description",
    dropCount = "Four Year Count",
    totalStudents = "Four Year Total"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Dropout") %>%
  filter(category=="B" | category=="C" | category=="E" | category=="F" | category=="H") %>%
  select(districtNumber, districtType, districtName, dropCount, totalStudents) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    dropCount=sum(dropCount, na.rm=TRUE),
    totalStudents=sum(totalStudents, na.rm=TRUE)
  ) %>%
  mutate(dropRate=dropCount/totalStudents) %>%
  drop_na(dropRate) %>%
  mutate(districtName=toupper(districtName)) %>%
  select(districtNumber, districtType, districtName, dropRate) %>%
  rename(dropRate2014 = dropRate) %>%
  drop_na(districtName)

drop.ethnicity.2013 <- read_csv("Education/Graduation and Dropout Rate/2013_grad.csv") %>%
  select ("District Number","District Type", "District Name","Demographic Category", "Demographic Description",  "Ending Status", "Four Year Count", "Four Year Total") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    endStatus = "Ending Status",
    category = "Demographic Category",
    description = "Demographic Description",
    dropCount = "Four Year Count",
    totalStudents = "Four Year Total"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Dropout") %>%
  filter(category=="B" | category=="C" | category=="E" | category=="F" | category=="H") %>%
  select(districtNumber, districtType, districtName, dropCount, totalStudents) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    dropCount=sum(dropCount, na.rm=TRUE),
    totalStudents=sum(totalStudents, na.rm=TRUE)
  ) %>%
  mutate(dropRate=dropCount/totalStudents) %>%
  drop_na(dropRate) %>%
  mutate(districtName=toupper(districtName)) %>%
  select(districtNumber, districtType, districtName, dropRate) %>%
  rename(dropRate2013 = dropRate) %>%
  drop_na(districtName)

drop.ethnicity.2012 <- read_csv("Education/Graduation and Dropout Rate/2012_grad.csv") %>%
  select ("District Number","District Type", "District Name","Demographic Category", "Demographic Description",  "Ending Status", "Four Year Count", "Four Year Total") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    endStatus = "Ending Status",
    category = "Demographic Category",
    description = "Demographic Description",
    dropCount = "Four Year Count",
    totalStudents = "Four Year Total"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Dropout") %>%
  filter(category=="B" | category=="C" | category=="E" | category=="F" | category=="H") %>%
  select(districtNumber, districtType, districtName, dropCount, totalStudents) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    dropCount=sum(dropCount, na.rm=TRUE),
    totalStudents=sum(totalStudents, na.rm=TRUE)
  ) %>%
  mutate(dropRate=dropCount/totalStudents) %>%
  drop_na(dropRate) %>%
  mutate(districtName=toupper(districtName)) %>%
  select(districtNumber, districtType, districtName, dropRate) %>%
  rename(dropRate2012 = dropRate) %>%
  drop_na(districtName)

drop.ethnicity.2012_2017 <- full_join(drop.ethnicity.2017, drop.ethnicity.2016, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(drop.ethnicity.2015, drop.ethnicity.2012_2017, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(drop.ethnicity.2014, drop.ethnicity.2012_2017, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(drop.ethnicity.2013, drop.ethnicity.2012_2017, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(drop.ethnicity.2012, drop.ethnicity.2012_2017, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
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

drop.ethnicity.2012_2017.tidy <- drop.ethnicity.2012_2017 %>%
  gather(year,dropRate,dropRate2012:dropRate2017) %>%
  mutate(
    year = replace(year, year =="dropRate2012" ,"2012"),
    year = replace(year, year =="dropRate2013" ,"2013"),
    year = replace(year, year =="dropRate2014" ,"2014"),
    year = replace(year, year =="dropRate2015" ,"2015"),
    year = replace(year, year =="dropRate2016" ,"2016"),
    year = replace(year, year =="dropRate2017" ,"2017")) %>%
  write_csv("Education/Graduation and Dropout Rate/drop_rate_ethnicity_2012_2017.csv",append=FALSE)
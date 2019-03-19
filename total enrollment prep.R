library(tidyverse)


# Ethnicity of students enrolled (by district) ---------------------------------

enrolled.2018 <- read_csv("2018_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2018 = totalStudents) %>%
  drop_na(districtName)

enrolled.2017 <- read_csv("2017_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
    ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2017 = totalStudents) %>%
  drop_na(districtName)

enrolled.2016 <- read_csv("2016_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2016 = totalStudents) %>%
  drop_na(districtName)

enrolled.2015 <- read_csv("2015_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2015 = totalStudents) %>%
  drop_na(districtName)

enrolled.2014 <- read_csv("2014_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2014 = totalStudents) %>%
  drop_na(districtName)

enrolled.2013 <- read_csv("2013_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2013 = totalStudents) %>%
  drop_na(districtName)

enrolled.2012 <- read_csv("2012_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2012 = totalStudents) %>%
  drop_na(districtName)

enrolled.2011 <- read_csv("2011_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2011 = totalStudents) %>%
  drop_na(districtName)

enrolled.2010 <- read_csv("2010_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2010 = totalStudents) %>%
  drop_na(districtName)

enrolled.2009 <- read_csv("2009_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2009 = totalStudents) %>%
  drop_na(districtName)

enrolled.2008 <- read_csv("2008_enrolled_ethnicity_district.csv") %>%
  select (DistrictNumber, DistrictType, DistrictName, TotalStudents) %>%
  rename(
    districtNumber = DistrictNumber,
    districtType = DistrictType,
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2008 = totalStudents) %>%
  drop_na(districtName)

enrolled.2007 <- read_csv("2007_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2007 = totalStudents) %>%
  drop_na(districtName)

enrolled.2006 <- read_csv("2006_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2006 = totalStudents) %>%
  drop_na(districtName)

enrolled.2005 <- read_csv("2005_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2005 = totalStudents) %>%
  drop_na(districtName)

enrolled.2004 <- read_csv("2004_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2004 = totalStudents) %>%
  drop_na(districtName)

enrolled.2003 <- read_csv("2003_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2003 = totalStudents) %>%
  drop_na(districtName)

enrolled.2002 <- read_csv("2002_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_tye, dst_nam, tot_tot) %>%
  rename(
    districtNumber = dst_num,
    districtType = dst_tye,
    districtName= dst_nam,
    totalStudents=tot_tot
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2002 = totalStudents) %>%
  drop_na(districtName)

enrolled.2001 <- read_csv("2001_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_tye, dst_nam, tot_tot) %>%
  rename(
    districtNumber = dst_num,
    districtType = dst_tye,
    districtName= dst_nam,
    totalStudents=tot_tot
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2001 = totalStudents) %>%
  drop_na(districtName)

enrolled.2000 <- read_csv("2000_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_tye, dst_nam, tot_tot) %>%
  rename(
    districtNumber = dst_num,
    districtType = dst_tye,
    districtName= dst_nam,
    totalStudents=tot_tot
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2000 = totalStudents) %>%
  drop_na(districtName)

enrolled.2018_2000 <- full_join(enrolled.2018, enrolled.2017, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2016, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2015, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2014, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2013, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2012, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2011, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2010, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2009, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2008, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2007, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2006, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2005, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2004, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2003, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2002, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2001, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(enrolled.2000, enrolled.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName)
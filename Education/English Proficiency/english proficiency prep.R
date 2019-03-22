library(tidyverse)


# English proficiency by district ---------------------------------

# 2019 ---------------------------------
english.2019 <- read_csv("2019_english.csv") %>%
  select ("District Number", "District Type", "District Name","Total Enrollment","Total English learner Identified Count") %>%
  rename(
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    LEPIdentifiedK12="Total English learner Identified Count",
    totalStudents="Total Enrollment"
  ) %>%
  drop_na(districtName) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPIdentifiedK12 = sum(LEPIdentifiedK12),
    totalStudents=sum(totalStudents)
  ) %>%
  ungroup() %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2019 = percentIdentified)

# 2018 ---------------------------------
english.2018 <- read_csv("2018_english.csv") %>%
  select ("District Number", "District Type", "District Name","Total Enrollment","Total English learner Identified Count") %>%
  rename(
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    LEPIdentifiedK12="Total English learner Identified Count",
    totalStudents="Total Enrollment"
  ) %>%
  drop_na(districtName) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPIdentifiedK12 = sum(LEPIdentifiedK12),
    totalStudents=sum(totalStudents)
  ) %>%
  ungroup() %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2018 = percentIdentified)

# 2017 ---------------------------------
english.2017 <- read_csv("2017_english.csv") %>%
  select ("District Number", "District Type", "District Name","Total Enrollment","Total English learner Identified Count") %>%
  rename(
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    LEPIdentifiedK12="Total English learner Identified Count",
    totalStudents="Total Enrollment"
  ) %>%
  drop_na(districtName) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPIdentifiedK12 = sum(LEPIdentifiedK12),
    totalStudents=sum(totalStudents)
  ) %>%
  ungroup() %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2017 = percentIdentified)

# 2016 ---------------------------------
english.2016 <- read_csv("2016_english.csv") %>%
  select ("District Number", "District Type", "District Name","Total Enrollment","Total English learner Identified Count") %>%
  rename(
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    LEPIdentifiedK12="Total English learner Identified Count",
    totalStudents="Total Enrollment"
  ) %>%
  drop_na(districtName) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPIdentifiedK12 = sum(LEPIdentifiedK12),
    totalStudents=sum(totalStudents)
  ) %>%
  ungroup() %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2016 = percentIdentified)

# 2015 ---------------------------------
english.2015 <- read_csv("2015_english.csv") %>%
  select ("District Number", "District Type", "District Name","Total Enrollment","Total English learner Identified Count") %>%
  rename(
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    LEPIdentifiedK12="Total English learner Identified Count",
    totalStudents="Total Enrollment"
  ) %>%
  drop_na(districtName) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPIdentifiedK12 = sum(LEPIdentifiedK12),
    totalStudents=sum(totalStudents)
  ) %>%
  ungroup() %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2015 = percentIdentified)

# 2014 ---------------------------------
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
  drop_na(districtName)

english.2014 <- read_csv("2014_english.csv") %>%
  select ("District Number", "District Type", "District Name", "Total English learner Identified Count") %>%
  rename(
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    LEPIdentifiedK12="Total English learner Identified Count"
  ) %>%
  drop_na(districtName) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  ungroup() %>%
  drop_na(districtName) %>%
  full_join(enrolled.2014, english.2014, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2014 = percentIdentified)

# 2013 ---------------------------------
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

english.2013 <- read_csv("2013_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12, LEPServedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPServedK12 = sum(LEPServedK12),
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  mutate(percentServed=LEPServedK12/LEPIdentifiedK12) %>%
  ungroup() %>%
  drop_na(districtName) %>%
  full_join(enrolled.2013, english.2013, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents2013
  ) %>%
  select(districtNumber, districtType, districtName, percentServed, percentIdentified) %>%
  rename (percentServed2013 = percentServed,
          percentIdentified2013 = percentIdentified)

# 2012 ---------------------------------
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

english.2012 <- read_csv("2012_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12, LEPServedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    LEPIdentifiedK12 = na_if(LEPIdentifiedK12, "NULL"),
    LEPIdentifiedK12 = as.numeric(LEPIdentifiedK12),
    LEPServedK12 = na_if(LEPServedK12, "NULL"),
    LEPServedK12 = as.numeric(LEPServedK12),
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPServedK12 = sum(LEPServedK12),
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  mutate(percentServed=LEPServedK12/LEPIdentifiedK12) %>%
  ungroup() %>%
  full_join(enrolled.2012, english.2012, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents2012
  ) %>%
  select(districtNumber, districtType, districtName, percentServed, percentIdentified) %>%
  rename (percentServed2012 = percentServed,
          percentIdentified2012 = percentIdentified) %>%
  drop_na(districtName)

# 2011 ---------------------------------
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

english.2011 <- read_csv("2011_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12, LEPServedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    LEPIdentifiedK12 = na_if(LEPIdentifiedK12, "NULL"),
    LEPIdentifiedK12 = as.numeric(LEPIdentifiedK12),
    LEPServedK12 = na_if(LEPServedK12, "NULL"),
    LEPServedK12 = as.numeric(LEPServedK12),
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPServedK12 = sum(LEPServedK12),
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  mutate(percentServed=LEPServedK12/LEPIdentifiedK12) %>%
  ungroup() %>%
  full_join(enrolled.2011, english.2011, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents2011
  ) %>%
  select(districtNumber, districtType, districtName, percentServed, percentIdentified) %>%
  rename (percentServed2011 = percentServed,
          percentIdentified2011 = percentIdentified) %>%
  drop_na(districtName)

# 2010 ---------------------------------
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

english.2010 <- read_csv("2010_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12, LEPServedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPServedK12 = sum(LEPServedK12),
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  mutate(percentServed=LEPServedK12/LEPIdentifiedK12) %>%
  ungroup() %>%
  full_join(enrolled.2010, english.2010, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents2010
  ) %>%
  select(districtNumber, districtType, districtName, percentServed, percentIdentified) %>%
  rename (percentServed2010 = percentServed,
          percentIdentified2010 = percentIdentified) %>%
  drop_na(districtName)

# 2009 ---------------------------------
# 2009 stops using identified vs. served
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
  drop_na(districtName)

english.2009 <- read_csv("2009_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPK12 = sum(LEPK12)
  ) %>%
  ungroup() %>%
  full_join(enrolled.2009, english.2009, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2009 = percentIdentified) %>%
  drop_na(districtName)

# 2008 ---------------------------------
#problem with summarize sum LEPK12
enrolled.2008 <- read_csv("2008_enrolled_ethnicity_district.csv") %>%
  select (DistrictNumber, DistrictType, DistrictName, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    districtNumber=DistrictNumber,
    districtType=DistrictType,
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
  drop_na(districtName)

#summarize to sum LEPK12 won't work, can't figure out why
english.2008 <- read_csv("2008_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  drop_na(districtName) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPK12 = sum(LEPK12, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  full_join(enrolled.2008, english.2008, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2008 = percentIdentified) %>%
  drop_na(districtName)

# 2007 ---------------------------------
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
  drop_na(districtName)

english.2007 <- read_csv("2007_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPK12 = sum(LEPK12)
  ) %>%
  ungroup() %>%
  full_join(enrolled.2007, english.2007, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2007 = percentIdentified) %>%
  drop_na(districtName)

# 2006 ---------------------------------
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
  drop_na(districtName)

english.2006 <- read_csv("2006_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPK12 = sum(LEPK12)
  ) %>%
  ungroup() %>%
  full_join(enrolled.2006, english.2006, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2006 = percentIdentified) %>%
  drop_na(districtName)

# 2005 ---------------------------------
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
  drop_na(districtName)

english.2005 <- read_csv("2005_english.csv") %>%
  select (dst_num, dst_tye, dst_nam, lep_k12) %>%
  rename(
    districtName=dst_nam,
    districtNumber = dst_num,
    districtType=dst_tye,
    LEPK12=lep_k12
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPK12 = sum(LEPK12)
  ) %>%
  ungroup() %>%
  full_join(enrolled.2005, english.2005, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2005 = percentIdentified) %>%
  drop_na(districtName)

# 2004 ---------------------------------
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
  drop_na(districtName)

english.2004 <- read_csv("2004_english.csv") %>%
  select (dst_num, dst_tye, dst_nam, lep_k12) %>%
  rename(
    districtName=dst_nam,
    districtNumber = dst_num,
    districtType=dst_tye,
    LEPK12=lep_k12
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPK12 = sum(LEPK12)
  ) %>%
  ungroup() %>%
  full_join(enrolled.2004, english.2004, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2004 = percentIdentified) %>%
  drop_na(districtName)

# 2003 ---------------------------------
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
  drop_na(districtName)

english.2003 <- read_csv("2003_english.csv") %>%
  select (dst_num, dst_tye, dst_nam, lep_k12) %>%
  rename(
    districtName=dst_nam,
    districtNumber = dst_num,
    districtType=dst_tye,
    LEPK12=lep_k12
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPK12 = sum(LEPK12)
  ) %>%
  ungroup() %>%
  full_join(enrolled.2003, english.2003, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2003 = percentIdentified) %>%
  drop_na(districtName)

# 2002 ---------------------------------
enrolled.2002 <- read_csv("2002_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_tye, dst_nam, tot_tot) %>%
  rename(
    districtName=dst_nam,
    districtNumber = dst_num,
    districtType=dst_tye,
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
  drop_na(districtName)

english.2002 <- read_csv("2002_english.csv") %>%
  select (dst_num, dst_tye, dst_nam, lep_k12) %>%
  rename(
    districtName=dst_nam,
    districtNumber = dst_num,
    districtType=dst_tye,
    LEPK12=lep_k12
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPK12 = sum(LEPK12)
  ) %>%
  ungroup() %>%
  full_join(enrolled.2002, english.2002, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2002 = percentIdentified) %>%
  drop_na(districtName)

# 2001 ---------------------------------


# 2000 ---------------------------------
enrolled.2000 <- read_csv("2000_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_tye, dst_nam, tot_tot) %>%
  rename(
    districtName=dst_nam,
    districtNumber = dst_num,
    districtType=dst_tye,
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
  drop_na(districtName)

english.2000 <- read_csv("2000_english.csv") %>%
  select (dst_num, dst_tye, dst_nam, lep_k12) %>%
  rename(
    districtName=dst_nam,
    districtNumber = dst_num,
    districtType=dst_tye,
    LEPK12=lep_k12
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPK12 = sum(LEPK12)
  ) %>%
  ungroup() %>%
  full_join(enrolled.2000, english.2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPK12/totalStudents
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2000 = percentIdentified) %>%
  drop_na(districtName)


english.2000_2019 <- full_join(english.2019, english.2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2017, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2016, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2015, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2014, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2013, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2012, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2011, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2010, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2009, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2008, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2007, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2006, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2005, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2004, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2003, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2002, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2000, english.2000_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName)

english.2000_2019.identified <- english.2000_2019 %>%
  select(districtNumber, districtType, districtName, percentIdentified2019, percentIdentified2018,percentIdentified2017,percentIdentified2016,percentIdentified2015,percentIdentified2014,percentIdentified2013,percentIdentified2012,percentIdentified2011,percentIdentified2010,percentIdentified2009,percentIdentified2008,percentIdentified2007,percentIdentified2006,percentIdentified2005,percentIdentified2004,percentIdentified2003,percentIdentified2002,percentIdentified2000) %>%
  gather(year,percentIdentified,percentIdentified2019:percentIdentified2000) %>%
  mutate(
    year = replace(year, year =="percentIdentified2019" ,"2019"),
    year = replace(year, year =="percentIdentified2018" ,"2018"),
    year = replace(year, year =="percentIdentified2017" ,"2017"),
    year = replace(year, year =="percentIdentified2016" ,"2016"),
    year = replace(year, year =="percentIdentified2015" ,"2015"),
    year = replace(year, year =="percentIdentified2014" ,"2014"),
    year = replace(year, year =="percentIdentified2013" ,"2013"),
    year = replace(year, year =="percentIdentified2012" ,"2012"),
    year = replace(year, year =="percentIdentified2011" ,"2011"),
    year = replace(year, year =="percentIdentified2010" ,"2010"),
    year = replace(year, year =="percentIdentified2009" ,"2009"),
    year = replace(year, year =="percentIdentified2008" ,"2008"),
    year = replace(year, year =="percentIdentified2007" ,"2007"),
    year = replace(year, year =="percentIdentified2006" ,"2006"),
    year = replace(year, year =="percentIdentified2005" ,"2005"),
    year = replace(year, year =="percentIdentified2004" ,"2004"),
    year = replace(year, year =="percentIdentified2003" ,"2003"),
    year = replace(year, year =="percentIdentified2002" ,"2002"),
    year = replace(year, year =="percentIdentified2000" ,"2000")
  )

english.2000_2019.served <- english.2000_2019 %>%
  select(districtNumber, districtType, districtName, percentServed2013,percentServed2012,percentServed2011,percentServed2010) %>%
  gather(year,percentServed,percentServed2013:percentServed2010) %>%
  mutate(
    year = replace(year, year =="percentServed2013" ,"2013"),
    year = replace(year, year =="percentServed2012" ,"2012"),
    year = replace(year, year =="percentServed2011" ,"2011"),
    year = replace(year, year =="percentServed2010" ,"2010")
  )

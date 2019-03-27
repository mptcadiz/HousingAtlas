library(tidyverse)


# English proficiency by district ---------------------------------

# 2019 ---------------------------------
english.2019 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2019_english.csv") %>%
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
  summarise(
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
english.2018 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2018_english.csv") %>%
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
  summarise(
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
english.2017 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2017_english.csv") %>%
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
  summarise(
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
english.2016 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2016_english.csv") %>%
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
  summarise(
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
english.2015 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2015_english.csv") %>%
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
  summarise(
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
enrolled.2014 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2014_enrolled_ethnicity_district.csv") %>%
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
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  drop_na(districtName)

english.2014 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2014_english.csv") %>%
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
  summarise(
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
enrolled.2013 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2013_enrolled_ethnicity_district.csv") %>%
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
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2013 = totalStudents) %>%
  drop_na(districtName)

english.2013 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2013_english.csv") %>%
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
  summarise(
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
enrolled.2012 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2012_enrolled_ethnicity_district.csv") %>%
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
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2012 = totalStudents) %>%
  drop_na(districtName)

english.2012 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2012_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    LEPIdentifiedK12 = na_if(LEPIdentifiedK12, "NULL"),
    LEPIdentifiedK12 = as.numeric(LEPIdentifiedK12),
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  ungroup() %>%
  full_join(enrolled.2012, english.2012, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents2012
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2012 = percentIdentified) %>%
  drop_na(districtName)

# 2011 ---------------------------------
enrolled.2011 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2011_enrolled_ethnicity_district.csv") %>%
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
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2011 = totalStudents) %>%
  drop_na(districtName)

english.2011 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2011_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    LEPIdentifiedK12 = na_if(LEPIdentifiedK12, "NULL"),
    LEPIdentifiedK12 = as.numeric(LEPIdentifiedK12),
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  ungroup() %>%
  full_join(enrolled.2011, english.2011, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents2011
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2011 = percentIdentified) %>%
  drop_na(districtName)

# 2010 ---------------------------------
enrolled.2010 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2010_enrolled_ethnicity_district.csv") %>%
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
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2010 = totalStudents) %>%
  drop_na(districtName)

english.2010 <- read_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/2010_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  ungroup() %>%
  full_join(enrolled.2010, english.2010, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(
    percentIdentified = LEPIdentifiedK12/totalStudents2010
  ) %>%
  select(districtNumber, districtType, districtName, percentIdentified) %>%
  rename (percentIdentified2010 = percentIdentified) %>%
  drop_na(districtName)

english.2010_2019 <- full_join(english.2019, english.2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2017, english.2010_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2016, english.2010_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2015, english.2010_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2014, english.2010_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2013, english.2010_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2012, english.2010_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2011, english.2010_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(english.2010, english.2010_2019, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName)


english.2010_2019.identified <- english.2010_2019 %>%
  select(districtNumber, districtType, districtName, percentIdentified2019, percentIdentified2018,percentIdentified2017,percentIdentified2016,percentIdentified2015,percentIdentified2014,percentIdentified2013,percentIdentified2012,percentIdentified2011,percentIdentified2010) %>%
  gather(year,percentIdentified,percentIdentified2019:percentIdentified2010) %>%
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
    year = replace(year, year =="percentIdentified2010" ,"2010")
  ) %>%
  write_csv("/home/cadiz003/HousingAtlas/Education/English Proficiency/english_identified_2010_2019.csv",append=FALSE)

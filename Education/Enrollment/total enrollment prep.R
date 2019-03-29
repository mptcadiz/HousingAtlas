library(tidyverse)


# Ethnicity of students enrolled (by district) ---------------------------------

enrolled.2018 <- read_csv("Education/Enrollment/2018_enrolled_ethnicity_district.csv") %>%
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
  rename (totalStudents2018 = totalStudents) %>%
  drop_na(districtName)

enrolled.2017 <- read_csv("Education/Enrollment/2017_enrolled_ethnicity_district.csv") %>%
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
  rename (totalStudents2017 = totalStudents) %>%
  drop_na(districtName)

enrolled.2016 <- read_csv("Education/Enrollment/2016_enrolled_ethnicity_district.csv") %>%
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
  rename (totalStudents2016 = totalStudents) %>%
  drop_na(districtName)

enrolled.2015 <- read_csv("Education/Enrollment/2015_enrolled_ethnicity_district.csv") %>%
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
  rename (totalStudents2015 = totalStudents) %>%
  drop_na(districtName)

enrolled.2014 <- read_csv("Education/Enrollment/2014_enrolled_ethnicity_district.csv") %>%
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
  rename (totalStudents2014 = totalStudents) %>%
  drop_na(districtName)

enrolled.2013 <- read_csv("Education/Enrollment/2013_enrolled_ethnicity_district.csv") %>%
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
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2012 <- read_csv("Education/Enrollment/2012_enrolled_ethnicity_district.csv") %>%
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
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2011 <- read_csv("Education/Enrollment/2011_enrolled_ethnicity_district.csv") %>%
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
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2010 <- read_csv("Education/Enrollment/2010_enrolled_ethnicity_district.csv") %>%
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
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2009 <- read_csv("Education/Enrollment/2009_enrolled_ethnicity_district.csv") %>%
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
  rename (totalStudents2009 = totalStudents) %>%
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2008 <- read_csv("Education/Enrollment/2008_enrolled_ethnicity_district.csv") %>%
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
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2008 = totalStudents) %>%
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2007 <- read_csv("Education/Enrollment/2007_enrolled_ethnicity_district.csv") %>%
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
  rename (totalStudents2007 = totalStudents) %>%
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2006 <- read_csv("Education/Enrollment/2006_enrolled_ethnicity_district.csv") %>%
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
  rename (totalStudents2006 = totalStudents) %>%
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2005 <- read_csv("Education/Enrollment/2005_enrolled_ethnicity_district.csv") %>%
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
  rename (totalStudents2005 = totalStudents) %>%
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2004 <- read_csv("Education/Enrollment/2004_enrolled_ethnicity_district.csv") %>%
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
  rename (totalStudents2004 = totalStudents) %>%
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2003 <- read_csv("Education/Enrollment/2003_enrolled_ethnicity_district.csv") %>%
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
  rename (totalStudents2003 = totalStudents) %>%
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2002 <- read_csv("Education/Enrollment/2002_enrolled_ethnicity_district.csv") %>%
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
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2002 = totalStudents) %>%
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2001 <- read_csv("Education/Enrollment/2001_enrolled_ethnicity_district.csv") %>%
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
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2001 = totalStudents) %>%
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

enrolled.2000 <- read_csv("Education/Enrollment/2000_enrolled_ethnicity_district.csv") %>%
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
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2000 = totalStudents) %>%
  drop_na(districtName) %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

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

enrolled.2018_2000.tidy <- enrolled.2018_2000 %>%
  gather(year,totalStudents,totalStudents2018:totalStudents2000) %>%
  mutate(
    year = replace(year, year =="totalStudents2000" ,"2000"),
    year = replace(year, year =="totalStudents2001" ,"2001"),
    year = replace(year, year =="totalStudents2002" ,"2002"),
    year = replace(year, year =="totalStudents2003" ,"2003"),
    year = replace(year, year =="totalStudents2004" ,"2004"),
    year = replace(year, year =="totalStudents2005" ,"2005"),
    year = replace(year, year =="totalStudents2006" ,"2006"),
    year = replace(year, year =="totalStudents2007" ,"2007"),
    year = replace(year, year =="totalStudents2008" ,"2008"),
    year = replace(year, year =="totalStudents2009" ,"2009"),
    year = replace(year, year =="totalStudents2010" ,"2010"),
    year = replace(year, year =="totalStudents2011" ,"2011"),
    year = replace(year, year =="totalStudents2012" ,"2012"),
    year = replace(year, year =="totalStudents2013" ,"2013"),
    year = replace(year, year =="totalStudents2014" ,"2014"),
    year = replace(year, year =="totalStudents2015" ,"2015"),
    year = replace(year, year =="totalStudents2016" ,"2016"),
    year = replace(year, year =="totalStudents2017" ,"2017"),
    year = replace(year, year =="totalStudents2018" ,"2018")) %>%
    write_csv("Education/Enrollment/enrolled_total_2000_2018.csv",append=FALSE)
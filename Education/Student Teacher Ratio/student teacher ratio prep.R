library(tidyverse)

# In students but not teachers
# MINNESOTA DEPARTMENT OF CORRECTIONS
# CENTRAL MINNESOTA JT. POWERS DIST.
# NORTHERN LIGHTS ACADEMY COOPERATIVE
# 
# In teachers but not students
# REGION 5-NATIONAL JOINT POWERS
# REGION 3 - NORTHEAST SERVICE COOP
# INFINITY:MINNESOTA DIGITAL ACADEMY

# Teacher count data ---------------------------------

teacher.2018 <- read_csv("Education/Student Teacher Ratio/2018_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TotFTE,SchName) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TotFTE
  ) %>%
  filter(SchName != "Totals") %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2018 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE")

teacher.2017 <- read_csv("Education/Student Teacher Ratio/2017_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TotFTE,SchName) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TotFTE
  ) %>%
  filter(SchName != "Totals") %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2017 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE")

teacher.2016 <- read_csv("Education/Student Teacher Ratio/2016_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TotFTE,SchName) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TotFTE
  ) %>%
  filter(SchName != "Totals") %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2016 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE")

teacher.2015 <- read_csv("Education/Student Teacher Ratio/2015_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TotFTE,SchName) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TotFTE
  ) %>%
  filter(SchName != "Totals") %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2015 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE")

teacher.2014 <- read_csv("Education/Student Teacher Ratio/2014_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TotFTE,SchName) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TotFTE
  ) %>%
  filter(SchName != "Totals") %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2014 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE")

teacher.2013 <- read_csv("Education/Student Teacher Ratio/2013_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TotFTE,SchName) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TotFTE
  ) %>%
  filter(SchName != "Totals") %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2013 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2012 <- read_csv("Education/Student Teacher Ratio/2012_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TotFTE,SchName) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TotFTE
  ) %>%
  filter(SchName != "Totals") %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2012 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2011 <- read_csv("Education/Student Teacher Ratio/2011_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TotFTE,SchName) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TotFTE
  ) %>%
  filter(SchName != "Totals") %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2011 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2010 <- read_csv("Education/Student Teacher Ratio/2010_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TotFTE,SchName) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TotFTE
  ) %>%
  filter(SchName != "Totals") %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2010 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2009 <- read_csv("Education/Student Teacher Ratio/2009_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TotFTE,SchName) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TotFTE
  ) %>%
  filter(SchName != "Totals") %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2009 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2008 <- read_csv("Education/Student Teacher Ratio/2008_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TotFTE,SchName) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TotFTE
  ) %>%
  filter(SchName != "Totals:") %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2008 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2007 <- read_csv("Education/Student Teacher Ratio/2007_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TotFTE,SchName) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TotFTE
  ) %>%
  filter(SchName != "Totals:") %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2007 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2006 <- read_csv("Education/Student Teacher Ratio/2006_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TchrALL) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TchrALL
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2006 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2005 <- read_csv("Education/Student Teacher Ratio/2005_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TchrALL) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TchrALL
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2005 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2004 <- read_csv("Education/Student Teacher Ratio/2004_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TchrALL) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TchrALL
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2004 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2003 <- read_csv("Education/Student Teacher Ratio/2003_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TchrALL) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TchrALL
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2003 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2002 <- read_csv("Education/Student Teacher Ratio/2002_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TchrALL) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TchrALL
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2002 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2001 <- read_csv("Education/Student Teacher Ratio/2001_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TchrALL) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TchrALL
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2001 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2000 <- read_csv("Education/Student Teacher Ratio/2000_teachers.csv") %>%
  select(DistNumb, DistType, DistName, TchrALL) %>%
  rename(
    districtName=DistName,
    districtNumber=DistNumb,
    districtType = DistType,
    totalTeachers = TchrALL
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalTeachers = sum(totalTeachers)
  ) %>%
  ungroup() %>%
  rename (totalTeachers2000 = totalTeachers) %>%
  drop_na(districtName) %>%
  filter(districtName!="STATEWIDE") %>%
  mutate(
    districtNumber = ifelse(districtNumber == "0769" & districtType == "01", "2769", districtNumber)
  )

teacher.2018_2000 <- full_join(teacher.2018, teacher.2017, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2016, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2015, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2014, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2013, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2012, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2011, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2010, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2009, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2008, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2007, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2006, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2005, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2004, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2003, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2002, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2001, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(teacher.2000, teacher.2018_2000, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley",  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0323", "02", "Franconia", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

teacher.2018_2000.tidy <- teacher.2018_2000 %>%
  gather(year,totalTeachers,totalTeachers2018:totalTeachers2000) %>%
  mutate(
    year = replace(year, year =="totalTeachers2000" ,2000),
    year = replace(year, year =="totalTeachers2001" ,2001),
    year = replace(year, year =="totalTeachers2002" ,2002),
    year = replace(year, year =="totalTeachers2003" ,2003),
    year = replace(year, year =="totalTeachers2004" ,2004),
    year = replace(year, year =="totalTeachers2005" ,2005),
    year = replace(year, year =="totalTeachers2006" ,2006),
    year = replace(year, year =="totalTeachers2007" ,2007),
    year = replace(year, year =="totalTeachers2008" ,2008),
    year = replace(year, year =="totalTeachers2009" ,2009),
    year = replace(year, year =="totalTeachers2010" ,2010),
    year = replace(year, year =="totalTeachers2011" ,2011),
    year = replace(year, year =="totalTeachers2012" ,2012),
    year = replace(year, year =="totalTeachers2013" ,2013),
    year = replace(year, year =="totalTeachers2014" ,2014),
    year = replace(year, year =="totalTeachers2015" ,2015),
    year = replace(year, year =="totalTeachers2016" ,2016),
    year = replace(year, year =="totalTeachers2017" ,2017),
    year = replace(year, year =="totalTeachers2018" ,2018)) %>%
   mutate(year=as.numeric(as.character(year))) %>%
  mutate(districtNumber=as.numeric(as.character(districtNumber)),
         districtType=as.numeric(as.character(districtType)),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtType = formatC(districtType, width = 2, flag = "0")
  )

# Student count data ---------------------------------

student.count_2000_2018 <- read_csv("Education/Enrollment/enrolled_total_2000_2018.csv") %>%
  mutate(
        #districtNumber=as.numeric(as.character(districtNumber)),
         districtType=as.numeric(as.character(districtType))
         ) %>%
  mutate(
    #districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtType = formatC(districtType, width = 2, flag = "0")
  )

#Merge students and teacher data ---------------------------------
student.teacher.ratio2_2000_2018 <- full_join(student.count_2000_2018, teacher.2018_2000.tidy, by = c("districtType","districtNumber","year")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  mutate(studentTeacherRatio = totalStudents/totalTeachers) %>%
  mutate(year=as.character(as.numeric(year))) %>%
  mutate(districtName = str_to_title(districtName)) %>%
  write_csv("Education/Student Teacher Ratio/student_teacher_ratio_2000_2018.csv",append=FALSE)

test <- read_csv("Education/Student Teacher Ratio/student_teacher_ratio_2000_2018.csv")
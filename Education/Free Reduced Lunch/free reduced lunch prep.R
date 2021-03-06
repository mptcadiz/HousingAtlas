library(tidyverse)


# Percent of students with free/reduced lunch (by district) ---------------------------------

free.red.lunch.2018 <- read_csv("Education/Free Reduced Lunch/2018_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, Grade, K12Enr, FreeK12, RedK12) %>%
  filter(Grade != "All Grades") %>%
  group_by(districtNumber,districtType, DistrictName) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2018 = freeRed/K12Enr,
    DistrictName=toupper(DistrictName)
  ) %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  select(DistrictName, percentLunch2018)

free.red.lunch.2017 <- read_csv("Education/Free Reduced Lunch/2017_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, Grade, K12Enr, FreeK12, RedK12) %>%
  filter(Grade != "All Grades") %>%
  group_by(districtNumber,districtType, DistrictName) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2017 = freeRed/K12Enr,
    DistrictName=toupper(DistrictName)
  ) %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  select(DistrictName, percentLunch2017)

free.red.lunch.2016 <- read_csv("Education/Free Reduced Lunch/2016_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, Grade, K12Enr, FreeK12, RedK12) %>%
  filter(Grade != "All Grades") %>%
  group_by(districtNumber,districtType, DistrictName) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2016 = freeRed/K12Enr,
    DistrictName=toupper(DistrictName)
  ) %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  select(DistrictName, percentLunch2016)

free.red.lunch.2015 <- read_csv("Education/Free Reduced Lunch/2015_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, Grade, K12Enr, FreeK12, RedK12) %>%
  filter(Grade != "All Grades") %>%
  group_by(districtNumber,districtType, DistrictName) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2015 = freeRed/K12Enr,
    DistrictName=toupper(DistrictName)
  ) %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  select(DistrictName, percentLunch2015)

free.red.lunch.2014 <- read_csv("Education/Free Reduced Lunch/2014_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, Grade, K12Enr, FreeK12, RedK12) %>%
  filter(Grade != "All Grades") %>%
  group_by(districtNumber,districtType, DistrictName) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2014 = freeRed/K12Enr,
    DistrictName=toupper(DistrictName)
  ) %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  select(DistrictName, percentLunch2014)

free.red.lunch.2013 <- read_csv("Education/Free Reduced Lunch/2013_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, Grade, K12Enr, FreeK12, RedK12) %>%
  filter(Grade != "All Grades") %>%
  group_by(districtNumber,districtType, DistrictName) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2013 = freeRed/K12Enr,
    DistrictName=toupper(DistrictName)
  ) %>%
  ungroup() %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  ) %>%
  select(DistrictNumber, DistrictType, DistrictName, percentLunch2013)

free.red.lunch.2012 <- read_csv("Education/Free Reduced Lunch/2012_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, Grade, K12Enr, FreeK12, RedK12) %>%
  filter(Grade != "All Grades") %>%
  mutate(
    K12Enr = na_if(K12Enr, "NULL"),
    FreeK12 = na_if(FreeK12, "NULL"),
    RedK12 = na_if(RedK12, "NULL")
  ) %>%
  mutate(
    K12Enr = as.numeric(K12Enr),
    FreeK12 = as.numeric(FreeK12),
    RedK12 = as.numeric(RedK12)
         ) %>%
  group_by(DistrictName, districtNumber, districtType) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2012 = freeRed/K12Enr
  ) %>%
  ungroup() %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  ) %>%
  select(DistrictNumber, DistrictType, DistrictName, percentLunch2012)

free.red.lunch.2011 <- read_csv("Education/Free Reduced Lunch/2011_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, Grade, K12Enr, FreeK12, RedK12) %>%
  filter(Grade != "All Grades") %>%
  mutate(
    K12Enr = na_if(K12Enr, "NULL"),
    FreeK12 = na_if(FreeK12, "NULL"),
    RedK12 = na_if(RedK12, "NULL")
  ) %>%
  mutate(
    K12Enr = as.numeric(K12Enr),
    FreeK12 = as.numeric(FreeK12),
    RedK12 = as.numeric(RedK12)
  ) %>%
  group_by(DistrictName, districtNumber, districtType) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2011 = freeRed/K12Enr
  ) %>%
  ungroup() %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  ) %>%
  select(DistrictNumber, DistrictType, DistrictName, percentLunch2011)

free.red.lunch.2010 <- read_csv("Education/Free Reduced Lunch/2010_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, Grade, K12Enr, FreeK12, RedK12) %>%
  filter(Grade != "All Grades") %>%
  group_by(districtNumber,districtType, DistrictName) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2010 = freeRed/K12Enr,
    DistrictName=toupper(DistrictName)
  ) %>%
  ungroup() %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  ) %>%
  select(DistrictNumber, DistrictType, DistrictName, percentLunch2010)

free.red.lunch.2009 <- read_csv("Education/Free Reduced Lunch/2009_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, grade, K12Enr, FreeK12, RedK12) %>%
  filter(grade != "All Grades") %>%
  group_by(districtNumber,districtType, DistrictName) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2009 = freeRed/K12Enr,
    DistrictName=toupper(DistrictName)
  ) %>%
  ungroup() %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  ) %>%
  select(DistrictNumber, DistrictType, DistrictName, percentLunch2009)

free.red.lunch.2008 <- read_csv("Education/Free Reduced Lunch/2008_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, Grade, K12Enr, FreeK12, RedK12) %>%
  filter(Grade == "All Grades") %>%
  group_by(districtNumber,districtType, DistrictName) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2008 = freeRed/K12Enr,
    DistrictName=toupper(DistrictName)
  ) %>%
  ungroup() %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  ) %>%
  select(DistrictNumber, DistrictType, DistrictName, percentLunch2008)

free.red.lunch.2007 <- read_csv("Education/Free Reduced Lunch/2007_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, grade, K12Enr, FreeK12, RedK12) %>%
  filter(grade != "All Grades") %>%
  group_by(districtNumber,districtType, DistrictName) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2007 = freeRed/K12Enr,
    DistrictName=toupper(DistrictName)
  ) %>%
  ungroup() %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  ) %>%
  select(DistrictNumber, DistrictType, DistrictName, percentLunch2007)

free.red.lunch.2006 <- read_csv("Education/Free Reduced Lunch/2006_free_red_lunch.csv") %>%
  select (districtNumber,districtType,DistrictName, grade, K12Enr, FreeK12, RedK12) %>%
  filter(grade != "All Grades") %>%
  group_by(districtNumber,districtType, DistrictName) %>%
  summarise(
    K12Enr = sum(K12Enr, na.rm=TRUE),
    FreeK12 = sum(FreeK12, na.rm=TRUE),
    RedK12 = sum(RedK12, na.rm=TRUE)
  ) %>%
  mutate(
    freeRed = FreeK12 + RedK12,
    percentLunch2006 = freeRed/K12Enr,
    DistrictName=toupper(DistrictName)
  ) %>%
  ungroup() %>%
  rename(
    DistrictNumber=districtNumber,
    DistrictType=districtType
  ) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  ) %>%
  select(DistrictNumber, DistrictType, DistrictName, percentLunch2006)


free.red.lunch.2006_2018 <- full_join(free.red.lunch.2018, free.red.lunch.2017, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(free.red.lunch.2016, free.red.lunch.2006_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(free.red.lunch.2015, free.red.lunch.2006_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(free.red.lunch.2014, free.red.lunch.2006_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(free.red.lunch.2013, free.red.lunch.2006_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(free.red.lunch.2012, free.red.lunch.2006_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(free.red.lunch.2011, free.red.lunch.2006_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(free.red.lunch.2010, free.red.lunch.2006_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(free.red.lunch.2009, free.red.lunch.2006_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(free.red.lunch.2008, free.red.lunch.2006_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(free.red.lunch.2007, free.red.lunch.2006_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(free.red.lunch.2006, free.red.lunch.2006_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName)
  # rename(districtName = DistrictName,
  #         districtNumber = DistrictNumber,
  #         districtType= DistrictType) %>%
  # rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  # rbind(c("2759", "01", "Eagle Valley", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  # rbind(c("0323", "02", "Franconia", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  # rbind(c("0815", "02", "Prinsburg",  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  # 


tidy.free.red.lunch.2006_2018 <- free.red.lunch.2006_2018 %>%
  gather(year,percentLunch,percentLunch2006:percentLunch2018) %>%
  mutate(
    year = replace(year, year =="percentLunch2006" ,"2006"),
    year = replace(year, year =="percentLunch2007" ,"2007"),
    year = replace(year, year =="percentLunch2008" ,"2008"),
    year = replace(year, year =="percentLunch2009" ,"2009"),
    year = replace(year, year =="percentLunch2010" ,"2010"),
    year = replace(year, year =="percentLunch2011" ,"2011"),
    year = replace(year, year =="percentLunch2012" ,"2012"),
    year = replace(year, year =="percentLunch2013" ,"2013"),
    year = replace(year, year =="percentLunch2014" ,"2014"),
    year = replace(year, year =="percentLunch2015" ,"2015"),
    year = replace(year, year =="percentLunch2016" ,"2016"),
    year = replace(year, year =="percentLunch2017" ,"2017"),
    year = replace(year, year =="percentLunch2018" ,"2018")) %>%
  rename(districtName=DistrictName) %>%
  mutate(districtName = str_to_title(districtName)) %>%
  write_csv("Education/Free Reduced Lunch/free_red_lunch_2006_2018.csv",append=FALSE)

lunchtest <- read_csv("Education/Free Reduced Lunch/free_red_lunch_2006_2018.csv") %>%
  rbind(c("0815", "02", "Prinsburg", 2006, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", 2007, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", 2008, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", 2009, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", 2010, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", 2011, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", 2012, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", 2013, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", 2014, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", 2015, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", 2016, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", 2017, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", 2018, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2006, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2007, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2008, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2009, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2010, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2011, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2012, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2013, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2014, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2015, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2016, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2017, NA)) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", 2018, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2006, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2007, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2008, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2009, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2010, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2011, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2012, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2013, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2014, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2015, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2016, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2017, NA)) %>%
  rbind(c("0323", "02", "Franconia", 2018, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2006, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2007, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2008, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2009, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2010, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2011, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2012, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2013, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2014, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2015, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2016, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2017, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", 2018, NA)) %>%
  mutate(districtName = str_to_title(districtName)) %>%
  write_csv("Education/Free Reduced Lunch/free_red_lunch_2006_2018.csv",append=FALSE)
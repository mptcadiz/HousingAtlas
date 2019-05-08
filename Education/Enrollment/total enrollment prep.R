library(tidyverse)


# Ethnicity of students enrolled (by district) ---------------------------------

enrolled.2018 <- read_csv("Education/Enrollment/2018_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  filter(Grade != "All Grades") %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2018 = totalStudents) %>%
  drop_na(districtName)

enrolled.2017 <- read_csv("Education/Enrollment/2017_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  filter(Grade != "All Grades") %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2017 = totalStudents) %>%
  drop_na(districtName)

enrolled.2016 <- read_csv("Education/Enrollment/2016_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  filter(Grade != "All Grades") %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2016 = totalStudents) %>%
  drop_na(districtName)

enrolled.2015 <- read_csv("Education/Enrollment/2015_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  filter(Grade != "All Grades") %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2015 = totalStudents) %>%
  drop_na(districtName)

enrolled.2014 <- read_csv("Education/Enrollment/2014_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  filter(Grade != "All Grades") %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    totalStudents = sum(totalStudents)
  ) %>%
  ungroup() %>%
  rename (totalStudents2014 = totalStudents) %>%
  drop_na(districtName)

enrolled.2013 <- read_csv("Education/Enrollment/2013_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  filter(Grade != "All Grades") %>%
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
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  filter(Grade != "All Grades") %>%
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
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  filter(Grade != "All Grades") %>%
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
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  filter(Grade != "All Grades") %>%
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
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  filter(Grade != "All Grades") %>%
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
  select (DistrictNumber, DistrictType, DistrictName, Grade, TotalStudents) %>%
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
  filter(Grade != "All Grades") %>%
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
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  filter(Grade != "All Grades") %>%
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
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
  rename(
    districtName=DistrictName,
    totalStudents=TotalStudents
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  filter(Grade != "All Grades") %>%
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
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
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
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
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
  select (districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
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
  drop_na(districtName) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0323", "02", "Franconia", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

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
    mutate(districtName = str_to_title(districtName)) %>%
    write_csv("Education/Enrollment/enrolled_total_2000_2018.csv",append=FALSE)

enrollment.change.2008_2018 <- enrolled.2018_2000 %>%
  mutate(totalStudents2018=as.numeric(as.character(totalStudents2018)),
         totalStudents2017=as.numeric(as.character(totalStudents2017)),
         totalStudents2016=as.numeric(as.character(totalStudents2016)),
         totalStudents2015=as.numeric(as.character(totalStudents2015)),
         totalStudents2014=as.numeric(as.character(totalStudents2014)),
         totalStudents2013=as.numeric(as.character(totalStudents2013)),
         totalStudents2012=as.numeric(as.character(totalStudents2012)),
         totalStudents2011=as.numeric(as.character(totalStudents2011)),
         totalStudents2010=as.numeric(as.character(totalStudents2010)),
         totalStudents2009=as.numeric(as.character(totalStudents2009)),
         totalStudents2008=as.numeric(as.character(totalStudents2008)),
         totalStudents2007=as.numeric(as.character(totalStudents2007)),
         totalStudents2006=as.numeric(as.character(totalStudents2006)),
         totalStudents2005=as.numeric(as.character(totalStudents2005)),
         totalStudents2004=as.numeric(as.character(totalStudents2004)),
         totalStudents2003=as.numeric(as.character(totalStudents2003)),
         totalStudents2002=as.numeric(as.character(totalStudents2002)),
         totalStudents2001=as.numeric(as.character(totalStudents2001)),
         totalStudents2000=as.numeric(as.character(totalStudents2000))
        ) %>%
  mutate(
        change2017 = (totalStudents2018-totalStudents2017)/totalStudents2018,
         change2016 = (totalStudents2018-totalStudents2016)/totalStudents2018,
         change2015 = (totalStudents2018-totalStudents2015)/totalStudents2018,
         change2014 = (totalStudents2018-totalStudents2014)/totalStudents2018,
         change2013 = (totalStudents2018-totalStudents2013)/totalStudents2018,
         change2012 = (totalStudents2018-totalStudents2012)/totalStudents2018,
         change2011 = (totalStudents2018-totalStudents2011)/totalStudents2018,
         change2010 = (totalStudents2018-totalStudents2010)/totalStudents2018,
         change2009 = (totalStudents2018-totalStudents2009)/totalStudents2018,
         change2008 = (totalStudents2018-totalStudents2008)/totalStudents2018,
         change2007 = (totalStudents2018-totalStudents2007)/totalStudents2018,
         change2006= (totalStudents2018-totalStudents2006)/totalStudents2018,
         change2005 = (totalStudents2018-totalStudents2005)/totalStudents2018,
         change2004 = (totalStudents2018-totalStudents2004)/totalStudents2018,
         change2003 = (totalStudents2018-totalStudents2003)/totalStudents2018,
         change2002 = (totalStudents2018-totalStudents2002)/totalStudents2018,
         change2001 = (totalStudents2018-totalStudents2001)/totalStudents2018,
         change2000 = (totalStudents2018-totalStudents2000)/totalStudents2018) %>%
  select (districtNumber, districtType, districtName, change2017:change2000) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0323", "02", "Franconia", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  gather(year,change,change2017:change2000) %>%
  mutate(
    year = replace(year, year =="change2000" ,"2000"),
    year = replace(year, year =="change2001" ,"2001"),
    year = replace(year, year =="change2002" ,"2002"),
    year = replace(year, year =="change2003" ,"2003"),
    year = replace(year, year =="change2004" ,"2004"),
    year = replace(year, year =="change2005" ,"2005"),
    year = replace(year, year =="change2006" ,"2006"),
    year = replace(year, year =="change2007" ,"2007"),
    year = replace(year, year =="change2008" ,"2008"),
    year = replace(year, year =="change2009" ,"2009"),
    year = replace(year, year =="change2010" ,"2010"),
    year = replace(year, year =="change2011" ,"2011"),
    year = replace(year, year =="change2012" ,"2012"),
    year = replace(year, year =="change2013" ,"2013"),
    year = replace(year, year =="change2014" ,"2014"),
    year = replace(year, year =="change2015" ,"2015"),
    year = replace(year, year =="change2016" ,"2016"),
    year = replace(year, year =="change2017" ,"2017"),
    year = replace(year, year =="change2018" ,"2018")) %>%
  mutate(districtName = str_to_title(districtName)) %>%
  write_csv("Education/Enrollment/enrollment_change_2000_2018.csv",append=FALSE)

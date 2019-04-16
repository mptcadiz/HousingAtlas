library(tidyverse)


# ACT scores (5 year averages) ---------------------------------

act.score.2008_2012 <- read_csv("Education/ACT Scores/2012_act.csv") %>%
  select("Analysis Level", "District Name", "ACT Dist Code", "Grad Year", "Avg Comp") %>%
  rename (
    districtName = "District Name",
    actCode = "ACT Dist Code",
    gradYear = "Grad Year",
    score = "Avg Comp",
    analysisLevel= "Analysis Level"
  ) %>%
  mutate(actCode=as.numeric(as.character(actCode))) %>%
  filter(analysisLevel == "District") %>%
  filter(districtName != "No District Identified") %>%
  drop_na(districtName) %>%
  spread(gradYear, score, fill=NA,convert=TRUE) %>%
  select(-analysisLevel)

act.score.2013_2017 <- read_csv("Education/ACT Scores/2017_act.csv") %>%
  select("Analysis Level", "District Name", "ACT Dist Code", "Grad Year", "Avg Comp") %>%
  rename (
    districtName = "District Name",
    actCode = "ACT Dist Code",
    gradYear = "Grad Year",
    score = "Avg Comp",
    analysisLevel= "Analysis Level"
  ) %>%
  mutate(actCode=as.numeric(as.character(actCode))) %>%
  filter(analysisLevel == "District") %>%
  filter(districtName != "No District Identified") %>%
  drop_na(districtName) %>%
  spread(gradYear, score, fill=NA,convert=TRUE) %>%
  select(-analysisLevel)

act.score.2018 <- read_csv("Education/ACT Scores/2018_act.csv") %>%
  select("Analysis Level", "District Name", "ACT Dist Code", "Grad Year", "Avg Comp") %>%
  rename (
    districtName = "District Name",
    actCode = "ACT Dist Code",
    gradYear = "Grad Year",
    score = "Avg Comp",
    analysisLevel= "Analysis Level"
  ) %>%
  filter(analysisLevel == "District") %>%
  filter(districtName != "No District Identified") %>%
  drop_na(districtName) %>%
  spread(gradYear, score, fill=NA,convert=TRUE) %>%
  select(-analysisLevel) %>%
  select(districtName, actCode,"2018")

act.score.2008_2018 <- full_join(act.score.2018, act.score.2013_2017, by = c("actCode")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(act.score.2008_2012, act.score.2008_2018, by = c("actCode")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  select(districtName, actCode, "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008") %>%
  rename(
    score2018="2018",
    score2017="2017",
    score2016="2016",
    score2015="2015",
    score2014="2014",
    score2013="2013",
    score2012="2012",
    score2011="2011",
    score2010="2010",
    score2009="2009",
    score2008="2008"
  ) %>%
  mutate(
    score2017=as.numeric(as.character(score2017)),
    score2016=as.numeric(as.character(score2016)),
    score2015=as.numeric(as.character(score2015)),
    score2014=as.numeric(as.character(score2014)),
    score2013=as.numeric(as.character(score2013))
         ) %>%
  gather(year,score,score2018:score2008) %>%
  mutate (
    year = replace(year, year =="score2008" ,"2008"),
    year = replace(year, year =="score2009" ,"2009"),
    year = replace(year, year =="score2010" ,"2010"),
    year = replace(year, year =="score2011" ,"2011"),
    year = replace(year, year =="score2012" ,"2012"),
    year = replace(year, year =="score2013" ,"2013"),
    year = replace(year, year =="score2014" ,"2014"),
    year = replace(year, year =="score2015" ,"2015"),
    year = replace(year, year =="score2016" ,"2016"),
    year = replace(year, year =="score2017" ,"2017"),
    year = replace(year, year =="score2018" ,"2018")
  ) %>%
  mutate(year=as.numeric(as.character(year))) %>%
  filter(districtName != "District Not Identified") %>%
  write_csv("Education/ACT Scores/act_scores_2008_2018.csv",append=FALSE)

# Number of ACT takers (5 year averages) ---------------------------------

act.n.2008_2012 <- read_csv("Education/ACT Scores/2012_act.csv") %>%
  select("Analysis Level", "District Name", "ACT Dist Code", "Grad Year", N) %>%
  rename (
    districtName = "District Name",
    actCode = "ACT Dist Code",
    gradYear = "Grad Year",
    analysisLevel= "Analysis Level"
  ) %>%
  mutate(actCode=as.numeric(as.character(actCode))) %>%
  filter(analysisLevel == "District") %>%
  filter(districtName != "No District Identified") %>%
  drop_na(districtName) %>%
  spread(gradYear, N, fill=NA,convert=TRUE) %>%
  select(-analysisLevel)
  # mutate(
  #   districtNumber=as.numeric(str_extract_all(districtName,"[0-9]+")),
  #   districtName=gsub(" [0-9]+", "", districtName),
  #   districtNumber = ifelse(districtName == "ALBERT LEA AREA SCHOOLS", 0241, districtNumber),
  #   districtNumber = ifelse(districtName == "ATWATER-COSMOS-GROVE CITY SD", 2396, districtNumber),
  #   districtNumber = ifelse(districtName == "BIRD IS-OLIVIA-LAKE LILLIAN SD", 2534, districtNumber),
  #   districtNumber = ifelse(districtName == "BUFFALO HANOVER MONTROSE SD", 0877, districtNumber),
  #   districtNumber = ifelse(districtName == "EASTERN CARVER CO SCHOOLS", 0112, districtNumber),
  #   districtNumber = ifelse(districtName == "GRANADA-HUNTLEY E CHAIN SD", 2536, districtNumber),
  #   districtNumber = ifelse(districtName == "MANKATO AREA PUBLIC SCHS", 0077, districtNumber),
  #   districtNumber = ifelse(districtName == "MINNEAPOLIS PUBLIC SCH DIST", 0001, districtNumber),
  #   districtNumber = ifelse(districtName == "MINNEOTA PUBLIC SCHOOLS", 0414, districtNumber),
  #   districtNumber = ifelse(districtName == "PLAINVIEW ELGIN MILLVILLE SD", 2899, districtNumber),
  #   districtNumber = ifelse(districtName == "WASECA PUBLIC SCHOOL DIST", 0829, districtNumber)
  #   ) %>%
  # mutate(
  #   districtNumber = formatC(districtNumber, width = 4, flag = "0")
  # )

act.n.2013_2017 <- read_csv("Education/ACT Scores/2017_act.csv") %>%
  select("Analysis Level", "District Name", "ACT Dist Code", "Grad Year", N) %>%
  rename (
    districtName = "District Name",
    actCode = "ACT Dist Code",
    gradYear = "Grad Year",
    analysisLevel= "Analysis Level"
  ) %>%
  mutate(actCode=as.numeric(as.character(actCode))) %>%
  filter(analysisLevel == "District") %>%
  filter(districtName != "No District Identified") %>%
  drop_na(districtName) %>%
  spread(gradYear, N, fill=NA,convert=TRUE) %>%
  select(-analysisLevel)

act.n.2018 <- read_csv("Education/ACT Scores/2018_act.csv") %>%
  select("Analysis Level", "District Name", "ACT Dist Code", "Grad Year", N) %>%
  rename (
    districtName = "District Name",
    actCode = "ACT Dist Code",
    gradYear = "Grad Year",
    analysisLevel= "Analysis Level"
  ) %>%
  filter(analysisLevel == "District") %>%
  filter(districtName != "District Not Identified") %>%
  drop_na(districtName) %>%
  spread(gradYear, N, fill=NA,convert=TRUE) %>%
  select(-analysisLevel) %>%
  select(districtName, actCode,"2018")  %>%
  mutate(
    districtNumber=as.numeric(str_extract_all(districtName,"[0-9]+")),
    districtName=gsub(" [0-9]+", "", districtName),
     districtNumber = ifelse(districtName == "ALBERT LEA AREA SCHOOLS", 0241, districtNumber),
    districtNumber = ifelse(districtName == "ATWATER-COSMOS-GROVE CITY SD", 2396, districtNumber),
    # districtNumber = ifelse(districtName == "BIRD IS-OLIVIA-LAKE LILLIAN SD", 2534, districtNumber),
    districtNumber = ifelse(districtName == "BUFFALO HANOVER MONTROSE SD", 0877, districtNumber),
    districtNumber = ifelse(districtName == "EASTERN CARVER CO SCHOOLS", 0112, districtNumber),
    districtNumber = ifelse(districtName == "GRANADA-HUNTLEY-EAST CHAIN SD", 2536, districtNumber),
    districtNumber = ifelse(districtName == "MANKATO AREA PUBLIC SCHS", 0077, districtNumber),
    districtNumber = ifelse(districtName == "MINNEAPOLIS PUBLIC SCH DIST", 0001, districtNumber),
    districtNumber = ifelse(districtName == "MINNEOTA PUBLIC SCHOOLS", 0414, districtNumber),
    districtNumber = ifelse(districtName == "PLAINVIEW ELGIN MILLVILLE SD", 2899, districtNumber),
    districtNumber = ifelse(districtName == "WASECA PUBLIC SCHOOL DIST", 0829, districtNumber),
    districtNumber = ifelse(districtName == "WAUBUN-OGEMA-WHITE EARTH PSD", 0435, districtNumber),
    districtNumber = ifelse(districtName == "WINONA AREA PUBLIC SCHOOL DIST", 0861, districtNumber)
  ) %>%
  # mutate(
  #   districtNumber = formatC(districtNumber, width = 4, flag = "0")
  # ) %>%
  select(districtName, districtNumber, actCode, "2018")

# ACT N (old) -------------
# act.n.2008_2018 <- full_join(act.n.2018, act.n.2013_2017, by = c("actCode")) %>%
#   rename(districtName=districtName.x) %>%
#   select(-districtName.y) %>%
#   drop_na(districtName) %>%
#   full_join(act.n.2008_2012, act.n.2008_2018, by = c("actCode")) %>%
#   rename(districtName=districtName.x) %>%
#   select(-districtName.y) %>%
#   drop_na(districtName) %>%
#   select(districtName, districtNumber, "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008") %>%
#   rename(
#     N2018="2018",
#     N2017="2017",
#     N2016="2016",
#     N2015="2015",
#     N2014="2014",
#     N2013="2013",
#     N2012="2012",
#     N2011="2011",
#     N2010="2010",
#     N2009="2009",
#     N2008="2008"
#   ) %>%
#   mutate(
#     N2017=as.numeric(as.character(N2017)),
#     N2016=as.numeric(as.character(N2016)),
#     N2015=as.numeric(as.character(N2015)),
#     N2014=as.numeric(as.character(N2014)),
#     N2013=as.numeric(as.character(N2013))
#   ) %>%
#   gather(year,N,N2018:N2008) %>%
#   mutate (
#     year = replace(year, year =="N2008" ,"2008"),
#     year = replace(year, year =="N2009" ,"2009"),
#     year = replace(year, year =="N2010" ,"2010"),
#     year = replace(year, year =="N2011" ,"2011"),
#     year = replace(year, year =="N2012" ,"2012"),
#     year = replace(year, year =="N2013" ,"2013"),
#     year = replace(year, year =="N2014" ,"2014"),
#     year = replace(year, year =="N2015" ,"2015"),
#     year = replace(year, year =="N2016" ,"2016"),
#     year = replace(year, year =="N2017" ,"2017"),
#     year = replace(year, year =="N2018" ,"2018")
#   ) %>%
#   mutate(year=as.numeric(as.character(year))) %>%
#   filter(districtName != "District Not Identified") %>%
#   filter(districtName != "MINNEAPOLIS PUBLIC SCH DIST") %>%
#   mutate(districtNumber = ifelse(districtName == "BOLD SCHOOL DISTRICT", 2534, districtNumber)) %>%
#   mutate(
#     districtNumber = formatC(districtNumber, width = 4, flag = "0")
#   )


#ACT N --------------
act.n.2008_2018 <- full_join(act.n.2018, act.n.2013_2017, by = c("actCode")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(act.n.2008_2012, act.n.2008_2018, by = c("actCode")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  select(districtName, districtNumber, "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008") %>%
  rename(
    N2018="2018",
    N2017="2017",
    N2016="2016",
    N2015="2015",
    N2014="2014",
    N2013="2013",
    N2012="2012",
    N2011="2011",
    N2010="2010",
    N2009="2009",
    N2008="2008"
  ) %>%
  mutate(
    N2017=as.numeric(as.character(N2017)),
    N2016=as.numeric(as.character(N2016)),
    N2015=as.numeric(as.character(N2015)),
    N2014=as.numeric(as.character(N2014)),
    N2013=as.numeric(as.character(N2013))
  ) %>%
  filter(districtName != "District Not Identified") %>%
  filter(districtName != "MINNEAPOLIS PUBLIC SCH DIST") %>%
  mutate(districtNumber = ifelse(districtName == "BOLD SCHOOL DISTRICT", 2534, districtNumber)) %>%
  mutate(
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  )


  enrolled.act.2018 <- read_csv("Education/Enrollment/2018_enrolled_ethnicity_district.csv") %>%
    select(districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
    rename(grade=Grade,
           totalStudents = TotalStudents,
           districtName = DistrictName) %>%
    mutate(
      districtType = formatC(districtType, width = 2, flag = "0"),
      districtNumber = formatC(districtNumber, width = 4, flag = "0"),
      districtName=toupper(districtName)
    ) %>%
    filter(
      str_detect(grade,"12")
    ) %>%
    select(-grade) %>%
    rename(totalStudents2018=totalStudents) %>%
    drop_na(districtName) %>%
    filter(districtName != "MINNEAPOLIS PUBLIC SCHOOL DISTRICT")
  
  enrolled.act.2017 <- read_csv("Education/Enrollment/2017_enrolled_ethnicity_district.csv") %>%
    select(districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
    rename(grade=Grade,
           totalStudents = TotalStudents,
           districtName = DistrictName) %>%
    mutate(
      districtType = formatC(districtType, width = 2, flag = "0"),
      districtNumber = formatC(districtNumber, width = 4, flag = "0"),
      districtName=toupper(districtName)
    ) %>%
    filter(
      str_detect(grade,"12")
    ) %>%
    select(-grade) %>%
    rename(totalStudents2017=totalStudents) %>%
    drop_na(districtName) %>%
    filter(districtName != "MINNEAPOLIS PUBLIC SCHOOL DISTRICT")
  
  enrolled.act.2016 <- read_csv("Education/Enrollment/2016_enrolled_ethnicity_district.csv") %>%
    select(districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
    rename(grade=Grade,
           totalStudents = TotalStudents,
           districtName = DistrictName) %>%
    mutate(
      districtType = formatC(districtType, width = 2, flag = "0"),
      districtNumber = formatC(districtNumber, width = 4, flag = "0"),
      districtName=toupper(districtName)
    ) %>%
    filter(
      str_detect(grade,"12")
    ) %>%
    select(-grade) %>%
    rename(totalStudents2016=totalStudents) %>%
    drop_na(districtName) %>%
    filter(districtName != "MINNEAPOLIS PUBLIC SCHOOL DISTRICT")
  
  enrolled.act.2015 <- read_csv("Education/Enrollment/2015_enrolled_ethnicity_district.csv") %>%
    select(districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
    rename(grade=Grade,
           totalStudents = TotalStudents,
           districtName = DistrictName) %>%
    mutate(
      districtType = formatC(districtType, width = 2, flag = "0"),
      districtNumber = formatC(districtNumber, width = 4, flag = "0"),
      districtName=toupper(districtName)
    ) %>%
    filter(
      str_detect(grade,"12")
    ) %>%
    select(-grade) %>%
    rename(totalStudents2015=totalStudents) %>%
    drop_na(districtName) %>%
    filter(districtName != "MINNEAPOLIS PUBLIC SCHOOL DIST.")
  
  enrolled.act.2014 <- read_csv("Education/Enrollment/2014_enrolled_ethnicity_district.csv") %>%
    select(districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
    rename(grade=Grade,
           totalStudents = TotalStudents,
           districtName = DistrictName) %>%
    mutate(
      districtType = formatC(districtType, width = 2, flag = "0"),
      districtNumber = formatC(districtNumber, width = 4, flag = "0"),
      districtName=toupper(districtName)
    ) %>%
    filter(
      str_detect(grade,"12")
    ) %>%
    select(-grade) %>%
    rename(totalStudents2014=totalStudents) %>%
    drop_na(districtName) %>%
    filter(districtName != "MINNEAPOLIS PUBLIC SCHOOL DIST.")
  
  enrolled.act.2013 <- read_csv("Education/Enrollment/2013_enrolled_ethnicity_district.csv") %>%
    select(districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
    rename(grade=Grade,
           totalStudents = TotalStudents,
           districtName = DistrictName) %>%
    mutate(
      districtType = formatC(districtType, width = 2, flag = "0"),
      districtNumber = formatC(districtNumber, width = 4, flag = "0"),
      districtName=toupper(districtName)
    ) %>%
    filter(
      str_detect(grade,"12")
    ) %>%
    select(-grade) %>%
    rename(totalStudents2013=totalStudents) %>%
    drop_na(districtName) %>%
    filter(districtName != "MINNEAPOLIS PUBLIC SCHOOL DIST.")
  
  enrolled.act.2012 <- read_csv("Education/Enrollment/2012_enrolled_ethnicity_district.csv") %>%
    select(districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
    rename(grade=Grade,
           totalStudents = TotalStudents,
           districtName = DistrictName) %>%
    mutate(
      districtType = formatC(districtType, width = 2, flag = "0"),
      districtNumber = formatC(districtNumber, width = 4, flag = "0"),
      districtName=toupper(districtName)
    ) %>%
    filter(
      str_detect(grade,"12")
    ) %>%
    select(-grade) %>%
    rename(totalStudents2012=totalStudents) %>%
    drop_na(districtName) %>%
    filter(districtName != "MINNEAPOLIS PUBLIC SCHOOL DIST.")
  
  enrolled.act.2011 <- read_csv("Education/Enrollment/2011_enrolled_ethnicity_district.csv") %>%
    select(districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
    rename(grade=Grade,
           totalStudents = TotalStudents,
           districtName = DistrictName) %>%
    mutate(
      districtType = formatC(districtType, width = 2, flag = "0"),
      districtNumber = formatC(districtNumber, width = 4, flag = "0"),
      districtName=toupper(districtName)
    ) %>%
    filter(
      str_detect(grade,"12")
    ) %>%
    select(-grade) %>%
    rename(totalStudents2011=totalStudents) %>%
    drop_na(districtName) %>%
    filter(districtName != "MINNEAPOLIS PUBLIC SCHOOL DIST.")
  
  enrolled.act.2010 <- read_csv("Education/Enrollment/2010_enrolled_ethnicity_district.csv") %>%
    select(districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
    rename(grade=Grade,
           totalStudents = TotalStudents,
           districtName = DistrictName) %>%
    mutate(
      districtType = formatC(districtType, width = 2, flag = "0"),
      districtNumber = formatC(districtNumber, width = 4, flag = "0"),
      districtName=toupper(districtName)
    ) %>%
    filter(
      str_detect(grade,"12")
    ) %>%
    select(-grade) %>%
    rename(totalStudents2010=totalStudents) %>%
    drop_na(districtName) %>%
    filter(districtName != "MINNEAPOLIS PUBLIC SCHOOL DIST.")
  
  enrolled.act.2009 <- read_csv("Education/Enrollment/2009_enrolled_ethnicity_district.csv") %>%
    select(districtNumber, districtType, DistrictName, Grade, TotalStudents) %>%
    rename(grade=Grade,
           totalStudents = TotalStudents,
           districtName = DistrictName) %>%
    mutate(
      districtType = formatC(districtType, width = 2, flag = "0"),
      districtNumber = formatC(districtNumber, width = 4, flag = "0"),
      districtName=toupper(districtName)
    ) %>%
    filter(
      str_detect(grade,"12")
    ) %>%
    select(-grade) %>%
    rename(totalStudents2009=totalStudents) %>%
    drop_na(districtName) %>%
    filter(districtName != "MINNEAPOLIS PUBLIC SCHOOL DIST.")
  
  enrolled.act.2008 <- read_csv("Education/Enrollment/2008_enrolled_ethnicity_district.csv") %>%
    select(DistrictNumber, DistrictType, DistrictName, Grade, TotalStudents) %>%
    rename(grade=Grade,
           totalStudents = TotalStudents,
           districtName = DistrictName,
           districtNumber = DistrictNumber,
           districtType = DistrictType) %>%
    mutate(
      districtType = formatC(districtType, width = 2, flag = "0"),
      districtNumber = formatC(districtNumber, width = 4, flag = "0"),
      districtName=toupper(districtName)
    ) %>%
    filter(
      str_detect(grade,"12")
    ) %>%
    select(-grade) %>%
    rename(totalStudents2008=totalStudents) %>%
    drop_na(districtName) %>%
    filter(districtName != "MINNEAPOLIS PUBLIC SCHOOL DIST.")
  
  enrolled.act.2008_2018 <- full_join(enrolled.act.2018, enrolled.act.2017, by = c("districtNumber","districtType")) %>%
    rename(districtName=districtName.x) %>%
    select(-districtName.y) %>%
    drop_na(districtName) %>%
    full_join(enrolled.act.2016, enrolled.act.2008_2018, by = c("districtNumber","districtType")) %>%
    rename(districtName=districtName.x) %>%
    select(-districtName.y) %>%
    drop_na(districtName) %>%
    full_join(enrolled.act.2015, enrolled.act.2008_2018, by = c("districtNumber","districtType")) %>%
    rename(districtName=districtName.x) %>%
    select(-districtName.y) %>%
    drop_na(districtName) %>%
    full_join(enrolled.act.2014, enrolled.act.2008_2018, by = c("districtNumber","districtType")) %>%
    rename(districtName=districtName.x) %>%
    select(-districtName.y) %>%
    drop_na(districtName) %>%
    full_join(enrolled.act.2013, enrolled.act.2008_2018, by = c("districtNumber","districtType")) %>%
    rename(districtName=districtName.x) %>%
    select(-districtName.y) %>%
    drop_na(districtName) %>%
    full_join(enrolled.act.2012, enrolled.act.2008_2018, by = c("districtNumber","districtType")) %>%
    rename(districtName=districtName.x) %>%
    select(-districtName.y) %>%
    drop_na(districtName) %>%
    full_join(enrolled.act.2011, enrolled.act.2008_2018, by = c("districtNumber","districtType")) %>%
    rename(districtName=districtName.x) %>%
    select(-districtName.y) %>%
    drop_na(districtName) %>%
    full_join(enrolled.act.2010, enrolled.act.2008_2018, by = c("districtNumber","districtType")) %>%
    rename(districtName=districtName.x) %>%
    select(-districtName.y) %>%
    drop_na(districtName) %>%
    full_join(enrolled.act.2009, enrolled.act.2008_2018, by = c("districtNumber","districtType")) %>%
    rename(districtName=districtName.x) %>%
    select(-districtName.y) %>%
    drop_na(districtName) %>%
    full_join(enrolled.act.2008, enrolled.act.2008_2018, by = c("districtNumber","districtType")) %>%
    rename(districtName=districtName.x) %>%
    select(-districtName.y) %>%
    drop_na(districtName)
  
  act.percent.takers.2008_2018 <- full_join(enrolled.act.2008_2018, act.n.2008_2018, by = c("districtNumber")) %>%
    rename(districtName=districtName.x) %>%
    select(-districtName.y) %>%
    drop_na(districtName) %>%
    drop_na(N2018) %>%
    rbind(c(0001, 03, "MINNEAPOLIS PUBLIC SCHOOL DISTRICT",36357,36538,36645,36404,36283,35262,34436,34336,34441,34680,34999,2719,1963,1959,1777,1663,1239,1217,1206,1245,1195,1209)) %>%
    mutate(
      totalStudents2018=as.numeric(as.character(totalStudents2018)),
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
      N2018=as.numeric(as.character(N2018)),
      N2017=as.numeric(as.character(N2017)),
      N2016=as.numeric(as.character(N2016)),
      N2015=as.numeric(as.character(N2015)),
      N2014=as.numeric(as.character(N2014)),
      N2013=as.numeric(as.character(N2013)),
      N2012=as.numeric(as.character(N2012)),
      N2011=as.numeric(as.character(N2011)),
      N2010=as.numeric(as.character(N2010)),
      N2009=as.numeric(as.character(N2009)),
      N2008=as.numeric(as.character(N2008)),
      districtNumber=as.numeric(as.character(districtNumber)),
      districtType=as.numeric(as.character(districtType))
           ) %>%
    mutate(
      percent2018=N2018/totalStudents2018,
      percent2017=N2017/totalStudents2017,
      percent2016=N2016/totalStudents2016,
      percent2015=N2015/totalStudents2015,
      percent2014=N2014/totalStudents2014,
      percent2013=N2013/totalStudents2013,
      percent2012=N2012/totalStudents2012,
      percent2011=N2011/totalStudents2011,
      percent2010=N2010/totalStudents2010,
      percent2009=N2009/totalStudents2009,
      percent2008=N2008/totalStudents2008
    ) %>%
    mutate(
      districtType = formatC(districtType, width = 2, flag = "0"),
      districtNumber = formatC(districtNumber, width = 4, flag = "0"),
      districtName=toupper(districtName)
    ) %>%
    select(districtNumber, districtType, districtName, percent2018:percent2008) %>%
    gather(year,percentTakers,percent2018:percent2008) %>%
    mutate (
      year = replace(year, year =="percent2008" ,"2008"),
      year = replace(year, year =="percent2009" ,"2009"),
      year = replace(year, year =="percent2010" ,"2010"),
      year = replace(year, year =="percent2011" ,"2011"),
      year = replace(year, year =="percent2012" ,"2012"),
      year = replace(year, year =="percent2013" ,"2013"),
      year = replace(year, year =="percent2014" ,"2014"),
      year = replace(year, year =="percent2015" ,"2015"),
      year = replace(year, year =="percent2016" ,"2016"),
      year = replace(year, year =="percent2017" ,"2017"),
      year = replace(year, year =="percent2018" ,"2018")
    ) %>%
    write_csv("Education/ACT Scores/act_percent_2008_2018.csv")
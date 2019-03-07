library(tidyverse)


# Ethnicity of students enrolled (by district) ---------------------------------

#enrolled.ethnicity.county.2018 <- read_csv("2018_enrolled_ethnicity_county.csv") %>%
#  mutate(DistrictCountyNumber = formatC(DistrictCountyNumber, width = 3, flag = "0")) %>%
#  select (DistrictCountyNumber, DistrictCountyName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
#  group_by(DistrictCountyName) %>%
#  summarize(
#    AMI_Male = sum(AMI_Male),
#    AMI_Female = sum(AMI_Female),
#    ASI_Male = sum(ASI_Male),
#    ASI_Female = sum(ASI_Female),
#    BLK_Male = sum(BLK_Male), 
#    BLK_Female = sum(BLK_Female), 
#    HIS_Male = sum(HIS_Male), 
#    HIS_Female = sum(HIS_Female), 
#    HPI_Male = sum(HPI_Male), 
#    HPI_Female = sum(HPI_Female),
#    MLT_Male = sum(MLT_Male),
#    MLT_Female = sum(MLT_Female), 
#    WHT_Male = sum(WHT_Male), 
#    WHT_Female = sum(WHT_Female), 
#    TotalMale = sum(TotalMale), 
#    TotalFemale = sum(TotalFemale), 
#    TotalMinority = sum(TotalMinority), 
#    TotalStudents = sum(TotalStudents)
#  ) %>%
#  mutate(
#    percentMinority = TotalMinority/TotalStudents
#  )

#2018 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2018 <- read_csv("2018_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    ASI_Male = sum(ASI_Male),
    ASI_Female = sum(ASI_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    HPI_Male = sum(HPI_Male), 
    HPI_Female = sum(HPI_Female),
    MLT_Male = sum(MLT_Male),
    MLT_Female = sum(MLT_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents,
    DistrictName=toupper(DistrictName)
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2018 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "LA CRESCENT MONTESSORI & STEM SCHOO", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ACADEM", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
  )


#2017 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2017 <- read_csv("2017_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    ASI_Male = sum(ASI_Male),
    ASI_Female = sum(ASI_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    HPI_Male = sum(HPI_Male), 
    HPI_Female = sum(HPI_Female),
    MLT_Male = sum(MLT_Male),
    MLT_Female = sum(MLT_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents,
    DistrictName=toupper(DistrictName)
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2017 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "LA CRESCENT MONTESSORI & STEM SCHOO", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ACADEM", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
  )

#2016 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2016 <- read_csv("2016_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    ASI_Male = sum(ASI_Male),
    ASI_Female = sum(ASI_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    HPI_Male = sum(HPI_Male), 
    HPI_Female = sum(HPI_Female),
    MLT_Male = sum(MLT_Male),
    MLT_Female = sum(MLT_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2016 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "LA CRESCENT MONTESSORI & STEM SCHOO", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ACADEM", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
  )


#2015 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2015 <- read_csv("2015_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    ASI_Male = sum(ASI_Male),
    ASI_Female = sum(ASI_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    HPI_Male = sum(HPI_Male), 
    HPI_Female = sum(HPI_Female),
    MLT_Male = sum(MLT_Male),
    MLT_Female = sum(MLT_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2015 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "JENNINGS COMMUNITY LEARNING CENTER", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "LA CRESCENT MONTESSORI & STEM SCHOO", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ACADEM", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
  )

#2014 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2014 <- read_csv("2014_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    ASI_Male = sum(ASI_Male),
    ASI_Female = sum(ASI_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    HPI_Male = sum(HPI_Male), 
    HPI_Female = sum(HPI_Female),
    MLT_Male = sum(MLT_Male),
    MLT_Female = sum(MLT_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2014 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "JENNINGS COMMUNITY LEARNING CENTER", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ACADEM", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
  )


#2013 to 2003 data uses API (Asian/Pacific Islander) instead of ASI and HPI separately. It does not have MULTI

#2013 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2013 <- read_csv("2013_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2013 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL-SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "JENNINGS COMMUNITY LEARNING CENTER", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ADMY", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
  )

#2012 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2012 <- read_csv("2012_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2012 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL-SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HIAWATHA LEADERSHIP ACADEMY", "HIAWATHA ACADEMIES"),
    DistrictName = str_replace(DistrictName, "JENNINGS COMMUNITY LEARNING CENTER", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH SCHOOL DISTRICT", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ADMY", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
  )

#2011 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2011 <- read_csv("2011_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2011 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAST RANGE ACADEMY OF TECH&SCIENCE", "EAST RANGE ACADEMY OF TECH-SCIENCE"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HIAWATHA LEADERSHIP ACADEMY", "HIAWATHA ACADEMIES"),
    DistrictName = str_replace(DistrictName, "JENNINGS COMMUNITY LEARNING CENTER", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH SCHOOL DISTRICT", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ADMY", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
    
  )

#2010 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2010 <- read_csv("2010_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, Total_Male, Total_Female, TotalMinority, TotalStudents) %>%
group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    Total_Male = sum(Total_Male), 
    Total_Female = sum(Total_Female), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2010 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAST RANGE ACADEMY OF TECH &SCIENCE", "EAST RANGE ACADEMY OF TECH-SCIENCE"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HIAWATHA LEADERSHIP ACADEMY", "HIAWATHA ACADEMIES"),
    DistrictName = str_replace(DistrictName, "JENNINGS COMMUNITY LEARNING CENTER", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH SCHOOL DISTRICT", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ADMY", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
  )
  


#2009 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2009 <- read_csv("2009_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2009 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAST RANGE ACADEMY OF TECH &SCIENCE", "EAST RANGE ACADEMY OF TECH-SCIENCE"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HIAWATHA LEADERSHIP ACADEMY", "HIAWATHA ACADEMIES"),
    DistrictName = str_replace(DistrictName, "JENNINGS COMMUNITY LEARNING CENTER", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "KASSON-MANTORVILLE SCHOOL DIST.", "KASSON-MANTORVILLE SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH SCHOOL DISTRICT", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ADMY", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
  )

#2008 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2008 <- read_csv("2008_enrolled_ethnicity_district.csv") %>%
  select (DistrictNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2008 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAST RANGE ACADEMY OF TECH &SCIENCE", "EAST RANGE ACADEMY OF TECH-SCIENCE"),
    DistrictName = str_replace(DistrictName, "GRANADA HUNTLEY-EAST CHAIN #2536", "GRANADA HUNTLEY-EAST CHAIN"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HIAWATHA LEADERSHIP ACADEMY", "HIAWATHA ACADEMIES"),
    DistrictName = str_replace(DistrictName, "HMONG ACADEMY", "HMONG COLLEGE PREP ACADEMY"),
    DistrictName = str_replace(DistrictName, "JENNINGS COMMUNITY LEARNING CENTER", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "KASSON-MANTORVILLE SCHOOL DIST.", "KASSON-MANTORVILLE SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH SCHOOL DISTRICT", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ADMY", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
    
  )

#2007 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2007 <- read_csv("2007_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2007 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRANADA HUNTLEY-EAST CHAIN #2536", "GRANADA HUNTLEY-EAST CHAIN"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HMONG ACADEMY", "HMONG COLLEGE PREP ACADEMY"),
    DistrictName = str_replace(DistrictName, "JENNINGS EXPERIENTIAL HIGH SCHOOL", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "KASSON-MANTORVILLE SCHOOL DIST.", "KASSON-MANTORVILLE SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH SCHOOL DISTRICT", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ADMY", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
  )

#2006 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2006 <- read_csv("2006_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2006 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRANADA HUNTLEY-EAST CHAIN #2536", "GRANADA HUNTLEY-EAST CHAIN"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HMONG ACADEMY", "HMONG COLLEGE PREP ACADEMY"),
    DistrictName = str_replace(DistrictName, "JENNINGS EXPERIENTIAL HIGH SCHOOL", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "KASSON-MANTORVILLE SCHOOL DIST.", "KASSON-MANTORVILLE SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH SCHOOL DISTRICT", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ADMY", "LAKES INTERNATIONAL LANGUAGE ACADEMY")
  )

#2005 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2005 <- read_csv("2005_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2005 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADA-BORUP", "ADA-BORUP PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADRIAN", "ADRIAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AITKIN", "AITKIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBANY", "ALBANY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBERT LEA", "ALBERT LEA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALDEN", "ALDEN-CONGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALEXANDRIA", "ALEXANDRIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANOKA-HENNEPIN", "ANOKA-HENNEPIN PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ANNANDALE", "ANNANDALE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ASHBY", "ASHBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AUSTIN", "AUSTIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BADGER", "BADGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BAGLEY", "BAGLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BALATON", "BALATON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BARNESVILLE", "BARNESVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "BARNUM", "BARNUM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BATTLE LAKE", "BATTLE LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BECKER", "BECKER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLE PLAINE", "BELLE PLAINE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLINGHAM", "BELLINGHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BEMIDJI", "BEMIDJI PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BIG LAKE", "BIG LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLACKDUCK", "BLACKDUCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLOOMING PRAIRIE", "BLOOMING PRAIRIE PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BLOOMINGTON", "BLOOMINGTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAHAM", "BRAHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAINERD", "BRAINERD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRANDON", "BRANDON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRECKENRIDGE", "BRECKENRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BREWSTER", "BREWSTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROOKLYN CENTER", "BROOKLYN CENTER SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWERVILLE", "BROWERVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWNS VALLEY", "BROWNS VALLEY PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BURNSVILLE", "BURNSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BUTTERFIELD", "BUTTERFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BYRON", "BYRON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CALEDONIA", "CALEDONIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CAMBRIDGE-ISANTI", "CAMBRIDGE-ISANTI PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "CAMPBELL-TINTAH", "CAMPBELL-TINTAH PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CANBY", "CANBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CANNON FALLS", "CANNON FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CARLTON", "CARLTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CASS LAKE-BENA SCHOOLS", "CASS LAKE-BENA PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CEDAR MOUNTAIN", "CEDAR MOUNTAIN PUBLIC SCHOOL DISTRICT"),
    #CENTENNIAL PUBLIC SCHOOL DISTRICT
    DistrictName = str_replace(DistrictName, "CHASKA", "CHASKA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHATFIELD", "CHATFIELD PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CHISHOLM", "CHISHOLM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHISAGO LAKES", "CHISAGO LAKES SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHOKIO-ALBERTA", "CHOKIO-ALBERTA PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CLEARBROOK-GONVICK", "CLEARBROOK-GONVICK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLEVELAND", "CLEVELAND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLIMAX", "CLIMAX PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLOQUET", "CLOQUET PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COLUMBIA HEIGHTS", "COLUMBIA HEIGHTS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "COMFREY", "COMFREY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COOK COUNTY", "COOK COUNTY PUBLIC SCHOOLS"),
    #CROMWELL
    DistrictName = str_replace(DistrictName, "CROOKSTON", "CROOKSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CROSBY-IRONTON", "CROSBY-IRONTON PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CYRUS", "CYRUS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DASSEL-COKATO", "DASSEL-COKATO PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DAWSON-BOYD", "DAWSON-BOYD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DEER RIVER", "DEER RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DELANO", "DELANO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DETROIT LAKES", "DETROIT LAKES PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DOVER-EYOTA", "DOVER-EYOTA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DULUTH", "DULUTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAGLE VALLEY PUBLIC SCHOOLS", "EAGLE VALLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAST GRAND FORKS", "EAST GRAND FORKS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "EDEN PRAIRIE", "EDEN PRAIRIE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDEN VALLEY-WATKINS", "EDEN VALLEY-WATKINS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDGERTON", "EDGERTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDINA", "EDINA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELGIN-MILLVILLE", "ELGIN-MILLVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ELK RIVER", "ELK RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELLSWORTH", "ELLSWORTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELY", "ELY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ESKO", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVANSVILLE", "EVANSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVELETH-GILBERT", "EVELETH-GILBERT PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FAIRMONT AREA SCHOOLS", "FAIRMONT AREA SCHOOL DISTRICT"),
    #FARIBAULT
    DistrictName = str_replace(DistrictName, "FARMINGTON", "FARMINGTON PUBLIC SCHOOL DISTRICT"),
    #FERGUS FALLS
    DistrictName = str_replace(DistrictName, "FERGUS FALLS AREA SP ED COOP", "FERGUS FALLS AREA SP. ED. COOP."),
    DistrictName = str_replace(DistrictName, "FERTILE-BELTRAMI", "FERTILE-BELTRAMI SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FISHER", "FISHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FLOODWOOD", "FLOODWOOD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOLEY", "FOLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOREST LAKE", "FOREST LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOSSTON", "FOSSTON PUBLIC SCHOOL DISTRICT"),
    #FRAZZEE
    DistrictName = str_replace(DistrictName, "FRIDLEY", "FRIDLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FULDA", "FULDA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GLENCOE-SILVER LAKE", "GLENCOE-SILVER LAKE SCHOOL DISTRICT"),
    #Goodhue
    DistrictName = str_replace(DistrictName, "GOODRIDGE", "GOODRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND MEADOW", "GRAND MEADOW PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND RAPIDS", "GRAND RAPIDS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GREENBUSH-MIDDLE RIVER", "GREENBUSH-MIDDLE RIVER SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "GREENWAY", "GREENWAY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRYGLA", "GRYGLA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HANCOCK", "HANCOCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),DistrictName = str_replace(DistrictName, "HASTINGS", "HASTINGS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAWLEY", "HAWLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAYFIELD", "HAYFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENDRICKS", "HENDRICKS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENNING", "HENNING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMAN-NORCROSS", "HERMAN-NORCROSS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMANTOWN", "HERMANTOWN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERON LAKE-OKABENA", "HERON LAKE-OKABENA SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HIBBING", "HIBBING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILL CITY", "HILL CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILLS-BEAVER CREEK", "HILLS-BEAVER CREEK SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HINCKLEY-FINLAYSON", "HINCKLEY-FINLAYSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOLDINGFORD", "HOLDINGFORD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOPKINS", "HOPKINS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOUSTON", "HOUSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HUTCHINSON", "HUTCHINSON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "INTERNATIONAL FALLS", "INTERNATIONAL FALLS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ISLE", "ISLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "IVANHOE", "IVANHOE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HMONG ACADEMY", "HMONG COLLEGE PREP ACADEMY"),
    DistrictName = str_replace(DistrictName, "JACKSON COUNTY CENTRAL", "JACKSON COUNTY CENTRAL SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "JORDAN", "JORDAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KELLIHER", "KELLIHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KENYON-WANAMINGO", "KENYON-WANAMINGO SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KIMBALL", "KIMBALL PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KINGSLAND", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KITTSON CENTRAL", "KITTSON CENTRAL SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAC QUI PARLE VALLEY", "LAC QUI PARLE VALLEY SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LAKE BENTON", "LAKE BENTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE CITY", "LAKE CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE OF THE WOODS", "LAKE OF THE WOODS SCHOOL DISTRICT"),
    #Lake Superior Public School Dist
    DistrictName = str_replace(DistrictName, "LAKEVIEW", "LAKEVIEW SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKEVILLE", "LAKEVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANCASTER", "LANCASTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANESBORO", "LANESBORO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAPORTE", "LAPORTE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LECENTER", "LECENTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEROY", "LEROY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LESTER PRAIRIE", "LESTER PRAIRIE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LESUEUR-HENDERSON", "LESUEUR-HENDERSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITCHFIELD", "LITCHFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLE FALLS", "LITTLE FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLEFORK-BIG FALLS", "LITTLEFORK-BIG FALLS SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LONG PRAIRIE-GREY EAGLE", "LONG PRAIRIE-GREY EAGLE SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "LUVERNE", "LUVERNE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYLE", "LYLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYND", "LYND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "JENNINGS EXPERIENTIAL HIGH SCHOOL", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "KASSON-MANTORVILLE", "KASSON-MANTORVILLE SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE AGASSIZ SP. ED. COOP.", "LAKE AGASSIZ SPECIAL ED. COOP."),
    DistrictName = str_replace(DistrictName, "LAKE PARK AUDUBON DISTRICT", "LAKE PARK AUDUBON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKES INTERNATIONAL LANGUAGE ADMY", "LAKES INTERNATIONAL LANGUAGE ACADEMY"),
    DistrictName = str_replace(DistrictName, "LEWISTON-ALTURA", "LEWISTON-ALTURA PUBLIC SCHOOL DIST.")
  )

#2004 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2004 <- read_csv("2004_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2004 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADA-BORUP", "ADA-BORUP PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADRIAN", "ADRIAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AITKIN", "AITKIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBANY", "ALBANY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBERT LEA", "ALBERT LEA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALDEN", "ALDEN-CONGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALEXANDRIA", "ALEXANDRIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANNANDALE", "ANNANDALE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANOKA-HENNEPIN", "ANOKA-HENNEPIN PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ASHBY", "ASHBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AUSTIN", "AUSTIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BADGER", "BADGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BAGLEY", "BAGLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BALATON", "BALATON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BARNESVILLE", "BARNESVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "BARNUM", "BARNUM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BATTLE LAKE", "BATTLE LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BECKER", "BECKER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLE PLAINE", "BELLE PLAINE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLINGHAM", "BELLINGHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BEMIDJI", "BEMIDJI PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BIG LAKE", "BIG LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLACKDUCK", "BLACKDUCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLOOMING PRAIRIE", "BLOOMING PRAIRIE PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BLOOMINGTON", "BLOOMINGTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAHAM", "BRAHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAINERD", "BRAINERD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRANDON", "BRANDON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRECKENRIDGE", "BRECKENRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BREWSTER", "BREWSTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROOKLYN CENTER", "BROOKLYN CENTER SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWERVILLE", "BROWERVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWNS VALLEY", "BROWNS VALLEY PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BURNSVILLE", "BURNSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BUTTERFIELD", "BUTTERFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BYRON", "BYRON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CALEDONIA", "CALEDONIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CAMBRIDGE-ISANTI", "CAMBRIDGE-ISANTI PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "CAMPBELL-TINTAH", "CAMPBELL-TINTAH PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CANBY", "CANBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CANNON FALLS", "CANNON FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CARLTON", "CARLTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CASS LAKE-BENA SCHOOLS", "CASS LAKE-BENA PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CEDAR MOUNTAIN", "CEDAR MOUNTAIN PUBLIC SCHOOL DISTRICT"),
    #CENTENNIAL PUBLIC SCHOOL DISTRICT
    DistrictName = str_replace(DistrictName, "CHASKA", "CHASKA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHATFIELD", "CHATFIELD PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CHISHOLM", "CHISHOLM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHISAGO LAKES", "CHISAGO LAKES SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHOKIO-ALBERTA", "CHOKIO-ALBERTA PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CLEARBROOK-GONVICK", "CLEARBROOK-GONVICK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLEVELAND", "CLEVELAND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLIMAX", "CLIMAX PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLOQUET", "CLOQUET PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COLUMBIA HEIGHTS", "COLUMBIA HEIGHTS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "COMFREY", "COMFREY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COOK COUNTY", "COOK COUNTY PUBLIC SCHOOLS"),
    #CROMWELL
    DistrictName = str_replace(DistrictName, "CROOKSTON", "CROOKSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CROSBY-IRONTON", "CROSBY-IRONTON PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CYRUS", "CYRUS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DASSEL-COKATO", "DASSEL-COKATO PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DAWSON-BOYD", "DAWSON-BOYD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DEER RIVER", "DEER RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DELANO", "DELANO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DETROIT LAKES", "DETROIT LAKES PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DOVER-EYOTA", "DOVER-EYOTA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DULUTH", "DULUTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAGLE VALLEY PUBLIC SCHOOLS", "EAGLE VALLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAST GRAND FORKS", "EAST GRAND FORKS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "EDEN PRAIRIE", "EDEN PRAIRIE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDEN VALLEY-WATKINS", "EDEN VALLEY-WATKINS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDGERTON", "EDGERTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDINA", "EDINA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELGIN-MILLVILLE", "ELGIN-MILLVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ELK RIVER", "ELK RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELLSWORTH", "ELLSWORTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELY", "ELY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ESKO", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVANSVILLE", "EVANSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVELETH-GILBERT", "EVELETH-GILBERT PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FAIRMONT AREA SCHOOLS", "FAIRMONT AREA SCHOOL DISTRICT"),
    #FARIBAULT
    DistrictName = str_replace(DistrictName, "FARMINGTON", "FARMINGTON PUBLIC SCHOOL DISTRICT"),
    #FERGUS FALLS
    DistrictName = str_replace(DistrictName, "FERGUS FALLS AREA SP ED COOP", "FERGUS FALLS AREA SP. ED. COOP."),
    DistrictName = str_replace(DistrictName, "FERTILE-BELTRAMI", "FERTILE-BELTRAMI SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FISHER", "FISHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FLOODWOOD", "FLOODWOOD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOLEY", "FOLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOREST LAKE", "FOREST LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOSSTON", "FOSSTON PUBLIC SCHOOL DISTRICT"),
    #FRAZZEE
    DistrictName = str_replace(DistrictName, "FRIDLEY", "FRIDLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FULDA", "FULDA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GLENCOE-SILVER LAKE", "GLENCOE-SILVER LAKE SCHOOL DISTRICT"),
    #Goodhue
    DistrictName = str_replace(DistrictName, "GOODRIDGE", "GOODRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND MEADOW", "GRAND MEADOW PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND RAPIDS", "GRAND RAPIDS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GREENBUSH-MIDDLE RIVER", "GREENBUSH-MIDDLE RIVER SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "GREENWAY", "GREENWAY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRYGLA", "GRYGLA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HANCOCK", "HANCOCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HASTINGS", "HASTINGS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAWLEY", "HAWLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAYFIELD", "HAYFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENDRICKS", "HENDRICKS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENNING", "HENNING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMAN-NORCROSS", "HERMAN-NORCROSS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMANTOWN", "HERMANTOWN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERON LAKE-OKABENA", "HERON LAKE-OKABENA SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HIBBING", "HIBBING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILL CITY", "HILL CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILLS-BEAVER CREEK", "HILLS-BEAVER CREEK SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HINCKLEY-FINLAYSON", "HINCKLEY-FINLAYSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOLDINGFORD", "HOLDINGFORD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOPKINS", "HOPKINS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOUSTON", "HOUSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HUTCHINSON", "HUTCHINSON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "INTERNATIONAL FALLS", "INTERNATIONAL FALLS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ISLE", "ISLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "IVANHOE", "IVANHOE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "JACKSON COUNTY CENTRAL", "JACKSON COUNTY CENTRAL SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "JORDAN", "JORDAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KELLIHER", "KELLIHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KENYON-WANAMINGO", "KENYON-WANAMINGO SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KIMBALL", "KIMBALL PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KINGSLAND", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KITTSON CENTRAL", "KITTSON CENTRAL SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAC QUI PARLE VALLEY", "LAC QUI PARLE VALLEY SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LAKE BENTON", "LAKE BENTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE CITY", "LAKE CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE OF THE WOODS", "LAKE OF THE WOODS SCHOOL DISTRICT"),
    #Lake Superior Public School Dist
    DistrictName = str_replace(DistrictName, "LAKEVIEW", "LAKEVIEW SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKEVILLE", "LAKEVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANCASTER", "LANCASTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANESBORO", "LANESBORO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAPORTE", "LAPORTE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LECENTER", "LECENTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEROY", "LEROY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LESTER PRAIRIE", "LESTER PRAIRIE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LESUEUR-HENDERSON", "LESUEUR-HENDERSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITCHFIELD", "LITCHFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLE FALLS", "LITTLE FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLEFORK-BIG FALLS", "LITTLEFORK-BIG FALLS SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LONG PRAIRIE-GREY EAGLE", "LONG PRAIRIE-GREY EAGLE SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "LUVERNE", "LUVERNE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYLE", "LYLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYND", "LYND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "JENNINGS EXPERIENTIAL HIGH SCHOOL", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "KASSON-MANTORVILLE", "KASSON-MANTORVILLE SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE AGASSIZ SP. ED. COOP.", "LAKE AGASSIZ SPECIAL ED. COOP."),
    DistrictName = str_replace(DistrictName, "LAKE PARK AUDUBON DISTRICT", "LAKE PARK AUDUBON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEWISTON-ALTURA", "LEWISTON-ALTURA PUBLIC SCHOOL DIST.")
  )

#2003 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2003 <- read_csv("2003_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2003 = percentMinority) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADA-BORUP", "ADA-BORUP PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADRIAN", "ADRIAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AITKIN", "AITKIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBANY", "ALBANY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBERT LEA", "ALBERT LEA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALDEN", "ALDEN-CONGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALEXANDRIA", "ALEXANDRIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANNANDALE", "ANNANDALE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANOKA-HENNEPIN", "ANOKA-HENNEPIN PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ASHBY", "ASHBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AUSTIN", "AUSTIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BADGER", "BADGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BAGLEY", "BAGLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BALATON", "BALATON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BARNESVILLE", "BARNESVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "BARNUM", "BARNUM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BATTLE LAKE", "BATTLE LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BECKER", "BECKER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLE PLAINE", "BELLE PLAINE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLINGHAM", "BELLINGHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BEMIDJI", "BEMIDJI PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BIG LAKE", "BIG LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLACKDUCK", "BLACKDUCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLOOMING PRAIRIE", "BLOOMING PRAIRIE PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BLOOMINGTON", "BLOOMINGTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAHAM", "BRAHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAINERD", "BRAINERD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRANDON", "BRANDON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRECKENRIDGE", "BRECKENRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BREWSTER", "BREWSTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROOKLYN CENTER", "BROOKLYN CENTER SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWERVILLE", "BROWERVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWNS VALLEY", "BROWNS VALLEY PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BURNSVILLE", "BURNSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BUTTERFIELD", "BUTTERFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BYRON", "BYRON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CALEDONIA", "CALEDONIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CAMBRIDGE-ISANTI", "CAMBRIDGE-ISANTI PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "CAMPBELL-TINTAH", "CAMPBELL-TINTAH PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CANBY", "CANBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CANNON FALLS", "CANNON FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CARLTON", "CARLTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CASS LAKE-BENA SCHOOLS", "CASS LAKE-BENA PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CEDAR MOUNTAIN", "CEDAR MOUNTAIN PUBLIC SCHOOL DISTRICT"),
    #CENTENNIAL PUBLIC SCHOOL DISTRICT
    DistrictName = str_replace(DistrictName, "CHASKA", "CHASKA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHATFIELD", "CHATFIELD PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CHISHOLM", "CHISHOLM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHISAGO LAKES", "CHISAGO LAKES SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHOKIO-ALBERTA", "CHOKIO-ALBERTA PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CLEARBROOK-GONVICK", "CLEARBROOK-GONVICK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLEVELAND", "CLEVELAND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLIMAX", "CLIMAX PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLOQUET", "CLOQUET PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COLUMBIA HEIGHTS", "COLUMBIA HEIGHTS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "COMFREY", "COMFREY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COOK COUNTY", "COOK COUNTY PUBLIC SCHOOLS"),
    #CROMWELL
    DistrictName = str_replace(DistrictName, "CROOKSTON", "CROOKSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CROSBY-IRONTON", "CROSBY-IRONTON PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CROW RIVER SP. ED. COOP.", "CROW RIVER SPECIAL EDUCATION COOP."),
    DistrictName = str_replace(DistrictName, "CYRUS", "CYRUS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DASSEL-COKATO", "DASSEL-COKATO PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DAWSON-BOYD", "DAWSON-BOYD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DEER RIVER", "DEER RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DELANO", "DELANO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DETROIT LAKES", "DETROIT LAKES PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DOVER-EYOTA", "DOVER-EYOTA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DULUTH", "DULUTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAGLE VALLEY", "EAGLE VALLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAST GRAND FORKS", "EAST GRAND FORKS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "EDEN PRAIRIE", "EDEN PRAIRIE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDEN VALLEY-WATKINS", "EDEN VALLEY-WATKINS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDGERTON", "EDGERTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDINA", "EDINA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELGIN-MILLVILLE", "ELGIN-MILLVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ELK RIVER", "ELK RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELLSWORTH", "ELLSWORTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELY", "ELY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ESKO", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVANSVILLE", "EVANSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVELETH-GILBERT", "EVELETH-GILBERT PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FAIRMONT AREA SCHOOLS", "FAIRMONT AREA SCHOOL DISTRICT"),
    #FARIBAULT
    DistrictName = str_replace(DistrictName, "FARMINGTON", "FARMINGTON PUBLIC SCHOOL DISTRICT"),
    #FERGUS FALLS
    DistrictName = str_replace(DistrictName, "FERGUS FALLS AREA SP ED COOP", "FERGUS FALLS AREA SP. ED. COOP."),
    DistrictName = str_replace(DistrictName, "FERTILE-BELTRAMI", "FERTILE-BELTRAMI SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FISHER", "FISHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FLOODWOOD", "FLOODWOOD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOLEY", "FOLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOREST LAKE", "FOREST LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOSSTON", "FOSSTON PUBLIC SCHOOL DISTRICT"),
    #FRAZZEE
    DistrictName = str_replace(DistrictName, "FRIDLEY", "FRIDLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FULDA", "FULDA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GLENCOE-SILVER LAKE", "GLENCOE-SILVER LAKE SCHOOL DISTRICT"),
    #Goodhue
    DistrictName = str_replace(DistrictName, "GOODRIDGE", "GOODRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND MEADOW", "GRAND MEADOW PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND RAPIDS", "GRAND RAPIDS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GREENBUSH-MIDDLE RIVER", "GREENBUSH-MIDDLE RIVER SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "GREENWAY", "GREENWAY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRYGLA", "GRYGLA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HANCOCK", "HANCOCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HASTINGS", "HASTINGS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAWLEY", "HAWLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAYFIELD", "HAYFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENDRICKS", "HENDRICKS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENNING", "HENNING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMAN-NORCROSS", "HERMAN-NORCROSS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMANTOWN", "HERMANTOWN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERON LAKE-OKABENA", "HERON LAKE-OKABENA SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HIBBING", "HIBBING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILL CITY", "HILL CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILLS-BEAVER CREEK", "HILLS-BEAVER CREEK SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HINCKLEY-FINLAYSON", "HINCKLEY-FINLAYSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOLDINGFORD", "HOLDINGFORD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOPKINS", "HOPKINS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOUSTON", "HOUSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HUTCHINSON", "HUTCHINSON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "INTERNATIONAL FALLS", "INTERNATIONAL FALLS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ISLE", "ISLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "IVANHOE", "IVANHOE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "INVER GROVE", "INVER GROVE HEIGHTS SCHOOLS"),
    DistrictName = str_replace(DistrictName, "JACKSON COUNTY CENTRAL", "JACKSON COUNTY CENTRAL SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "JORDAN", "JORDAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KELLIHER", "KELLIHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KENYON-WANAMINGO", "KENYON-WANAMINGO SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KIMBALL", "KIMBALL PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KINGSLAND", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KITTSON CENTRAL", "KITTSON CENTRAL SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAC QUI PARLE VALLEY", "LAC QUI PARLE VALLEY SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LAKE BENTON", "LAKE BENTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE CITY", "LAKE CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE OF THE WOODS", "LAKE OF THE WOODS SCHOOL DISTRICT"),
    #Lake Superior Public School Dist
    DistrictName = str_replace(DistrictName, "LAKEVIEW", "LAKEVIEW SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKEVILLE", "LAKEVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANCASTER", "LANCASTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANESBORO", "LANESBORO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAPORTE", "LAPORTE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LECENTER", "LECENTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEROY", "LEROY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LESTER PRAIRIE", "LESTER PRAIRIE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LESUEUR-HENDERSON", "LESUEUR-HENDERSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITCHFIELD", "LITCHFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLE FALLS", "LITTLE FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLEFORK-BIG FALLS", "LITTLEFORK-BIG FALLS SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LONG PRAIRIE-GREY EAGLE", "LONG PRAIRIE-GREY EAGLE SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "LUVERNE", "LUVERNE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYLE", "LYLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYND", "LYND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "JENNINGS EXPERIENTIAL HIGH SCHOOL", "JENNINGS COMMUNITY SCHOOL"),
    DistrictName = str_replace(DistrictName, "KASSON-MANTORVILLE", "KASSON-MANTORVILLE SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE AGASSIZ SP. ED. COOP.", "LAKE AGASSIZ SPECIAL ED. COOP."),
    DistrictName = str_replace(DistrictName, "LAKE PARK AUDUBON DISTRICT", "LAKE PARK AUDUBON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEWISTON-ALTURA", "LEWISTON-ALTURA PUBLIC SCHOOL DIST.")
  )

#2002 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2002 <- read_csv("2002_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_nam, ami_mal, ami_fem, api_mal, api_fem, his_mal, his_fem, blk_mal, blk_fem, wht_mal, wht_fem, tot_mal, tot_fem, tot_mny, tot_tot) %>%
  group_by(dst_nam) %>%
  summarize(
    ami_mal = sum(ami_mal),
    ami_fem = sum(ami_fem),
    api_mal = sum(api_mal),
    api_fem = sum(api_fem),
    his_mal = sum(his_mal),
    his_fem = sum(his_fem),
    blk_mal = sum(blk_mal),
    blk_fem = sum(blk_fem),
    wht_mal = sum(wht_mal),
    wht_fem = sum(wht_fem),
    tot_mal = sum(tot_mal),
    tot_fem = sum(tot_fem),
    tot_mny = sum(tot_mny),
    tot_tot = sum(tot_tot)
  ) %>%
  mutate(
    percentMinority = tot_mny/tot_tot
  ) %>%
  select(dst_nam,percentMinority) %>%
  rename(
    DistrictName = dst_nam,
    percentMinority2002 = percentMinority
  ) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADA-BORUP", "ADA-BORUP PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADRIAN", "ADRIAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AITKIN", "AITKIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBANY", "ALBANY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBERT LEA", "ALBERT LEA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALDEN", "ALDEN-CONGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALEXANDRIA", "ALEXANDRIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANNANDALE", "ANNANDALE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANOKA-HENNEPIN", "ANOKA-HENNEPIN PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ASHBY", "ASHBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AUSTIN", "AUSTIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BADGER", "BADGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BAGLEY", "BAGLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BALATON", "BALATON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BARNESVILLE", "BARNESVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "BARNUM", "BARNUM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BATTLE LAKE", "BATTLE LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BECKER", "BECKER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLE PLAINE", "BELLE PLAINE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLINGHAM", "BELLINGHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BEMIDJI", "BEMIDJI PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BIG LAKE", "BIG LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLACKDUCK", "BLACKDUCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLOOMING PRAIRIE", "BLOOMING PRAIRIE PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BLOOMINGTON", "BLOOMINGTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAHAM", "BRAHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAINERD", "BRAINERD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRANDON", "BRANDON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRECKENRIDGE", "BRECKENRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BREWSTER", "BREWSTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROOKLYN CENTER", "BROOKLYN CENTER SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWERVILLE", "BROWERVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWNS VALLEY", "BROWNS VALLEY PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BURNSVILLE", "BURNSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BUTTERFIELD", "BUTTERFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BYRON", "BYRON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CALEDONIA", "CALEDONIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CAMBRIDGE-ISANTI", "CAMBRIDGE-ISANTI PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "CAMPBELL-TINTAH", "CAMPBELL-TINTAH PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CANBY", "CANBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CANNON FALLS", "CANNON FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CARLTON", "CARLTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CASS LAKE-BENA SCHOOLS", "CASS LAKE-BENA PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CEDAR MOUNTAIN", "CEDAR MOUNTAIN PUBLIC SCHOOL DISTRICT"),
    #CENTENNIAL PUBLIC SCHOOL DISTRICT
    DistrictName = str_replace(DistrictName, "CHASKA", "CHASKA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHATFIELD", "CHATFIELD PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CHISHOLM", "CHISHOLM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHISAGO LAKES", "CHISAGO LAKES SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHOKIO-ALBERTA", "CHOKIO-ALBERTA PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CLEARBROOK-GONVICK", "CLEARBROOK-GONVICK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLEVELAND", "CLEVELAND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLIMAX", "CLIMAX PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLOQUET", "CLOQUET PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COLUMBIA HEIGHTS", "COLUMBIA HEIGHTS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "COMFREY", "COMFREY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COOK COUNTY", "COOK COUNTY PUBLIC SCHOOLS"),
    #CROMWELL
    DistrictName = str_replace(DistrictName, "CROOKSTON", "CROOKSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CROSBY-IRONTON", "CROSBY-IRONTON PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CYRUS", "CYRUS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DASSEL-COKATO", "DASSEL-COKATO PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DAWSON-BOYD", "DAWSON-BOYD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DEER RIVER", "DEER RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DELANO", "DELANO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DETROIT LAKES", "DETROIT LAKES PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DOVER-EYOTA", "DOVER-EYOTA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DULUTH", "DULUTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAGLE VALLEY", "EAGLE VALLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAST GRAND FORKS", "EAST GRAND FORKS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "EDEN PRAIRIE", "EDEN PRAIRIE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDEN VALLEY-WATKINS", "EDEN VALLEY-WATKINS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDGERTON", "EDGERTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDINA", "EDINA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELGIN-MILLVILLE", "ELGIN-MILLVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ELK RIVER", "ELK RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELLSWORTH", "ELLSWORTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELY", "ELY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ESKO", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVANSVILLE", "EVANSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVELETH-GILBERT", "EVELETH-GILBERT PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FAIRMONT AREA SCHOOLS", "FAIRMONT AREA SCHOOL DISTRICT"),
    #FARIBAULT
    DistrictName = str_replace(DistrictName, "FARMINGTON", "FARMINGTON PUBLIC SCHOOL DISTRICT"),
    #FERGUS FALLS
    DistrictName = str_replace(DistrictName, "FERGUS FALLS AREA SP ED COOP", "FERGUS FALLS AREA SP. ED. COOP."),
    DistrictName = str_replace(DistrictName, "FERTILE-BELTRAMI", "FERTILE-BELTRAMI SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FISHER", "FISHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FLOODWOOD", "FLOODWOOD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOLEY", "FOLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOREST LAKE", "FOREST LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOSSTON", "FOSSTON PUBLIC SCHOOL DISTRICT"),
    #FRAZZEE
    DistrictName = str_replace(DistrictName, "FRIDLEY", "FRIDLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FULDA", "FULDA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FRESHWATER ED. DISTRICT", "FRESHWATER ED. DIST."),
    DistrictName = str_replace(DistrictName, "GLENCOE-SILVER LAKE", "GLENCOE-SILVER LAKE SCHOOL DISTRICT"),
    #Goodhue
    DistrictName = str_replace(DistrictName, "GOODRIDGE", "GOODRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND MEADOW", "GRAND MEADOW PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND RAPIDS", "GRAND RAPIDS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GREENBUSH-MIDDLE RIVER", "GREENBUSH-MIDDLE RIVER SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "GREENWAY", "GREENWAY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRYGLA", "GRYGLA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HANCOCK", "HANCOCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HASTINGS", "HASTINGS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAWLEY", "HAWLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAYFIELD", "HAYFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENDRICKS", "HENDRICKS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENNING", "HENNING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMAN-NORCROSS", "HERMAN-NORCROSS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMANTOWN", "HERMANTOWN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERON LAKE-OKABENA", "HERON LAKE-OKABENA SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HIBBING", "HIBBING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILL CITY", "HILL CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILLS-BEAVER CREEK", "HILLS-BEAVER CREEK SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HINCKLEY-FINLAYSON", "HINCKLEY-FINLAYSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOLDINGFORD", "HOLDINGFORD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOPKINS", "HOPKINS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOUSTON", "HOUSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HUTCHINSON", "HUTCHINSON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "INTERNATIONAL FALLS", "INTERNATIONAL FALLS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ISLE", "ISLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "IVANHOE", "IVANHOE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "INVER GROVE", "INVER GROVE HEIGHTS SCHOOLS"),
    #Goodhue
    DistrictName = str_replace(DistrictName, "JACKSON COUNTY CENTRAL", "JACKSON COUNTY CENTRAL SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "JORDAN", "JORDAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KELLIHER", "KELLIHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KENYON-WANAMINGO", "KENYON-WANAMINGO SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KIMBALL", "KIMBALL PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KINGSLAND", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KITTSON CENTRAL", "KITTSON CENTRAL SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAC QUI PARLE VALLEY", "LAC QUI PARLE VALLEY SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LAKE BENTON", "LAKE BENTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE CITY", "LAKE CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE OF THE WOODS", "LAKE OF THE WOODS SCHOOL DISTRICT"),
    #Lake Superior Public School Dist
    DistrictName = str_replace(DistrictName, "LAKEVIEW", "LAKEVIEW SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKEVILLE", "LAKEVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANCASTER", "LANCASTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANESBORO", "LANESBORO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAPORTE", "LAPORTE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LECENTER", "LECENTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEROY", "LEROY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LESTER PRAIRIE", "LESTER PRAIRIE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LESUEUR-HENDERSON", "LESUEUR-HENDERSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITCHFIELD", "LITCHFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLE FALLS", "LITTLE FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLEFORK-BIG FALLS", "LITTLEFORK-BIG FALLS SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LONG PRAIRIE-GREY EAGLE", "LONG PRAIRIE-GREY EAGLE SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "LUVERNE", "LUVERNE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYLE", "LYLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYND", "LYND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KASSON-MANTORVILLE", "KASSON-MANTORVILLE SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE AGASSIZ SP. ED. COOP.", "LAKE AGASSIZ SPECIAL ED. COOP."),
    DistrictName = str_replace(DistrictName, "LAKE PARK AUDUBON DISTRICT", "LAKE PARK AUDUBON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEWISTON-ALTURA", "LEWISTON-ALTURA PUBLIC SCHOOL DIST.")
  )

#2001 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2001 <- read_csv("2001_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_nam, ami_mal, ami_fem, api_mal, api_fem, his_mal, his_fem, blk_mal, blk_fem, wht_mal, wht_fem, tot_mal, tot_fem, tot_mny, tot_tot) %>%
  group_by(dst_nam) %>%
  summarize(
    ami_mal = sum(ami_mal),
    ami_fem = sum(ami_fem),
    api_mal = sum(api_mal),
    api_fem = sum(api_fem),
    his_mal = sum(his_mal),
    his_fem = sum(his_fem),
    blk_mal = sum(blk_mal),
    blk_fem = sum(blk_fem),
    wht_mal = sum(wht_mal),
    wht_fem = sum(wht_fem),
    tot_mal = sum(tot_mal),
    tot_fem = sum(tot_fem),
    tot_mny = sum(tot_mny),
    tot_tot = sum(tot_tot)
  ) %>%
  mutate(
    percentMinority = tot_mny/tot_tot
  ) %>%
  select(dst_nam,percentMinority) %>%
  
  rename(
    DistrictName = dst_nam,
    percentMinority2001 = percentMinority
  ) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADA-BORUP", "ADA-BORUP PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADRIAN", "ADRIAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AITKIN", "AITKIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBANY", "ALBANY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBERT LEA", "ALBERT LEA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALDEN", "ALDEN-CONGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALEXANDRIA", "ALEXANDRIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANNANDALE", "ANNANDALE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANOKA-HENNEPIN", "ANOKA-HENNEPIN PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ASHBY", "ASHBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AUSTIN", "AUSTIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BADGER", "BADGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BAGLEY", "BAGLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BALATON", "BALATON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BARNESVILLE", "BARNESVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "BARNUM", "BARNUM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BATTLE LAKE", "BATTLE LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BECKER", "BECKER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLE PLAINE", "BELLE PLAINE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLINGHAM", "BELLINGHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BEMIDJI", "BEMIDJI PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BIG LAKE", "BIG LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BENTON-STEARNS ED. DIST.", "BENTON-STEARNS ED. DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLACKDUCK", "BLACKDUCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLOOMING PRAIRIE", "BLOOMING PRAIRIE PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BLOOMINGTON", "BLOOMINGTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAHAM", "BRAHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAINERD", "BRAINERD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRANDON", "BRANDON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRECKENRIDGE", "BRECKENRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BREWSTER", "BREWSTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROOKLYN CENTER", "BROOKLYN CENTER SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWERVILLE", "BROWERVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWNS VALLEY", "BROWNS VALLEY PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BURNSVILLE", "BURNSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BUTTERFIELD", "BUTTERFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BYRON", "BYRON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CALEDONIA", "CALEDONIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CAMBRIDGE-ISANTI", "CAMBRIDGE-ISANTI PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "CAMPBELL-TINTAH", "CAMPBELL-TINTAH PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CANBY", "CANBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CANNON FALLS", "CANNON FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CARLTON", "CARLTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CASS LAKE", "CASS LAKE-BENA PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CEDAR MOUNTAIN", "CEDAR MOUNTAIN PUBLIC SCHOOL DISTRICT"),
    #CENTENNIAL PUBLIC SCHOOL DISTRICT
    DistrictName = str_replace(DistrictName, "CHASKA", "CHASKA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHATFIELD", "CHATFIELD PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CHISHOLM", "CHISHOLM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHISAGO LAKES", "CHISAGO LAKES SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHOKIO-ALBERTA", "CHOKIO-ALBERTA PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CLEARBROOK-GONVICK", "CLEARBROOK-GONVICK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLEVELAND", "CLEVELAND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLIMAX", "CLIMAX PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLOQUET", "CLOQUET PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COLUMBIA HEIGHTS", "COLUMBIA HEIGHTS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "COMFREY", "COMFREY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COOK COUNTY", "COOK COUNTY PUBLIC SCHOOLS"),
    #CROMWELL
    DistrictName = str_replace(DistrictName, "CROOKSTON", "CROOKSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CROSBY-IRONTON", "CROSBY-IRONTON PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CYRUS", "CYRUS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CENTRAL MINNESOTA JOINT POWERS DIST", "CENTRAL MINNESOTA JT. POWERS DIST."),
    DistrictName = str_replace(DistrictName, "CROW RIVER SP ED COOP", "CROW RIVER SP. ED. COOP."),
    DistrictName = str_replace(DistrictName, "DASSEL-COKATO", "DASSEL-COKATO PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DAWSON-BOYD", "DAWSON-BOYD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DEER RIVER", "DEER RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DELANO", "DELANO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DETROIT LAKES", "DETROIT LAKES PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DOVER-EYOTA", "DOVER-EYOTA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DULUTH", "DULUTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAGLE VALLEY", "EAGLE VALLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAST GRAND FORKS", "EAST GRAND FORKS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "EDEN PRAIRIE", "EDEN PRAIRIE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDEN VALLEY-WATKINS", "EDEN VALLEY-WATKINS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDGERTON", "EDGERTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDINA", "EDINA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELGIN-MILLVILLE", "ELGIN-MILLVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ELK RIVER", "ELK RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELLSWORTH", "ELLSWORTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELY", "ELY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ESKO", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVANSVILLE", "EVANSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVELETH-GILBERT", "EVELETH-GILBERT PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FAIRMONT AREA SCHOOLS", "FAIRMONT AREA SCHOOL DISTRICT"),
    #FARIBAULT
    DistrictName = str_replace(DistrictName, "FARMINGTON", "FARMINGTON PUBLIC SCHOOL DISTRICT"),
    #FERGUS FALLS
    DistrictName = str_replace(DistrictName, "FERGUS FALLS AREA SP ED COOP", "FERGUS FALLS AREA SP. ED. COOP."),
    DistrictName = str_replace(DistrictName, "FERTILE-BELTRAMI", "FERTILE-BELTRAMI SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FISHER", "FISHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FLOODWOOD", "FLOODWOOD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOLEY", "FOLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOREST LAKE", "FOREST LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOSSTON", "FOSSTON PUBLIC SCHOOL DISTRICT"),
    #FRAZZEE
    DistrictName = str_replace(DistrictName, "FRIDLEY", "FRIDLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FULDA", "FULDA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GLENCOE-SILVER LAKE", "GLENCOE-SILVER LAKE SCHOOL DISTRICT"),
    #Goodhue
    DistrictName = str_replace(DistrictName, "GOODRIDGE", "GOODRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND MEADOW", "GRAND MEADOW PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND RAPIDS", "GRAND RAPIDS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GREENBUSH-MIDDLE RIVER", "GREENBUSH-MIDDLE RIVER SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "GREENWAY", "GREENWAY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRYGLA", "GRYGLA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HANCOCK", "HANCOCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HASTINGS", "HASTINGS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAWLEY", "HAWLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAYFIELD", "HAYFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENDRICKS", "HENDRICKS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENNING", "HENNING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMAN-NORCROSS", "HERMAN-NORCROSS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMANTOWN", "HERMANTOWN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERON LAKE-OKABENA", "HERON LAKE-OKABENA SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HIBBING", "HIBBING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILL CITY", "HILL CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILLS-BEAVER CREEK", "HILLS-BEAVER CREEK SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HINCKLEY-FINLAYSON", "HINCKLEY-FINLAYSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOLDINGFORD", "HOLDINGFORD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOPKINS", "HOPKINS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOUSTON", "HOUSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HUTCHINSON", "HUTCHINSON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "INTERNATIONAL FALLS", "INTERNATIONAL FALLS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ISLE", "ISLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "IVANHOE", "IVANHOE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HIAWATHA VALLEY ED. DIST.", "HIAWATHA VALLEY ED. DISTRICT"),
    DistrictName = str_replace(DistrictName, "INVER GROVE", "INVER GROVE HEIGHTS SCHOOLS"),
    DistrictName = str_replace(DistrictName, "JACKSON COUNTY CENTRAL", "JACKSON COUNTY CENTRAL SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "JORDAN", "JORDAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KELLIHER", "KELLIHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KENYON-WANAMINGO", "KENYON-WANAMINGO SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KIMBALL", "KIMBALL PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KINGSLAND", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KITTSON CENTRAL", "KITTSON CENTRAL SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAC QUI PARLE VALLEY", "LAC QUI PARLE VALLEY SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LAKE BENTON", "LAKE BENTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE CITY", "LAKE CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE OF THE WOODS", "LAKE OF THE WOODS SCHOOL DISTRICT"),
    #Lake Superior Public School Dist
    DistrictName = str_replace(DistrictName, "LAKEVIEW", "LAKEVIEW SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKEVILLE", "LAKEVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANCASTER", "LANCASTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANESBORO", "LANESBORO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAPORTE", "LAPORTE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LECENTER", "LECENTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEROY", "LEROY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LESTER PRAIRIE", "LESTER PRAIRIE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LESUEUR-HENDERSON", "LESUEUR-HENDERSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITCHFIELD", "LITCHFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLE FALLS", "LITTLE FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLEFORK-BIG FALLS", "LITTLEFORK-BIG FALLS SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LONG PRAIRIE-GREY EAGLE", "LONG PRAIRIE-GREY EAGLE SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "LUVERNE", "LUVERNE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYLE", "LYLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYND", "LYND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KASSON-MANTORVILLE", "KASSON-MANTORVILLE SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE AGASSIZ SP ED COOP", "LAKE AGASSIZ SPECIAL ED. COOP."),
    DistrictName = str_replace(DistrictName, "LAKE PARK AUDUBON DISTRICT", "LAKE PARK AUDUBON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEWISTON", "LEWISTON-ALTURA PUBLIC SCHOOL DIST.")
  )

#2000 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2000 <- read_csv("2000_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_nam, ami_mal, ami_fem, api_mal, api_fem, his_mal, his_fem, blk_mal, blk_fem, wht_mal, wht_fem, tot_mal, tot_fem, tot_mny, tot_tot) %>%
  group_by(dst_nam) %>%
  summarize(
    ami_mal = sum(ami_mal),
    ami_fem = sum(ami_fem),
    api_mal = sum(api_mal),
    api_fem = sum(api_fem),
    his_mal = sum(his_mal),
    his_fem = sum(his_fem),
    blk_mal = sum(blk_mal),
    blk_fem = sum(blk_fem),
    wht_mal = sum(wht_mal),
    wht_fem = sum(wht_fem),
    tot_mal = sum(tot_mal),
    tot_fem = sum(tot_fem),
    tot_mny = sum(tot_mny),
    tot_tot = sum(tot_tot)
  ) %>%
  mutate(
    percentMinority = tot_mny/tot_tot
  ) %>%
  select(dst_nam,percentMinority) %>%
  
  rename(
    DistrictName = dst_nam,
    percentMinority2000 = percentMinority
  ) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADA-BORUP", "ADA-BORUP PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADRIAN", "ADRIAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AITKIN", "AITKIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBANY", "ALBANY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBERT LEA", "ALBERT LEA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALDEN", "ALDEN-CONGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALEXANDRIA", "ALEXANDRIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANNANDALE", "ANNANDALE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANOKA-HENNEPIN", "ANOKA-HENNEPIN PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ASHBY", "ASHBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AUSTIN", "AUSTIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BADGER", "BADGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BAGLEY", "BAGLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BALATON", "BALATON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BARNESVILLE", "BARNESVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "BARNUM", "BARNUM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BATTLE LAKE", "BATTLE LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BECKER", "BECKER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLE PLAINE", "BELLE PLAINE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLINGHAM", "BELLINGHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BEMIDJI", "BEMIDJI PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BIG LAKE", "BIG LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BENTON-STEARNS ED. DIST.", "BENTON-STEARNS ED. DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLACKDUCK", "BLACKDUCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLOOMING PRAIRIE", "BLOOMING PRAIRIE PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BLOOMINGTON", "BLOOMINGTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAHAM", "BRAHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAINERD", "BRAINERD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRANDON", "BRANDON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRECKENRIDGE", "BRECKENRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BREWSTER", "BREWSTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROOKLYN CENTER", "BROOKLYN CENTER SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWERVILLE", "BROWERVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWNS VALLEY", "BROWNS VALLEY PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BURNSVILLE", "BURNSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BUTTERFIELD", "BUTTERFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BYRON", "BYRON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CALEDONIA", "CALEDONIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CAMBRIDGE-ISANTI", "CAMBRIDGE-ISANTI PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "CAMPBELL-TINTAH", "CAMPBELL-TINTAH PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CANBY", "CANBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CANNON FALLS", "CANNON FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CARLTON", "CARLTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CASS LAKE", "CASS LAKE-BENA PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CEDAR MOUNTAIN", "CEDAR MOUNTAIN PUBLIC SCHOOL DISTRICT"),
    #CENTENNIAL PUBLIC SCHOOL DISTRICT
    DistrictName = str_replace(DistrictName, "CHASKA", "CHASKA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHATFIELD", "CHATFIELD PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CHISHOLM", "CHISHOLM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHISAGO LAKES", "CHISAGO LAKES SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHOKIO-ALBERTA", "CHOKIO-ALBERTA PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CLEARBROOK-GONVICK", "CLEARBROOK-GONVICK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLEVELAND", "CLEVELAND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLIMAX", "CLIMAX PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLOQUET", "CLOQUET PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COLUMBIA HEIGHTS", "COLUMBIA HEIGHTS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "COMFREY", "COMFREY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COOK COUNTY", "COOK COUNTY PUBLIC SCHOOLS"),
    #CROMWELL
    DistrictName = str_replace(DistrictName, "CROOKSTON", "CROOKSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CROSBY-IRONTON", "CROSBY-IRONTON PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CYRUS", "CYRUS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CENTRAL MINNESOTA JOINT POWERS DIST", "CENTRAL MINNESOTA JT. POWERS DIST."),
    DistrictName = str_replace(DistrictName, "CROW RIVER SP ED COOP", "CROW RIVER SP. ED. COOP."),
    DistrictName = str_replace(DistrictName, "DASSEL-COKATO", "DASSEL-COKATO PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DAWSON-BOYD", "DAWSON-BOYD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DEER RIVER", "DEER RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DELANO", "DELANO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DETROIT LAKES", "DETROIT LAKES PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DOVER-EYOTA", "DOVER-EYOTA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DULUTH", "DULUTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAGLE VALLEY", "EAGLE VALLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAST GRAND FORKS", "EAST GRAND FORKS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "EDEN PRAIRIE", "EDEN PRAIRIE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDEN VALLEY-WATKINS", "EDEN VALLEY-WATKINS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDGERTON", "EDGERTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDINA", "EDINA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELGIN-MILLVILLE", "ELGIN-MILLVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ELK RIVER", "ELK RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELLSWORTH", "ELLSWORTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELY", "ELY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ESKO", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVANSVILLE", "EVANSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVELETH-GILBERT", "EVELETH-GILBERT PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FAIRMONT AREA SCHOOLS", "FAIRMONT AREA SCHOOL DISTRICT"),
    #FARIBAULT
    DistrictName = str_replace(DistrictName, "FARMINGTON", "FARMINGTON PUBLIC SCHOOL DISTRICT"),
    #FERGUS FALLS
    DistrictName = str_replace(DistrictName, "FERGUS FALLS AREA SP ED COOP", "FERGUS FALLS AREA SP. ED. COOP."),
    DistrictName = str_replace(DistrictName, "FERTILE-BELTRAMI", "FERTILE-BELTRAMI SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FISHER", "FISHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FLOODWOOD", "FLOODWOOD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOLEY", "FOLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOREST LAKE", "FOREST LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOSSTON", "FOSSTON PUBLIC SCHOOL DISTRICT"),
    #FRAZZEE
    DistrictName = str_replace(DistrictName, "FRIDLEY", "FRIDLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FULDA", "FULDA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GLENCOE-SILVER LAKE", "GLENCOE-SILVER LAKE SCHOOL DISTRICT"),
    #Goodhue
    DistrictName = str_replace(DistrictName, "GOODRIDGE", "GOODRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND MEADOW", "GRAND MEADOW PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND RAPIDS", "GRAND RAPIDS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GREENBUSH-MIDDLE RIVER", "GREENBUSH-MIDDLE RIVER SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "GREENWAY", "GREENWAY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRYGLA", "GRYGLA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HANCOCK", "HANCOCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HASTINGS", "HASTINGS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAWLEY", "HAWLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAYFIELD", "HAYFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENDRICKS", "HENDRICKS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENNING", "HENNING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMAN-NORCROSS", "HERMAN-NORCROSS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMANTOWN", "HERMANTOWN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERON LAKE-OKABENA", "HERON LAKE-OKABENA SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HIBBING", "HIBBING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILL CITY", "HILL CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILLS-BEAVER CREEK", "HILLS-BEAVER CREEK SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HINCKLEY-FINLAYSON", "HINCKLEY-FINLAYSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOLDINGFORD", "HOLDINGFORD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOPKINS", "HOPKINS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOUSTON", "HOUSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HUTCHINSON", "HUTCHINSON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "INTERNATIONAL FALLS", "INTERNATIONAL FALLS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ISLE", "ISLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "IVANHOE", "IVANHOE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HIAWATHA VALLEY ED. DIST.", "HIAWATHA VALLEY ED. DISTRICT"),
    DistrictName = str_replace(DistrictName, "INVER GROVE", "INVER GROVE HEIGHTS SCHOOLS"),
    DistrictName = str_replace(DistrictName, "JACKSON COUNTY CENTRAL", "JACKSON COUNTY CENTRAL SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "JORDAN", "JORDAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KELLIHER", "KELLIHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KENYON-WANAMINGO", "KENYON-WANAMINGO SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KIMBALL", "KIMBALL PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KINGSLAND", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KITTSON CENTRAL", "KITTSON CENTRAL SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAC QUI PARLE VALLEY", "LAC QUI PARLE VALLEY SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LAKE BENTON", "LAKE BENTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE CITY", "LAKE CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE OF THE WOODS", "LAKE OF THE WOODS SCHOOL DISTRICT"),
    #Lake Superior Public School Dist
    DistrictName = str_replace(DistrictName, "LAKEVIEW", "LAKEVIEW SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKEVILLE", "LAKEVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANCASTER", "LANCASTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANESBORO", "LANESBORO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAPORTE", "LAPORTE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LECENTER", "LECENTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEROY", "LEROY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LESTER PRAIRIE", "LESTER PRAIRIE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LESUEUR-HENDERSON", "LESUEUR-HENDERSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITCHFIELD", "LITCHFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLE FALLS", "LITTLE FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLEFORK-BIG FALLS", "LITTLEFORK-BIG FALLS SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LONG PRAIRIE-GREY EAGLE", "LONG PRAIRIE-GREY EAGLE SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "LUVERNE", "LUVERNE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYLE", "LYLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYND", "LYND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KASSON-MANTORVILLE", "KASSON-MANTORVILLE SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LA CRESCENT MONTESSORI ACADEMY", "LA CRESCENT MONTESSORI & STEM SCHOOL"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE AGASSIZ SP ED COOP", "LAKE AGASSIZ SPECIAL ED. COOP."),
    DistrictName = str_replace(DistrictName, "LAKE PARK AUDUBON DISTRICT", "LAKE PARK AUDUBON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEWISTON", "LEWISTON-ALTURA PUBLIC SCHOOL DIST.")
  )

#1999 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.1999 <- read_csv("1999_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_nam, ami_mal, ami_fem, api_mal, api_fem, his_mal, his_fem, blk_mal, blk_fem, wht_mal, wht_fem, tot_mal, tot_fem, tot_mny, tot_tot) %>%
  group_by(dst_nam) %>%
  summarize(
    ami_mal = sum(ami_mal),
    ami_fem = sum(ami_fem),
    api_mal = sum(api_mal),
    api_fem = sum(api_fem),
    his_mal = sum(his_mal),
    his_fem = sum(his_fem),
    blk_mal = sum(blk_mal),
    blk_fem = sum(blk_fem),
    wht_mal = sum(wht_mal),
    wht_fem = sum(wht_fem),
    tot_mal = sum(tot_mal),
    tot_fem = sum(tot_fem),
    tot_mny = sum(tot_mny),
    tot_tot = sum(tot_tot)
  ) %>%
  mutate(
    percentMinority = tot_mny/tot_tot
  ) %>%
  select(dst_nam,percentMinority) %>%
  
  rename(
    DistrictName = dst_nam,
    percentMinority1999 = percentMinority
  ) %>%
  mutate(
    DistrictName = str_replace(DistrictName, "A.C.G.C.", "A.C.G.C. PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADA-BORUP", "ADA-BORUP PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ADRIAN", "ADRIAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AITKIN", "AITKIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBANY", "ALBANY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALBERT LEA", "ALBERT LEA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALDEN", "ALDEN-CONGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ALEXANDRIA", "ALEXANDRIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANNANDALE", "ANNANDALE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ANOKA-HENNEPIN", "ANOKA-HENNEPIN PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ASHBY", "ASHBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "AUSTIN", "AUSTIN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BADGER", "BADGER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BALATON", "BALATON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BARNESVILLE", "BARNESVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "BARNUM", "BARNUM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BATTLE LAKE", "BATTLE LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BECKER", "BECKER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BAGLEY", "BAGLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLE PLAINE", "BELLE PLAINE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BELLINGHAM", "BELLINGHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BEMIDJI", "BEMIDJI PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BIG LAKE", "BIG LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BENTON-STEARNS ED. DIST.", "BENTON-STEARNS ED. DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLACKDUCK", "BLACKDUCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BLOOMING PRAIRIE", "BLOOMING PRAIRIE PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BLOOMINGTON", "BLOOMINGTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAHAM", "BRAHAM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRAINERD", "BRAINERD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRANDON", "BRANDON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BRECKENRIDGE", "BRECKENRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BREWSTER", "BREWSTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROOKLYN CENTER", "BROOKLYN CENTER SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWERVILLE", "BROWERVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BROWNS VALLEY", "BROWNS VALLEY PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "BURNSVILLE", "BURNSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BUTTERFIELD", "BUTTERFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "BYRON", "BYRON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CALEDONIA", "CALEDONIA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CAMBRIDGE-ISANTI", "CAMBRIDGE-ISANTI PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "CAMPBELL-TINTAH", "CAMPBELL-TINTAH PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CANBY", "CANBY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CANNON FALLS", "CANNON FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CARLTON", "CARLTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CASS LAKE", "CASS LAKE-BENA PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CEDAR MOUNTAIN", "CEDAR MOUNTAIN PUBLIC SCHOOL DISTRICT"),
    #CENTENNIAL PUBLIC SCHOOL DISTRICT
    DistrictName = str_replace(DistrictName, "CHASKA", "CHASKA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHATFIELD", "CHATFIELD PUBLIC SCHOOLS"),
    DistrictName = str_replace(DistrictName, "CHISHOLM", "CHISHOLM PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHISAGO LAKES", "CHISAGO LAKES SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CHOKIO-ALBERTA", "CHOKIO-ALBERTA PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CLEARBROOK-GONVICK", "CLEARBROOK-GONVICK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLEVELAND", "CLEVELAND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLIMAX", "CLIMAX PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CLOQUET", "CLOQUET PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COLUMBIA HEIGHTS", "COLUMBIA HEIGHTS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "COMFREY", "COMFREY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "COOK COUNTY", "COOK COUNTY PUBLIC SCHOOLS"),
    #CROMWELL
    DistrictName = str_replace(DistrictName, "CROOKSTON", "CROOKSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CROSBY-IRONTON", "CROSBY-IRONTON PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "CYRUS", "CYRUS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "CENTRAL MINNESOTA JOINT POWERS DIST", "CENTRAL MINNESOTA JT. POWERS DIST."),
    DistrictName = str_replace(DistrictName, "CROW RIVER SP ED COOP", "CROW RIVER SP. ED. COOP."),
    DistrictName = str_replace(DistrictName, "DASSEL-COKATO", "DASSEL-COKATO PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DAWSON-BOYD", "DAWSON-BOYD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DEER RIVER", "DEER RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DELANO", "DELANO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DETROIT LAKES", "DETROIT LAKES PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "DOVER-EYOTA", "DOVER-EYOTA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "DULUTH", "DULUTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAGLE VALLEY", "EAGLE VALLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EAST GRAND FORKS", "EAST GRAND FORKS PUBLIC SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "EDEN PRAIRIE", "EDEN PRAIRIE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDEN VALLEY-WATKINS", "EDEN VALLEY-WATKINS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDGERTON", "EDGERTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EDINA", "EDINA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELGIN-MILLVILLE", "ELGIN-MILLVILLE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "ELK RIVER", "ELK RIVER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELLSWORTH", "ELLSWORTH PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ELY", "ELY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ESKO", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVANSVILLE", "EVANSVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "EVELETH-GILBERT", "EVELETH-GILBERT PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FAIRMONT AREA SCHOOLS", "FAIRMONT AREA SCHOOL DISTRICT"),
    #FARIBAULT
    DistrictName = str_replace(DistrictName, "FARMINGTON", "FARMINGTON PUBLIC SCHOOL DISTRICT"),
    #FERGUS FALLS
    DistrictName = str_replace(DistrictName, "FERGUS FALLS AREA SP ED COOP", "FERGUS FALLS AREA SP. ED. COOP."),
    DistrictName = str_replace(DistrictName, "FERTILE-BELTRAMI", "FERTILE-BELTRAMI SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FISHER", "FISHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FLOODWOOD", "FLOODWOOD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOLEY", "FOLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOREST LAKE", "FOREST LAKE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FOSSTON", "FOSSTON PUBLIC SCHOOL DISTRICT"),
    #FRAZZEE
    DistrictName = str_replace(DistrictName, "FRIDLEY", "FRIDLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "FULDA", "FULDA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GLENCOE-SILVER LAKE", "GLENCOE-SILVER LAKE SCHOOL DISTRICT"),
    #Goodhue
    DistrictName = str_replace(DistrictName, "GOODRIDGE", "GOODRIDGE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND MEADOW", "GRAND MEADOW PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRAND RAPIDS", "GRAND RAPIDS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GREENBUSH-MIDDLE RIVER", "GREENBUSH-MIDDLE RIVER SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "GREENWAY", "GREENWAY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "GRYGLA", "GRYGLA PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HANCOCK", "HANCOCK PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HARVEST PREP SCHOOL/SEED ACADEMY", "HARVEST PREPARATORY SCHOOL"),
    DistrictName = str_replace(DistrictName, "HASTINGS", "HASTINGS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAWLEY", "HAWLEY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HAYFIELD", "HAYFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENDRICKS", "HENDRICKS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HENNING", "HENNING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMAN-NORCROSS", "HERMAN-NORCROSS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERMANTOWN", "HERMANTOWN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HERON LAKE-OKABENA", "HERON LAKE-OKABENA SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HIBBING", "HIBBING PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILL CITY", "HILL CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HILLS-BEAVER CREEK", "HILLS-BEAVER CREEK SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HINCKLEY-FINLAYSON", "HINCKLEY-FINLAYSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOLDINGFORD", "HOLDINGFORD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOPKINS", "HOPKINS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HOUSTON", "HOUSTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HUTCHINSON", "HUTCHINSON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "INTERNATIONAL FALLS", "INTERNATIONAL FALLS SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "ISLE", "ISLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "IVANHOE", "IVANHOE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "HIAWATHA VALLEY ED. DIST.", "HIAWATHA VALLEY ED. DISTRICT"),
    DistrictName = str_replace(DistrictName, "INVER GROVE", "INVER GROVE HEIGHTS SCHOOLS"),
    DistrictName = str_replace(DistrictName, "JACKSON COUNTY CENTRAL", "JACKSON COUNTY CENTRAL SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "JORDAN", "JORDAN PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KELLIHER", "KELLIHER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KENYON-WANAMINGO", "KENYON-WANAMINGO SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KIMBALL", "KIMBALL PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KINGSLAND", "ESKO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KITTSON CENTRAL", "KITTSON CENTRAL SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAC QUI PARLE VALLEY", "LAC QUI PARLE VALLEY SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LAKE BENTON", "LAKE BENTON PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE CITY", "LAKE CITY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE OF THE WOODS", "LAKE OF THE WOODS SCHOOL DISTRICT"),
    #Lake Superior Public School Dist
    DistrictName = str_replace(DistrictName, "LAKEVIEW", "LAKEVIEW SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKEVILLE", "LAKEVILLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANCASTER", "LANCASTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LANESBORO", "LANESBORO PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAPORTE", "LAPORTE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LECENTER", "LECENTER PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEROY", "LEROY PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LESTER PRAIRIE", "LESTER PRAIRIE PUBLIC SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LESUEUR-HENDERSON", "LESUEUR-HENDERSON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITCHFIELD", "LITCHFIELD PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLE FALLS", "LITTLE FALLS PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LITTLEFORK-BIG FALLS", "LITTLEFORK-BIG FALLS SCHOOL DIST."),
    DistrictName = str_replace(DistrictName, "LONG PRAIRIE-GREY EAGLE", "LONG PRAIRIE-GREY EAGLE SCHOOL DIST"),
    DistrictName = str_replace(DistrictName, "LUVERNE", "LUVERNE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYLE", "LYLE PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LYND", "LYND PUBLIC SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "KASSON-MANTORVILLE", "KASSON-MANTORVILLE SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LACRESCENT-HOKAH", "LA CRESCENT-HOKAH SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LAKE AGASSIZ SP ED COOP", "LAKE AGASSIZ SPECIAL ED. COOP."),
    DistrictName = str_replace(DistrictName, "LAKE PARK", "LAKE PARK AUDUBON SCHOOL DISTRICT"),
    DistrictName = str_replace(DistrictName, "LEWISTON", "LEWISTON-ALTURA PUBLIC SCHOOL DIST.")
  )

enrolled.ethnicity.1999_2018 <- full_join(enrolled.ethnicity.district.1999, enrolled.ethnicity.district.2000, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2001, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2002, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2003, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2004, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2005, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2006, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2007, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2008, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2009, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2010, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2011, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2012, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2013, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2014, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2015, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2016, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2017, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2018, enrolled.ethnicity.1999_2018, by = c("DistrictName"))
  


enrolled.ethnicity.1999_2018.tidy <- enrolled.ethnicity.1999_2018 %>%
gather(year,percentMinority,percentMinority1999:percentMinority2018) %>%
  mutate(
    year = replace(year, year =="percentMinority1999" ,"1999"),
    year = replace(year, year =="percentMinority2000" ,"2000"),
    year = replace(year, year =="percentMinority2001" ,"2001"),
    year = replace(year, year =="percentMinority2002" ,"2002"),
    year = replace(year, year =="percentMinority2003" ,"2003"),
    year = replace(year, year =="percentMinority2004" ,"2004"),
    year = replace(year, year =="percentMinority2005" ,"2005"),
    year = replace(year, year =="percentMinority2006" ,"2006"),
    year = replace(year, year =="percentMinority2007" ,"2007"),
    year = replace(year, year =="percentMinority2008" ,"2008"),
    year = replace(year, year =="percentMinority2009" ,"2009"),
    year = replace(year, year =="percentMinority2010" ,"2010"),
    year = replace(year, year =="percentMinority2011" ,"2011"),
    year = replace(year, year =="percentMinority2012" ,"2012"),
    year = replace(year, year =="percentMinority2013" ,"2013"),
    year = replace(year, year =="percentMinority2014" ,"2014"),
    year = replace(year, year =="percentMinority2015" ,"2015"),
    year = replace(year, year =="percentMinority2016" ,"2016"),
    year = replace(year, year =="percentMinority2017" ,"2017"),
    year = replace(year, year =="percentMinority2018" ,"2018"))



#----------------------------------------
ethnicity.test <- read_csv("2018_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
          AMI_Male = sum(AMI_Male),
            AMI_Female = sum(AMI_Female),
            BLK_Male = sum(BLK_Male), 
            BLK_Female = sum(BLK_Female), 
            HIS_Male = sum(HIS_Male), 
            HIS_Female = sum(HIS_Female), 
            HPI_Male = sum(HPI_Male), 
            HPI_Female = sum(HPI_Female),
            MLT_Male = sum(MLT_Male),
            MLT_Female = sum(MLT_Female), 
            WHT_Male = sum(WHT_Male), 
            WHT_Female = sum(WHT_Female), 
            TotalMale = sum(TotalMale), 
            TotalFemale = sum(TotalFemale), 
            TotalMinority = sum(TotalMinority), 
            TotalStudents = sum(TotalStudents)
          ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  )
#  rename(countyName = COUNTY,
#         value1990=EST001,
#         countyFIPS=COUNTYA)

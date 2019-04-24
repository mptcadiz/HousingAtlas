library(tidyverse)


# Ethnicity of students enrolled (by district) ---------------------------------

#2018 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2018 <- read_csv("Education/Enrollment/2018_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName,Grade, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, districtType, districtNumber) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2018 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType)


#2017 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2017 <- read_csv("Education/Enrollment/2017_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, districtType, districtNumber) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents,
    DistrictName=toupper(DistrictName)
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2017 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType)

#2016 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2016 <- read_csv("Education/Enrollment/2016_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, districtNumber, districtType) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents,
    DistrictName=toupper(DistrictName)
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2016 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType)


#2015 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2015 <- read_csv("Education/Enrollment/2015_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, districtNumber, districtType) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2015 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType)
 
#2014 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2014 <- read_csv("Education/Enrollment/2014_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, districtNumber, DistrictName,Grade, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, districtNumber, districtType) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2014 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType)


#2013 to 2003 data uses API (Asian/Pacific Islander) instead of ASI and HPI separately. It does not have MULTI

#2013 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2013 <- read_csv("Education/Enrollment/2013_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade,AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, districtNumber, districtType) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2013 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

#2012 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2012 <- read_csv("Education/Enrollment/2012_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName,Grade, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, districtNumber, districtType) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2012 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

#2011 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2011 <- read_csv("Education/Enrollment/2011_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade,AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, districtNumber, districtType) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2011 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

#2010 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2010 <- read_csv("Education/Enrollment/2010_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName,Grade, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, Total_Male, Total_Female, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, districtNumber, districtType) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2010 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )


#2009 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2009 <- read_csv("Education/Enrollment/2009_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, districtNumber, districtType) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2009 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

#2008 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2008 <- read_csv("Education/Enrollment/2008_enrolled_ethnicity_district.csv") %>%
  select (DistrictNumber, DistrictType, DistrictName, Grade, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, DistrictNumber, DistrictType) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictNumber, DistrictType, DistrictName,percentMinority) %>%
  rename (percentMinority2008 = percentMinority) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

#2007 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2007 <- read_csv("Education/Enrollment/2007_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, districtNumber, districtType) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2007 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

#2006 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2006 <- read_csv("Education/Enrollment/2006_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  filter(Grade != "All Grades") %>%
  group_by(DistrictName, districtType, districtNumber) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2006 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

#2005 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2005 <- read_csv("Education/Enrollment/2005_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName,Grade, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName, districtType, districtNumber) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2005 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

#2004 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2004 <- read_csv("Education/Enrollment/2004_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, Grade,AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName, districtType, districtNumber) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2004 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

#2003 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2003 <- read_csv("Education/Enrollment/2003_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, districtType, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName, districtType, districtNumber) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(districtNumber, districtType, DistrictName,percentMinority) %>%
  rename (percentMinority2003 = percentMinority,
          DistrictNumber = districtNumber,
          DistrictType = districtType) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

#2002 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2002 <- read_csv("Education/Enrollment/2002_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_tye, dst_nam, ami_mal, ami_fem, api_mal, api_fem, his_mal, his_fem, blk_mal, blk_fem, wht_mal, wht_fem, tot_mal, tot_fem, tot_mny, tot_tot) %>%
  group_by(dst_nam, dst_num, dst_tye) %>%
  summarise(
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
  ungroup() %>%
  mutate(
    percentMinority = tot_mny/tot_tot
  ) %>%
  select(dst_num, dst_tye, dst_nam,percentMinority) %>%
  rename(
    DistrictName = dst_nam,
    DistrictType = dst_tye,
    DistrictNumber = dst_num,
    percentMinority2002 = percentMinority
  ) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

#2001 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2001 <- read_csv("Education/Enrollment/2001_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_nam, dst_tye, ami_mal, ami_fem, api_mal, api_fem, his_mal, his_fem, blk_mal, blk_fem, wht_mal, wht_fem, tot_mal, tot_fem, tot_mny, tot_tot) %>%
  group_by(dst_nam, dst_num, dst_tye) %>%
  summarise(
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
  ungroup() %>%
  select(dst_num, dst_tye,dst_nam,percentMinority) %>%
  rename(
    DistrictName = dst_nam,
    DistrictType = dst_tye,
    DistrictNumber = dst_num,
    percentMinority2001 = percentMinority
  ) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

#2000 Ethnicity of students enrolled (by district) ---------------------------------
enrolled.ethnicity.district.2000 <- read_csv("Education/Enrollment/2000_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_tye, dst_nam, ami_mal, ami_fem, api_mal, api_fem, his_mal, his_fem, blk_mal, blk_fem, wht_mal, wht_fem, tot_mal, tot_fem, tot_mny, tot_tot) %>%
  group_by(dst_nam, dst_num, dst_tye) %>%
  summarise(
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
  ungroup() %>%
  select(dst_num, dst_tye, dst_nam,percentMinority) %>%
  rename(
    DistrictName = dst_nam,
    DistrictType = dst_tye,
    DistrictNumber = dst_num,
    percentMinority2000 = percentMinority
  ) %>%
  mutate(
    DistrictNumber = ifelse(DistrictNumber == "0769" & DistrictType == "01", "2769", DistrictNumber)
  )

enrolled.ethnicity.2000_2018 <- full_join(enrolled.ethnicity.district.2018, enrolled.ethnicity.district.2017, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2016,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2015,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2014,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2013,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2012,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2011,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2010,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2009,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2008,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2007,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2006,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2005,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2004,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2003,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2002,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2001,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  full_join(enrolled.ethnicity.district.2000,enrolled.ethnicity.2000_2018, by = c("DistrictType","DistrictNumber")) %>%
  rename(DistrictName=DistrictName.x) %>%
  select(-DistrictName.y) %>%
  drop_na(DistrictName) %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0323", "02", "Franconia", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  

enrolled.ethnicity.2000_2018.tidy <- enrolled.ethnicity.2000_2018 %>%
gather(year,percentMinority,percentMinority2000:percentMinority2018) %>%
  mutate(
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
    year = replace(year, year =="percentMinority2018" ,"2018")) %>%
  rename(districtName = DistrictName,
         districtType = DistrictType) %>%
  write_csv("Education/Enrollment/enrolled_ethnicity_2000_2018.csv",append=FALSE)



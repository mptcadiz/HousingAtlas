library(tidyverse)


# Ethnicity of students ---------------------------------

student.ethnicity.county.2018 <- read_csv("2018_student_ethnicity_county.csv") %>%
  mutate(DistrictCountyNumber = formatC(DistrictCountyNumber, width = 3, flag = "0")) %>%
  select (DistrictCountyNumber, DistrictCountyName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictCountyName) %>%
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

student.ethnicity.district.2018 <- read_csv("2018_student_ethnicity_district.csv") %>%
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

ethnicity.test <- read_csv("2018_student_ethnicity_district.csv") %>%
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

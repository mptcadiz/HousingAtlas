library(tidyverse)


# Ethnicity of students enrolled (by district) ---------------------------------

enrollment.change.2008_2018 <- read_csv("Education/Enrollment/enrolled_total_2000_2018.csv") %>%
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
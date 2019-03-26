library(tidyverse)


# Graduation rates by district ---------------------------------

grad.ethnicity.2017 <- read_csv("2017_grad.csv") %>%
  select ("District Number","District Type", "District Name",  "Ending Status", "Four Year Percent") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    gradRate = "Four Year Percent",
    endStatus = "Ending Status"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  #drop_na(gradRate) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(gradRate=mean(gradRate, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  rename(gradRate2017 = gradRate) %>%
  drop_na(districtName)
library(tidyverse)


# English proficiency by district ---------------------------------

english.2013 <- read_csv("2013_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12, LEPServedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPServedK12 = sum(LEPServedK12),
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  mutate(percentServed=LEPServedK12/LEPIdentifiedK12) %>%
  ungroup() %>%
  rename (percentServed2013 = percentServed) %>%
  drop_na(districtName)


english.2012 <- read_csv("2012_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12, LEPServedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    LEPIdentifiedK12 = na_if(LEPIdentifiedK12, "NULL"),
    LEPIdentifiedK12 = as.numeric(LEPIdentifiedK12),
    LEPServedK12 = na_if(LEPServedK12, "NULL"),
    LEPServedK12 = as.numeric(LEPServedK12),
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPServedK12 = sum(LEPServedK12),
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  mutate(percentServed=LEPServedK12/LEPIdentifiedK12) %>%
  ungroup() %>%
  rename (percentServed2012 = percentServed) %>%
  drop_na(districtName)


english.2011 <- read_csv("2011_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12, LEPServedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    LEPIdentifiedK12 = na_if(LEPIdentifiedK12, "NULL"),
    LEPIdentifiedK12 = as.numeric(LEPIdentifiedK12),
    LEPServedK12 = na_if(LEPServedK12, "NULL"),
    LEPServedK12 = as.numeric(LEPServedK12),
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPServedK12 = sum(LEPServedK12),
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  mutate(percentServed=LEPServedK12/LEPIdentifiedK12) %>%
  ungroup() %>%
  rename (percentServed2011 = percentServed) %>%
  drop_na(districtName)


english.2010 <- read_csv("2010_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12, LEPServedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPServedK12 = sum(LEPServedK12),
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  mutate(percentServed=LEPServedK12/LEPIdentifiedK12) %>%
  ungroup() %>%
  rename (percentServed2010 = percentServed) %>%
  drop_na(districtName)


english.2009 <- read_csv("2009_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12, LEPServedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPServedK12 = sum(LEPServedK12),
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  mutate(percentServed=LEPServedK12/LEPIdentifiedK12) %>%
  ungroup() %>%
  rename (percentServed2009 = percentServed) %>%
  drop_na(districtName)

english.2008 <- read_csv("2008_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12, LEPServedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPServedK12 = sum(LEPServedK12),
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  mutate(percentServed=LEPServedK12/LEPIdentifiedK12) %>%
  ungroup() %>%
  rename (percentServed2008 = percentServed) %>%
  drop_na(districtName)

english.2007 <- read_csv("2007_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12, LEPServedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPServedK12 = sum(LEPServedK12),
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  mutate(percentServed=LEPServedK12/LEPIdentifiedK12) %>%
  ungroup() %>%
  rename (percentServed2007 = percentServed) %>%
  drop_na(districtName)

english.2006 <- read_csv("2006_english.csv") %>%
  select (districtNumber, districtType, DistrictName, LEPIdentifiedK12, LEPServedK12) %>%
  rename(
    districtName=DistrictName
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarize(
    LEPServedK12 = sum(LEPServedK12),
    LEPIdentifiedK12 = sum(LEPIdentifiedK12)
  ) %>%
  mutate(percentServed=LEPServedK12/LEPIdentifiedK12) %>%
  ungroup() %>%
  rename (percentServed2006 = percentServed) %>%
  drop_na(districtName)
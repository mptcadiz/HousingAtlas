#03/04/19 - remove rows where all columns are NA https://stackoverflow.com/questions/41609912/remove-rows-where-all-variables-are-na-using-dplyr#
library(tidyverse)


# Languages spoken at home (by district) ---------------------------------

home.lang.2018 <- read_csv("2018_home_lang.csv") %>%
  select ("District Number","District Type", "District Name", "Language Name", "Home Primary Language", Enrollments) %>%
  rename (
    enrollment=Enrollments,
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    language="Language Name",
    languageCode = "Home Primary Language"
  ) %>%
  mutate(enrollmentTotal = enrollment,
         language = ifelse(languageCode != "011", "OTHER", language),
         language = ifelse(languageCode == "011", "ENGLISH", language)
  ) %>%
  select(-languageCode) %>%
  group_by(districtNumber, districtType, districtName, language) %>%
  summarize(enrollment=sum(enrollment)) %>%
  ungroup() %>%
  group_by(districtNumber, districtType, districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup() %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  select(districtNumber, districtType, districtName, language, percentLang) %>%
  rename(percentLang2018 = percentLang) %>%
  drop_na(districtName)

home.lang.2017 <- read_csv("2017_home_lang.csv") %>%
  select (districtNumber, districtType, districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(enrollmentTotal = enrollment,
         language = ifelse(languageCode != "011", "OTHER", language),
         language = ifelse(languageCode == "011", "ENGLISH", language)
  ) %>%
  select(-languageCode) %>%
  group_by(districtNumber, districtType, districtName, language) %>%
  summarize(enrollment=sum(enrollment)) %>%
  ungroup() %>%
  group_by(districtNumber, districtType, districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup() %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  select(districtNumber, districtType, districtName, language, percentLang) %>%
  rename(percentLang2017 = percentLang) %>%
  drop_na(districtName)


home.lang.2016 <- read_csv("2016_home_lang.csv") %>%
  select (districtNumber, districtType, districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(enrollmentTotal = enrollment,
         language = ifelse(languageCode != "011", "OTHER", language),
         language = ifelse(languageCode == "011", "ENGLISH", language)
  ) %>%
  select(-languageCode) %>%
  group_by(districtNumber, districtType, districtName, language) %>%
  summarize(enrollment=sum(enrollment)) %>%
  ungroup() %>%
  group_by(districtNumber, districtType, districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup() %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  select(districtNumber, districtType, districtName, language, percentLang) %>%
  rename(percentLang2016 = percentLang) %>%
  drop_na(districtName)

home.lang.2015 <- read_csv("2015_home_lang.csv") %>%
  select (districtNumber, districtType, districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(enrollmentTotal = enrollment,
         language = ifelse(languageCode != "011", "OTHER", language),
         language = ifelse(languageCode == "011", "ENGLISH", language)
  ) %>%
  select(-languageCode) %>%
  group_by(districtNumber, districtType, districtName, language) %>%
  summarize(enrollment=sum(enrollment)) %>%
  ungroup() %>%
  group_by(districtNumber, districtType, districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup() %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  select(districtNumber, districtType, districtName, language, percentLang) %>%
  rename(percentLang2015 = percentLang) %>%
  drop_na(districtName)

home.lang.2014 <- read_csv("2014_home_lang.csv") %>%
  select (districtNumber, districtType, districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(enrollmentTotal = enrollment,
         language = ifelse(languageCode != "011", "OTHER", language),
         language = ifelse(languageCode == "011", "ENGLISH", language)
  ) %>%
  select(-languageCode) %>%
  group_by(districtNumber, districtType, districtName, language) %>%
  summarize(enrollment=sum(enrollment)) %>%
  ungroup() %>%
  group_by(districtNumber, districtType, districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup() %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  select(districtNumber, districtType, districtName, language, percentLang) %>%
  rename(percentLang2014 = percentLang) %>%
  drop_na(districtName)

home.lang.2013 <- read_csv("2013_home_lang.csv") %>%
  select (districtNumber, districtType, districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(languageCode = formatC(languageCode, width = 3, flag = "0"),
         districtType = formatC(districtType, width = 2, flag = "0"),
         districtNumber = formatC(districtNumber, width = 4, flag = "0"),
         enrollmentTotal = enrollment,
         language = ifelse(languageCode != "011", "OTHER", language),
         language = ifelse(languageCode == "011", "ENGLISH", language)
  ) %>%
  select(-languageCode) %>%
  group_by(districtNumber, districtType, districtName, language) %>%
  summarize(enrollment=sum(enrollment)) %>%
  ungroup() %>%
  group_by(districtNumber, districtType, districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup() %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  select(districtNumber, districtType, districtName, language, percentLang) %>%
  rename(percentLang2013 = percentLang) %>%
  drop_na(districtName)

home.lang.2012 <- read_csv("2012_home_lang.csv") %>%
  select (districtNumber, districtType, districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(enrollmentTotal = enrollment,
         language = ifelse(languageCode != "011", "OTHER", language),
         language = ifelse(languageCode == "011", "ENGLISH", language)
  ) %>%
  select(-languageCode) %>%
  group_by(districtNumber, districtType, districtName, language) %>%
  summarize(enrollment=sum(enrollment)) %>%
  ungroup() %>%
  group_by(districtNumber, districtType, districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup() %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  select(districtNumber, districtType, districtName, language, percentLang) %>%
  rename(percentLang2012 = percentLang) %>%
  drop_na(districtName)

home.lang.2011 <- read_csv("2011_home_lang.csv") %>%
  select (districtType, districtNumber, districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(enrollmentTotal = enrollment,
         language = ifelse(languageCode != "011", "OTHER", language),
         language = ifelse(languageCode == "011", "ENGLISH", language)
  ) %>%
  select(-languageCode) %>%
  group_by(districtNumber, districtType, districtName, language) %>%
  summarize(enrollment=sum(enrollment)) %>%
  ungroup() %>%
  group_by(districtNumber, districtType, districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup() %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  select(districtNumber, districtType, districtName, language, percentLang) %>%
  rename(percentLang2011 = percentLang) %>%
  drop_na(districtName)

home.lang.2010 <- read_csv("2010_home_lang.csv") %>%
  select (districtNumber, districtType, districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(enrollmentTotal = enrollment,
         language = ifelse(languageCode != "011", "OTHER", language),
         language = ifelse(languageCode == "011", "ENGLISH", language)
  ) %>%
  select(-languageCode) %>%
  group_by(districtNumber, districtType, districtName, language) %>%
  summarize(enrollment=sum(enrollment)) %>%
  ungroup() %>%
  group_by(districtNumber, districtType, districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup() %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  select(districtNumber, districtType, districtName, language, percentLang) %>%
  rename(percentLang2010 = percentLang) %>%
  drop_na(districtName)

home.lang.2009 <- read_csv("2009_home_lang.csv") %>%
  select (districtNumber, districtType, districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(enrollmentTotal = enrollment,
         language = ifelse(languageCode != "011", "OTHER", language),
         language = ifelse(languageCode == "011", "ENGLISH", language)
  ) %>%
  select(-languageCode) %>%
  group_by(districtNumber, districtType, districtName, language) %>%
  summarize(enrollment=sum(enrollment)) %>%
  ungroup() %>%
  group_by(districtNumber, districtType, districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup() %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  select(districtNumber, districtType, districtName, language, percentLang) %>%
  rename(percentLang2009 = percentLang) %>%
  drop_na(districtName)

home.lang.2008 <- read_csv("2008_home_lang.csv") %>%
  select (districtNumber, districtType, districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(enrollmentTotal = enrollment,
         language = ifelse(languageCode != "011", "OTHER", language),
         language = ifelse(languageCode == "011", "ENGLISH", language)
  ) %>%
  select(-languageCode) %>%
  group_by(districtNumber, districtType, districtName, language) %>%
  summarize(enrollment=sum(enrollment)) %>%
  ungroup() %>%
  group_by(districtNumber, districtType, districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup() %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  select(districtNumber, districtType, districtName, language, percentLang) %>%
  rename(percentLang2008 = percentLang) %>%
  drop_na(districtName)

home.lang.2007 <- read_csv("2007_home_lang.csv") %>%
  select (dst_num, dst_tye, district, language, homprm_lng, k12count) %>%
  rename (
    enrollment=k12count,
    languageCode = homprm_lng,
    districtName = district,
    districtNumber = dst_num,
    districtType = dst_tye
  ) %>%
  mutate(enrollmentTotal = enrollment,
         language = ifelse(languageCode != "011", "OTHER", language),
         language = ifelse(languageCode == "011", "ENGLISH", language)
  ) %>%
  select(-languageCode) %>%
  group_by(districtNumber, districtType, districtName, language) %>%
  summarize(enrollment=sum(enrollment)) %>%
  ungroup() %>%
  group_by(districtNumber, districtType, districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup() %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  select(districtNumber, districtType, districtName, language, percentLang) %>%
  rename(percentLang2008 = percentLang) %>%
  drop_na(districtName)


home.lang.2008_2018 <- full_join(home.lang.2018, home.lang.2017, by = c("districtType","districtNumber","language")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtNumber) %>%
  full_join(home.lang.2018, home.lang.2016,home.lang.2008_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtNumber) 


home.lang.2007_2008 <- full_join(home.lang.2007, home.lang.2008, by=c("districtName","languageCode","language")) %>%
  full_join(home.lang.2009, home.lang.2007_2008, by=c("districtName","languageCode","language")) %>%
  full_join(home.lang.2010, home.lang.2007_2008, by=c("districtName","languageCode","language")) %>%
  full_join(home.lang.2011, home.lang.2007_2008, by=c("districtName","languageCode","language")) %>%
  full_join(home.lang.2012, home.lang.2007_2008, by=c("districtName","languageCode","language")) %>%
  full_join(home.lang.2013, home.lang.2007_2008, by=c("districtName","languageCode","language")) %>%
  full_join(home.lang.2014, home.lang.2007_2008, by=c("districtName","languageCode","language")) %>%
  full_join(home.lang.2015, home.lang.2007_2008, by=c("districtName","languageCode","language")) %>%
  full_join(home.lang.2016, home.lang.2007_2008, by=c("districtName","languageCode","language")) %>%
  full_join(home.lang.2017, home.lang.2007_2008, by=c("districtName","languageCode","language")) %>%
  full_join(home.lang.2018, home.lang.2007_2008, by=c("districtName","languageCode","language"))

home.lang.2008_2018 <- full_join(home.lang.2008, home.lang.2009, by=c("districtName","languageCode")) %>%
  select(-language.y) %>%
  rename(language=language.x) %>%
  full_join(home.lang.2010, home.lang.2008_2018, by=c("districtName", "languageCode")) %>%
  select(-language.y) %>%
  rename(language=language.x) %>%
  full_join(home.lang.2011, home.lang.2008_2018, by=c("districtName", "languageCode")) %>%
  select(-language.y) %>%
  rename(language=language.x) %>%
  full_join(home.lang.2012, home.lang.2008_2018, by=c("districtName", "languageCode")) %>%
  select(-language.y) %>%
  rename(language=language.x) %>%
  full_join(home.lang.2013, home.lang.2008_2018, by=c("districtName", "languageCode")) %>%
  select(-language.y) %>%
  rename(language=language.x) %>%
  full_join(home.lang.2014, home.lang.2008_2018, by=c("districtName", "languageCode")) %>%
  select(-language.y) %>%
  rename(language=language.x) %>%
  full_join(home.lang.2015, home.lang.2008_2018, by=c("districtName", "languageCode")) %>%
  select(-language.y) %>%
  rename(language=language.x) %>%
  full_join(home.lang.2016, home.lang.2008_2018, by=c("districtName", "languageCode")) %>%
  select(-language.y) %>%
  rename(language=language.x) %>%
  full_join(home.lang.2017, home.lang.2008_2018, by=c("districtName", "languageCode")) %>%
  select(-language.y) %>%
  rename(language=language.x) %>%
  full_join(home.lang.2018, home.lang.2008_2018, by=c("districtName", "languageCode")) %>%
  select(-language.y) %>%
  rename(language=language.x)


#free.red.lunch.2008_2018 <- full_join(free.red.lunch.2018, free.red.lunch.2017, by = c("DistrictName")) %>%
#  full_join(free.red.lunch.2016, free.red.lunch.2006_2018, by =c("DistrictName")) %>%
#  gather(year,percentLunch,percentLunch2006:percentLunch2018) %>%
#  mutate(
#    year = replace(year, year =="percentLunch2006" ,"2006"),
#    year = replace(year, year =="percentLunch2018" ,"2018"))
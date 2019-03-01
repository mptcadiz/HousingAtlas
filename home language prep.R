library(tidyverse)


# Languages spoken at home (by district) ---------------------------------

home.lang.2018 <- read_csv("2018_home_lang.csv") %>%
  select ("District Name", "Language Name", "Home Primary Language", Enrollments) %>%
  rename (
    enrollment=Enrollments,
    districtName="District Name",
    language="Language Name",
    languageCode = "Home Primary Language"
  ) %>%
  mutate(enrollmentTotal = enrollment) %>%
  group_by(districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup %>%
  mutate(
    percentLang = enrollment/enrollmentTotal
  ) %>%
  mutate(
    percentLang=as.factor(percentLang)
  ) %>%
  select(districtName, language, languageCode, percentLang)

home.lang.2017 <- read_csv("2017_home_lang.csv") %>%
  select (districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(enrollmentTotal = enrollment) %>%
  group_by(districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup %>%
  mutate(
    percentLang = enrollment/enrollmentTotal
  ) %>%
  mutate(
    percentLang=as.factor(percentLang)
  ) %>%
  select(districtName, language, languageCode, percentLang)

home.lang.2016 <- read_csv("2016_home_lang.csv") %>%
  select (districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(enrollmentTotal = enrollment) %>%
  group_by(districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup %>%
  mutate(
    percentLang = enrollment/enrollmentTotal
  ) %>%
  mutate(
    percentLang=as.factor(percentLang)
  ) %>%
  select(districtName, language, languageCode, percentLang)
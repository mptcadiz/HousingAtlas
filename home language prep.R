#03/04/19 - remove rows where all columns are NA https://stackoverflow.com/questions/41609912/remove-rows-where-all-variables-are-na-using-dplyr#
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
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang),
    districtName = str_replace(districtName, "DIST.", "DISTRICT")
  ) %>%
  select(districtName, language, languageCode, percentLang) %>%
  mutate(districtName=toupper(districtName)) %>%
  mutate(language=toupper(language)) %>%
  rename(percentLang2018 = percentLang)

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
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang),
    districtName = str_replace(districtName, "DIST.", "DISTRICT")
  ) %>%
  select(districtName, language, languageCode, percentLang) %>%
  mutate(districtName=toupper(districtName)) %>%
  mutate(language=toupper(language)) %>%
  rename(percentLang2017 = percentLang)


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
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang),
    districtName = str_replace(districtName, "DIST.", "DISTRICT")
  ) %>%
  select(districtName, language, languageCode, percentLang) %>%
  mutate(language=toupper(language)) %>%
  rename(percentLang2016 = percentLang)

home.lang.2015 <- read_csv("2015_home_lang.csv") %>%
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
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang),
    districtName = str_replace(districtName, "DIST.", "DISTRICT")
  ) %>%
  select(districtName, language, languageCode, percentLang) %>%
  mutate(language=toupper(language)) %>%
  rename(percentLang2015 = percentLang)

home.lang.2014 <- read_csv("2014_home_lang.csv") %>%
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
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang),
    districtName = str_replace(districtName, "DIST.", "DISTRICT")
  ) %>%
  select(districtName, language, languageCode, percentLang) %>%
  mutate(language=toupper(language)) %>%
  rename(percentLang2014 = percentLang)

home.lang.2013 <- read_csv("2013_home_lang.csv") %>%
  select (districtName, languageName, homePrimaryLanguage, enrollments) %>%
  rename (
    enrollment=enrollments,
    language=languageName,
    languageCode = homePrimaryLanguage,
    districtName = districtName
  ) %>%
  mutate(enrollmentTotal = enrollment,
         languageCode=as.integer(languageCode),
         languageCode = formatC(languageCode, width = 3, flag = "0")) %>%
  group_by(districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang),
    districtName = str_replace(districtName, "DIST.", "DISTRICT")
  ) %>%
  select(districtName, language, languageCode, percentLang) %>%
  mutate(language=toupper(language)) %>%
  rename(percentLang2013 = percentLang)

home.lang.2012 <- read_csv("2012_home_lang.csv") %>%
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
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang),
    districtName = str_replace(districtName, "DIST.", "DISTRICT")
  ) %>%
  select(districtName, language, languageCode, percentLang) %>%
  mutate(language=toupper(language)) %>%
  rename(percentLang2012 = percentLang)

home.lang.2011 <- read_csv("2011_home_lang.csv") %>%
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
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang),
    districtName = str_replace(districtName, "DIST.", "DISTRICT")
  ) %>%
  select(districtName, language, languageCode, percentLang) %>%
  mutate(language=toupper(language)) %>%
  rename(percentLang2011 = percentLang)

home.lang.2010 <- read_csv("2010_home_lang.csv") %>%
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
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang),
    districtName = str_replace(districtName, "DIST.", "DISTRICT")
  ) %>%
  select(districtName, language, languageCode, percentLang) %>%
  mutate(language=toupper(language)) %>%
  rename(percentLang2010 = percentLang)

home.lang.2009 <- read_csv("2009_home_lang.csv") %>%
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
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang),
    districtName = str_replace(districtName, "DIST.", "DISTRICT")
  ) %>%
  select(districtName, language, languageCode, percentLang) %>%
  mutate(language=toupper(language)) %>%
  rename(percentLang2009 = percentLang)

home.lang.2008 <- read_csv("2008_home_lang.csv") %>%
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
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang),
    districtName = str_replace(districtName, "DIST.", "DISTRICT")
  ) %>%
  select(districtName, language, languageCode, percentLang) %>%
  mutate(language=toupper(language)) %>%
  rename(percentLang2008 = percentLang)

home.lang.2007 <- read_csv("2007_home_lang.csv") %>%
  select (district, language, homprm_lng, k12count) %>%
  rename (
    enrollment=k12count,
    languageCode = homprm_lng,
    districtName = district
  ) %>%
  mutate(enrollmentTotal = enrollment) %>%
  group_by(districtName) %>%
  mutate(enrollmentTotal = sum(enrollment)) %>%
  ungroup %>%
  mutate(
    percentLang = enrollment/enrollmentTotal,
    percentLang=as.factor(percentLang),
    districtName = str_replace(districtName, "DIST.", "DISTRICT")
  ) %>%
  select(districtName, language, languageCode, percentLang) %>%
  mutate(language=toupper(language)) %>%
  rename(percentLang2007 = percentLang)


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
  
#need to figure out how to change language from NA to language name, since language code merges fine
anti.home.lang.2008_2018 <- home.lang.2008_2018 %>%
  filter(is.na(language)) %>%
  replace(., is.na(.), 0) %>%
  mutate (
    language = ifelse(language == 0 & languageCode == "055", "BURMESE", language),
    language = ifelse(language == 0 & languageCode == "429", "AMAZIGH", language),
    language = ifelse(language == 0 & languageCode == "425", "MAM", language),
    language = ifelse(language == 0 & languageCode == "423", "BARI", language),
    language = ifelse(language == 0 & languageCode == "421", "TEM, COCOTOLI (KOKOTOLI), TEMBA, TIM, TIMU", language),
    language = ifelse(language == 0 & languageCode == "420", "KANUMA", language),
    language = ifelse(language == 0 & languageCode == "419", "LOW GERMAN", language),
    language = ifelse(language == 0 & languageCode == "418", "IDOMA", language),
    language = ifelse(language == 0 & languageCode == "417", "BISAYA, BASAYA, DUSUN, TUTONG", language),
    language = ifelse(language == 0 & languageCode == "416", "CHIN", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    #find 413
    language = ifelse(language == 0 & languageCode == "413", "holder", language),
    language = ifelse(language == 0 & languageCode == "412", "CHALDEAN NEO-ARAMAIC", language),
    language = ifelse(language == 0 & languageCode == "411", "KAREN, S'GAW", language),
    language = ifelse(language == 0 & languageCode == "410", "KAREN, PWO", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    language = ifelse(language == 0 & languageCode == "414", "LISU", language),
    language = ifelse(language == 0 & languageCode == "055", "BURMESE", language)
          )

#free.red.lunch.2008_2018 <- full_join(free.red.lunch.2018, free.red.lunch.2017, by = c("DistrictName")) %>%
#  full_join(free.red.lunch.2016, free.red.lunch.2006_2018, by =c("DistrictName")) %>%
#  gather(year,percentLunch,percentLunch2006:percentLunch2018) %>%
#  mutate(
#    year = replace(year, year =="percentLunch2006" ,"2006"),
#    year = replace(year, year =="percentLunch2018" ,"2018"))
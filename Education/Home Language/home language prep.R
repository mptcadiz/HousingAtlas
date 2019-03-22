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
    percentEng = enrollment/enrollmentTotal,
    percentEng=as.factor(percentEng)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  filter(language=="ENGLISH") %>%
  select(districtNumber, districtType, districtName, percentEng) %>%
  rename(percentEng2018 = percentEng) %>%
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
    percentEng = enrollment/enrollmentTotal,
    percentEng=as.factor(percentEng)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  filter(language=="ENGLISH") %>%
  select(districtNumber, districtType, districtName, percentEng) %>%
  rename(percentEng2017 = percentEng) %>%
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
    percentEng = enrollment/enrollmentTotal,
    percentEng=as.factor(percentEng)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  filter(language=="ENGLISH") %>%
  select(districtNumber, districtType, districtName, percentEng) %>%
  rename(percentEng2016 = percentEng) %>%
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
    percentEng = enrollment/enrollmentTotal,
    percentEng=as.factor(percentEng)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  filter(language=="ENGLISH") %>%
  select(districtNumber, districtType, districtName, percentEng) %>%
  rename(percentEng2015 = percentEng) %>%
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
    percentEng = enrollment/enrollmentTotal,
    percentEng=as.factor(percentEng)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  filter(language=="ENGLISH") %>%
  select(districtNumber, districtType, districtName, percentEng) %>%
  rename(percentEng2014 = percentEng) %>%
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
    percentEng = enrollment/enrollmentTotal,
    percentEng=as.factor(percentEng)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  filter(language=="ENGLISH") %>%
  select(districtNumber, districtType, districtName, percentEng) %>%
  rename(percentEng2013 = percentEng) %>%
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
    percentEng = enrollment/enrollmentTotal,
    percentEng=as.factor(percentEng)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  filter(language=="ENGLISH") %>%
  select(districtNumber, districtType, districtName, percentEng) %>%
  rename(percentEng2012 = percentEng) %>%
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
    percentEng = enrollment/enrollmentTotal,
    percentEng=as.factor(percentEng)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  filter(language=="ENGLISH") %>%
  select(districtNumber, districtType, districtName, percentEng) %>%
  rename(percentEng2011 = percentEng) %>%
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
    percentEng = enrollment/enrollmentTotal,
    percentEng=as.factor(percentEng)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  filter(language=="ENGLISH") %>%
  select(districtNumber, districtType, districtName, percentEng) %>%
  rename(percentEng2010 = percentEng) %>%
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
    percentEng = enrollment/enrollmentTotal,
    percentEng=as.factor(percentEng)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  filter(language=="ENGLISH") %>%
  select(districtNumber, districtType, districtName, percentEng) %>%
  rename(percentEng2009 = percentEng) %>%
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
    percentEng = enrollment/enrollmentTotal,
    percentEng=as.factor(percentEng)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  filter(language=="ENGLISH") %>%
  select(districtNumber, districtType, districtName, percentEng) %>%
  rename(percentEng2008 = percentEng) %>%
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
    percentEng = enrollment/enrollmentTotal,
    percentEng=as.factor(percentEng)
  ) %>%
  mutate(
    districtName=toupper(districtName)
  ) %>%
  filter(language=="ENGLISH") %>%
  select(districtNumber, districtType, districtName, percentEng) %>%
  rename(percentEng2007 = percentEng) %>%
  drop_na(districtName)


home.lang.2008_2018 <- full_join(home.lang.2018, home.lang.2017, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(home.lang.2016,home.lang.2008_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(home.lang.2015,home.lang.2008_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(home.lang.2014,home.lang.2008_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(home.lang.2013,home.lang.2008_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(home.lang.2012,home.lang.2008_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(home.lang.2011,home.lang.2008_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(home.lang.2010,home.lang.2008_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(home.lang.2009,home.lang.2008_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(home.lang.2008,home.lang.2008_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(home.lang.2007,home.lang.2008_2018, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName)

home.lang.2008_2018.tidy <- home.lang.2008_2018 %>%
  gather(year,percentEng,percentEng2007:percentEng2018) %>%
  mutate(
    year = replace(year, year =="percentEng2007" ,"2007"),
    year = replace(year, year =="percentEng2008" ,"2008"),
    year = replace(year, year =="percentEng2009" ,"2009"),
    year = replace(year, year =="percentEng2010" ,"2010"),
    year = replace(year, year =="percentEng2011" ,"2011"),
    year = replace(year, year =="percentEng2012" ,"2012"),
    year = replace(year, year =="percentEng2013" ,"2013"),
    year = replace(year, year =="percentEng2014" ,"2014"),
    year = replace(year, year =="percentEng2015" ,"2015"),
    year = replace(year, year =="percentEng2016" ,"2016"),
    year = replace(year, year =="percentEng2017" ,"2017"),
    year = replace(year, year =="percentEng2018" ,"2018"))
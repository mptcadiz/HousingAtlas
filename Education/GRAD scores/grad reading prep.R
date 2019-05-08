library(tidyverse)


# GRAD: reading (public schools, by district) ---------------------------------

grad.reading.2012 <- read_csv("Education/GRAD scores/2012_grad_reading.csv") %>%
  select ("District Number", "District Type", "District Name", "Percent Passed", "Average Score") %>%
  rename(
    districtNumber= "District Number",
    districtType = "District Type",
    districtName="District Name",
    percentPassed = "Percent Passed",
    averageScore = "Average Score"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  rename (averageScore2012 = averageScore,
          percentPassed2012 = percentPassed) %>%
  drop_na(districtName)

grad.reading.2011 <- read_csv("Education/GRAD scores/2011_grad_reading.csv") %>%
  select ("District Number", "District Type", "District Name", "Percent Passed", "Average Score") %>%
  rename(
    districtNumber= "District Number",
    districtType = "District Type",
    districtName="District Name",
    percentPassed = "Percent Passed",
    averageScore = "Average Score"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  rename (averageScore2011 = averageScore,
          percentPassed2011 = percentPassed) %>%
  drop_na(districtName)

grad.reading.2010 <- read_csv("Education/GRAD scores/2010_grad_reading.csv") %>%
  select ("District Number", "District Type", "District Name", "Percent Passed", "Average Score") %>%
  rename(
    districtNumber= "District Number",
    districtType = "District Type",
    districtName="District Name",
    percentPassed = "Percent Passed",
    averageScore = "Average Score"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  rename (averageScore2010 = averageScore,
          percentPassed2010 = percentPassed) %>%
  drop_na(districtName)

grad.reading.2009 <- read_csv("Education/GRAD scores/2009_grad_reading.csv") %>%
  select ("District Number", "District Type", "District Name", "Percent Passed", "Average Score") %>%
  rename(
    districtNumber= "District Number",
    districtType = "District Type",
    districtName="District Name",
    percentPassed = "Percent Passed",
    averageScore = "Average Score"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  rename (averageScore2009 = averageScore,
          percentPassed2009 = percentPassed) %>%
  drop_na(districtName)

grad.reading.2008 <- read_csv("Education/GRAD scores/2008_grad_reading.csv") %>%
  select ("District Number", "District Type", "District Name", "Percent Passed", "Average Score") %>%
  rename(
    districtNumber= "District Number",
    districtType = "District Type",
    districtName="District Name",
    percentPassed = "Percent Passed",
    averageScore = "Average Score"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  rename (averageScore2008 = averageScore,
          percentPassed2008 = percentPassed) %>%
  drop_na(districtName)

grad.reading.2008_2012 <- full_join(grad.reading.2012, grad.reading.2011, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.reading.2010, grad.reading.2008_2012, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.reading.2009, grad.reading.2008_2012, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.reading.2008, grad.reading.2008_2012, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName)

grad.reading.2008_2012.pass <- grad.reading.2008_2012 %>%
  select(districtNumber, districtType, districtName, percentPassed2008, percentPassed2009, percentPassed2010, percentPassed2011, percentPassed2012) %>%
  gather(year,percentPassed,percentPassed2012:percentPassed2008) %>%
  mutate(
    year = replace(year, year =="percentPassed2008" ,"2008"),
    year = replace(year, year =="percentPassed2009" ,"2009"),
    year = replace(year, year =="percentPassed2010" ,"2010"),
    year = replace(year, year =="percentPassed2011" ,"2011"),
    year = replace(year, year =="percentPassed2012" ,"2012"))

grad.reading.2008_2012.score <- grad.reading.2008_2012 %>%
  select(districtNumber, districtType, districtName, averageScore2008, averageScore2009, averageScore2010, averageScore2011, averageScore2012) %>%
  gather(year,averageScore,averageScore2012:averageScore2008) %>%
  mutate(
    year = replace(year, year =="averageScore2008" ,"2008"),
    year = replace(year, year =="averageScore2009" ,"2009"),
    year = replace(year, year =="averageScore2010" ,"2010"),
    year = replace(year, year =="averageScore2011" ,"2011"),
    year = replace(year, year =="averageScore2012" ,"2012"))


grad.reading.2008_2012.tidy <- full_join(grad.reading.2008_2012.score, grad.reading.2008_2012.pass, by = c("districtType","districtNumber", "districtName","year"))

# GRAD: reading (public schools, by district) ---------------------------------

grad.reading.nonpub.2012 <- read_csv("Education/GRAD scores/2012_grad_reading_nonpub.csv") %>%
  select ("District Number", "District Type", "District Name", "Percent Passed", "Average Score") %>%
  rename(
    districtNumber= "District Number",
    districtType = "District Type",
    districtName="District Name",
    percentPassed = "Percent Passed",
    averageScore = "Average Score"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  rename (averageScore2012 = averageScore,
          percentPassed2012 = percentPassed) %>%
  drop_na(districtName)

grad.reading.nonpub.2011 <- read_csv("Education/GRAD scores/2011_grad_reading_nonpub.csv") %>%
  select ("District Number", "District Type", "District Name", "Percent Passed", "Average Score") %>%
  rename(
    districtNumber= "District Number",
    districtType = "District Type",
    districtName="District Name",
    percentPassed = "Percent Passed",
    averageScore = "Average Score"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  rename (averageScore2011 = averageScore,
          percentPassed2011 = percentPassed) %>%
  drop_na(districtName)

grad.reading.nonpub.2010 <- read_csv("Education/GRAD scores/2010_grad_reading_nonpub.csv") %>%
  select (districtNumber, districtType, districtName, percentPassed, averageScore) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtName, districtNumber, districtType) %>%
  summarise(
    percentPassed=mean(percentPassed, na.rm=TRUE),
    averageScore=mean(averageScore, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  rename (averageScore2010 = averageScore,
          percentPassed2010 = percentPassed) %>%
  drop_na(districtName)

grad.reading.nonpub.2009 <- read_csv("Education/GRAD scores/2009_grad_reading_nonpub.csv") %>%
  select ("District Number", "District Type", "District Name", "Percent Passed", "Average Score") %>%
  rename(
    districtNumber= "District Number",
    districtType = "District Type",
    districtName="District Name",
    percentPassed = "Percent Passed",
    averageScore = "Average Score"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtName, districtNumber, districtType) %>%
  summarise(
    percentPassed=mean(percentPassed, na.rm=TRUE),
    averageScore=mean(averageScore, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  rename (averageScore2009 = averageScore,
          percentPassed2009 = percentPassed) %>%
  drop_na(districtName)

grad.reading.nonpub.2008 <- read_csv("Education/GRAD scores/2008_grad_reading_nonpub.csv") %>%
  select ("District Number", "District Type", "District Name", "Percent Passed", "Average Score") %>%
  rename(
    districtNumber= "District Number",
    districtType = "District Type",
    districtName="District Name",
    percentPassed = "Percent Passed",
    averageScore = "Average Score"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0"),
    districtName=toupper(districtName)
  ) %>%
  group_by(districtName, districtNumber, districtType) %>%
  summarise(
    percentPassed=mean(percentPassed, na.rm=TRUE),
    averageScore=mean(averageScore, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  rename (averageScore2008 = averageScore,
          percentPassed2008 = percentPassed) %>%
  drop_na(districtName)

grad.reading.nonpub.2008_2012 <- full_join(grad.reading.nonpub.2012, grad.reading.nonpub.2011, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.reading.nonpub.2010, grad.reading.nonpub.2008_2012, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.reading.nonpub.2009, grad.reading.nonpub.2008_2012, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.reading.nonpub.2008, grad.reading.nonpub.2008_2012, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName)

grad.reading.nonpub.2008_2012.score <- grad.reading.nonpub.2008_2012 %>%
  select(districtNumber, districtType, districtName, averageScore2008, averageScore2009, averageScore2010, averageScore2011, averageScore2012) %>%
  gather(year,averageScore,averageScore2012:averageScore2008) %>%
  mutate(
    year = replace(year, year =="averageScore2008" ,"2008"),
    year = replace(year, year =="averageScore2009" ,"2009"),
    year = replace(year, year =="averageScore2010" ,"2010"),
    year = replace(year, year =="averageScore2011" ,"2011"),
    year = replace(year, year =="averageScore2012" ,"2012"))

grad.reading.nonpub.2008_2012.pass <- grad.reading.nonpub.2008_2012 %>%
  select(districtNumber, districtType, districtName, percentPassed2008, percentPassed2009, percentPassed2010, percentPassed2011, percentPassed2012) %>%
  gather(year,percentPassed,percentPassed2012:percentPassed2008) %>%
  mutate(
    year = replace(year, year =="percentPassed2008" ,"2008"),
    year = replace(year, year =="percentPassed2009" ,"2009"),
    year = replace(year, year =="percentPassed2010" ,"2010"),
    year = replace(year, year =="percentPassed2011" ,"2011"),
    year = replace(year, year =="percentPassed2012" ,"2012"))

grad.reading.nonpub.2008_2012.tidy <- full_join(grad.reading.nonpub.2008_2012.score, grad.reading.nonpub.2008_2012.pass, by = c("districtType","districtNumber", "districtName","year"))

# GRAD: math (public and nonpublic schools, by district) ---------------------------------

grad.reading.2008_2012_total <- bind_rows(grad.reading.2008_2012.tidy, grad.reading.nonpub.2008_2012.tidy) %>%
  mutate(districtName = str_to_title(districtName)) %>%
  write_csv("Education/GRAD scores/grad_reading_2008_2012.csv",append=FALSE)
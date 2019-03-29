library(tidyverse)


# GRAD: math (public schools, by district) ---------------------------------

grad.math.2013 <- read_csv("Education/GRAD scores/2013_grad_math.csv") %>%
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
  rename (averageScore2013 = averageScore,
          percentPassed2013 = percentPassed) %>%
  drop_na(districtName)

grad.math.2012 <- read_csv("Education/GRAD scores/2012_grad_math.csv") %>%
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

grad.math.2011 <- read_csv("Education/GRAD scores/2011_grad_math.csv") %>%
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

grad.math.2010 <- read_csv("Education/GRAD scores/2010_grad_math.csv") %>%
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

grad.math.2009 <- read_csv("Education/GRAD scores/2009_grad_math.csv") %>%
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


grad.math.2009_2013 <- full_join(grad.math.2013, grad.math.2012, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.math.2011, grad.math.2009_2013, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.math.2010, grad.math.2009_2013, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.math.2009, grad.math.2009_2013, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName)


grad.math.2009_2013.score <- grad.math.2009_2013 %>%
  select(districtNumber, districtType, districtName, averageScore2009, averageScore2010, averageScore2011, averageScore2012, averageScore2013) %>%
  gather(year,averageScore,averageScore2013:averageScore2009) %>%
  mutate(
    year = replace(year, year =="averageScore2009" ,"2009"),
    year = replace(year, year =="averageScore2010" ,"2010"),
    year = replace(year, year =="averageScore2011" ,"2011"),
    year = replace(year, year =="averageScore2012" ,"2012"),
    year = replace(year, year =="averageScore2013" ,"2013"))

grad.math.2009_2013.pass <- grad.math.2009_2013 %>%
  select(districtNumber, districtType, districtName, percentPassed2009, percentPassed2010, percentPassed2011, percentPassed2012, percentPassed2013) %>%
  gather(year,percentPassed,percentPassed2013:percentPassed2009) %>%
  mutate(
    year = replace(year, year =="percentPassed2009" ,"2009"),
    year = replace(year, year =="percentPassed2010" ,"2010"),
    year = replace(year, year =="percentPassed2011" ,"2011"),
    year = replace(year, year =="percentPassed2012" ,"2012"),
    year = replace(year, year =="percentPassed2013" ,"2013"))

grad.math.2009_2013.tidy <- full_join(grad.math.2009_2013.score, grad.math.2009_2013.pass, by = c("districtType","districtNumber", "districtName","year"))



# GRAD: math (nonpublic schools, by district) ---------------------------------

grad.math.nonpub.2013 <- read_csv("Education/GRAD scores/2013_grad_math_nonpub.csv") %>%
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
  rename (averageScore2013 = averageScore,
          percentPassed2013 = percentPassed) %>%
  drop_na(districtName)

grad.math.nonpub.2012 <- read_csv("Education/GRAD scores/2012_grad_math_nonpub.csv") %>%
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

grad.math.nonpub.2011 <- read_csv("Education/GRAD scores/2011_grad_math_nonpub.csv") %>%
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

grad.math.nonpub.2010 <- read_csv("Education/GRAD scores/2010_grad_math_nonpub.csv") %>%
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
  rename (averageScore2010 = averageScore,
          percentPassed2010 = percentPassed) %>%
  drop_na(districtName)

grad.math.nonpub.2009 <- read_csv("Education/GRAD scores/2009_grad_math_nonpub.csv") %>%
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

grad.math.nonpub.2009_2013 <- full_join(grad.math.nonpub.2013, grad.math.nonpub.2012, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.math.nonpub.2011, grad.math.nonpub.2009_2013, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.math.nonpub.2010, grad.math.nonpub.2009_2013, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.math.nonpub.2009, grad.math.nonpub.2009_2013, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName)


grad.math.nonpub.2009_2013.score <- grad.math.nonpub.2009_2013 %>%
  select(districtNumber, districtType, districtName, averageScore2009, averageScore2010, averageScore2011, averageScore2012, averageScore2013) %>%
  gather(year,averageScore,averageScore2013:averageScore2009) %>%
  mutate(
    year = replace(year, year =="averageScore2009" ,"2009"),
    year = replace(year, year =="averageScore2010" ,"2010"),
    year = replace(year, year =="averageScore2011" ,"2011"),
    year = replace(year, year =="averageScore2012" ,"2012"),
    year = replace(year, year =="averageScore2013" ,"2013"))

grad.math.nonpub.2009_2013.pass <- grad.math.nonpub.2009_2013 %>%
  select(districtNumber, districtType, districtName, percentPassed2009, percentPassed2010, percentPassed2011, percentPassed2012, percentPassed2013) %>%
  gather(year,percentPassed,percentPassed2013:percentPassed2009) %>%
  mutate(
    year = replace(year, year =="percentPassed2009" ,"2009"),
    year = replace(year, year =="percentPassed2010" ,"2010"),
    year = replace(year, year =="percentPassed2011" ,"2011"),
    year = replace(year, year =="percentPassed2012" ,"2012"),
    year = replace(year, year =="percentPassed2013" ,"2013"))

grad.math.nonpub.2009_2013.tidy <- full_join(grad.math.nonpub.2009_2013.score, grad.math.nonpub.2009_2013.pass, by = c("districtType","districtNumber", "districtName","year"))


# GRAD: math (public and nonpublic schools, by district) ---------------------------------

grad.math.2009_2013_total <- bind_rows(grad.math.2009_2013.tidy, grad.math.nonpub.2009_2013.tidy) %>%
  write_csv("Education/GRAD scores/grad_math_2009_2013.csv",append=FALSE)

library(tidyverse)


# ACT scores (5 year averages) ---------------------------------

act.score.2008_2012 <- read_csv("Education/ACT Scores/2012_act.csv") %>%
  select("Analysis Level", "District Name", "ACT Dist Code", "Grad Year", "Avg Comp") %>%
  rename (
    districtName = "District Name",
    actCode = "ACT Dist Code",
    gradYear = "Grad Year",
    score = "Avg Comp",
    analysisLevel= "Analysis Level"
  ) %>%
  mutate(actCode=as.numeric(as.character(actCode))) %>%
  filter(analysisLevel == "District") %>%
  filter(districtName != "No District Identified") %>%
  drop_na(districtName) %>%
  spread(gradYear, score, fill=NA,convert=TRUE) %>%
  select(-analysisLevel)

act.score.2013_2017 <- read_csv("Education/ACT Scores/2017_act.csv") %>%
  select("Analysis Level", "District Name", "ACT Dist Code", "Grad Year", "Avg Comp") %>%
  rename (
    districtName = "District Name",
    actCode = "ACT Dist Code",
    gradYear = "Grad Year",
    score = "Avg Comp",
    analysisLevel= "Analysis Level"
  ) %>%
  mutate(actCode=as.numeric(as.character(actCode))) %>%
  filter(analysisLevel == "District") %>%
  filter(districtName != "No District Identified") %>%
  drop_na(districtName) %>%
  spread(gradYear, score, fill=NA,convert=TRUE) %>%
  select(-analysisLevel)

act.score.2018 <- read_csv("Education/ACT Scores/2018_act.csv") %>%
  select("Analysis Level", "District Name", "ACT Dist Code", "Grad Year", "Avg Comp") %>%
  rename (
    districtName = "District Name",
    actCode = "ACT Dist Code",
    gradYear = "Grad Year",
    score = "Avg Comp",
    analysisLevel= "Analysis Level"
  ) %>%
  filter(analysisLevel == "District") %>%
  filter(districtName != "No District Identified") %>%
  drop_na(districtName) %>%
  spread(gradYear, score, fill=NA,convert=TRUE) %>%
  select(-analysisLevel) %>%
  select(districtName, actCode,"2018")

act.score.2008_2018 <- full_join(act.score.2018, act.score.2013_2017, by = c("actCode")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(act.score.2008_2012, act.score.2008_2018, by = c("actCode")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  select(districtName, actCode, "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008") %>%
  rename(
    score2018="2018",
    score2017="2017",
    score2016="2016",
    score2015="2015",
    score2014="2014",
    score2013="2013",
    score2012="2012",
    score2011="2011",
    score2010="2010",
    score2009="2009",
    score2008="2008"
  ) %>%
  mutate(
    score2017=as.numeric(as.character(score2017)),
    score2016=as.numeric(as.character(score2016)),
    score2015=as.numeric(as.character(score2015)),
    score2014=as.numeric(as.character(score2014)),
    score2013=as.numeric(as.character(score2013))
         ) %>%
  gather(year,score,score2018:score2008) %>%
  mutate (
    year = replace(year, year =="score2008" ,"2008"),
    year = replace(year, year =="score2009" ,"2009"),
    year = replace(year, year =="score2010" ,"2010"),
    year = replace(year, year =="score2011" ,"2011"),
    year = replace(year, year =="score2012" ,"2012"),
    year = replace(year, year =="score2013" ,"2013"),
    year = replace(year, year =="score2014" ,"2014"),
    year = replace(year, year =="score2015" ,"2015"),
    year = replace(year, year =="score2016" ,"2016"),
    year = replace(year, year =="score2017" ,"2017"),
    year = replace(year, year =="score2018" ,"2018")
  ) %>%
  mutate(year=as.numeric(as.character(year))) %>%
  filter(districtName != "District Not Identified") %>%
  write_csv("Education/ACT Scores/act_scores_2008_2018.csv",append=FALSE)

# Number of ACT takers (5 year averages) ---------------------------------

act.n.2008_2012 <- read_csv("Education/ACT Scores/2012_act.csv") %>%
  select("Analysis Level", "District Name", "ACT Dist Code", "Grad Year", N) %>%
  rename (
    districtName = "District Name",
    actCode = "ACT Dist Code",
    gradYear = "Grad Year",
    analysisLevel= "Analysis Level"
  ) %>%
  mutate(actCode=as.numeric(as.character(actCode))) %>%
  filter(analysisLevel == "District") %>%
  filter(districtName != "No District Identified") %>%
  drop_na(districtName) %>%
  spread(gradYear, N, fill=NA,convert=TRUE) %>%
  select(-analysisLevel)

act.n.2013_2017 <- read_csv("Education/ACT Scores/2017_act.csv") %>%
  select("Analysis Level", "District Name", "ACT Dist Code", "Grad Year", N) %>%
  rename (
    districtName = "District Name",
    actCode = "ACT Dist Code",
    gradYear = "Grad Year",
    analysisLevel= "Analysis Level"
  ) %>%
  mutate(actCode=as.numeric(as.character(actCode))) %>%
  filter(analysisLevel == "District") %>%
  filter(districtName != "No District Identified") %>%
  drop_na(districtName) %>%
  spread(gradYear, N, fill=NA,convert=TRUE) %>%
  select(-analysisLevel)

act.n.2018 <- read_csv("Education/ACT Scores/2018_act.csv") %>%
  select("Analysis Level", "District Name", "ACT Dist Code", "Grad Year", N) %>%
  rename (
    districtName = "District Name",
    actCode = "ACT Dist Code",
    gradYear = "Grad Year",
    analysisLevel= "Analysis Level"
  ) %>%
  filter(analysisLevel == "District") %>%
  filter(districtName != "No District Identified") %>%
  drop_na(districtName) %>%
  spread(gradYear, N, fill=NA,convert=TRUE) %>%
  select(-analysisLevel) %>%
  select(districtName, actCode,"2018")

act.n.2008_2018 <- full_join(act.n.2018, act.n.2013_2017, by = c("actCode")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(act.n.2008_2012, act.n.2008_2018, by = c("actCode")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  select(districtName, actCode, "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008") %>%
  rename(
    N2018="2018",
    N2017="2017",
    N2016="2016",
    N2015="2015",
    N2014="2014",
    N2013="2013",
    N2012="2012",
    N2011="2011",
    N2010="2010",
    N2009="2009",
    N2008="2008"
  ) %>%
  mutate(
    N2017=as.numeric(as.character(N2017)),
    N2016=as.numeric(as.character(N2016)),
    N2015=as.numeric(as.character(N2015)),
    N2014=as.numeric(as.character(N2014)),
    N2013=as.numeric(as.character(N2013))
  ) %>%
  gather(year,N,N2018:N2008) %>%
  mutate (
    year = replace(year, year =="N2008" ,"2008"),
    year = replace(year, year =="N2009" ,"2009"),
    year = replace(year, year =="N2010" ,"2010"),
    year = replace(year, year =="N2011" ,"2011"),
    year = replace(year, year =="N2012" ,"2012"),
    year = replace(year, year =="N2013" ,"2013"),
    year = replace(year, year =="N2014" ,"2014"),
    year = replace(year, year =="N2015" ,"2015"),
    year = replace(year, year =="N2016" ,"2016"),
    year = replace(year, year =="N2017" ,"2017"),
    year = replace(year, year =="N2018" ,"2018")
  ) %>%
  mutate(year=as.numeric(as.character(year))) %>%
  filter(districtName != "District Not Identified") %>%
  
  
  
  write_csv("Education/ACT Scores/act_n_2008_2018.csv",append=FALSE)

  enrolled.act.2018 <- read_csv("Education/Enrollment/2018_enrolled_ethnicity_district.csv")

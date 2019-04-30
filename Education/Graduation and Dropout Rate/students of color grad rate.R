library(tidyverse)


# Graduation rates by district ---------------------------------

grad.ethnicity.2017 <- read_csv("Education/Graduation and Dropout Rate/2017_grad.csv") %>%
  select ("District Number","District Type", "District Name","Demographic Category", "Demographic Description",  "Ending Status", "Four Year Count", "Four Year Total") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    endStatus = "Ending Status",
    category = "Demographic Category",
    description = "Demographic Description",
    gradCount = "Four Year Count",
    totalStudents = "Four Year Total"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  filter(category=="B" | category=="C" | category=="E" | category=="F" | category=="H") %>%
  select(districtNumber, districtType, districtName, gradCount, totalStudents) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    gradCount=sum(gradCount, na.rm=TRUE),
    totalStudents=sum(totalStudents, na.rm=TRUE)
  ) %>%
  mutate(gradRate=gradCount/totalStudents) %>%
  drop_na(gradRate) %>%
  mutate(districtName=toupper(districtName)) %>%
  select(districtNumber, districtType, districtName, gradRate) %>%
  rename(gradRate2017 = gradRate) %>%
  drop_na(districtName)

grad.ethnicity.2016 <- read_csv("Education/Graduation and Dropout Rate/2016_grad.csv") %>%
  select ("District Number","District Type", "District Name","Demographic Category", "Demographic Description",  "Ending Status", "Four Year Count", "Four Year Total") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    endStatus = "Ending Status",
    category = "Demographic Category",
    description = "Demographic Description",
    gradCount = "Four Year Count",
    totalStudents = "Four Year Total"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  filter(category=="B" | category=="C" | category=="E" | category=="F" | category=="H") %>%
  select(districtNumber, districtType, districtName, gradCount, totalStudents) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    gradCount=sum(gradCount, na.rm=TRUE),
    totalStudents=sum(totalStudents, na.rm=TRUE)
  ) %>%
  mutate(gradRate=gradCount/totalStudents) %>%
  drop_na(gradRate) %>%
  mutate(districtName=toupper(districtName)) %>%
  select(districtNumber, districtType, districtName, gradRate) %>%
  rename(gradRate2016 = gradRate) %>%
  drop_na(districtName)

grad.ethnicity.2015 <- read_csv("Education/Graduation and Dropout Rate/2015_grad.csv") %>%
  select ("District Number","District Type", "District Name","Demographic Category", "Demographic Description",  "Ending Status", "Four Year Count", "Four Year Total") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    endStatus = "Ending Status",
    category = "Demographic Category",
    description = "Demographic Description",
    gradCount = "Four Year Count",
    totalStudents = "Four Year Total"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  filter(category=="B" | category=="C" | category=="E" | category=="F" | category=="H") %>%
  select(districtNumber, districtType, districtName, gradCount, totalStudents) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    gradCount=sum(gradCount, na.rm=TRUE),
    totalStudents=sum(totalStudents, na.rm=TRUE)
  ) %>%
  mutate(gradRate=gradCount/totalStudents) %>%
  drop_na(gradRate) %>%
  mutate(districtName=toupper(districtName)) %>%
  select(districtNumber, districtType, districtName, gradRate) %>%
  rename(gradRate2015 = gradRate) %>%
  drop_na(districtName)

grad.ethnicity.2014 <- read_csv("Education/Graduation and Dropout Rate/2014_grad.csv") %>%
  select ("District Number","District Type", "District Name","Demographic Category", "Demographic Description",  "Ending Status", "Four Year Count", "Four Year Total") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    endStatus = "Ending Status",
    category = "Demographic Category",
    description = "Demographic Description",
    gradCount = "Four Year Count",
    totalStudents = "Four Year Total"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  filter(category=="B" | category=="C" | category=="E" | category=="F" | category=="H") %>%
  select(districtNumber, districtType, districtName, gradCount, totalStudents) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    gradCount=sum(gradCount, na.rm=TRUE),
    totalStudents=sum(totalStudents, na.rm=TRUE)
  ) %>%
  mutate(gradRate=gradCount/totalStudents) %>%
  drop_na(gradRate) %>%
  mutate(districtName=toupper(districtName)) %>%
  select(districtNumber, districtType, districtName, gradRate) %>%
  rename(gradRate2014 = gradRate) %>%
  drop_na(districtName)

grad.ethnicity.2013 <- read_csv("Education/Graduation and Dropout Rate/2013_grad.csv") %>%
  select ("District Number","District Type", "District Name","Demographic Category", "Demographic Description",  "Ending Status", "Four Year Count", "Four Year Total") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    endStatus = "Ending Status",
    category = "Demographic Category",
    description = "Demographic Description",
    gradCount = "Four Year Count",
    totalStudents = "Four Year Total"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  filter(category=="B" | category=="C" | category=="E" | category=="F" | category=="H") %>%
  select(districtNumber, districtType, districtName, gradCount, totalStudents) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    gradCount=sum(gradCount, na.rm=TRUE),
    totalStudents=sum(totalStudents, na.rm=TRUE)
  ) %>%
  mutate(gradRate=gradCount/totalStudents) %>%
  drop_na(gradRate) %>%
  mutate(districtName=toupper(districtName)) %>%
  select(districtNumber, districtType, districtName, gradRate) %>%
  rename(gradRate2013 = gradRate) %>%
  drop_na(districtName)

grad.ethnicity.2012 <- read_csv("Education/Graduation and Dropout Rate/2012_grad.csv") %>%
  select ("District Number","District Type", "District Name","Demographic Category", "Demographic Description",  "Ending Status", "Four Year Count", "Four Year Total") %>%
  rename (
    districtName="District Name",
    districtNumber="District Number",
    districtType="District Type",
    endStatus = "Ending Status",
    category = "Demographic Category",
    description = "Demographic Description",
    gradCount = "Four Year Count",
    totalStudents = "Four Year Total"
  ) %>%
  mutate(
    districtType = formatC(districtType, width = 2, flag = "0"),
    districtNumber = formatC(districtNumber, width = 4, flag = "0")
  ) %>%
  filter(endStatus=="Graduate") %>%
  filter(category=="B" | category=="C" | category=="E" | category=="F" | category=="H") %>%
  select(districtNumber, districtType, districtName, gradCount, totalStudents) %>%
  group_by(districtNumber, districtType, districtName) %>%
  summarise(
    gradCount=sum(gradCount, na.rm=TRUE),
    totalStudents=sum(totalStudents, na.rm=TRUE)
  ) %>%
  mutate(gradRate=gradCount/totalStudents) %>%
  drop_na(gradRate) %>%
  mutate(districtName=toupper(districtName)) %>%
  select(districtNumber, districtType, districtName, gradRate) %>%
  rename(gradRate2012 = gradRate) %>%
  drop_na(districtName)

grad.ethnicity.2012_2017 <- full_join(grad.ethnicity.2017, grad.ethnicity.2016, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.ethnicity.2015, grad.ethnicity.2012_2017, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.ethnicity.2014, grad.ethnicity.2012_2017, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.ethnicity.2013, grad.ethnicity.2012_2017, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  full_join(grad.ethnicity.2012, grad.ethnicity.2012_2017, by = c("districtType","districtNumber")) %>%
  rename(districtName=districtName.x) %>%
  select(-districtName.y) %>%
  drop_na(districtName) %>%
  write_csv("Education/Graduation and Dropout Rate/grad_rate_ethnicity_2012_2017.csv",append=FALSE)

gradtest <- read_csv("Education/Graduation and Dropout Rate/grad_rate_ethnicity_2012_2017.csv") %>%
  rbind(c("0000", "01", "Minneapolis-St. Paul International Airport", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0001", "01", "Aitkin", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0002", "01", "Hill City", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0004", "01", "McGregor", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0023", "01", "Frazee-Vergas", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0025", "01", "Pine Point", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0032", "01", "Blackduck", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0036", "01", "Kelliher", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0051", "01", "Foley", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0075", "01", "St. Clair", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0081", "01", "Comfrey", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0084", "01", "Sleepy Eye", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0085", "01", "Springfield", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0088", "01", "New Ulm", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0091", "01", "Barnum", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0093", "01", "Carlton", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0095", "01", "Cromwell-Wright", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0097", "01", "Moose Lake", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0099", "01", "Esko", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0100", "01", "Wrenshall", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0108", "01", "Central", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0111", "01", "Watertown-Mayer", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0113", "01", "Walker-Hackensack-Akeley", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0116", "01", "Pillager", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0118", "01", "Northland Community", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0129", "01", "Montevideo", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0138", "01", "North Branch", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0139", "01", "Rush City", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0146", "01", "Barnesville", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0150", "01", "Hawley", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0173", "01", "Mountain Lake", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0177", "01", "Windom", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0182", "01", "Crosby-Ironton", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0186", "01", "Pequot Lakes", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0195", "01", "Randolph", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0203", "01", "Hayfield", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0204", "01", "Kasson-Mantorville", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0206", "01", "Alexandria", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0213", "01", "Osakis", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0227", "01", "Chatfield", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0229", "01", "Lanesboro", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0238", "01", "Mabel-Canton", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0239", "01", "Rushford-Peterson", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0242", "01", "Alden-Conger", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0252", "01", "Cannon Falls", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0253", "01", "Goodhue", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0255", "01", "Pine Island", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0256", "01", "Red Wing", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0261", "01", "Ashby", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0264", "01", "Herman-Norcross", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0323", "02", "Franconia", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0277", "01", "Westonka", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0278", "01", "Orono", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0297", "01", "Spring Grove", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0299", "01", "Caledonia", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0300", "01", "La Crescent-Hokah", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0306", "01", "Laporte", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0308", "01", "Nevis", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0309", "01", "Park Rapids", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0314", "01", "Braham", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0316", "01", "Greenway", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0319", "01", "Nashwauk-Keewatin", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0330", "01", "Heron Lake-Okabena", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0332", "01", "Mora", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0333", "01", "Ogilvie", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0345", "01", "New London-Spicer", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0356", "01", "Lancaster", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0361", "01", "International Falls", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0362", "01", "Littlefork-Big Falls", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0363", "01", "South Koochingching", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0378", "01", "Dawson-Boyd", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0381", "01", "Lake Superior", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0390", "01", "Lake of the Woods", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0391", "01", "Cleveland", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0402", "01", "Hendricks", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0403", "01", "Ivanhoe", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0404", "01", "Lake Benton", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0414", "01", "Minneota", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0415", "01", "Lynd", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0423", "01", "Hutchinson", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0441", "01", "Marshall County Central", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0447", "01", "Grygla", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0458", "01", "Truman", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0463", "01", "Eden Valley-Watkins", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0466", "01", "Dassel-Cokato", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0473", "01", "Isle", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0477", "01", "Princeton", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0482", "01", "Little Falls", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0484", "01", "Pierz", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0485", "01", "Royalton", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0486", "01", "Swanville", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0487", "01", "Upsala", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0495", "01", "Grand Meadow", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0497", "01", "Lyle", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0499", "01", "Leroy-Ostrander", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0500", "01", "Southland", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0505", "01", "Fulda", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0507", "01", "Nicollet", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0511", "01", "Adrian", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0514", "01", "Ellsworth", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0531", "01", "Byron", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0533", "01", "Dover-Eyota", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0534", "01", "Stewartville", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0542", "01", "Battle Lake", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0544", "01", "Fergus Falls", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0545", "01", "Henning", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0547", "01", "Parkers Prairie", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0550", "01", "Underwood", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0553", "01", "New York Mills", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0561", "01", "Goodridge", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0577", "01", "Willow River", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0578", "01", "Pine City", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0581", "01", "Edgerton", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0592", "01", "Climax-Shelly", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0595", "01", "East Grand Forks", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0599", "01", "Fertile-Beltrami", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0600", "01", "Fisher", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0601", "01", "Fosston", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0630", "01", "Red Lake Falls", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0600", "01", "Fisher", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0635", "01", "Milroy", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0640", "01", "Wabasso", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0671", "01", "Hills-Beaver Creek", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0676", "01", "Badger", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0682", "01", "Roseau", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0695", "01", "Chisholm", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0696", "01", "Ely", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0698", "01", "Floodwood", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0700", "01", "Hermantown", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0701", "01", "Hibbing", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0704", "01", "Proctor", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0706", "01", "Virginia", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0707", "01", "Nett Lake", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0712", "01", "Mountain Iron-Buhl", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0716", "01", "Belle Plaine", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0726", "01", "Becker", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0738", "01", "Holdingford", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0739", "01", "Kimball", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0741", "01", "Paynesville", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0743", "01", "Sauk Centre", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0745", "01", "Albany", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0748", "01", "Sartell-St. Stephen", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0756", "01", "Blooming Prairie", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0763", "01", "Medford", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0768", "01", "Hancock", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0771", "01", "Chokio-Alberta", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0775", "01", "Kerkhoven-Murdock-Sunburg", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0777", "01", "Benson", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0786", "01", "Bertha-Hewitt", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0787", "01", "Browerville", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0801", "01", "Browns Valley", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0803", "01", "Wheaton Area", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0811", "01", "Wabasha-Kellogg", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0813", "01", "Lake City", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0815", "02", "Prinsburg", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0818", "01", "Verndale", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0820", "01", "Sebeka", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0821", "01", "Menahga", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0836", "01", "Butterfiled-Odin", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0846", "01", "Breckenridge", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0850", "01", "Rothsay", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0852", "01", "Campbell-Tintah", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0857", "01", "Lewiston-Altura", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0876", "01", "Annandale", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0879", "01", "Delano", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0881", "01", "Maple Lake", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0883", "01", "Rockford", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0891", "01", "Canby", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0912", "01", "Milaca", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("0914", "01", "Ulen-Hitterdal", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2071", "01", "Lake Crystal-Wellcome Memorial", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2125", "01", "Triton", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2134", "01", "United South Central", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2135", "01", "Maple River", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2137", "01", "Kingsland", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2143", "01", "Waterville-Elysian-Morristown", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2144", "01", "Chisago Lakes", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2149", "01", "Minnewaska", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2154", "01", "Eveleth-Gilbert", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2155", "01", "Wadena-Deer Creek", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2159", "01", "Buffalo Lake-Hector-Stewart", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2164", "01", "Dilworth-Glyndon-Felton", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2165", "01", "Hinkcley-Finlayson", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2167", "01", "Lakeview", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2168", "01", "N.R.H.E.G.", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2169", "01", "Murray County Central", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2170", "01", "Staples-Motley", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2171", "01", "Kittson Central", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2172", "01", "Kenyon-Wanamingo", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2174", "01", "Pine River-Backus", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2176", "01", "Warren-Alvarado-Oslo", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2180", "01", "M.A.C.C.R.A.Y", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2184", "01", "Luverne", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2190", "01", "Yellow Medicine East", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2198", "01", "Fillmore Central", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2215", "01", "Norman County East", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2311", "01", "Clearbrook-Gonvick", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2342", "01", "West Central Area", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2358", "01", "Tri-County", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2364", "01", "Belgrade-Brooten-Elrosa", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2396", "01", "A.C.G.C.", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2448", "01", "Martin County West", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2527", "01", "Norman County West", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2534", "01", "Bird Island-Olivia-Lake Lillian", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2536", "01", "Granada-Huntley-East Chain", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2580", "01", "East Central", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2609", "01", "Win-E-Mac", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2683", "01", "Greenbush-Middle River", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2687", "01", "Howard Lake-Waverly-Winsted", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2689", "01", "Pipestone Area", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2711", "01", "Mesabi East", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2754", "01", "Cedar Mountain", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2759", "01", "Eagle Valley", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2769", "01", "Morris Area", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2805", "01", "Zumbrota-Mazeppa", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2835", "01", "Janesville-Waldorf-Pemberton", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2853", "01", "Lac qui Parle Valley", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2854", "01", "Ada-Borup", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2856", "01", "Stephen-Argyle Central", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2860", "01", "Blue Earth Area", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2884", "01", "Red Rock Central", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2886", "01", "Glenville-Emmons", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2888", "01", "Clinton-Graceville-Beardsley", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2889", "01", "Lake Park-Audubon", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2895", "01", "Jackson Coutny Central", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2899", "01", "Plainview-Elgin-Millville", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2902", "01", "Russell-Tyler-Ruthton", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2903", "01", "Ortonville", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2904", "01", "Tracy Area", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2906", "01", "Red Lake County Central", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2908", "01", "Brandon-Evansville", NA, NA, NA, NA, NA, NA)) %>%
  rbind(c("2907", "01", "Round Lake-Brewster", NA, NA, NA, NA, NA, NA))
  
grad.ethnicity.2012_2017.tidy <- gradtest %>%
  gather(year,gradRate,gradRate2012:gradRate2017) %>%
  mutate(
    year = replace(year, year =="gradRate2012" ,"2012"),
    year = replace(year, year =="gradRate2013" ,"2013"),
    year = replace(year, year =="gradRate2014" ,"2014"),
    year = replace(year, year =="gradRate2015" ,"2015"),
    year = replace(year, year =="gradRate2016" ,"2016"),
    year = replace(year, year =="gradRate2017" ,"2017")) %>%
write_csv("Education/Graduation and Dropout Rate/grad_rate_ethnicity_2012_2017.csv",append=FALSE)
  
 
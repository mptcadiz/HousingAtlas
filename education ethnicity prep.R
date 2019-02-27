library(tidyverse)


# Ethnicity of students enrolled (by district) ---------------------------------

#enrolled.ethnicity.county.2018 <- read_csv("2018_enrolled_ethnicity_county.csv") %>%
#  mutate(DistrictCountyNumber = formatC(DistrictCountyNumber, width = 3, flag = "0")) %>%
#  select (DistrictCountyNumber, DistrictCountyName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
#  group_by(DistrictCountyName) %>%
#  summarize(
#    AMI_Male = sum(AMI_Male),
#    AMI_Female = sum(AMI_Female),
#    ASI_Male = sum(ASI_Male),
#    ASI_Female = sum(ASI_Female),
#    BLK_Male = sum(BLK_Male), 
#    BLK_Female = sum(BLK_Female), 
#    HIS_Male = sum(HIS_Male), 
#    HIS_Female = sum(HIS_Female), 
#    HPI_Male = sum(HPI_Male), 
#    HPI_Female = sum(HPI_Female),
#    MLT_Male = sum(MLT_Male),
#    MLT_Female = sum(MLT_Female), 
#    WHT_Male = sum(WHT_Male), 
#    WHT_Female = sum(WHT_Female), 
#    TotalMale = sum(TotalMale), 
#    TotalFemale = sum(TotalFemale), 
#    TotalMinority = sum(TotalMinority), 
#    TotalStudents = sum(TotalStudents)
#  ) %>%
#  mutate(
#    percentMinority = TotalMinority/TotalStudents
#  )


enrolled.ethnicity.district.2018 <- read_csv("2018_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    ASI_Male = sum(ASI_Male),
    ASI_Female = sum(ASI_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    HPI_Male = sum(HPI_Male), 
    HPI_Female = sum(HPI_Female),
    MLT_Male = sum(MLT_Male),
    MLT_Female = sum(MLT_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2018 = percentMinority)

enrolled.ethnicity.district.2017 <- read_csv("2017_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    ASI_Male = sum(ASI_Male),
    ASI_Female = sum(ASI_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    HPI_Male = sum(HPI_Male), 
    HPI_Female = sum(HPI_Female),
    MLT_Male = sum(MLT_Male),
    MLT_Female = sum(MLT_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2017 = percentMinority)

enrolled.ethnicity.district.2016 <- read_csv("2016_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    ASI_Male = sum(ASI_Male),
    ASI_Female = sum(ASI_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    HPI_Male = sum(HPI_Male), 
    HPI_Female = sum(HPI_Female),
    MLT_Male = sum(MLT_Male),
    MLT_Female = sum(MLT_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2016 = percentMinority)


enrolled.ethnicity.district.2015 <- read_csv("2015_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    ASI_Male = sum(ASI_Male),
    ASI_Female = sum(ASI_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    HPI_Male = sum(HPI_Male), 
    HPI_Female = sum(HPI_Female),
    MLT_Male = sum(MLT_Male),
    MLT_Female = sum(MLT_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2015 = percentMinority)

enrolled.ethnicity.district.2014 <- read_csv("2014_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    ASI_Male = sum(ASI_Male),
    ASI_Female = sum(ASI_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    HPI_Male = sum(HPI_Male), 
    HPI_Female = sum(HPI_Female),
    MLT_Male = sum(MLT_Male),
    MLT_Female = sum(MLT_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2014 = percentMinority)


#2013 to 2003 data uses API (Asian/Pacific Islander) instead of ASI and HPI separately. It does not have MULTI

enrolled.ethnicity.district.2013 <- read_csv("2013_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2013 = percentMinority)

enrolled.ethnicity.district.2012 <- read_csv("2012_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2012 = percentMinority)

enrolled.ethnicity.district.2011 <- read_csv("2011_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2011 = percentMinority)

enrolled.ethnicity.district.2010 <- read_csv("2010_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, Total_Male, Total_Female, TotalMinority, TotalStudents) %>%
group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    Total_Male = sum(Total_Male), 
    Total_Female = sum(Total_Female), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2010 = percentMinority)
  


enrolled.ethnicity.district.2009 <- read_csv("2009_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2009 = percentMinority)

enrolled.ethnicity.district.2008 <- read_csv("2008_enrolled_ethnicity_district.csv") %>%
  select (DistrictNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2008 = percentMinority)

enrolled.ethnicity.district.2007 <- read_csv("2007_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2007 = percentMinority)

enrolled.ethnicity.district.2006 <- read_csv("2006_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2006 = percentMinority)

enrolled.ethnicity.district.2005 <- read_csv("2005_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2005 = percentMinority)

enrolled.ethnicity.district.2004 <- read_csv("2004_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2004 = percentMinority)

enrolled.ethnicity.district.2003 <- read_csv("2003_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, API_Male, API_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
    AMI_Male = sum(AMI_Male),
    AMI_Female = sum(AMI_Female),
    API_Male = sum(API_Male),
    API_Female = sum(API_Female),
    BLK_Male = sum(BLK_Male), 
    BLK_Female = sum(BLK_Female), 
    HIS_Male = sum(HIS_Male), 
    HIS_Female = sum(HIS_Female), 
    WHT_Male = sum(WHT_Male), 
    WHT_Female = sum(WHT_Female), 
    TotalMale = sum(TotalMale), 
    TotalFemale = sum(TotalFemale), 
    TotalMinority = sum(TotalMinority), 
    TotalStudents = sum(TotalStudents)
  ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  ) %>%
  select(DistrictName,percentMinority) %>%
  rename (percentMinority2003 = percentMinority)

enrolled.ethnicity.district.2002 <- read_csv("2002_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_nam, ami_mal, ami_fem, api_mal, api_fem, his_mal, his_fem, blk_mal, blk_fem, wht_mal, wht_fem, tot_mal, tot_fem, tot_mny, tot_tot) %>%
  group_by(dst_nam) %>%
  summarize(
    ami_mal = sum(ami_mal),
    ami_fem = sum(ami_fem),
    api_mal = sum(api_mal),
    api_fem = sum(api_fem),
    his_mal = sum(his_mal),
    his_fem = sum(his_fem),
    blk_mal = sum(blk_mal),
    blk_fem = sum(blk_fem),
    wht_mal = sum(wht_mal),
    wht_fem = sum(wht_fem),
    tot_mal = sum(tot_mal),
    tot_fem = sum(tot_fem),
    tot_mny = sum(tot_mny),
    tot_tot = sum(tot_tot)
  ) %>%
  mutate(
    percentMinority = tot_mny/tot_tot
  ) %>%
  select(dst_nam,percentMinority) %>%
  
  rename(
    DistrictName = dst_nam,
    percentMinority2002 = percentMinority
  )

enrolled.ethnicity.district.2001 <- read_csv("2001_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_nam, ami_mal, ami_fem, api_mal, api_fem, his_mal, his_fem, blk_mal, blk_fem, wht_mal, wht_fem, tot_mal, tot_fem, tot_mny, tot_tot) %>%
  group_by(dst_nam) %>%
  summarize(
    ami_mal = sum(ami_mal),
    ami_fem = sum(ami_fem),
    api_mal = sum(api_mal),
    api_fem = sum(api_fem),
    his_mal = sum(his_mal),
    his_fem = sum(his_fem),
    blk_mal = sum(blk_mal),
    blk_fem = sum(blk_fem),
    wht_mal = sum(wht_mal),
    wht_fem = sum(wht_fem),
    tot_mal = sum(tot_mal),
    tot_fem = sum(tot_fem),
    tot_mny = sum(tot_mny),
    tot_tot = sum(tot_tot)
  ) %>%
  mutate(
    percentMinority = tot_mny/tot_tot
  ) %>%
  select(dst_nam,percentMinority) %>%
  
  rename(
    DistrictName = dst_nam,
    percentMinority2001 = percentMinority
  )

enrolled.ethnicity.district.2000 <- read_csv("2000_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_nam, ami_mal, ami_fem, api_mal, api_fem, his_mal, his_fem, blk_mal, blk_fem, wht_mal, wht_fem, tot_mal, tot_fem, tot_mny, tot_tot) %>%
  group_by(dst_nam) %>%
  summarize(
    ami_mal = sum(ami_mal),
    ami_fem = sum(ami_fem),
    api_mal = sum(api_mal),
    api_fem = sum(api_fem),
    his_mal = sum(his_mal),
    his_fem = sum(his_fem),
    blk_mal = sum(blk_mal),
    blk_fem = sum(blk_fem),
    wht_mal = sum(wht_mal),
    wht_fem = sum(wht_fem),
    tot_mal = sum(tot_mal),
    tot_fem = sum(tot_fem),
    tot_mny = sum(tot_mny),
    tot_tot = sum(tot_tot)
  ) %>%
  mutate(
    percentMinority = tot_mny/tot_tot
  ) %>%
  select(dst_nam,percentMinority) %>%
  
  rename(
    DistrictName = dst_nam,
    percentMinority2000 = percentMinority
  )

enrolled.ethnicity.district.1999 <- read_csv("1999_enrolled_ethnicity_district.csv") %>%
  select (dst_num, dst_nam, ami_mal, ami_fem, api_mal, api_fem, his_mal, his_fem, blk_mal, blk_fem, wht_mal, wht_fem, tot_mal, tot_fem, tot_mny, tot_tot) %>%
  group_by(dst_nam) %>%
  summarize(
    ami_mal = sum(ami_mal),
    ami_fem = sum(ami_fem),
    api_mal = sum(api_mal),
    api_fem = sum(api_fem),
    his_mal = sum(his_mal),
    his_fem = sum(his_fem),
    blk_mal = sum(blk_mal),
    blk_fem = sum(blk_fem),
    wht_mal = sum(wht_mal),
    wht_fem = sum(wht_fem),
    tot_mal = sum(tot_mal),
    tot_fem = sum(tot_fem),
    tot_mny = sum(tot_mny),
    tot_tot = sum(tot_tot)
  ) %>%
  mutate(
    percentMinority = tot_mny/tot_tot
  ) %>%
  select(dst_nam,percentMinority) %>%
  
  rename(
    DistrictName = dst_nam,
    percentMinority1999 = percentMinority
  )

enrolled.ethnicity.1999_2018 <- full_join(enrolled.ethnicity.district.1999, enrolled.ethnicity.district.2000, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2001, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2002, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2003, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2004, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2005, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2006, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2007, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2008, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2009, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2010, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2011, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2012, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2013, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2014, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2015, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2016, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2017, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  full_join(enrolled.ethnicity.district.2018, enrolled.ethnicity.1999_2018, by = c("DistrictName")) %>%
  gather(year,percentMinority,percentMinority1999:percentMinority2018) %>%
  mutate(
    year = replace(year, year =="percentMinority1999" ,"1999"),
    year = replace(year, year =="percentMinority2000" ,"2000"),
    year = replace(year, year =="percentMinority2001" ,"2001"),
    year = replace(year, year =="percentMinority2002" ,"2002"),
    year = replace(year, year =="percentMinority2003" ,"2003"),
    year = replace(year, year =="percentMinority2004" ,"2004"),
    year = replace(year, year =="percentMinority2005" ,"2005"),
    year = replace(year, year =="percentMinority2006" ,"2006"),
    year = replace(year, year =="percentMinority2007" ,"2007"),
    year = replace(year, year =="percentMinority2008" ,"2008"),
    year = replace(year, year =="percentMinority2009" ,"2009"),
    year = replace(year, year =="percentMinority2010" ,"2010"),
    year = replace(year, year =="percentMinority2011" ,"2011"),
    year = replace(year, year =="percentMinority2012" ,"2012"),
    year = replace(year, year =="percentMinority2013" ,"2013"),
    year = replace(year, year =="percentMinority2014" ,"2014"),
    year = replace(year, year =="percentMinority2015" ,"2015"),
    year = replace(year, year =="percentMinority2016" ,"2016"),
    year = replace(year, year =="percentMinority2017" ,"2017"),
    year = replace(year, year =="percentMinority2018" ,"2018"))







#----------------------------------------
ethnicity.test <- read_csv("2018_enrolled_ethnicity_district.csv") %>%
  select (districtNumber, DistrictName, AMI_Male, AMI_Female, ASI_Male, ASI_Female, BLK_Male, BLK_Female, HIS_Male, HIS_Female, HPI_Male, HPI_Female, MLT_Male, MLT_Female, WHT_Male, WHT_Female, TotalMale, TotalFemale, TotalMinority, TotalStudents) %>%
  group_by(DistrictName) %>%
  summarize(
          AMI_Male = sum(AMI_Male),
            AMI_Female = sum(AMI_Female),
            BLK_Male = sum(BLK_Male), 
            BLK_Female = sum(BLK_Female), 
            HIS_Male = sum(HIS_Male), 
            HIS_Female = sum(HIS_Female), 
            HPI_Male = sum(HPI_Male), 
            HPI_Female = sum(HPI_Female),
            MLT_Male = sum(MLT_Male),
            MLT_Female = sum(MLT_Female), 
            WHT_Male = sum(WHT_Male), 
            WHT_Female = sum(WHT_Female), 
            TotalMale = sum(TotalMale), 
            TotalFemale = sum(TotalFemale), 
            TotalMinority = sum(TotalMinority), 
            TotalStudents = sum(TotalStudents)
          ) %>%
  mutate(
    percentMinority = TotalMinority/TotalStudents
  )
#  rename(countyName = COUNTY,
#         value1990=EST001,
#         countyFIPS=COUNTYA)

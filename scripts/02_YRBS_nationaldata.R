##########################################
# YRBS national data, 2007-2023

##########################################

here::i_am("scripts/02_YRBS_nationaldata.R")
source(here::here("scripts/01_births_pop.R"))
options(survey.lonely.psu = "adjust")

###### POP AND BIRTH DATA, BY RACE/ETHNICITY
# from CDC WONDER
pop_raceEth_updated<- read.csv(here("raw_data", "pop_bridged2006_2019_single2020_2022.csv")) %>%
  dplyr::select(-type)

births_raceEth_updated<- read.csv(here("raw_data","births_bridged2006_2019_single2020_2022.csv")) %>%
  dplyr::select(yr, natality, raceEth)

#combine
births_pop_raceEth_updated<- merge(pop_raceEth_updated, births_raceEth_updated, by=c("yr", "raceEth"), all.y=TRUE) %>%
  mutate(raceEth = case_when(
    raceEth=="black" ~ "Black",
    raceEth=="hispanic" ~"Hispanic/Latino",
    raceEth=="white"~ "White"
  )) %>%
  rename(population=pop, births=natality) 

#births per 1000 data for T1 (by race)
births_T1_national<- births_pop_raceEth_updated %>%
  filter(yr>=2008, yr<=2024) %>%
  filter(yr %% 2==0) %>%
  mutate(births_per_1000 = births/(population/1000)) %>%
  mutate(yr_groups3 = case_when(
    yr %in% c(2008, 2010, 2012) ~ "group2007_2011",
    yr %in% c(2014, 2016, 2018) ~ "group2013_2017",
    yr %in% c(2020, 2022, 2024) ~ "group2019_2023"
  )) %>%
  group_by(raceEth, yr_groups3) %>%
  summarize(weighted_avg_births = sum(births_per_1000 * population) / sum(population), .groups = 'drop') %>%
  mutate(weighted_avg_births=round(weighted_avg_births,1))

#births overall and by age group
births_national<- read.csv(here("raw_data","births_national.csv"))

## overall
births_national2<- births_national%>%
  mutate(Births=as.integer(Births)) %>%
  mutate(births_per_1000 = Births/(Pop/1000)) %>%
  mutate(yr_groups3 = case_when(
    Year %in% c(2008, 2010, 2012) ~ "group2007_2011",
    Year %in% c(2014, 2016, 2018) ~ "group2013_2017",
    Year %in% c(2020, 2022, 2024) ~ "group2019_2023"
  )) %>%
  group_by(yr_groups3) %>%
  summarize(weighted_avg_births = sum(births_per_1000 * Pop) / sum(Pop), .groups = 'drop') %>%
  mutate(weighted_avg_births=round(weighted_avg_births,1)) %>%
  filter(!(is.na(yr_groups3)))

## older
births_national2_1819<- births_national%>%
  dplyr::select(Year, Pop, births18_19) %>%
  mutate(Births=as.integer(births18_19)) %>%
  mutate(births_per_1000 = Births/(Pop/1000)) %>%
  mutate(yr_groups3 = case_when(
    Year %in% c(2008, 2010, 2012) ~ "group2007_2011",
    Year %in% c(2014, 2016, 2018) ~ "group2013_2017",
    Year %in% c(2020, 2022, 2024) ~ "group2019_2023"
  )) %>%
  group_by(yr_groups3) %>%
  summarize(weighted_avg_births = sum(births_per_1000 * Pop) / sum(Pop), .groups = 'drop') %>%
  mutate(weighted_avg_births=round(weighted_avg_births,1)) %>%
  filter(!(is.na(yr_groups3)))

## younger
births_national2_1517<- births_national%>%
  dplyr::select(Year, Pop, births15_17) %>%
  mutate(Births=as.integer(births15_17)) %>%
  mutate(births_per_1000 = Births/(Pop/1000)) %>%
  mutate(yr_groups3 = case_when(
    Year %in% c(2008, 2010, 2012) ~ "group2007_2011",
    Year %in% c(2014, 2016, 2018) ~ "group2013_2017",
    Year %in% c(2020, 2022, 2024) ~ "group2019_2023"
  )) %>%
  group_by(yr_groups3) %>%
  summarize(weighted_avg_births = sum(births_per_1000 * Pop) / sum(Pop), .groups = 'drop') %>%
  mutate(weighted_avg_births=round(weighted_avg_births,1)) %>%
  filter(!(is.na(yr_groups3)))


###### YRBS NATIONAL DATA
## FCNS
create_protection <- function(data, sexactive, protection) {
  data1 <- data %>%
    #1=didn't use protection, 2= used protection
    mutate(no_protection = case_when(
      .data[[sexactive]] %in% c(3, 4, 5, 6, 7, 8) & .data[[protection]] == 2 ~ 1,
      .data[[sexactive]] %in% c(3, 4, 5, 6, 7, 8) & .data[[protection]] %in% c(3, 4, 5, 6, 7, 8) ~ 2,
      TRUE ~ NA_integer_)) %>%
    return(data1)
}

create_larc <- function(data, sexactive, protection, year) {
  # no larc: 1 = didn't use LARC, 2 = used LARC (LARC is IUD or implant)
  data1 <- data %>%
    mutate(no_larc = case_when(
      year < 2013 ~ NA_integer_,  # Assign NA for years before 2013
      # For 2013 and onwards
      year >= 2013 ~ case_when(
        .data[[sexactive]] %in% c(3, 4, 5, 6, 7, 8) & .data[[protection]] %in% c(5) ~ 2,  # LARC (IUD or implant)
        .data[[sexactive]] %in% c(3, 4, 5, 6, 7, 8) & .data[[protection]] %in% c(2, 3, 4, 6, 7, 8) ~ 1,  # Non-LARC methods
        TRUE ~ NA_integer_  # Otherwise NA
      )
    )) 
  return(data1)
}

create_no_hm_effective<- function(data, sexactive, protection, year) {
  #1= didn't use HM_effective, 2= did use
  data1<- data %>%
    mutate(no_hm_effective = case_when(
      year< 2013 ~ case_when(
        .data[[sexactive]] %in% c(3,4,5,6,7,8) & .data[[protection]] %in% c(2,4,6,7,8) ~ 1,
        .data[[sexactive]] %in% c(3,4,5,6,7,8) & .data[[protection]] %in% c(3,5) ~ 2,
        TRUE ~ NA_integer_
      ),
      year>=2013 ~ case_when(
        .data[[sexactive]] %in% c(3,4,5,6,7,8) & .data[[protection]] %in% c(2,4,7,8) ~ 1,
        .data[[sexactive]] %in% c(3,4,5,6,7,8) & .data[[protection]] %in% c(3,5,6) ~ 2,
        TRUE ~ NA_integer_
      )
    )) %>%
    return(data1)
}

## read in YRBS data
national2023<- read_sas(here("raw_data/YRBS/national", "yrbs2023.sas7bdat")) %>%
  filter(Q1 %in% c("3", "4", "5", "6", "7"),
         Q2=="1") %>%
  create_larc(., sexactive="Q59", protection= "Q62", year=2023) %>%
  create_protection(., sexactive="Q59", protection="Q62") %>%
  create_no_hm_effective(., sexactive="Q59", protection= "Q62", year=2023)

national2021<-read_sas(here("raw_data/YRBS/national", "yrbs2021.sas7bdat")) %>%
  filter(Q1 %in% c("3", "4", "5", "6", "7"),
         Q2=="1") %>%
  create_larc(., sexactive="Q60", protection= "Q63", year=2021) %>%
  create_protection(., sexactive="Q60", protection="Q63") %>%
  create_no_hm_effective(., sexactive="Q60", protection= "Q63", year=2021)

national2019<- read_sas(here("raw_data/YRBS/national", "yrbs2019.sas7bdat"))%>%
  filter(Q1 %in% c("3", "4", "5", "6", "7"),
         Q2=="1") %>%
  create_larc(., sexactive="Q61", protection= "Q64", year=2019) %>%
  create_protection(., sexactive="Q61", protection="Q64") %>%
  create_no_hm_effective(., sexactive="Q61", protection= "Q64", year=2019) 

national2017<-read_sas(here("raw_data/YRBS/national", "yrbs2017.sas7bdat")) %>%
  filter(Q1 %in% c("3", "4", "5", "6", "7"),
         Q2=="1") %>%
  create_larc(., sexactive="Q62", protection= "Q65", year=2017) %>%
  create_protection(., sexactive="Q62", protection="Q65") %>%
  create_no_hm_effective(., sexactive="Q62", protection= "Q65", year=2017)

national2015<- read_sas(here("raw_data/YRBS/national", "yrbs2015.sas7bdat"))%>%
  filter(Q1 %in% c("3", "4", "5", "6", "7"),
         Q2=="1") %>%
  create_larc(., sexactive="Q63", protection= "Q66", year=2015) %>%
  create_protection(., sexactive="Q63", protection="Q66") %>%
  create_no_hm_effective(., sexactive="Q63", protection= "Q66", year=2015)

national2013<-read_sas(here("raw_data/YRBS/national", "yrbs2013.sas7bdat")) %>%
  filter(Q1 %in% c("3", "4", "5", "6", "7"),
         Q2=="1") %>%
  create_larc(., sexactive="Q62", protection= "Q65", year=2013) %>%
  create_protection(., sexactive="Q62", protection="Q65") %>%
  create_no_hm_effective(., sexactive="Q62", protection= "Q65", year=2013) 

national2011<- read_sas(here("raw_data/YRBS/national", "yrbs2011.sas7bdat")) %>%
  filter(Q1 %in% c("3", "4", "5", "6", "7"),
         Q2=="1") %>%
  create_larc(., sexactive="Q63", protection="Q66", year=2011) %>%
  create_protection(., sexactive="Q63", protection="Q66") %>%
  create_no_hm_effective(., sexactive="Q63", protection="Q66", year=2011)

national2009<-read_sas(here("raw_data/YRBS/national", "yrbs2009.sas7bdat")) %>%
  filter(Q1 %in% c("3", "4", "5", "6", "7"),
         Q2=="1") %>%
  create_larc(., sexactive="Q61", protection="Q64", year=2009) %>%
  create_protection(., sexactive="Q61", protection="Q64") %>%
  create_no_hm_effective(., sexactive="Q61", protection="Q64", year=2009)

national2007<- read_sas(here("raw_data/YRBS/national", "yrbs2007.sas7bdat"))%>%
  filter(Q1 %in% c("3", "4", "5", "6", "7"),
         Q2=="1") %>%
  rename(WEIGHT=Weight, STRATUM=Stratum)  %>%
  create_protection(., sexactive="Q61", protection="Q64") %>%
  create_larc(., sexactive="Q61", protection="Q64", year=2007) %>%
  create_no_hm_effective(., sexactive="Q61", protection="Q64", year=2007)

national2005<- read_sas(here("raw_data/YRBS/national", "yrbs2005.sas7bdat")) %>%
  filter(Q1 %in% c("3", "4", "5", "6", "7"),
         Q2=="1") %>%
  rename(WEIGHT=Weight, STRATUM=Stratum) %>%
  create_protection(., sexactive="Q60", protection="Q63") %>%
  mutate(RACETH = case_when(
    Q4==3 ~ 3, #black
    Q4==6 ~ 5, #white,
    Q4 %in% c(4,7) ~ 6, #hispanic
    TRUE ~ 1)) %>%
  create_larc(., sexactive="Q60", protection="Q63", year=2005) %>%
  create_no_hm_effective(., sexactive="Q60", protection="Q63", year=2005)

## clean data- create national_streamlined
combining_yrs_fcn<- function(data, year_val, sexactive_var, condom_var, raceeth_var, forced_intercourse_var) {
  data1<- data %>%
    mutate(yr=year_val,
           sexactive_var = as.integer(!!sym(sexactive_var)),
           condom_var = as.integer(!!sym(condom_var)),
           forced_intercourse_var = as.integer(!!sym(forced_intercourse_var)),
           raceth_var1 = as.character(!!sym(raceeth_var))) %>%
    rename(race_eth_variable=raceth_var1) %>%
    #sexactive
    mutate(sexactive = case_when(
      sexactive_var %in% c(3, 4, 5, 6, 7, 8)  ~ 1,
      sexactive_var %in% c(1, 2)  ~ 2,
      TRUE ~ NA_integer_
    )) %>%
    #no condom use (2=used condom, 1=didn't use condom)
    mutate(no_condom=case_when(
      condom_var==2 & sexactive_var %in% c(3, 4, 5, 6, 7, 8) ~ 2,
      condom_var==3 & sexactive_var %in% c(3, 4, 5, 6, 7, 8) ~ 1,
      TRUE ~ NA_integer_
    )) %>%
    #forced intercourse
    mutate(forced_intercourse = case_when(
      forced_intercourse_var==1 & sexactive_var %in% c(3, 4, 5, 6, 7, 8) ~ 1,  
      forced_intercourse_var==2  & sexactive_var %in% c(3, 4, 5, 6, 7, 8) ~ 2,
      TRUE ~ NA_integer_
    )) %>%
    #select and return
    dplyr::select(yr, PSU, WEIGHT, STRATUM, no_protection, sexactive, no_condom, Q1, race_eth_variable, no_larc, forced_intercourse, no_hm_effective)
  return(data1)
}

national2005_streamlined<- combining_yrs_fcn(data=national2005, year_val="2005", sexactive_var="Q60", condom_var="Q62", raceeth_var="RACETH", forced_intercourse_var="Q22") 
national2007_streamlined<- combining_yrs_fcn(data=national2007, year_val="2007", sexactive_var="Q61", condom_var="Q63", raceeth_var="RaceEth", forced_intercourse_var="Q22") 
national2009_streamlined<- combining_yrs_fcn(data=national2009, year_val="2009", sexactive_var="Q61", condom_var="Q63", raceeth_var="RACEETH", forced_intercourse_var="Q21") 
national2011_streamlined<- combining_yrs_fcn(data=national2011, year_val="2011", sexactive_var="Q63", condom_var="Q65", raceeth_var="RACEETH", forced_intercourse_var="Q21") 
national2013_streamlined<- combining_yrs_fcn(data=national2013, year_val="2013", sexactive_var="Q62", condom_var="Q64", raceeth_var="RACEETH", forced_intercourse_var="Q21") 

national2015_streamlined<- combining_yrs_fcn(data=national2015, year_val="2015", sexactive_var="Q63", condom_var="Q65", raceeth_var="RACEETH", forced_intercourse_var="Q21") 
national2017_streamlined<- combining_yrs_fcn(data=national2017, year_val="2017", sexactive_var="Q62", condom_var="Q64", raceeth_var="RACEETH", forced_intercourse_var="Q19") 
national2019_streamlined<- combining_yrs_fcn(data=national2019, year_val="2019", sexactive_var="Q61", condom_var="Q63", raceeth_var="RACEETH", forced_intercourse_var="Q19") 
national2021_streamlined<- combining_yrs_fcn(data=national2021, year_val="2021", sexactive_var="Q60", condom_var="Q62", raceeth_var="RACEETH", forced_intercourse_var="Q19") 
national2023_streamlined<- combining_yrs_fcn(data=national2023, year_val="2023", sexactive_var="Q59", condom_var="Q61", raceeth_var="RACEETH", forced_intercourse_var="Q19") 

national_streamlined<- bind_rows(national2005_streamlined,national2007_streamlined,national2009_streamlined,
                                 national2011_streamlined,national2013_streamlined,national2015_streamlined,
                                 national2017_streamlined,national2019_streamlined,national2021_streamlined,
                                 national2023_streamlined)

## subgroups
### ages 14-16
national_ages14_16<- national_streamlined %>%
  filter(Q1 %in% c("3", "4", "5"))

### ages17up
national_ages17up<- national_streamlined %>%
  filter(Q1 %in% c("6", "7"))

### white
national_white<- national_streamlined %>%
  filter(race_eth_variable == "5")

### black
national_black<- national_streamlined %>%
  filter(race_eth_variable == "3")

### hispanic
national_hispanic<- national_streamlined %>%
  filter(race_eth_variable %in% c("6","7"))

## 3 YEAR GROUPINGS 
national_grouped_yrs<- function(data, year_val_min, year_val_max) {
  national_streamlined1<- data %>%
    mutate(yr=as.integer(yr)) %>%
    filter(yr>=year_val_min, yr<=year_val_max)
  yrbsdes <- svydesign(id = ~PSU, weight = ~WEIGHT, strata =~interaction(STRATUM, yr), data = national_streamlined1, nest = TRUE)

  no_protection<- svyciprop(~I(no_protection == 1), yrbsdes, na.rm = TRUE)
  percent_no_protection <- no_protection[1]
  percent_no_protection_lb <- attr(no_protection, "ci")[1]
  percent_no_protection_ub <- attr(no_protection, "ci")[2]
  results_national<- data.frame(percent_no_protection, percent_no_protection_lb, percent_no_protection_ub)
  
  sexactive<- svyciprop(~I(sexactive == 1), yrbsdes, na.rm = TRUE)
  results_national$percent_sexually_active <- sexactive[1]
  results_national$percent_sexually_active_lb <- attr(sexactive, "ci")[1]
  results_national$percent_sexually_active_ub <- attr(sexactive, "ci")[2]

  no_condom<- svyciprop(~I(no_condom == 1), yrbsdes, na.rm = TRUE)
  results_national$percent_no_condom <- no_condom[1]
  results_national$percent_no_condom_lb <- attr(no_condom, "ci")[1]
  results_national$percent_no_condom_ub <- attr(no_condom, "ci")[2]
  
  forced_intercourse <- svyciprop(~I(forced_intercourse == 1), yrbsdes, na.rm = TRUE)
  results_national$percent_sa_forced_intercourse <- forced_intercourse[1]
  results_national$percent_sa_forced_intercourse_lb <- attr(forced_intercourse, "ci")[1]
  results_national$percent_sa_forced_intercourse_ub <- attr(forced_intercourse, "ci")[2]

  no_hm_effective<- svyciprop(~I(no_hm_effective==1), yrbsdes, na.rm=TRUE)
  results_national$percent_no_hm_effective<- no_hm_effective[1]
  results_national$percent_no_hm_effective_lb <- attr(no_hm_effective, "ci")[1]
  results_national$percent_no_hm_effective_ub <- attr(no_hm_effective, "ci")[2]
  return(results_national)
}

#overall
overall_2007_2011<- national_grouped_yrs(national_streamlined, 2007, 2011) %>% mutate(yr_groups3 = "group2007_2011", region="US", stratify = "overall")
overall_2013_2017<- national_grouped_yrs(national_streamlined, 2013, 2014) %>% mutate(yr_groups3 = "group2013_2017", region="US", stratify = "overall")
overall_2019_2023<- national_grouped_yrs(national_streamlined, 2019, 2023) %>% mutate(yr_groups3 = "group2019_2023", region="US", stratify = "overall")

#by age
younger_2007_2011<- national_grouped_yrs(national_ages14_16, 2007, 2011) %>% mutate(yr_groups3 = "group2007_2011", region="US", stratify = "14_16")
younger_2013_2017<- national_grouped_yrs(national_ages14_16, 2013, 2017) %>% mutate(yr_groups3 = "group2013_2017", region="US", stratify = "14_16")
younger_2019_2023<- national_grouped_yrs(national_ages14_16, 2019, 2023) %>% mutate(yr_groups3 = "group2019_2023", region="US", stratify = "14_16")

older_2007_2011<- national_grouped_yrs(national_ages17up, 2007, 2011) %>% mutate(yr_groups3 = "group2007_2011", region="US", stratify = "17up")
older_2013_2017<- national_grouped_yrs(national_ages17up, 2013, 2017) %>% mutate(yr_groups3 = "group2013_2017", region="US", stratify = "17up")
older_2019_2023<- national_grouped_yrs(national_ages17up, 2019, 2023) %>% mutate(yr_groups3 = "group2019_2023", region="US", stratify = "17up")

#by race
white_2007_2011<- national_grouped_yrs(national_white, 2007, 2011) %>% mutate(yr_groups3 = "group2007_2011", region="US", stratify = "white")
white_2013_2017<- national_grouped_yrs(national_white, 2013, 2017) %>% mutate(yr_groups3 = "group2013_2017", region="US", stratify = "white")
white_2019_2023<- national_grouped_yrs(national_white, 2019, 2023) %>% mutate(yr_groups3 = "group2019_2023", region="US", stratify = "white")

black_2007_2011<- national_grouped_yrs(national_black, 2007, 2011) %>% mutate(yr_groups3 = "group2007_2011", region="US", stratify = "black")
black_2013_2017<- national_grouped_yrs(national_black, 2013, 2017) %>% mutate(yr_groups3 = "group2013_2017", region="US", stratify = "black")
black_2019_2023<- national_grouped_yrs(national_black, 2019, 2023) %>% mutate(yr_groups3 = "group2019_2023", region="US", stratify = "black")

hispanic_2007_2011<- national_grouped_yrs(national_hispanic, 2007, 2011) %>% mutate(yr_groups3 = "group2007_2011", region="US", stratify = "hispanic")
hispanic_2013_2017<- national_grouped_yrs(national_hispanic, 2013, 2017) %>% mutate(yr_groups3 = "group2013_2017", region="US", stratify = "hispanic")
hispanic_2019_2023<- national_grouped_yrs(national_hispanic, 2019, 2023) %>% mutate(yr_groups3 = "group2019_2023", region="US", stratify = "hispanic")

#rbind all
national_dataset_final<- rbind(
  overall_2007_2011, overall_2013_2017, overall_2019_2023,
  younger_2007_2011, younger_2013_2017, younger_2019_2023,
  older_2007_2011, older_2013_2017, older_2019_2023,
  white_2007_2011, white_2013_2017, white_2019_2023,
  black_2007_2011, black_2013_2017, black_2019_2023,
  hispanic_2007_2011, hispanic_2013_2017, hispanic_2019_2023
) 


## BY YR
national_each_yr<- function(data) {
  yrbsdes <- svydesign(id = ~PSU, weight = ~WEIGHT, strata = ~STRATUM, data = data, nest = TRUE)

  no_protection<- svyciprop(~I(no_protection == 1), yrbsdes, na.rm = TRUE)
  percent_no_protection <- no_protection[1]
  percent_no_protection_lb <- attr(no_protection, "ci")[1]
  percent_no_protection_ub <- attr(no_protection, "ci")[2]
  results_national<- data.frame(percent_no_protection, percent_no_protection_lb, percent_no_protection_ub)
  
  sexactive<- svyciprop(~I(sexactive == 1), yrbsdes, na.rm = TRUE)
  results_national$percent_sexually_active <- sexactive[1]
  results_national$percent_sexually_active_lb <- attr(sexactive, "ci")[1]
  results_national$percent_sexually_active_ub <- attr(sexactive, "ci")[2]

  no_condom<- svyciprop(~I(no_condom == 1), yrbsdes, na.rm = TRUE)
  results_national$percent_no_condom <- no_condom[1]
  results_national$percent_no_condom_lb <- attr(no_condom, "ci")[1]
  results_national$percent_no_condom_ub <- attr(no_condom, "ci")[2]

  forced_intercourse <- svyciprop(~I(forced_intercourse == 1), yrbsdes, na.rm = TRUE)
  results_national$percent_sa_forced_intercourse <- forced_intercourse[1]
  results_national$percent_sa_forced_intercourse_lb <- attr(forced_intercourse, "ci")[1]
  results_national$percent_sa_forced_intercourse_ub <- attr(forced_intercourse, "ci")[2]

  no_hm_effective<- svyciprop(~I(no_hm_effective==1), yrbsdes, na.rm=TRUE)
  results_national$percent_no_hm_effective<- no_hm_effective[1]
  results_national$percent_no_hm_effective_lb <- attr(no_hm_effective, "ci")[1]
  results_national$percent_no_hm_effective_ub <- attr(no_hm_effective, "ci")[2]
  return(results_national)
}

national_larc<- function(data) {
  yrbsdes <- svydesign(id = ~PSU, weight = ~WEIGHT, strata = ~STRATUM, data = data, nest = TRUE)
  no_larc <- svyciprop(~I(no_larc == 1), yrbsdes, na.rm = TRUE)
  percent_no_larc <- no_larc[1]
  percent_no_larc_lb <- attr(no_larc, "ci")[1]
  percent_no_larc_ub <- attr(no_larc, "ci")[2]
  results<- data.frame(percent_no_larc, percent_no_larc_lb, percent_no_larc_ub)
}

call_each_yr_fcn<- function(group_name, data) {
  #sexactive, no_protection, no_condom, forced_intercourse, no_hm_effective
  weighted<- data %>%
    group_by(yr) %>%
    group_modify(~ national_each_yr(.x)) 
  #no_larc
  weighted_larc<- data %>%
    filter(yr %in% c("2013", "2015", "2017", "2019", "2021", "2023")) %>%
    group_by(yr) %>%
    group_modify(~ national_larc(.x)) 
  #combine
  weighted_final<- merge(weighted, weighted_larc, by=c("yr"), all.x=TRUE) %>%
    mutate(stratify=group_name)
  #return
  return(weighted_final)
}

national_overall_weighted<- call_each_yr_fcn("overall", national_streamlined)
national_younger_weighted<- call_each_yr_fcn("younger", national_ages14_16)
national_older_weighted<- call_each_yr_fcn("older", national_ages17up)
national_white_weighted<- call_each_yr_fcn("white", national_white)
national_black_weighted<- call_each_yr_fcn("black", national_black)
national_hispanic_weighted<- call_each_yr_fcn("hispanic", national_hispanic)

national_by_yr<- rbind(national_overall_weighted,
                       national_younger_weighted, 
                       national_older_weighted,
                       national_white_weighted,
                       national_black_weighted,
                       national_hispanic_weighted) %>%
  filter(yr!=2005)

##########################################
# YRBS state data, 2007-2023

## This file reads in data from the YRBS state CSV files

## Variables:
###  1) sexually_active: % Sexually active in the past 3 mo
###  2) no_condom: % sexually active who did not use a condom the last time they had sexual intercourse 
###  3) no_prevention: % sexually active who used no method to prevent pregnancy the last time they had sexual intercourse
###  4) no_LARC: % sexually active who did not report using LARC as 1 method of pregnancy prevention in last sexual intercourse
###  5) forced: % sexually active who have been forced to have sexual intercourse when they did not want to
###  6) highly_effective: % sexually active who used highly effective (birth control pills or LARC) 
##########################################

################### READ IN STATE DATA ###################
options(survey.lonely.psu = "adjust")
here::i_am("scripts/03_YRBS_statedata.R")
source(here::here("scripts/02_YRBS_nationaldata.R"))

select_vars_fcn<- function(data) {
  data1<- data %>%
    dplyr::select(sitename, age, sex, grade, PSU, year, weight, stratum, q60, q62, q63,race4, q59, q19)
  return(data1)
}

ad_sadcq <- read.csv(here("raw_data/YRBS/state", "AD_SADCQ.csv"))  %>%
  select_vars_fcn()
eh_sadcq <- read.csv(here("raw_data/YRBS/state", "EH_SADCQ.csv")) %>%
  select_vars_fcn()
il_sadcq <- read.csv(here("raw_data/YRBS/state", "IL_SADCQ.csv")) %>%
  select_vars_fcn()
mp_sadcq <- read.csv(here("raw_data/YRBS/state", "MP_SADCQ.csv")) %>%
  select_vars_fcn()
qt_sadcq <- read.csv(here("raw_data/YRBS/state", "QT_SADCQ.csv")) %>%
  select_vars_fcn()
uz_sadcq <- read.csv(here("raw_data/YRBS/state", "UZ_SADCQ.csv")) %>%
  select_vars_fcn()

all_statesthrough_2021 <- rbind(ad_sadcq, eh_sadcq, il_sadcq, mp_sadcq, qt_sadcq, uz_sadcq) %>%
  mutate(sitename=str_replace(sitename, "\\s*\\([^\\)]+\\)", "")) %>%
  #create qn60 (1=sexually active, 2=not sexually active)
  mutate(qn60 = case_when(
    q60 %in% c(3, 4, 5, 6, 7, 8)  ~ 1,
    q60 %in% c(1, 2)  ~ 2,
    TRUE ~ NA_integer_
  )) %>%
  #create no_protection (no protection = 1, protection =  2)
  mutate(no_protection = case_when(
    q60 %in% c(3, 4, 5, 6, 7, 8) & q63 == 2 ~ 1,
    q60 %in% c(3, 4, 5, 6, 7, 8) & q63 %in% c(3, 4, 5, 6, 7, 8) ~ 2,
    TRUE ~ NA_integer_
  )) %>%
  #create no_condom (1=no condom use, 2=used condom)
  mutate(no_condom=case_when(
    q60 %in% c(3, 4, 5, 6, 7, 8) & q62 %in% c(3) ~ 1,
    q60 %in% c(3, 4, 5, 6, 7, 8) & q62 %in% c(2) ~ 2,
    TRUE ~ NA_integer_
  )) %>%
  rename(state=sitename) %>%
  #LARC (IUD or IMPLANT)
  #2011 and before: 5=injectable, ring, implant, IUD
  mutate(no_larc = case_when(
    year < 2013 ~ NA_integer_, 
    #2013 on: 5=IUD or implant, 6=shot/patch/birth control ring
    year >= 2013 ~ case_when(
      q60 %in% c(3, 4, 5, 6, 7, 8) & q63 %in% c(5) ~ 2,
      q60 %in% c(3, 4, 5, 6, 7, 8) & q63 %in% c(2, 3, 4, 6, 7, 8) ~ 1,
      TRUE ~ NA_integer_
    )
  )) %>%
  #forced to have sexual intercourse, forced_intercourse (1=forced, 2=not forced)
  mutate(forced_intercourse = case_when(
    q60 %in% c(3, 4, 5, 6, 7, 8) & q19 %in% c(1) ~ 1,
    q60 %in% c(3, 4, 5, 6, 7, 8) & q19 %in% c(2) ~ 2)) %>%
  filter(sex==1,
         age %in% c(3,4,5,6,7)) %>%
  #not highly or moderately effective (1= not using highly/moderately effective BC, 2= using highly/moderately effective BC)
  mutate(no_hm_effective = case_when(
    #2011 and before: 3=BC pills, 5=injectable, ring, implant, IUD
    year < 2013 ~ case_when(
      q60 %in% c(3, 4, 5, 6, 7, 8) & q63 %in% c(2, 4, 6, 7, 8) ~ 1,
      q60 %in% c(3, 4, 5, 6, 7, 8) & q63 %in% c(3, 5) ~ 2,
      TRUE ~ NA_integer_
    ),
    #2013 on: 5=IUD or implant, 6=shot/patch/birth control ring
    year >= 2013 ~ case_when(
      q60 %in% c(3, 4, 5, 6, 7, 8) & q63 %in% c(2, 4, 7, 8) ~ 1,
      q60 %in% c(3, 4, 5, 6, 7, 8) & q63 %in% c(3, 5, 6) ~ 2,
      TRUE ~ NA_integer_
    ),
    TRUE ~ NA_integer_ 
  )) %>%
  select(-c(q60, q62, q63, q59, q19))


# READ IN 2023 DATA
select_vars_fcn_2023<- function(data) {
  data1<- data %>%
    dplyr::select(sitename, age, sex, grade, PSU, year, weight, stratum, race4, q19, q59, q61, q62)
  return(data1)
}
ad2023<-read_sas(here("raw_data/YRBS/state", "yrbs2023ad.sas7bdat")) %>%
  select_vars_fcn_2023()
eh2023<-read_sas(here("raw_data/YRBS/state", "yrbs2023eh.sas7bdat")) %>%
  select_vars_fcn_2023()
il2023<-read_sas(here("raw_data/YRBS/state", "yrbs2023il.sas7bdat")) %>%
  select_vars_fcn_2023()
m2023<- read_sas(here("raw_data/YRBS/state", "yrbs2023m.sas7bdat")) %>%
  select_vars_fcn_2023()
np2023<- read_sas(here("raw_data/YRBS/state", "yrbs2023np.sas7bdat")) %>%
  select_vars_fcn_2023()
qt2023<-read_sas(here("raw_data/YRBS/state", "yrbs2023qt.sas7bdat")) %>%
  select_vars_fcn_2023()
uz2023<-read_sas(here("raw_data/YRBS/state", "yrbs2023uz.sas7bdat")) %>%
  select_vars_fcn_2023()

all_states2023 <- rbind(ad2023, eh2023, il2023, m2023, np2023, qt2023, uz2023) %>%
  mutate(sitename=str_replace(sitename, "\\s*\\([^\\)]+\\)", "")) %>%
  #create qn60 (1=sexually active, 2=not sexually active)
  mutate(qn60 = case_when(
    q59 %in% c(3, 4, 5, 6, 7, 8)  ~ 1,
    q59 %in% c(1, 2)  ~ 2,
    TRUE ~ NA_integer_
  )) %>%
  #create no_protection (no protection = 1, protection =  2)
  mutate(no_protection = case_when(
    q59 %in% c(3, 4, 5, 6, 7, 8) & q62 == 2 ~ 1,
    q59 %in% c(3, 4, 5, 6, 7, 8) & q62 %in% c(3, 4, 5, 6, 7, 8) ~ 2,
    TRUE ~ NA_integer_
  )) %>%
  #create no_condom (1=no condom use, 2=used condom)
  mutate(no_condom=case_when(
    q59 %in% c(3, 4, 5, 6, 7, 8) & q61 %in% c(3) ~ 1,
    q59 %in% c(3, 4, 5, 6, 7, 8) & q61 %in% c(2) ~ 2,
    TRUE ~ NA_integer_
  )) %>%
  rename(state=sitename) %>%
  #LARC (IUD or IMPLANT)
  #2011 and before: 5=injectable, ring, implant, IUD
  mutate(no_larc = case_when(
    year < 2013 ~ NA_integer_, 
    #2013 on: 5=IUD or implant, 6=shot/patch/birth control ring
    year >= 2013 ~ case_when(
      q59 %in% c(3, 4, 5, 6, 7, 8) & q62 %in% c(5) ~ 2,
      q59 %in% c(3, 4, 5, 6, 7, 8) & q62 %in% c(2, 3, 4, 6, 7, 8) ~ 1,
      TRUE ~ NA_integer_
    ),
    TRUE ~ NA_integer_ 
  )) %>%
  #forced to have sexual intercourse, forced_intercourse (1=forced, 2=not forced)
  mutate(forced_intercourse = case_when(
    q59 %in% c(3, 4, 5, 6, 7, 8) & q19 %in% c(1) ~ 1,
    q59 %in% c(3, 4, 5, 6, 7, 8) & q19 %in% c(2) ~ 2)) %>%
  filter(sex==1,
         age %in% c(3,4,5,6,7)) %>%
  #not highly or moderately effective (1= not using highly/moderately effective BC, 2= using highly/moderately effective BC)
  mutate(no_hm_effective = case_when(
    #2011 and before: 3=BC pills, 5=injectable, ring, implant, IUD
    year < 2013 ~ case_when(
      q59 %in% c(3, 4, 5, 6, 7, 8) & q62 %in% c(2, 4, 6, 7, 8) ~ 1,
      q59 %in% c(3, 4, 5, 6, 7, 8) & q62 %in% c(3, 5) ~ 2,
      TRUE ~ NA_integer_
    ),
    #2013 on: 5=IUD or implant, 6=shot/patch/birth control ring
    year >= 2013 ~ case_when(
      q59 %in% c(3, 4, 5, 6, 7, 8) & q62 %in% c(2, 4, 7, 8) ~ 1,
      q59 %in% c(3, 4, 5, 6, 7, 8) & q62 %in% c(3, 5, 6) ~ 2,
      TRUE ~ NA_integer_
    ),
    TRUE ~ NA_integer_ 
  )) %>%
  select(-c(q19, q59, q61, q62)) %>%
  filter(year==2023) %>%
  mutate(state = recode(state, "New York (NYA)" = "New York"))

# EXCLUDE STATES IF MISSING 5+ YRS OF DATA
#combine
all_states<- rbind(all_statesthrough_2021, all_states2023)

#count # of observations per year and state
num_obs_yr_state <- all_states %>%
  arrange(year) %>%
  group_by(state, year) %>%
  summarize(count_qn60 = sum(!is.na(qn60)), .groups = 'drop') 

#pivot so each row is a state, each column is a year
num_yrs_data<- num_obs_yr_state %>%
  dplyr::select(state, year, count_qn60) %>%
  filter(year>=2007) %>%
  pivot_wider(
    names_from= year, 
    values_from = count_qn60,
    names_prefix="year_"
  ) %>%
  #count # of observations that aren't NA and aren't 0
  mutate(
    non_na_non_zero_count = rowSums(!is.na(dplyr::select(., starts_with("year_"))) & dplyr::select(., starts_with("year_")) != 0)
  ) 

states_less_than_5_yrs_data<- num_yrs_data %>%
  # Filter states with non_na_non_zero_count < 5 (ie, states that should be excluded)
  filter(non_na_non_zero_count < 5) %>%
  # Select only the state names
  dplyr::select(state) %>%
  # Remove duplicate state names if needed
  distinct()

#to filter these out: 
all_states2<- all_states %>%
  #remove states where state in all_states matches state in states_less_than_5_yrs_data
  anti_join(states_less_than_5_yrs_data, by = "state")

#to combine data with a count of # of years not missing data:
all_states3<- merge(all_states2, num_yrs_data %>% dplyr::select(state, non_na_non_zero_count), by="state") %>%
  filter(year>=2007) %>%
  arrange(year)

#add region
county_census1<- county_census %>%
  dplyr::select(STNAME, REGION) %>%
  rename(state=STNAME,
         region=REGION) %>%
  distinct(state, region) %>%
  mutate(region = case_when (
    region==1 ~ "Northeast",
    region==2 ~ "Midwest",
    region==3 ~ "South",
    region==4 ~ "West",
    TRUE ~ "Other"
  )) %>%
  filter(region!="Other")

all_states4<- merge(all_states3, county_census1, by="state", all.x=TRUE) %>%
  filter(state!="Idaho")

###### GET POINT ESTIMATES for sexually active, no protection, no condom, 3yr groupings (Figure)
state_PE_CI_YRBS <- function(data, var, newvar_name) {
  percent_var <- paste0("percent_", newvar_name)
  percent_lb_var <- paste0("percent_", newvar_name, "_lb")
  percent_ub_var <- paste0("percent_", newvar_name, "_ub")
  #check if all obs are NA
  if (all(is.na(data[[var]]))) {
    return(setNames(data.frame(NA, NA, NA), c(percent_var, percent_lb_var, percent_ub_var)))
  }
  else{
    tryCatch({
      yrbsdes <- svydesign(id = ~PSU, weight = ~weight, strata = ~stratum, data = data, nest = TRUE)
      yrbsdes <- update(yrbsdes, temp_var = data[[var]])
      qn_result <- svyciprop(~I(temp_var == 1), yrbsdes, na.rm = TRUE)
      #pe, lb, ub
      percent_value <- qn_result[1]
      percent_lb_value <- attr(qn_result, "ci")[1]
      percent_ub_value <- attr(qn_result, "ci")[2]
      return(setNames(data.frame(percent_value, percent_lb_value, percent_ub_value), 
                      c(percent_var, percent_lb_var, percent_ub_var)))
    }, error = function(e) {
      return(setNames(data.frame(NA, NA, NA), c(percent_var, percent_lb_var, percent_ub_var)))
    })
  }
}

HS_sexual_activity <- all_states4 %>%
  group_by(state, year) %>%
  group_modify(~ state_PE_CI_YRBS(data=.,var="qn60", newvar_name ="sexually_active")) 

HS_no_condom <- all_states4 %>%
  group_by(state, year) %>%
  group_modify(~ state_PE_CI_YRBS(data=.,var="no_condom", newvar_name ="no_condom")) 

HS_no_protection <- all_states4 %>%
  group_by(state, year) %>%
  group_modify(~ state_PE_CI_YRBS(data=.,var="no_protection", newvar_name ="no_protection")) 

HS_no_larc <- all_states4 %>%
  filter(year>=2013) %>%
  group_by(state, year) %>%
  group_modify(~ state_PE_CI_YRBS(data=.,var="no_larc", newvar_name ="no_larc")) 

HS_forced_intercourse <- all_states4 %>%
  group_by(state, year) %>%
  group_modify(~ state_PE_CI_YRBS(data=.,var="forced_intercourse", newvar_name ="forced_intercourse")) 

HS_hm_effective <- all_states4 %>%
  group_by(state, year) %>%
  group_modify(~ state_PE_CI_YRBS(data=.,var="no_hm_effective", newvar_name ="no_hm_effective")) 


HS_data_YRBS <- reduce(list(HS_sexual_activity, 
                            HS_no_condom, 
                            HS_no_protection,
                            HS_no_larc,
                            HS_forced_intercourse,
                            HS_hm_effective), 
                       function(x, y) merge(x, y, by=c("state", "year"), all=TRUE))

row_state_year_fcn<- function(data) {
  all_states_yrs_template <- expand.grid(state = unique(data$state),
                                         year = seq(2007, 2023, by=2))
  data2 <- merge(all_states_yrs_template, data, by = c("state", "year"), all.x = TRUE)
  return(data2)
}

HS_data_YRBS_1<-row_state_year_fcn(HS_data_YRBS) 
HS_data_YRBS_2<- merge(HS_data_YRBS_1, county_census1, by="state", all.x=TRUE) 

# USE RF IF PE IS MISSING
years_2007on<- c(2007,2009,2011,2013,2015,2017,2019,2021,2023)

rf_function<- function(data, variable) {
  #dataset
  data_filtered<- data %>%
    filter(!is.na(!!sym(variable))) %>%
    dplyr::select(state, !!sym(variable), year)
  
  #run RF
  control <- trainControl(method = 'repeatedcv', number = 5, repeats = 1)
  #Set the seed for reproducibility
  set.seed(123)
  # Define the tuning grid
  tunegrid <- expand.grid(.mtry = c(1:5))
  # Run the model
  mods <- data.frame()
  for (i in 1:10) {
    # Train the random forest model
    rf <- train(as.formula(paste(variable, "~ .")),
                data = data_filtered,
                method = 'rf',
                maxdepth = i,
                tuneGrid = tunegrid,
                trControl = control)
    # Store the results
    mods <- bind_rows(mods, rf$results %>% mutate(id = i))
  }
  # find model with best CV RMSE
  best = mods[which.min(mods$RMSE),]
  # run best model
  rf_best <- train(as.formula(paste(variable, "~ .")),
                   data=data_filtered,
                   method='rf',
                   maxdepth = best$id,
                   tuneGrid=expand.grid(.mtry=c(best$mtry)),
                   trControl=control)
  
  #create df for predictions
  states_2007on <- unique(data_filtered$state)
  combinations_2007on <- expand.grid(state = states_2007on, year = years_2007on)
  #predict
  state_predictions_2007on <- predict(rf_best, newdata = combinations_2007on)
  combinations_2007on[[paste0(variable, "_rf")]] <- state_predictions_2007on
  #return
  return(combinations_2007on)
}

HS_data_YRBS_sexually_active <- rf_function(HS_data_YRBS_2, "percent_sexually_active")
HS_data_YRBS_no_condom<- rf_function(HS_data_YRBS_2, "percent_no_condom")
HS_data_YRBS_no_protection<- rf_function(HS_data_YRBS_2, "percent_no_protection")
HS_data_YRBS_no_larc<- rf_function(HS_data_YRBS_2, "percent_no_larc") %>%
  mutate(percent_no_larc_rf = ifelse(year<2013, NA, percent_no_larc_rf))
HS_data_YRBS_forced_intercourse<- rf_function(HS_data_YRBS_2, "percent_forced_intercourse")
HS_data_YRBS_no_hm_effective<- rf_function(HS_data_YRBS_2, "percent_no_hm_effective")

HS_data_YRBS_3 <- reduce(list(HS_data_YRBS_2,HS_data_YRBS_sexually_active, HS_data_YRBS_no_condom, HS_data_YRBS_no_protection,
                              HS_data_YRBS_no_larc,HS_data_YRBS_forced_intercourse,HS_data_YRBS_no_hm_effective),
                         function(x, y) merge(x, y, by=c("state", "year"), all.x=TRUE)) %>%
                  mutate(yr_lead=year+1)


# if true value isn't given, use RF value
HS_data_YRBS_4<- HS_data_YRBS_3  %>%
  mutate(mix_sexually_active = ifelse(is.na(percent_sexually_active), percent_sexually_active_rf, percent_sexually_active),
         mix_no_condom = ifelse(is.na(percent_no_condom), percent_no_condom_rf, percent_no_condom),
         mix_no_protection = ifelse(is.na(percent_no_protection), percent_no_protection_rf, percent_no_protection),
         mix_no_larc = ifelse(is.na(percent_no_larc), percent_no_larc_rf, percent_no_larc),
         mix_forced_intercourse = ifelse(is.na(percent_forced_intercourse), percent_forced_intercourse_rf, percent_forced_intercourse),
         mix_no_hm_effective = ifelse(is.na(percent_no_hm_effective), percent_no_hm_effective_rf, percent_no_hm_effective)) %>%
  dplyr::select(state, year, yr_lead, mix_sexually_active, mix_no_condom, mix_no_protection, mix_no_larc, mix_forced_intercourse,
                mix_no_hm_effective, region)

# merge w/births and pop (births_data, from 01_births_pop)
births_pop<- births_data %>%
  dplyr::select(yr, state, pop15_19female) %>%
  rename(yr_pop=yr)

HS_data_YRBS_5<- merge(HS_data_YRBS_4, births_pop, by.x=c("state", "yr_lead"), by.y=c("state", "yr_pop"), all.x=TRUE)

### WEIGHTED BY REGION, 3YR GROUPS (LINE 649-716, 748-792)
region_yrgroups<- HS_data_YRBS_5 %>%
  mutate(yr_group = case_when(
    year %in% c(2007, 2009, 2011) ~ "group2007_2011",
    year %in% c(2013, 2015, 2017) ~ "group2013_2017",
    year %in% c(2019, 2021, 2023) ~ "group2019_2023"
  )) %>%
  group_by(yr_group, region) %>%
  summarize(
    weighted_sexually_active = (sum(mix_sexually_active * pop15_19female) / sum(pop15_19female)) * 100,
    weighted_no_condom = (sum(mix_no_condom * pop15_19female) / sum(pop15_19female)) * 100,
    weighted_forced_intercourse = (sum(mix_forced_intercourse * pop15_19female) / sum(pop15_19female)) * 100,
    weighted_no_protection = (sum(mix_no_protection * pop15_19female) / sum(pop15_19female)) * 100,
    weighted_no_hm_effective = (sum(mix_no_hm_effective *pop15_19female)/ sum(pop15_19female)) *100)


### BY YR
HS_data_YRBS_5_region_year<- HS_data_YRBS_5 %>%
  group_by(year, region) %>%
  summarize(weighted_no_condom = (sum(mix_no_condom * pop15_19female) / sum(pop15_19female)) * 100,
            weighted_forced_intercourse = (sum(mix_forced_intercourse * pop15_19female) / sum(pop15_19female)) * 100,
            weighted_sexually_active = (sum(mix_sexually_active * pop15_19female) / sum(pop15_19female)) * 100,
            weighted_no_protection = (sum(mix_no_protection * pop15_19female) / sum(pop15_19female)) * 100,
            weighted_no_hm_effective = (sum(mix_no_hm_effective *pop15_19female) / sum(pop15_19female)) *100) %>%
  dplyr::select(year, region, weighted_no_condom, weighted_forced_intercourse, weighted_sexually_active, weighted_no_protection,
                weighted_no_hm_effective)

HS_data_YRBS_5_region_year_noCI_larc<- HS_data_YRBS_5 %>%
  filter(year>=2013) %>%
  group_by(year, region) %>%
  summarize(weighted_no_larc = (sum(mix_no_larc * pop15_19female) / sum(pop15_19female)) * 100) %>%
  dplyr::select(year, region, weighted_no_larc)


region_byyr_noCI<- merge(HS_data_YRBS_5_region_year, HS_data_YRBS_5_region_year_noCI_larc, 
                                               by=c("year", "region"), all=TRUE)


####### FIG (STATE MAP) DATA
fig4_data<- HS_data_YRBS_5 %>%
  mutate(yr_group = case_when(
    year %in% c(2007, 2009, 2011) ~ "group2007_2011",
    year %in% c(2013, 2015, 2017) ~ "group2013_2017",
    year %in% c(2019, 2021, 2023) ~ "group2019_2023"
  )) %>%
  #group by year group, region
  group_by(yr_group, state) %>%
  #get weighted estimates
  summarize(
    percent_sexually_active = (sum(mix_sexually_active * pop15_19female) / sum(pop15_19female)) *100,
    percent_no_protection = (sum(mix_no_protection * pop15_19female) / sum(pop15_19female)) *100,
    percent_no_condom = (sum(mix_no_condom * pop15_19female) / sum(pop15_19female)) *100)


##########################################
# YRBS state data, 2007-2023 - bootstrapping and CIs

## This file takes in the state data from 03_YRBS_statedata
## RF imputation with states with 5+yrs of data from 2007-2023 to get region data: by year and by 3-yr groups

## Variables:
###  1) sexually_active: % Sexually active in the past 3 mo
###  2) no_condom: % sexually active who did not use a condom the last time they had sexual intercourse 
###  3) no_prevention: % sexually active who used no method to prevent pregnancy the last time they had sexual intercourse
###  4) no_LARC: % sexually active who did not report using LARC as 1 method of pregnancy prevention in last sexual intercourse
###  5) forced: % sexually active who have been forced to have sexual intercourse when they did not want to
###  6) highly_effective: % sexually active who used highly effective (birth control pills or LARC) 

## Strategy:
###  500 bootstrap replicates
###  for each replicate: find point estimate for each state/yr combo, RF PE, pe_mix (use RF if missing PE), avg by region/year combo
###  then take 2.5th and 97.5th percentile of point estimates to find UB and LB
##########################################

options(survey.lonely.psu = "adjust")
here::i_am("scripts/04_YRBS_state_rf_ci.R")
source(here::here("scripts/03_YRBS_statedata.R"))

pop_data<- pop_data %>% 
  rename(pop15_19female=population, year=yr)

#county census (region/state)
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

######### SURVEY DESIGN/FCNS
yrbs_svy<- svydesign(id = ~PSU, 
                     weight = ~weight, 
                     strata = ~stratum, 
                     data = all_states4, 
                     nest = TRUE)

rf_function<- function(data) {
  #dataset
  data_filtered<- data %>%
    filter(!is.na(pe)) 
  #run RF
  control <- trainControl(method = 'repeatedcv', number = 5, repeats = 1)
  # Define the tuning grid
  tunegrid <- expand.grid(.mtry = c(1:5))
  # Run the model
  mods <- data.frame()
  for (i in 1:10) {
    # Train the random forest model
    rf <- train(as.formula(paste("pe", "~ .")),
                data = data_filtered,
                method = 'rf',
                maxdepth = i,
                tuneGrid = tunegrid,
                trControl = control)
    # Store the results
    mods <- bind_rows(mods, rf$results %>% mutate(id = i))}
  # find model with best CV RMSE
  best = mods[which.min(mods$RMSE),]
  # run best model
  rf_best <- train(as.formula(paste("pe", "~ .")),
                   data=data_filtered,
                   method='rf',
                   maxdepth = best$id,
                   tuneGrid=expand.grid(.mtry=c(best$mtry)),
                   trControl=control)
  #create df for predictions
  states_2007on <- unique(data_filtered$state)
  years_2007on<- unique(data_filtered$year)
  combinations_2007on <- expand.grid(state = states_2007on, year = years_2007on)
  #predict
  state_predictions_2007on <- predict(rf_best, newdata = combinations_2007on)
  combinations_2007on[[paste0("pe", "_rf")]] <- state_predictions_2007on
  #return
  return(combinations_2007on)
}

process_weight <- function(weight_col, pe_var, data) {
  df_grouped <- data %>%
    group_by(state, year) %>%
    group_modify(~ {
      #Check if all observations are NA or if any stratum has only one PSU
      if(all(is.na(.x[[pe_var]]))) {
        return(pe_df<- data.frame(pe_svy=NA))
      } 
      else {
        tryCatch({
          # Survey design for each group
          survey_design <- svydesign(
            id = ~PSU, 
            weight = as.formula(paste0("~", weight_col)), 
            strata = ~stratum, 
            data = .x, 
            nest = TRUE
          )
          # Calculate point estimate
          prop_formula <- as.formula(paste0("~I(", pe_var, " == 1)"))
          # proportion
          pe_svy <- svyciprop(prop_formula, survey_design, na.rm = TRUE)
          # Extract the proportion estimate (first element)
          pe_df <- data.frame(pe_svy = as.numeric(pe_svy[1]))
          return(pe_df)}, error=function(e) {
            return(pe_df<- data.frame(pe_svy=NA))
          })
      }}) %>%
    rename(pe = pe_svy)
  # Call rf code
  df_rf <- rf_function(df_grouped)
  # Combine original dataset and pe_rf column in df_rf
  df2 <- merge(df_grouped, df_rf, by = c("state", "year"), all.y = TRUE) %>% 
    mutate(pe_mix = ifelse(is.na(pe), pe_rf, pe))
  # Average results for each region/year group, weighting by pop
  df3 <- merge(df2, pop_data, by = c("state", "year"), all.x = TRUE)
  df3_region_yrgroups <- merge(df3, county_census1, by = c("state"), all.x = TRUE) %>%
    mutate(yr_groups3 = case_when(
      year %in% c(2007, 2009, 2011) ~ "group2007_2011",
      year %in% c(2013, 2015, 2017) ~ "group2013_2017",
      year %in% c(2019, 2021, 2023) ~ "group2019_2023"
    )) %>%
    group_by(region, yr_groups3) %>%
    summarize(weighted_avg_pe_mix = sum(pe_mix * pop15_19female) / sum(pop15_19female), .groups = 'drop') %>%
    mutate(type = "yr_group") %>%
    rename(group = yr_groups3)
  #by yr
  df3_region_byyr <- merge(df3, county_census1, by = c("state"), all.x = TRUE) %>%
    group_by(region, year) %>%
    summarize(weighted_avg_pe_mix = sum(pe_mix * pop15_19female) / sum(pop15_19female), .groups = 'drop') %>%
    mutate(type = "by_yr") %>%
    rename(group = year)
  return(list(df3_region_yrgroups, df3_region_byyr))
}


# BOOTSTRAP FCN: run bootstrap design, create df with weights, original data.
set.seed(123)
bootstrap_rep_design<-
  as_bootstrap_design(design = yrbs_svy,
                      type = "Rao-Wu-Yue-Beaumont",
                      replicates= 500, 
                      compress=TRUE ) 

df_bs<-as_data_frame_with_weights(design = bootstrap_rep_design,
                                  full_wgt_name= "FULL_SAMPLE_WGT",
                                  rep_wgt_prefix = "REP_WGT") 

#from 2013 on (for no_larc)
yrbs_svy_2013on<- svydesign(id = ~PSU, 
                            weight = ~weight, 
                            strata = ~stratum, 
                            data = all_states4 %>% filter(year>=2013), 
                            nest = TRUE)

bootstrap_rep_design_2013on<-
  as_bootstrap_design(design = yrbs_svy_2013on,
                      type = "Rao-Wu-Yue-Beaumont",
                      replicates= 500, 
                      compress=TRUE) 

df_bs_2013on<-as_data_frame_with_weights(design = bootstrap_rep_design_2013on,
                                         full_wgt_name= "FULL_SAMPLE_WGT",
                                         rep_wgt_prefix = "REP_WGT") 

ci_fcn<- function(data, var) {
  #orig data, to get PE
  data_orig<- data %>%
    filter(type==var) %>%
    select(region, group, weighted_avg_pe_mix)
  #CIs
  data2<- data %>%
    filter(type== var) %>%
    group_by(region, group) %>%
    summarize(
      p.5 = quantile(weighted_avg_pe_mix, 0.005, na.rm=TRUE),
      p99.5 = quantile(weighted_avg_pe_mix, .995, na.rm=TRUE),
      p50 =  quantile(weighted_avg_pe_mix, .5, na.rm=TRUE),
      avg = mean(weighted_avg_pe_mix, na.rm=TRUE),
      p2.5 = quantile(weighted_avg_pe_mix, 0.025, na.rm = TRUE),
      p97.5 = quantile(weighted_avg_pe_mix, 0.975, na.rm = TRUE)
    ) 
  data3<- merge(data2, data_orig, by=c("region", "group"))
}

bootstrap_fcn<- function(pe_var, replicates, df_bs) {
  results_list <- foreach(i = 1:replicates, .combine = 'rbind', .packages = c('dplyr', 'survey')) %dopar% {
    weight_col <- paste0("REP_WGT", i)
    process_weight(weight_col, pe_var, df_bs)}
  final_result <- rbindlist(results_list)
  #call CI
  ci_byyr<- ci_fcn(final_result,"by_yr")
  ci_yrgroups<- ci_fcn(final_result, "yr_group")
  #return
  return(list(ci_byyr, ci_yrgroups))
}

##### RUN FOR EACH OUTCOME. 
qn60_ci<- bootstrap_fcn("qn60", 500, df_bs)
no_condom_ci<- bootstrap_fcn("no_condom", 500, df_bs)
no_larc_ci<- bootstrap_fcn("no_larc", 500, df_bs_2013on) 
forced_intercourse_ci<- bootstrap_fcn("forced_intercourse",500, df_bs)
no_protection_ci<- bootstrap_fcn("no_protection", 500, df_bs) 
no_hm_effective_ci<- bootstrap_fcn("no_hm_effective", 500, df_bs) 


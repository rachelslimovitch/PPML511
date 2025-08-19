##########################################
# TABLES AND FIGURES

# Read in data (national YRBS data, state YRBS data, pop/births)

# Table 1: 3 yr groups, each outcome variable, overall, by age, by region, by race/ethnicity
##########################################

here::i_am("scripts/05_tables_figures.R")
source(here::here("scripts/04_YRBS_state_rf_ci.R"))

# rename: by year[1], by yrgroup[2], from 04_YRBS_state_rf_ci.R (will need to encapsulate in as.data.frame if saving as RData file in prior script)
sexually_active_byyr<- qn60_ci[1]
sexually_active_yrgroups<- qn60_ci[2]

no_condom_state_byyr<- no_condom_ci[1]
no_condom_state_yrgroups<- no_condom_ci[2]

no_larc_ci_byyr<- no_larc_ci[1]
no_larc_ci_byyrgroups<- no_larc_ci[2]

no_hm_effective_byyr<- no_hm_effective_ci[1]
no_hm_effective_byyrgroups<- no_hm_effective_ci[2]

no_protection_byyr<- no_protection_ci[1]
no_protection_byyrgroups<- no_protection_ci[2]

forced_intercourse_byyr<- forced_intercourse_ci[1]
forced_intercourse_byyrgroups<- forced_intercourse_ci[2]

# fcn to redo CI groups
rename_ci <- function(data, var_name) {
  lb_name <- paste0(var_name, "_lb")
  ub_name <- paste0(var_name, "_ub")
  avg_name <- paste0(var_name, "avg")
  # Perform renaming and scaling
  data1<- data %>%
    rename(!!lb_name := p2.5, !!ub_name := p97.5, yr_groups3 = group, !!avg_name := avg) %>%
    mutate(!!lb_name := !!sym(lb_name) * 100, !!ub_name := !!sym(ub_name) * 100, !!avg_name := !!sym(avg_name)) %>%
    select(region, yr_groups3, !!sym(lb_name), !!sym(ub_name)) %>%
    distinct(region, yr_groups3, .keep_all = TRUE)
  return(data1)
}

region_ci_sexually_active <- rename_ci(sexually_active_yrgroups, "sexually_active")
region_ci_no_condom<- rename_ci(no_condom_state_yrgroups, "no_condom")
region_ci_noprotection<-rename_ci(no_protection_byyrgroups, "no_protection")
region_ci_no_hm_effective<- rename_ci(no_hm_effective_byyrgroups, "no_hm_effective")
region_ci_forced_intercourse<- rename_ci(forced_intercourse_byyrgroups, "forced_intercourse")

### POP/BIRTHS DATA
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

births_region<- merge(births_data, county_census1, by="state", all.x=TRUE) %>%
  filter(as.integer(yr) %% 2 ==0) %>%
  group_by(region, yr) %>%
  summarise(
    total_births = sum(births),  # Sum of births in the region
    total_pop = sum(pop15_19female),  # Sum of population in the region
    births_per_1000 = (total_births / (total_pop/1000))   # Calculate births per 1000
  ) %>%
  ungroup() 

births_region_yrgroups<- births_region %>%
  filter(yr>=2008, yr<=2024) %>%
  filter(yr %% 2==0) %>%
  mutate(yr_groups3 = case_when(
    yr %in% c(2008, 2010, 2012) ~ "group2007_2011",
    yr %in% c(2014, 2016, 2018) ~ "group2013_2017",
    yr %in% c(2020, 2022, 2024) ~ "group2019_2023"
  )) %>%
  group_by(yr_groups3, region) %>%
  summarize(weighted_avg_births = sum(births_per_1000 * total_pop) / sum(total_pop), .groups = 'drop') %>%
  mutate(weighted_avg_births=round(weighted_avg_births,1))

births_pop_overall<- births_data %>%
  group_by(yr) %>%
  summarize(births_us=sum(births), pop15_19female_us=sum(pop15_19female)) %>%
  mutate(births_per_1000 = births_us/(pop15_19female_us/1000))


###### COMBINE: 1 DF FOR REGION DATA, WITH PE AND CI'S FROM BOOTSTRAPPING
region_yrgroups2<- region_yrgroups %>% rename(yr_groups3=yr_group)

#merge PE from regionyrgroups w/CI from bootstrapping
region_yrgroups3<- merge(merge(merge(merge(merge(region_yrgroups2, region_ci_sexually_active, by=c("region", "yr_groups3")),
                                           region_ci_no_condom, by = c("region", "yr_groups3")),
                                     region_ci_noprotection, by=c("region", "yr_groups3")),
                               region_ci_no_hm_effective, by=c("region", "yr_groups3")),
                         region_ci_forced_intercourse, by = c("region", "yr_groups3"))

#add national data (overall, by age, by race/ethnicity)
national_yrgroups <- national_dataset_final %>%
  mutate(across(
    c("percent_no_protection", "percent_no_protection_lb", "percent_no_protection_ub",
      "percent_sexually_active", "percent_sexually_active_lb", "percent_sexually_active_ub",
      "percent_no_condom", "percent_no_condom_lb", "percent_no_condom_ub",
      "percent_sa_forced_intercourse", "percent_sa_forced_intercourse_lb", "percent_sa_forced_intercourse_ub",
      "percent_no_hm_effective", "percent_no_hm_effective_lb", "percent_no_hm_effective_ub"),
    ~ round(. * 100,1)
  ))

national_byyr<- national_dataset_final %>%
  mutate(across(c("percent_no_protection", "percent_no_protection_lb", "percent_no_protection_ub",
                  "percent_sexually_active", "percent_sexually_active_lb", "percent_sexually_active_ub",
                  "percent_no_condom", "percent_no_condom_lb", "percent_no_condom_ub",
                  "percent_sa_forced_intercourse", "percent_sa_forced_intercourse_lb", "percent_sa_forced_intercourse_ub",
                  "percent_no_hm_effective", "percent_no_hm_effective_lb", "percent_no_hm_effective_ub"),
                ~ round(. * 100,1)))

#############################################################
################### TABLE 1 - 3 YR GROUPS ###################
#############################################################
region_yrgroups3_formatted <- region_yrgroups3 %>%
  mutate(
    weighted_sexually_active = paste0(round(weighted_sexually_active, 1), 
                                      " (", round(sexually_active_lb, 1), 
                                      ", ", round(sexually_active_ub, 1), ")"),
    weighted_no_condom = paste0(round(weighted_no_condom, 1), 
                                " (", round(no_condom_lb, 1), 
                                ", ", round(no_condom_ub, 1), ")"),
    weighted_no_protection = paste0(round(weighted_no_protection, 1), 
                                    " (", round(no_protection_lb, 1), 
                                    ", ", round(no_protection_ub, 1), ")"),
    weighted_no_hm_effective = paste0(round(weighted_no_hm_effective, 1), 
                                      " (", round(no_hm_effective_lb, 1), 
                                      ", ", round(no_hm_effective_ub, 1), ")"),
    weighted_forced_intercourse = paste0(round(weighted_forced_intercourse, 1), 
                                         " (", round(forced_intercourse_lb,1), 
                                         ", ", round(forced_intercourse_ub,1), ")")) %>%
  select(region, yr_groups3, 
         weighted_sexually_active, 
         weighted_no_condom, 
         weighted_no_protection, 
         weighted_no_hm_effective, 
         weighted_forced_intercourse)
#format so one row per region
region_yrgroups3_wide <- region_yrgroups3_formatted %>%
  mutate(region = factor(region, levels = c("Northeast", "West", "Midwest", "South"))) %>%  # Reorder 'region'
  arrange(region) %>%
  pivot_wider(
    id_cols = region,  # Keep 'region' as the identifier
    names_from = yr_groups3,  # Columns will be created for each 'yr_groups3'
    values_from = c(weighted_sexually_active, 
                    weighted_no_condom, 
                    weighted_no_protection, 
                    weighted_no_hm_effective, 
                    weighted_forced_intercourse) 
  )

#### NATIONAL
region_yrgroups3_formatted <- region_yrgroups3 %>%
  mutate(
    weighted_sexually_active = paste0(round(weighted_sexually_active, 1), 
                                      " (", round(sexually_active_lb, 1), 
                                      ", ", round(sexually_active_ub, 1), ")"),
    weighted_no_condom = paste0(round(weighted_no_condom, 1), 
                                " (", round(no_condom_lb, 1), 
                                ", ", round(no_condom_ub, 1), ")"),
    weighted_no_protection = paste0(round(weighted_no_protection, 1), 
                                    " (", round(no_protection_lb, 1), 
                                    ", ", round(no_protection_ub, 1), ")"),
    weighted_no_hm_effective = paste0(round(weighted_no_hm_effective, 1), 
                                      " (", round(no_hm_effective_lb, 1), 
                                      ", ", round(no_hm_effective_ub, 1), ")"),
    weighted_forced_intercourse = paste0(round(weighted_forced_intercourse, 1), 
                                         " (", round(forced_intercourse_lb,1), 
                                         ", ", round(forced_intercourse_ub,1), ")")) %>%
  select(region, yr_groups3, 
         weighted_sexually_active, 
         weighted_no_condom, 
         weighted_no_protection, 
         weighted_no_hm_effective, 
         weighted_forced_intercourse)
#format so one row per region
region_yrgroups3_wide <- region_yrgroups3_formatted %>%
  mutate(region = factor(region, levels = c("Northeast", "West", "Midwest", "South"))) %>%  # Reorder 'region'
  arrange(region) %>%
  pivot_wider(
    id_cols = region,  # Keep 'region' as the identifier
    names_from = yr_groups3,  # Columns will be created for each 'yr_groups3'
    values_from = c(weighted_sexually_active, 
                    weighted_no_condom, 
                    weighted_no_protection, 
                    weighted_no_hm_effective, 
                    weighted_forced_intercourse)  )

## NATIONAL, YR GROUPS
national_yrgroups_t1_fcn_overall<- function(data) {
  # Create the new dataframe with the desired format
  formatted_data <- data %>%
    mutate(
      percent_sexually_active = paste0(round(percent_sexually_active,1), 
                                       " (", round(percent_sexually_active_lb,1), 
                                       ", ", round(percent_sexually_active_ub,1), ")"),
      percent_no_condom = paste0(round(percent_no_condom,1), 
                                 " (", round(percent_no_condom_lb,1), 
                                 ", ", round(percent_no_condom_ub,1), ")"),
      percent_no_protection = paste0(round(percent_no_protection,1), 
                                     " (", round(percent_no_protection_lb,1), 
                                     ", ", round(percent_no_protection_ub,1), ")"),
      percent_no_hm_effective = paste0(round(percent_no_hm_effective,1), 
                                       " (", round(percent_no_hm_effective_lb,1), 
                                       ", ", round(percent_no_hm_effective_ub,1), ")"),   
      percent_sa_forced_intercourse = paste0(round(percent_sa_forced_intercourse,1), 
                                             " (", round(percent_sa_forced_intercourse_lb,1), 
                                             ", ", round(percent_sa_forced_intercourse_ub,1), ")"))  %>%
    distinct(yr_groups3, .keep_all=TRUE) }
# Reshape the data to wide format
reshaped_data_function_overall <- function(data){
  data1<- data %>%
    select(percent_sexually_active, 
           percent_no_condom, 
           percent_no_protection, 
           percent_no_hm_effective,
           percent_sa_forced_intercourse, yr_groups3) %>%
    pivot_wider(
      #id_cols = stratify,  
      names_from = yr_groups3,  # Columns will be created for each 'yr_groups3'
      values_from = c(percent_sexually_active, 
                      percent_no_condom, 
                      percent_no_protection, 
                      percent_no_hm_effective,
                      percent_sa_forced_intercourse)
    )
}

# OVERALL
national_yrgroups_overall<- national_yrgroups %>%
  mutate(stratify = "overall") %>%
  national_yrgroups_t1_fcn_overall() %>%
  reshaped_data_function_overall()

# STRATIFIED BY RACE AND AGE
national_yrgroups_t1_fcn_race_age<- function(data) {
  formatted_data <- data %>%
    mutate(
      percent_sexually_active = paste0(round(percent_sexually_active,1), 
                                       " (", round(percent_sexually_active_lb,1), 
                                       ", ", round(percent_sexually_active_ub,1), ")"),
      percent_no_condom = paste0(round(percent_no_condom,1), 
                                 " (", round(percent_no_condom_lb,1), 
                                 ", ", round(percent_no_condom_ub,1), ")"),
      percent_no_protection = paste0(round(percent_no_protection,1), 
                                     " (", round(percent_no_protection_lb,1), 
                                     ", ", round(percent_no_protection_ub,1), ")"),
      percent_no_hm_effective = paste0(round(percent_no_hm_effective,1), 
                                       " (", round(percent_no_hm_effective_lb,1), 
                                       ", ", round(percent_no_hm_effective_ub,1), ")"),   
      percent_sa_forced_intercourse = paste0(round(percent_sa_forced_intercourse,1), 
                                             " (", round(percent_sa_forced_intercourse_lb,1), 
                                             ", ", round(percent_sa_forced_intercourse_ub,1), ")"))  
}
reshaped_data_function_race_age<- function(data){
  data1<- data %>%
    select(percent_sexually_active, 
           percent_no_condom, 
           percent_no_protection, 
           percent_no_hm_effective, 
           percent_sa_forced_intercourse, yr_groups3, stratify) %>% 
    pivot_wider(
      id_cols = stratify,  
      names_from = yr_groups3,  # Columns will be created for each 'yr_groups3'
      values_from = c(percent_sexually_active, 
                      percent_no_condom, 
                      percent_no_protection, 
                      percent_no_hm_effective, 
                      percent_sa_forced_intercourse)
    )
}

#by age
national_yrgroups_byage<- national_yrgroups %>%
  filter(stratify %in% c("14_16", "17up")) %>%
  national_yrgroups_t1_fcn_race_age() %>%
  reshaped_data_function_race_age()

#by race
national_yrgroups_byrace<- national_yrgroups %>%
  filter(stratify %in% c("white", "black", "hispanic")) %>%
  national_yrgroups_t1_fcn_race_age() %>%
  reshaped_data_function_race_age()

#############################################################
################### SUPP TABLE 1 -NO LARC ###################
#############################################################
region_yr_larc<- region_byyr_noCI %>%
  select(region, year, weighted_no_larc) %>%
  mutate(weighted_no_larc = round(weighted_no_larc, 2))

no_larc_ci_byyr<- as.data.frame(no_larc_ci[1]) %>%  #NOTE - MAY NOT NEED WRAPPED AS AS.DATA.FRAME IF NOT SAVED AS RDATA
  mutate(combined_95 = paste0(" (", round(p2.5*100, 1), ", ", round(p97.5*100, 1), ")")) %>%
  rename(year = group) %>%
  select(region, year, combined_95) %>%
  distinct(region, year, .keep_all = TRUE)

region_yr_larc2<- merge(region_yr_larc, no_larc_ci_byyr, by=c("region", "year")) %>%
  mutate(combined = paste0 (weighted_no_larc, combined_95))

national_yr_larc<- national_byyr %>%
  select(yr, percent_no_larc, percent_no_larc_lb, percent_no_larc_ub, stratify) %>%
  filter(yr==2023) %>% #was yr>=2013
  rename(year=yr) %>%
  mutate(combined = paste0(round(percent_no_larc,1), 
                           " (", round(percent_no_larc_lb, 1), 
                           ", ", round(percent_no_larc_ub, 1), ")"))


#############################################################
###### FIG 1: trends by region/nationally by yr  ###### 
#############################################################

national_byyr_fig1<- national_byyr %>%
  filter(stratify=="overall") %>%
  dplyr::select(yr, percent_sexually_active, percent_sexually_active_ub, percent_sexually_active_lb,
                percent_no_condom, percent_no_condom_lb, percent_no_condom_ub,
                percent_no_protection, percent_no_protection_lb, percent_no_protection_ub) %>% 
  mutate(region= "US")

new_labels <- c("percent_no_protection" = "Percent sexually active who used no method to prevent pregnancy the last time they had sexual intercourse",
                "percent_no_condom" = "Percent sexually active who did not use a condom the last time they had sexual intercourse",
                "percent_sexually_active" = "Percent sexually active")

# region- need to add on CIs
region_byyr_fig1<- region_byyr_noCI %>%
  select(region, year, weighted_sexually_active, weighted_no_condom, weighted_no_protection)

no_condom_state_byyr_fig1<- no_condom_state_byyr %>%
  distinct(region, group, .keep_all = TRUE) %>%
  rename(year=group, weighted_no_condom_ub=p97.5, weighted_no_condom_lb=p2.5)  %>%
  mutate(weighted_no_condom_lb= weighted_no_condom_lb*100,
         weighted_no_condom_ub= weighted_no_condom_ub*100) %>%
  select(region, year, weighted_no_condom_ub, weighted_no_condom_lb)

sexually_active_state_byyr_fig1<- sexually_active_byyr %>%
  distinct(region, group, .keep_all = TRUE) %>%
  rename(year=group, weighted_sexually_active_ub=p97.5, weighted_sexually_active_lb=p2.5)  %>%
  mutate(weighted_sexually_active_lb= weighted_sexually_active_lb*100,
         weighted_sexually_active_ub= weighted_sexually_active_ub*100) %>%
  select(region, year, weighted_sexually_active_ub, weighted_sexually_active_lb)

no_protection_state_byyr_fig1<- no_protection_byyr %>%
  dplyr::select(region, group, p2.5, p97.5) %>%
  distinct(region, group, .keep_all = TRUE) %>%
  rename(year=group, weighted_no_protection_ub=p97.5, weighted_no_protection_lb=p2.5)  %>%
  mutate(weighted_no_protection_lb= weighted_no_protection_lb*100,
         weighted_no_protection_ub= weighted_no_protection_ub*100) %>%
  select(region, year, weighted_no_protection_ub, weighted_no_protection_lb)

region_byyr_fig1a<- merge(merge(merge(region_byyr_fig1, no_condom_state_byyr_fig1, by=c("region", "year")), sexually_active_state_byyr_fig1, by=c("region", "year")),
                          no_protection_state_byyr_fig1, by=c("region", "year"))

# COMBINE REGION + NATIONAL
fig1_all_region<- region_byyr_fig1a %>%
  select(region, year, 
         #sexual active
         weighted_sexually_active, weighted_sexually_active_ub, weighted_sexually_active_lb,
         #condom
         weighted_no_condom, weighted_no_condom_ub, weighted_no_condom_lb,
         #protection
         weighted_no_protection, weighted_no_protection_ub, weighted_no_protection_lb) %>%
  rename(yr=year) %>%
  rename_with(~ gsub("weighted", "percent", .)) 

fig1_all<- rbind(fig1_all_region, national_byyr_fig1)
fig1_all$region <- factor(fig1_all$region, levels = c("US", "Northeast", "West", "Midwest", "South"))

#PLOT
fig1_plot<- 
  ggplot(fig1_all %>% gather(var, value, percent_no_protection, percent_sexually_active, 
                             percent_no_condom) %>% 
           mutate(var = factor(var, levels = c("percent_sexually_active", "percent_no_condom", "percent_no_protection"))),aes(x=yr, y=value, group=var, col=var)) +
  theme_classic() +
  geom_line(linewidth=.5) +
  geom_point() +
  facet_grid(~region) +
  labs(col="",x="Year", y="") +
  scale_color_manual(
    values = c("percent_sexually_active" = "darkgreen",
               "percent_no_condom" = "red",
               "percent_no_protection" = "blue"),
    labels = new_labels) +
  geom_ribbon(fill="grey", aes(ymin = percent_no_condom_lb, ymax =percent_no_condom_ub),alpha = 0.1, colour = "grey" ) +
  geom_ribbon(fill="grey", aes(ymin = percent_sexually_active_lb, ymax =percent_sexually_active_ub),alpha = 0.1, colour = "gray") +
  geom_ribbon(fill="grey", aes(ymin = percent_no_protection_lb, ymax =percent_no_protection_ub), alpha = 0.1, colour="grey") +
  labs(col=" ", x="Year", y="", title="Figure 1. Trends in sexual activity and behaviors among high school females aged 14 years and older, stratified by region.") +
  scale_x_continuous(breaks = seq(2007, 2023, by = 2)) +
  scale_y_continuous(limits=c(0,63), breaks = seq(0,63, by=10)) +
  theme(
    axis.title = element_text(size = 16),       
    axis.text = element_text(size = 12),     
    legend.text = element_text(size = 16),   
    legend.title = element_text(size = 16),
    axis.text.x=element_text(size=12, angle=90, vjust=0.5),
    legend.position="bottom",
    strip.text = element_text(size = 14),
    plot.title = element_text(size = 20)) +
  guides(col = guide_legend(nrow = 3)) 

ggsave(plot=fig1_plot, "Fig1.jpeg", width=20, height=7 )


#######################################################################
### FIG 2: trends by region/nationally by yr, among all female HS  ####
#######################################################################

fig2_facet_data<- fig1_all %>%
  mutate(no_condom_mult = (percent_no_condom* percent_sexually_active)/100,
         no_condom_mult_lb = (percent_no_condom_lb * percent_sexually_active_lb)/100,
         no_condom_mult_ub = (percent_no_condom_ub * percent_sexually_active_ub)/100,
         no_protection_mult = (percent_no_protection * percent_sexually_active)/100,
         no_protection_mult_lb = (percent_no_protection_lb * percent_sexually_active_lb)/100,
         no_protection_mult_ub = (percent_no_protection_ub * percent_sexually_active_ub)/100) 

new_labels_f2 <- c("no_protection_mult" = "Percent who used no method to prevent pregnancy the last time they had sexual intercourse",
                   "no_condom_mult" = "Percent who did not use a condom the last time they had sexual intercourse",
                   "percent_sexually_active" = "Percent sexually active")
#plot
fig2_plot_facet<- 
  ggplot(fig2_facet_data %>% gather(var, value, no_condom_mult, no_protection_mult, 
                                    percent_sexually_active) %>% 
           mutate(var = factor(var, levels = c("percent_sexually_active", "no_condom_mult", "no_protection_mult"))),aes(x=yr, y=value, group=var, col=var)) +
  theme_classic() +
  geom_line(linewidth=.5) +
  geom_point() +
  facet_grid(~region) +
  labs(col="", x="Year", y="") +
  scale_color_manual(
    values = c("percent_sexually_active" = "darkgreen",
               "no_condom_mult" = "red",
               "no_protection_mult" = "blue"),
    labels = new_labels_f2) +
  geom_ribbon(fill="grey", aes(ymin = no_condom_mult_lb, ymax =no_condom_mult_ub),alpha = 0.1, colour = "grey" ) +
  geom_ribbon(fill="grey", aes(ymin = percent_sexually_active_lb, ymax =percent_sexually_active_ub),alpha = 0.1, colour = "gray") +
  geom_ribbon(fill="grey", aes(ymin = no_protection_mult_lb, ymax =no_protection_mult_ub), alpha = 0.1, colour="grey") +
  labs(col=" ", x="Year", y="", title="Figure 2. Trends in sexual activity and overall sexual behaviors among high school females aged 14 years and older, stratified by region. ") +
  scale_x_continuous(breaks = seq(2005, 2023, by = 2)) +
  scale_y_continuous(limits=c(0,40), breaks = seq(0,40, by=10)) +
  theme(
    axis.title = element_text(size = 16),       
    axis.text = element_text(size = 12),     
    legend.text = element_text(size = 16),   
    legend.title = element_text(size = 16),
    axis.text.x=element_text(size=12, angle=90, vjust=0.5),
    legend.position="bottom",
    strip.text = element_text(size = 14),
    plot.title = element_text(size = 20)) +
  guides(col = guide_legend(nrow = 3)) 

ggsave(plot=fig2_plot_facet, "Fig2.jpeg", width=20, height=7 )


#######################################################################
########### FIG 3: birth rates, by diff populations  ############
#######################################################################
fig3_facet_data<- fig2_facet_data %>%
  mutate(lead_yr = yr + 1) 

births_combined<- rbind(births_pop_overall %>% mutate(region="US") %>% select(region, yr, births_per_1000),
                        births_region %>% select(region,yr, births_per_1000))

fig3_facet_data_v2<- merge(fig3_facet_data, births_combined, by.x=c("lead_yr", "region"), by.y=c("yr", "region")) %>%
  mutate(birthsper1000_dividedbypercentsexual = births_per_1000 / (percent_sexually_active/100),
         birthsper1000_dividedbypercentsexual_ub = births_per_1000 / (percent_sexually_active_lb/100),
         birthsper1000_dividedbypercentsexual_lb = births_per_1000 / (percent_sexually_active_ub/100),
         birthsper1000_dividedbypercentsexual_noprotection = births_per_1000 / (percent_no_protection/100),
         birthsper1000_dividedbypercentsexual_noprotection_ub = births_per_1000 / (percent_no_protection_lb/100),
         birthsper1000_dividedbypercentsexual_noprotection_lb = births_per_1000 / (percent_no_protection_ub/100)) %>%
  dplyr::select(lead_yr, region, births_per_1000, birthsper1000_dividedbypercentsexual, birthsper1000_dividedbypercentsexual_ub,
                birthsper1000_dividedbypercentsexual_lb, birthsper1000_dividedbypercentsexual_noprotection,
                birthsper1000_dividedbypercentsexual_noprotection_lb, birthsper1000_dividedbypercentsexual_noprotection_ub)

#Plot
labels_fig3 <- c("births_per_1000" = "Births per 1000 female students aged 15-19",
                 "birthsper1000_dividedbypercentsexual" = "Births per 1000 sexually active female students aged 15-19",
                 "birthsper1000_dividedbypercentsexual_noprotection" = "Births per 1000 sexually active female students aged 15-19 using no method to prevent pregnancy")

fig3_plot_facet<- 
  ggplot(fig3_facet_data_v2%>% gather(var, value, births_per_1000, birthsper1000_dividedbypercentsexual, 
                                      birthsper1000_dividedbypercentsexual_noprotection) %>% 
           mutate(var = factor(var, levels = c("births_per_1000", "birthsper1000_dividedbypercentsexual", "birthsper1000_dividedbypercentsexual_noprotection"))),
         aes(x=lead_yr, y=value, group=var, col=var)) +
  theme_classic() +
  geom_line(linewidth=.5) +
  geom_point() +
  facet_grid(~region) +
  labs(col="", x="Year", y="") +
  scale_color_manual(values = c("births_per_1000" = "black", 
                                "birthsper1000_dividedbypercentsexual" = "darkgreen", 
                                "birthsper1000_dividedbypercentsexual_noprotection" = "blue"),
                     labels = labels_fig3) +
  geom_ribbon(fill="grey", aes(ymin = birthsper1000_dividedbypercentsexual_lb, ymax =birthsper1000_dividedbypercentsexual_ub),alpha = 0.1, colour = "gray") +
  geom_ribbon(fill="grey", aes(ymin = birthsper1000_dividedbypercentsexual_noprotection_lb, ymax =birthsper1000_dividedbypercentsexual_noprotection_ub), alpha = 0.1, colour="grey") +
  labs(col=" ", x="Year", y="", title="Figure 3. Births per 1000, births per 1000 among sexually active teenagers, and births per 1000 among sexually active teenagers not using protection,\nstratified by region.") +
  scale_x_continuous(breaks = seq(2006, 2024, by = 2)) +
  scale_y_continuous(limits=c(0,415), breaks = seq(0,415, by=50)) +
  theme(
    axis.title = element_text(size = 16),       
    axis.text = element_text(size = 12),     
    legend.text = element_text(size = 16),   
    legend.title = element_text(size = 16),
    axis.text.x=element_text(size=12, angle=90, vjust=0.5),
    legend.position="bottom",
    strip.text = element_text(size = 14),
    plot.title = element_text(size = 20)) +
  guides(col = guide_legend(nrow = 3)) 

ggsave(plot=fig3_plot_facet, "Fig3.jpeg", width=20, height=7 )

#######################################################################
################### SUPP FIG1: US MAP  ####################
#######################################################################

county_census1 <- county_census %>%
  filter(!is.na(REGION), REGION != "X") %>%
  dplyr::select(STNAME) %>%
  rename(state = STNAME) %>%
  distinct(state) 

county_census_expanded <- county_census1 %>%
  crossing(yr_group = c("group2007_2011", "group2019_2023"))

fig4_v2<- merge(fig4_data, county_census_expanded, by=c("state", "yr_group"), all.y=TRUE) %>% select(-X)

calculate_quartiles <- function(variable) {
  variable <- enquo(variable)
  quartiles <- fig4_v2%>%
    summarise(
      Q1 = quantile(!!variable, 0.25, na.rm = TRUE),
      Q2 = quantile(!!variable, 0.50, na.rm = TRUE),
      Q3 = quantile(!!variable, 0.75, na.rm = TRUE)
    )
  return(list(quartiles$Q1,quartiles$Q2, quartiles$Q3))
}

sexually_active_quartiles<- calculate_quartiles(percent_sexually_active)
no_protection_quartiles<- calculate_quartiles(percent_no_protection)
no_condom_quartiles<- calculate_quartiles(percent_no_condom)

# Use quartiles from above:
fig4_v3<- fig4_v2 %>%
  mutate(shade_SA = case_when(
    is.na(percent_sexually_active) ~ "Data not available",
    percent_sexually_active < sexually_active_quartiles[1] ~ "Less than 25.3%",
    percent_sexually_active < sexually_active_quartiles[2] ~ "Between 25.3% and 29.7%",
    percent_sexually_active < sexually_active_quartiles[3] ~ "Between 29.7% and 35.9%",
    percent_sexually_active > sexually_active_quartiles[3] ~ "Above 35.9%"),
  shade_no_protection = case_when(
    is.na(percent_no_protection) ~ "Data not available",
    percent_no_protection < no_protection_quartiles[1] ~ "Less than 10.9%",
    percent_no_protection < no_protection_quartiles[2] ~ "Between 10.9% and 13.1%",
    percent_no_protection < no_protection_quartiles[3] ~ "Between 13.1% and 14.5%",
    percent_no_protection > no_protection_quartiles[3] ~ "Above 14.5%"),
  shade_no_condom = case_when(
    is.na(percent_no_condom) ~ "Data not available",
    percent_no_condom < no_condom_quartiles[1] ~ "Less than 43.9%",
    percent_no_condom < no_condom_quartiles[2] ~ "Between 43.9% and 49.7%",
    percent_no_condom < no_condom_quartiles[3] ~ "Between 49.7% and 52.1%",
    percent_no_condom > no_condom_quartiles[3] ~ "Above 52.1%"))

# functions for plots
create_fig_map <- function(group, shade_var, cat1, cat2, cat3, cat4, var_name, title) {
  fill_values <- setNames(c("white", "#FFFFD4", "#FED98E", "#FE9929", "#CC4C02"),
                          c("Data not available", cat1, cat2, cat3, cat4))
  plot_usmap(data = fig4_v3 %>% filter(yr_group == !!group), values = shade_var) +
    scale_fill_manual(values = fill_values,
                      name = var_name,
                      limits = c("Data not available", cat1, cat2, cat3, cat4)) +
    theme(legend.position = "right",
          plot.title = element_text(size = 14, hjust=0.5),   #only for SA
          legend.text = element_text(size = 12),  
          legend.title = element_text(size = 12),
          legend.spacing.y = unit(1, "cm"))
}

create_fig_map_no_legend <- function(group, shade_var, cat1, cat2, cat3, cat4, var_name, title) {
  fill_values <- setNames(c("white", "#FFFFD4", "#FED98E", "#FE9929", "#CC4C02"),
                          c("Data not available", cat1, cat2, cat3, cat4))
  plot_usmap(data = fig4_v3 %>% filter(yr_group == !!group), values = shade_var) +
    scale_fill_manual(values = fill_values,
                      name = var_name,
                      limits = c("Data not available", cat1, cat2, cat3, cat4)) +
    theme(legend.position = "none",
          plot.title = element_text(size = 14, hjust=0.5),   #only for SA
          legend.text = element_text(size = 12),  
          legend.title = element_text(size = 12),
          legend.spacing.y = unit(1, "cm"))
}

# PLOTS
## sexually active
sexactive_20192023 <- create_fig_map(
  group = "group2019_2023", 
  shade_var = "shade_SA", 
  cat1 = "Less than 25.3%",
  cat2 = "Between 25.3% and 29.7%", 
  cat3 = "Between 29.7% and 35.9%", 
  cat4 = "Above 35.9%",
  var_name = "Percent sexually active           ", 
  title = "2019-2023"
)

sexactive_20072011 <- create_fig_map_no_legend(
  group = "group2007_2011", 
  shade_var = "shade_SA", 
  cat1 = "Less than 25.3%",
  cat2 = "Between 25.3% and 29.7%", 
  cat3 = "Between 29.7% and 35.9%", 
  cat4 = "Above 35.9%",
  var_name = "Percent sexually active", 
  title = "2007-2011"
)
leg_sa<- as_ggplot(get_legend(sexactive_20192023, position = NULL))
fig_sa<- ggarrange(sexactive_20072011, sexactive_20192023, legend="none") 

## protection
noprotection_20192023 <- create_fig_map(
  group = "group2019_2023", 
  shade_var = "shade_no_protection", 
  cat1 = "Less than 10.9%",
  cat2 = "Between 10.9% and 13.1%", 
  cat3 = "Between 13.1% and 14.5%", 
  cat4 = "Above 14.5%",
  var_name = "Percent sexually active not using protection", 
  title = "2019-2023"
)

noprotection_20072011 <- create_fig_map_no_legend(
  group = "group2007_2011", 
  shade_var = "shade_no_protection", 
  cat1 = "Less than 10.9%",
  cat2 = "Between 10.9% and 13.1%", 
  cat3 = "Between 13.1% and 14.5%", 
  cat4 = "Above 14.5%",
  var_name = "Percent sexually active not using protection", 
  title = "2007-2011"
)

leg_protection<- as_ggplot(get_legend(noprotection_20192023, position = NULL))
fig_noprotection<- ggarrange(noprotection_20072011, noprotection_20192023, common.legend = TRUE, legend = "none")

## condom
nocondom_20192023 <- create_fig_map_no_legend(
  group = "group2019_2023", 
  shade_var = "shade_no_condom", 
  cat1 = "Less than 43.9%",
  cat2 = "Between 43.9% and 49.7%", 
  cat3 = "Between 49.7% and 52.1%", 
  cat4 = "Above 52.1%",
  var_name = "Percent sexually active not using a condom", 
  title = "2019-2023"
)

nocondom_20072011 <- create_fig_map(
  group = "group2007_2011", 
  shade_var = "shade_no_condom", 
  cat1 = "Less than 43.9%",
  cat2 = "Between 43.9% and 49.7%", 
  cat3 = "Between 49.7% and 52.1%", 
  cat4 = "Above 52.1%",
  var_name = "Percent sexually active not using a condom", 
  #title = "2007-2011"
)

leg_condom<- as_ggplot(get_legend(nocondom_20072011, position = NULL))
fig_nocondom<- ggarrange(nocondom_20072011, nocondom_20192023, common.legend =TRUE, legend = "none")

# PUT TOGETHER
fig_supp<- ggarrange(fig_sa, fig_noprotection, fig_nocondom, legend="none", ncol=1, nrow=3)
ggsave(plot=fig_supp, "Fig_Supp.jpeg", width=8, height=7 )

legends<- ggarrange(leg_sa, leg_protection, leg_condom, legend="none", ncol=1, nrow=3, align="v")
ggsave(plot=legends, "legends_supp.jpg", width=8, height=7 )

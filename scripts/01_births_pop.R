##########################################
# compiling all birth and population data

# population data: ages 15-19, by state, by year (https://data.census.gov/table/ACSST1Y2022.S0101?q=population%20by%20age%20by%20state)
# births data: from CDC WONDER
##########################################

here::i_am("scripts/01_births_pop.R")
source(here::here("scripts/00_libraries.R"))



##### FUNCTION: clean pop data, when total is given
population_data<- function(data, year) {
  #pick female columns, rename
  data_v2 <- data %>%
    select(`Label..Grouping.`, contains("Female..Estimate")) %>%
    select(-contains("percent")) %>%
    rename_at(vars(-Label..Grouping.), ~ gsub("\\.\\.Female\\.\\.Estimate", "", .)) %>%
    rename_all(~ gsub("\\.", " ", .))
  #choose row (ages 15-19)
  data_v3<- data_v2 %>%
    slice(6) %>%
    select(-"Label  Grouping ") 
  #change to long format, make pop integer vals
  data_v4<- pivot_longer(data_v3, cols=everything(), 
                         names_to="state", values_to= "population") %>%
    mutate(population = as.integer(gsub(",", "", population))) %>%
    mutate(yr=year)
  return(data_v4)
}

##### FUNCTION: clean pop data, when total isn't given
population_data_nototal<- function(data, year) {
  data_v2<- data %>%
    select(`Label..Grouping.`, contains("Female..Estimate")) %>%
    select(-contains("percent")) %>%
    rename_at(vars(-Label..Grouping.), ~ gsub("\\.\\.Female\\.\\.Estimate", "", .)) %>%
    rename_all(~ gsub("\\.", " ", .)) %>%
    slice(1,6) %>%
    select(-"Label  Grouping ") %>%
    #transpose
    t() %>%
    as.data.frame() 
  data_v3<- data_v2 %>%
    #rename
    rename(state_pop=V1,
           percent=V2) %>%
    #reformat
    mutate(state_pop = as.integer(gsub(",", "", state_pop)),
           percent=as.double(gsub("%","", percent)),
           population=round(state_pop*(percent/100)),
           state=rownames(data_v2),
           yr=year) %>%
    select(state, population, yr)
  return(data_v3)
}



##### READ IN DATA
pop2023_clean <- read.csv(here("raw_data", "acs2023.csv")) %>%
  population_data(., "2023")

pop2022_clean <- read.csv(here("raw_data", "acs2022.csv")) %>%
  population_data(., "2022")

pop2021_clean <- read.csv(here("raw_data", "acs2021.csv")) %>%
  population_data(., "2021")

pop2020_clean <- read.csv(here("raw_data", "acs2020.csv")) %>%
  population_data(., "2020")

pop2019_clean <- read.csv(here("raw_data", "acs2019.csv")) %>%
  population_data(., "2019")

pop2018_clean <- read.csv(here("raw_data", "acs2018.csv")) %>%
  population_data(., "2018")

pop2017_clean <- read.csv(here("raw_data", "acs2017.csv")) %>%
  population_data(., "2017")

pop2016_clean<- read.csv(here("raw_data", "acs2016.csv")) %>%
  population_data_nototal(., "2016")

pop2015_clean <- read.csv(here("raw_data", "acs2015.csv")) %>%
  population_data_nototal(., "2015")

pop2014_clean <- read.csv(here("raw_data", "acs2014.csv")) %>%
  population_data_nototal(., "2014")

pop2013_clean <- read.csv(here("raw_data", "acs2013.csv")) %>%
  population_data_nototal(., "2013")

pop2012_clean <- read.csv(here("raw_data", "acs2012.csv")) %>%
  population_data_nototal(., "2012")

pop2011_clean <- read.csv(here("raw_data", "acs2011.csv")) %>%
  population_data_nototal(., "2011")

pop2010_clean <- read.csv(here("raw_data", "acs2010.csv")) %>%
  population_data_nototal(., "2010")

##### COMBINE
pop_data<- bind_rows(pop2023_clean, pop2022_clean, 
                     pop2021_clean, pop2020_clean, pop2019_clean,
                     pop2018_clean, pop2017_clean, pop2016_clean,
                     pop2015_clean, pop2014_clean, pop2013_clean,
                     pop2012_clean, pop2011_clean, pop2010_clean)


################# BIRTHS DATA #################
# Births ages 15-19, 2007-2024
births_2007_2023<-read.csv(here("raw_data", "natality_2007_2023_15to19.csv")) %>%
  select(State, Year, Births) %>%
  rename(state=State,
         yr=Year,
         births=Births)

#for 2024: Dec2023-Nov2024 (don't have Dec2024)
births2024<- read.csv(here("raw_data", "natality_2024_15to19.csv")) %>%
  select(State.of.Residence, births) %>%
  mutate(yr=2024) %>%
  mutate(yr=as.integer(yr)) %>%
  rename(state=State.of.Residence)

births2024<- read.csv(here("raw_data", "natality_2024_15to19.csv")) %>%
  select(State.of.Residence, births) %>%
  mutate(yr=2024) %>%
  mutate(yr=as.integer(yr)) %>%
  rename(state=State.of.Residence)

births2007_2024<- rbind(births_2007_2023, births2024)

################# MERGE POP AND BIRTHS DATA #################
# yr 2024: use 2024 pop (2024 pop not released)
births_data <- merge(pop_data, births2007_2024, by = c("yr", "state"), all.y=TRUE)

#fill in missing pop values
births_data <- births_data %>%
  mutate(population = case_when(
    yr == 2020 ~ pop_data$population[pop_data$yr == 2019][match(state, pop_data$state[pop_data$yr == 2019])],
    yr %in% c(seq(2007,2009, by=1)) ~ pop_data$population[pop_data$yr == 2010][match(state,pop_data$state[pop_data$yr == 2010])],
    yr == 2024 ~ pop_data$population[pop_data$yr == 2023][match(state, pop_data$state[pop_data$yr == 2023])],
    TRUE ~ population
  )) %>%
  #create column, pop per 1000
  mutate(births_per_1000 = births/(population/1000)) %>%
  mutate(yr=as.integer(yr)) %>%
  #rename
  rename(pop15_19female = population)


###### ages 15-17 ######
births2007_2023<- read.csv(here("raw_data","2007_2023_ages15to17.csv")) %>%
  rename(yr=Year, births=Births) %>%
  select(yr, births)

births2024_younger<- read.csv(here("raw_data", "2024_ages15to17.csv")) %>%
  mutate(births=births_jan24_nov24 + births_dec23) %>%
  mutate(yr=Year) %>%
  select(yr,births) %>%
  filter(!(is.na(yr)))

births_younger<- rbind(births2007_2023, births2024_younger)

#merge with pop
pop_byyr<- pop_data %>%
  group_by(yr) %>%
  summarize(total_pop = sum(population))

births_data_younger<- merge(births_younger, pop_byyr, by="yr", all.x=TRUE) %>%
  rename(population = total_pop) %>%
  mutate(population = case_when(
    yr == 2020 ~ pop_byyr$total_pop[pop_byyr$yr==2019],
    yr %in% c(seq(2007,2009, by=1)) ~ pop_byyr$total_pop[pop_byyr$yr == 2010],
    yr == 2024 ~ pop_byyr$total_pop[pop_byyr$yr == 2023],
    TRUE ~ population)) %>%
  #create column, pop per 1000
  mutate(births_per_1000 = births/(population/1000)) %>%
  mutate(yr=as.integer(yr)) %>%
  #rename
  rename(pop15_17female = population)


###### ages 18-19 ######
births2007_2023_older<- read.csv(here("raw_data","2007_2023_ages18to19.csv")) %>%
  rename(yr=Year, births=Births) %>%
  select(yr, births)

#births2024
births2024_older<- read.csv(here("raw_data", "2024_ages18to19.csv")) %>%
  mutate(births=births_jan24_nov24 + births_dec23) %>%
  mutate(yr=Year) %>%
  select(yr,births) %>%
  filter(!(is.na(yr)))

births_older<- rbind(births2007_2023_older, births2024_older)

#merge with pop
births_data_older<- merge(births_older, pop_byyr, by="yr", all.x=TRUE) %>%
  rename(population = total_pop) %>%
  mutate(population = case_when(
    yr == 2020 ~ pop_byyr$total_pop[pop_byyr$yr==2019],
    yr %in% c(seq(2007,2009, by=1)) ~ pop_byyr$total_pop[pop_byyr$yr == 2010],
    yr == 2024 ~ pop_byyr$total_pop[pop_byyr$yr == 2023],
    TRUE ~ population
  )) %>%
  #create column, pop per 1000
  mutate(births_per_1000 = births/(population/1000)) %>%
  mutate(yr=as.integer(yr)) %>%
  #rename
  rename(pop18_19female = population)

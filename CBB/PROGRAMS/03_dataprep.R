## Bill Chew
## 01.11.2020
## Program to clean College basketball data for analysis (Run after datacleaning_cbb.R program)
####################
rm(list = ls())
options(tibble.width = Inf)

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(tidyverse)

# if at school
home <- "C:/Users/wlchew/OneDrive - University of Iowa/Creative Component/Data"
# if at home
home <- "C:/Users/Bill Chew/OneDrive for Business/Creative Component/Data"
home <- "C:/Users/wlche/OneDrive - University of Iowa/Creative Component/Data/up_to_2019/cbb"

list.files(home)


dat_cbb.raw <- read_csv(file.path(home,"cleaned_cbbdata.csv"))

dat_cbb <- 
  dat_cbb.raw %>% 
  mutate(Opp=as.character(Opp),
         Date = as.Date(Date),
         yr = year(Date),
         mth = month(Date)) %>% 
  # establish season in cbb data
  mutate(season = ifelse(mth < 5, yr - 1, yr))


# List of D1 Schools, any team with at least 8 home appearances
list.d1.schools <- 
  dat_cbb %>% 
  count(School, season, name = 'n_sch') %>% 
  full_join(dat_cbb %>% count(Opp, season, name = "n_opp"),
   by = c("School" = "Opp", "season")) %>% 
  arrange(School) %>% 
  mutate(tot_app = n_sch + n_opp) %>% 
  filter(!is.na(n_sch)) %>% 
  filter(n_sch > 8) %>% pull(School) %>% unique()

# Games between two D1 teams
# ID Where the game was played
d1_games <- 
  dat_cbb %>% 
  filter(School %in% list.d1.schools & Opp %in% list.d1.schools) %>% 
  mutate(Site = ifelse(is.na(site),"H",as.character(site))) %>% 
  mutate(School_Home = ifelse(Site == "H", 1,0),
         School_Away = ifelse(Site == "@",1,0),
         Neutral = ifelse(Site =="N",1,0))

# School is Home team
s.home <- 
  d1_games %>% 
  filter(School_Home == 1) %>% 
  rename(Home_Team = School, Away_Team = Opp) %>% 
  rename_at(vars(starts_with("S_")), function(x) str_replace(x, "S_", "Home_")) %>% 
  rename_at(vars(starts_with("O_")), function(x) str_replace(x, "O_", "Away_")) %>% 
  select(-Result)
  

# School is away team
s.away <- 
  d1_games %>%
  filter(School_Away == 1) %>% 
  rename(Home_Team = Opp, Away_Team = School) %>% 
  rename_at(vars(starts_with("S_")), function(x) str_replace(x, "S_", "Away_")) %>% 
  rename_at(vars(starts_with("O_")), function(x) str_replace(x, "O_", "Home_")) %>% 
  select(-Result)

# Neutral Site, define home team as first alphabetic team
s.neutral.base <- 
  d1_games %>% 
  filter(Neutral == 1) %>% 
  mutate(alpha_home = pmin(School, Opp))

# For School "Home"
neutral.s.home <- 
  s.neutral.base %>% 
  filter(alpha_home == School) %>% 
  rename(Home_Team = School, Away_Team = Opp) %>% 
  rename_at(vars(starts_with("S_")), function(x) str_replace(x, "S_", "Home_")) %>% 
  rename_at(vars(starts_with("O_")), function(x) str_replace(x, "O_", "Away_")) %>% 
  select(-Result, -alpha_home)

# For School "Away"
neutral.s.away <- 
  s.neutral.base %>% 
  filter(alpha_home == Opp) %>% 
  rename(Home_Team = Opp, Away_Team = School) %>% 
  rename_at(vars(starts_with("S_")), function(x) str_replace(x, "S_", "Away_")) %>% 
  rename_at(vars(starts_with("O_")), function(x) str_replace(x, "O_", "Home_")) %>% 
  select(-Result, -alpha_home)

# Put Together and reduce down to a single game per record
out.dat <- 
  bind_rows(s.home, s.away, neutral.s.away, neutral.s.away) %>% 
  arrange(Date, Home_Team, Away_Team) %>% 
  select(-School_Home, -School_Away) %>% 
  group_by(Date, Home_Team, site, Away_Team, Site, yr, mth, season) %>% 
  summarize_all(function(x) mean(x, na.rm = T))

# Output
write.csv(out.dat,file.path(home,"cbb_gamedata_2010_2019.csv"),
         row.names = FALSE)


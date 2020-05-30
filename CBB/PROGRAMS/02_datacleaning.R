## Bill Chew
## 01.11.2020
## Program to clean College basketball data (Run after web_scrape_cbb.R program)
####################

rm(list = ls())
options(tibble.width = Inf)
# data cleaning

school <- "C:/Users/wlchew/OneDrive - University of Iowa/Creative Component/Data/cbb"
home <- "C:/Users/Bill Chew/OneDrive for Business/Creative Component/Data/cbb"
home <- "C:/Users/wlche/OneDrive - University of Iowa/Creative Component/Data/up_to_2019/cbb"

library(dplyr)
library(tidyr)
library(readr)

## Column Names ####

team.std <- c("id","Date","School","site","Opp","Result","MP")

pts <- c("FG","FGA","FGPct","2P","2PA",
         "2PPct","3P","3PA","3PPct","FT",
         "FTA","FTPct","Pts")

n.asst <- c("Asst","STL","BLK","TOV","PF")

n.orb <- c("ORB","DRB","TRB")

#### Importing data ####

scores <- read_csv(file.path(home,"mbb_2010_2019.csv"),
                   col_names =
                     c("drop",team.std,paste0("S_",pts),paste0("O_",pts)))

asst <- read_csv(file.path(home,"mbb_asst_2010_2019.csv"),
                 col_names = c("drop",team.std,paste0("S_",n.asst)))

opp_asst <- read_csv(file.path(home,"mbb_oppasst_2010_2019.csv",sep=''),
                     col_names = c("drop",team.std,paste0("O_",n.asst)))

orb <- read_csv(file.path(home,"mbb_orb_2010_2019.csv"),
                col_names = c("drop",team.std,paste0("S_",n.orb)))

opp_orb <- read_csv(file.path(home,"mbb_opporb_2010_2019.csv"),
                    col_names = c("drop",team.std,paste0("O_",n.orb)))

# Function to prep data for joining
dat.prep <- function(in.dat){
  out.dat <- 
    in.dat %>% 
    filter(!is.na(id)) %>% 
    select(-id, -drop) %>% 
    mutate(Date = as.Date(Date, "%Y-%m-%d")) %>% 
    mutate_at(vars("MP",starts_with("S_"), starts_with("O_")), as.numeric) 
  return(out.dat)
}


# Prepare each dataset
prep.scores <- dat.prep(scores)

prep.asst <- dat.prep(asst)

prep.opp_asst <- dat.prep(opp_asst)

prep.orb <- dat.prep(orb)

prep.opp_orb <- dat.prep(opp_orb)

# join data together
output.data <- 
  prep.scores %>% 
  left_join(prep.asst) %>% 
  left_join(prep.opp_asst) %>% 
  left_join(prep.orb) %>% 
  left_join(prep.opp_orb)


# Writing CSV  ####

write.csv(output.data, file.path(home,"cleaned_cbbdata.csv"),row.names = FALSE)


# Unique teams ####
# School
Schools <- 
  output.data %>% 
  select(School, Date) %>% 
  group_by(School) %>% 
  summarize(min_date = min(Date, na.rm = TRUE),
            max_date = max(Date, na.rm = TRUE))

# Opponent
Opps <- 
  output.data %>% 
  select(Opp, Date) %>% 
  group_by(Opp) %>% 
  summarize(min_date = min(Date, na.rm = TRUE),
            max_date = max(Date, na.rm = TRUE))

# Combine and reduce
teams.table <- 
  Schools %>%
  full_join(Opps, by = c("School" = "Opp")) %>% 
  mutate(Min_date = pmin(min_date.x, min_date.y, na.rm = TRUE),
         Max_date = pmax(max_date.x, max_date.y, na.rm = TRUE)) %>% 
  select(School, Min_date, Max_date)


write.csv(teams.table, file.path(home,"teams_table.csv"),row.names = FALSE)

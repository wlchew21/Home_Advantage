library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

rm(list = ls())

# if at school
home<- "C:\\Users\\wlche\\OneDrive - University of Iowa\\Creative Component\\Data"
#if at home
# home <- "C:\\Users\\Bill Chew\\OneDrive for Business\\Creative Component\\Data"

data.dir <- file.path("C:", "Users", "wlche", "OneDrive - University of Iowa",
                     "Creative Component", "Data", "up_to_2019", "cbb")

core.dat <- read.csv(file.path(data.dir,"cbb_gamedata_2010_2019.csv"),
                     stringsAsFactors = FALSE)

div1.dat <- mutate(core.dat, Date = as.Date(Date))


# Assign each team a number
teams.num <- 
  div1.dat %>% select(Tm = Home_Team, season) %>% 
  bind_rows(div1.dat %>% select(Tm = Away_Team, season)) %>% 
  unique() %>% 
  group_by(season) %>% 
  mutate(Tm_num = rank(Tm)) %>% 
  ungroup

game.data.clean <- 
  div1.dat %>% 
  left_join(teams.num, by = c("Home_Team" = "Tm", "season")) %>% 
  rename(Home_Team_Number = Tm_num) %>% 
  left_join(teams.num, by = c("Away_Team" = "Tm", "season")) %>% 
  rename(Away_Team_Number = Tm_num) %>% 
  group_by(Home_Team_Number, Away_Team_Number) %>% 
  mutate(k = rank(Date), 
         non_neutral = ifelse(Neutral == 1, 0, 1),
         Home_FG_pct = Home_FG/Home_FGA, 
         Away_FG_pct = Away_FG/Away_FGA,
         Home_2pt_pct = Home_2P/Home_2PA,
         Away_2pt_pct = Away_2P/Away_2PA,
         Home_FT_pct = Home_FT/Home_FTA,
         Away_FT_pct = Away_FT/Away_FTA,
         Home_3pt_pct = Home_3P/Home_3PA,
         Away_3pt_pct = Away_3P/Away_3PA
  ) %>% 
  rename(i = Home_Team_Number, j = Away_Team_Number)
    
# Function to give ANOVA and Estimates table
HCA.analysis <- function(dat.in, y.home, y.away, lab, out = "est"){
  stopifnot(out %in% c("est", "anova"))
  
  Y <- 
  dat.in %>% 
    rename(yhome = !!y.home, yaway = !!y.away) %>% 
    mutate(y_use = yhome - yaway)
  
  c <- max(Y$i,Y$j)
  n <- nrow(Y)
  # Default values of X matrix to 0
  X <- matrix(rep(0,c*n), nrow=n)
  
  # Replace the ith column with a 1 for the ith team is the home team in the wth game
  # Replace the jth column with a -1 for the jth team is the away team in the wth game
  for( w in 1:n){
    X[w,Y$i[w]] <- 1
    X[w,Y$j[w]] <- -1
  }
  
  szn <- Y$season %>% unique
  
  # ANOVA - ####
  y.ml2 <-  lm(Y$y_use ~ Y$non_neutral + X - 1)

  # If only output the estimates
  if(out == "est"){
    
    ci.est <- confint(y.ml2) %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      filter(rowname == "Y$non_neutral")
    
    y.coef.est <- 
      summary(y.ml2)$coefficients %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      filter(rowname == "Y$non_neutral") %>% 
      left_join(ci.est, by = "rowname") %>% 
      mutate(HCA = lab, season = szn) %>% 
      select(HCA, season, everything()) %>% select(-rowname)

    ret <- y.coef.est
  }else{
    # If only output the anova
    # Grab and format ANOVA Table
    y.anova_out <- 
      anova(y.ml2) %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "Effect") %>% 
      mutate(Effect = case_when(Effect == "Y$non_neutral" ~ "M2 | M1",
                                Effect == "X" ~ "M1", 
                                Effect == "Residuals" ~ "Residual")) %>% 
      add_row(Effect = "Total", Df = sum(.$Df), `Sum Sq` = sum(.$`Sum Sq`)) %>% 
      mutate(arr_int = case_when(Effect == "M1" ~ 1,
                                 Effect == "M2 | M1" ~ 2,
                                 Effect == "Residual" ~ 3,
                                 Effect == "Total" ~ 4)) %>% 
      arrange(arr_int) %>% select(-arr_int) %>% 
      mutate(`Pr(>F)` = ifelse(`Pr(>F)` < 0.001, "< 0.001", as.character(round(`Pr(>F)`, 3)))) %>% 
      mutate_if(is.numeric, function(x) round(x, 2)) %>% 
      mutate(Effect = paste0("(", lab, " ", szn, "-", szn+1, ") ", Effect))
    
    ret <- y.anova_out
  }
  
  
  print(glue::glue("{szn} : {lab}"))
  return(ret)
 
}
  
  

dat.in <- game.data.clean %>% group_by(season) %>% filter(season != 2019) %>% group_split()
  

y.home <- "Home_Pts"
y.away <- "Away_Pts"
lab = "PTS"

# Additional Function for double lapply loop
HCA.runner <- function(dat.in, list.args, ind){
  lapply(seq_along(dat.in), function(q) HCA.analysis(dat.in = dat.in[[q]], y.home = list.args$y.home[ind],
                                                     y.away = list.args$y.away[ind], lab =  list.args$lab[ind],
                                                     out = list.args$out)) %>% 
    bind_rows()
}

# Arguments in a list
list.args <- list(y.home = c("Home_Pts", "Home_2P", "Home_3P", "Home_FT",
                             "Home_FTA", "Home_2PA", "Home_3PA", "Home_Asst", 
                             "Home_STL", "Home_BLK", "Home_TOV", "Home_PF",
                             "Home_DRB", "Home_ORB", "Home_TRB", "Home_FG_pct",
                             "Home_2pt_pct", "Home_FT_pct", "Home_3pt_pct"), 
                  y.away = c("Away_Pts", "Away_2P", "Away_3P", "Away_FT",
                             "Away_FTA", "Away_2PA", "Away_3PA", "Away_Asst", 
                             "Away_STL", "Away_BLK", "Away_TOV", "Away_PF",
                             "Away_DRB", "Away_ORB", "Away_TRB", "Away_FG_pct",
                             "Away_2pt_pct", "Away_FT_pct", "Away_3pt_pct"), 
                  lab = c("Pts", "2P", "3P", "FT",
                          "FTA", "2PA", "3PA", "Asst", 
                          "STL", "BLK", "TOV", "PF",
                          "DRB", "ORB", "TRB", "FG_pct",
                          "2pt_pct", "FT_pct", "3pt_pct"),
                  out = "est")

out.hca <- lapply(seq_along(list.args$y.home), function(p) HCA.runner(dat.in, list.args, p)) %>% bind_rows()

list.args$out <- "anova"

out.anova <- lapply(seq_along(list.args$y.home), function(p) HCA.runner(dat.in, list.args, p)) %>% bind_rows()

# library(xlsx)
# write.xlsx(out_tab, file=paste0(results,"CBB_all_Results.xlsx"),
#            sheetName="ANOVA", row.names=TRUE)
# write.xlsx(HCA2, file=paste0(results,"CBB_all_Results.xlsx"),
#            sheetName="HCA", append=TRUE, row.names=FALSE)

######
# Program name: 01_web_scrape.R
# Program location: ./CBB/PROGRAMS/
#
# Author: Bill Chew | 05/30/2020
#
#####

# Clear the environment
rm(list = ls())

# load packages
library(tidyverse)
library(rvest)

# Set options and paths
options(scipen = 999)
# Path to save Data files into
save.path <- file.path("./CBB/DATA/")


# Web Scrape function ---------------------------------------------------------

# Function to scrape pages from the link plus offset - apply through multiple offsets
cbb.scrape <- function(link, max.offset, off.start = 0){
  offset <- off.start # offset to loop through all pages 
  # Track how long it takes
  start <- Sys.time()
  
  # Initialize the output data
  z <- data.frame() 
  
  off.seq <- seq(off.start,max.offset,100)

  while(offset <= max.offset) {
    # Scrape page
    dd <- html_session(paste(link,offset,sep='')) %>%
      html_node("table#stats") %>%
      html_table()

    z <- rbind(z,dd)

    offset <- offset + 100 # increase by 100 for each iteration
  }
  # )
  end <- Sys.time()
return(list(dat = z, time = end - start, offset))
}

# Insert the stat into the function to go to that link
link.base <- function(stat){
  paste('https://www.sports-reference.com/',
        'cbb/play-index/tgl_finder.cgi?request=1',
        '&match=game&year_min=2011&comp_schl_rk=eq',
        '&val_schl_rk=ANY&comp_opp_rk=eq&val_opp_rk=ANY',
        '&game_type=A&is_range=N&order_by=',
        stat,     # ordered by date
        '&order_by_asc=Y&offset=',sep='')
}

# Scrapes -----------------------------------------------------------------
# Found through trial and error
m.off <- 105300

which.stats <- c("date_game","orb","opp_orb","ast","opp_ast")
csv.stats <- c("mbb_2010_2019.csv",
               "mbb_orb_2010_2019.csv",
               "mbb_opporb_2010_2019.csv",
               "mbb_asst_2010_2019.csv",
               "mbb_oppasst_2010_2019.csv")

off.start <- seq(0,m.off, 5000)
max.off <- c(off.start[-1] - 100,m.off)

data.frame(off.start, max.off)

check <- cbb.scrape(link.base(which.stats[1]), max.off[length(max.off)], off.start[length(off.start)])
tail(check[[1]])
# i = 1
# j = 11
timings <- list()
big.start <- Sys.time()
n.i <- length(which.stats)
n.j <- length(off.start)
for(i in 1:n.i){
  out.frame <- data.frame()
  start.time <- Sys.time()
  for(j in 1:n.j){
    int.frame <- cbb.scrape(link.base(which.stats[i]), max.off[j], off.start[j])
    out.frame <- rbind(out.frame, int.frame[[1]])
    print(paste0(round(100*(j/n.j), 2), "% complete"))
  }
  end.time <- Sys.time()
  timings[[i]] <- end.time - start.time
  write.csv(out.frame, file.path(save.path, "up_to_2019","cbb",csv.stats[i]))
  print(paste0(i, " out of ", n.i, " completed"))
}
big.end <- Sys.time()
big.end - big.start


# Coaches info ------------------------------------------------------------

## Coaches - ID Div 1 ####
yr <- 2011

z5 <- data.frame()

while(yr <=2020) {
  dd <- html_session(paste0("https://www.sports-reference.com/cbb/",
                           "seasons/",yr,"-coaches.html")) %>%
    html_table(header=F)
  dd[[1]] <- cbind(dd[[1]],yr)
  z5 <- cbind(rbind(z5,dd[[1]]))
  
  yr <- yr + 1
  
}

write.csv(z5,file.path(save.path, "up_to_2019","cbb",
                   "Div1_coaches.csv"))

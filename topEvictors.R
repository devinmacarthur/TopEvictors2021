library(dplyr)
library(tidyr)

# Wrangling data from OLC
OLCJanMarch <- read.xlsx(file.choose(), 1)
OLCApril <- read.xlsx(file.choose(), 2)
OLCMay <- read.xlsx(file.choose(), 3)

OLCJanMay <- rbind(OLCJanMarch %>% select(Case.., Notice), 
                   OLCApril %>% select(Case.., Notice), 
                   OLCMay %>% select(Case.., Notice))
OLCJanMay <- OLCJanMay %>% 
  rename(case_code = Case..)

# trying to creat sepearte tables for each evictor
for (i in 1:nrow(getTopEvictors())) {
  case_parties %>% 
    filter(party_side == "Plaintiff") %>%
    filter(name == getTopEvictors()[i,]$name)
}



topEvictorsWithOLCNotices()

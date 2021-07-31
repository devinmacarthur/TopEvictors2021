library(dplyr)
library(openxlsx)
library(tidyr)

case_overviews <- readRDS("data/full_scrape_07_22_2021/case_overviews.rds")
case_parties <- readRDS("data/full_scrape_07_22_2021/case_parties.rds")
OLCJanMay2021 <- readRDS("data/OLCDataEntry/OLCJanMay2021.rds")
NoticeFreqWithDescript <- readRDS("data/NoticeFreqWithDescript.rds")

fromDate <- "2021-01-01"
toDate <- "2021-05-30"

joinOverviewsParties <- function() {
  case_overviews %>%  
    mutate(date = as.Date(date, format = "%m/%d/%Y")) %>% 
    right_join(case_parties, by = "case_code") %>%
    filter(date >= fromDate & date <= toDate) %>% 
    return()
}

topEvictorFrequency <- function() {
  joinOverviewsParties() %>% 
    filter(party_side == "Plaintiff") %>% 
    group_by(name) %>%
    summarise(freq = n()) %>% 
    arrange(desc(freq)) %>% 
    filter(freq >= 10) %>% 
    return()
}

topEvictorsWithOLCNotices <- function() {
  topEvictorFrequency() %>%
    inner_join(joinOverviewsParties(), by = "name") %>% 
    left_join(OLCJanMay2021 %>% select(case_code, Notice), by = "case_code") %>% 
    select(name, freq, Notice, case_code, style, date, status, location) %>% 
    rename(numberOfCases = freq) %>% 
    return()
}

# Get Notice frequencies and dictionary
pivotTableEvictorNotice <- function() {
  topEvictorsWithOLCNotices() %>% 
    group_by(name, Notice) %>% 
    summarise(freq = n()) %>% 
    spread(key=Notice, value=freq) %>% 
    return()
}

OLCNoticeFrequency <- function() {
  OLCJanMay2021 %>% 
    group_by(Notice) %>% 
    summarise(freq = n()) %>% 
    arrange(desc(freq)) %>% 
    mutate(pct = round(100*freq / sum(freq), 1)) %>% 
    left_join(NoticeFreqWithDescript %>% select(Notice, Description), by = "Notice") %>% 
    return()
}

topEvictorNoticeFreq <- function() {
  topEvictorsWithOLCNotices() %>% 
    group_by(Notice) %>% 
    summarise(freq = n()) %>% 
    arrange(desc(freq)) %>% 
    mutate(pct = round(100*freq / sum(freq), 1)) %>%
    right_join(NoticeFreqWithDescript %>% select(Notice, Description), by = "Notice") %>% 
    return()
}

topEvictorTopNoticePct <- function(row) {
  topEvictorsWithOLCNotices() %>% 
    filter(name == as.character(topEvictorFrequency()[row,1])) %>%  
    group_by(Notice) %>% 
    summarise(freq = n()) %>%
    arrange(desc(freq)) %>% 
    mutate(pct = round(freq / sum(freq), 3)) %>% 
    return()
}

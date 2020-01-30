#Load libraries
library(colorspace)
library (ggplot2)
library (dplyr)
library (tidyr)
library (reshape2)
library(ComplexHeatmap)
library(circlize)
library(readxl)
library(knitr)
library(kableExtra)
library(lubridate)
library (igraph)
library(plotly)
library(hms)
library(dendextend)
# http://www.bioconductor.org/packages/release/bioc/html/hopach.html

##Stop with the scientific notation already
options(scipen=999)

##read excel
data <- read_excel("/Users/file.xlsx", sheet = "sheet1") 

##Filter,group by, summarise
##Overall Counts
t2a <- data %>%
  filter(!is.na(COGIPF_SESSIONID)) %>%
  group_by(reportType) %>%
  summarise(
    reportCount = n_distinct(COGIPF_REPORTNAME_Clean),
    reportPerc = reportCount/229,
    totSec = sum(actionSeconds),
    totSecPerc = totSec/9085936.84
  )

##Overall Counts by each session
t2b <- data %>%
  filter(!is.na(COGIPF_SESSIONID)) %>%
  group_by(reportType, COGIPF_REPORTNAME_Clean) %>%
  summarise(
    reportCount = n_distinct(COGIPF_REPORTNAME_Clean),
    totSec = sum(actionSeconds),
    totSecPerc = totSec/9085936.84
  ) %>%
###arrange by report type and sort descending by the percent of use
  arrange(reportType, desc(totSecPerc))

kable(t2, caption = "Counts, and Percentage of Access for IDW Reports, 2016-17",  digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "responsive"), fixed_thead = T)


##Other Code examples for summary tables

##Need to create factors for some variables
data$reportType <- as.factor(data$reportType)
data$COGIPF_SESSIONID <- as.factor(data$COGIPF_SESSIONID)

##Overall Summary by Report types
t3 <- data %>%
  filter(!is.na(COGIPF_SESSIONID)) %>%
  group_by(reportType) %>%
  summarise(
    sessionCount = n_distinct(COGIPF_SESSIONID),
    sessionPerc = sessionCount/10345,
    totSec = sum(actionSeconds),
    totalhh_mm_ss = round_hms(as.hms(sum(actionSeconds)), 1),
    #durSumPer = seconds_to_period(totSec),
    #dhmsTotSec = sprintf("%1.0fD %1.0fH %1.0fM %1.0fS", day(totSec), totSec@hour, minute(totSec), second(totSec)),
    totSecPerc = totSec/9085936.84,
    totAct = n(), 
    actionCountPerc = totAct/125198, 
    meanActionSec = totSec/totAct,
    meanSechh_mm_ss = round_hms(as.hms(totSec/totAct), 1),
    stdevActionSec = sd(actionSeconds),
    sdSechh_mm_ss = round_hms(as.hms(sd(actionSeconds)), 1),
    medActionSec = median(actionSeconds),
    medSechh_mm_ss = round_hms(as.hms(median(actionSeconds)), 1),
    maxActionSeconds = max(actionSeconds),
    maxSechh_mm_ss = round_hms(as.hms(max(actionSeconds)), 1)
  )

##Other Variables
#sdDuration = round_hms(as.hms(sd(elapsTimeSec)), 1)
#percActions = totalAction/4502*100,
#totalhh_mm_ss = round_hms(as.hms(sum(elapsTimeSec)), 1),
#percDuration = totalSeconds/321237*100,
#aveDuration = round_hms(as.hms(mean(elapsTimeSec)), 1),
#SDDuration = round_hms(as.hms(sd(elapsTimeSec)), 1),
#minDuration = round_hms(as.hms(min(elapsTimeSec)), 1),
#quart1Duration = round_hms(as.hms(quantile(elapsTimeSec, 0.25)), 1),
#medDuration = round_hms(as.hms(median(elapsTimeSec)), 1),
#quart3Duration = round_hms(as.hms(quantile(elapsTimeSec, 0.75)), 1),
#maxDuration = round_hms(as.hms(max(elapsTimeSec)), 1)
#durRepSum = as.duration(sum(elapsTimeSec))
#Average = mean(elapsTimeSec),
#SD = sd(elapsTimeSec),
#Minimum = min(elapsTimeSec),
#firstQuart = quantile(elapsTimeSec, 0.25),
#Median = median(elapsTimeSec),
#thirdQuart = quantile (elapsTimeSec, 0.75),
#Maximum = max(elapsTimeSec)
#durSumPer = seconds_to_period(repSum), Does not work with dplyr. Class 4 object
#formDurSum = sprintf("%1.0fD %1.0fH %1.0fM %1.0fS", day(durSum), durSum@hour, minute(durSum), second(durSum)), 

##Overall Summary by Report types and Sessions
t2b <- data %>%
  filter(!is.na(COGIPF_SESSIONID)) %>%
  group_by(reportType, COGIPF_REPORTNAME_Clean) %>%
  summarise(
    reportCount = n_distinct(COGIPF_REPORTNAME_Clean),
    totSec = sum(actionSeconds),
    totSecPerc = totSec/9085936.84
  ) %>%
  arrange(reportType, desc(totSecPerc))

##Pull in excel cells to make table
t3_action_descript <- read_excel("/Users/file.xlsx", 
                                 sheet = "t3_action_descript", 
                                 range = "A1:K9", 
                                 col_types = c("text", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "text", "text", "text", "text"))

##Use kable() to display nice table in r
kable(t3_action_descript, caption = "Table 3 Descriptive Statistics of Report Access, N = 129255 Report Actions",  digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "responsive"),
                fixed_thead = T) 


##Links/Sources
#https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
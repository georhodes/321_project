

# Install and load appropriate libraries
#install.packages("dplyr")
library("dplyr")
#install.packages("readr")
library("readr")
#install.packages("haven")
library("haven")
library("tibble")


#load National Survey on Drug Abuse and Mental Health and create object nsduh2015.
load("/Users/george/Documents/School/UW/SOC321/honors_thesis/honors_thesis/NSDUH-2015-survey-data.rda")
names(PUF2015_102016) <- tolower(names(PUF2015_102016))

#subset to relevant variables. 
subset_nsduh2015 <- PUF2015_102016 %>%
  select(txevrrcvd, alclottm, catag6, sexage, newrace2, irmaritstat, eduhighcat,
         al30est, alcbng30d, irpinc3, irfamin3, poverty3, coutyp2) 
#rename columns
colnames(subset_nsduh2015) <- c("treatment", "alcohol", "age", "sexage", "race", "marital", "edu", 
                                "drink30", "binge30", "income", "famincome", "poverty", "countytype")


# Who spent time in drug treatment ever? (TX01)		TXEVRRCVD	1=yes
#recieved_treatment <- nsduh2015 %>%
 # select(Variables/columns you want to view) %>%
  #filter(txevrrcvd == 1)
#recovered_respondants set to those that have recieved treatment AND not drank in last 12 months.
recovered_respondants <- subset_nsduh2015 %>%
  filter(treatment == 1 & alcohol == 93)
  # 925 observations. 

# Find out who spent no time getting or drinking alcohol in past 12 months
# (DRALC01) ALCLOTTM 
#83= Did not use or used < 6 dys Log asn 117
#93= Did not use past 12 mo or <6 dys 11935
#sober12months <-
  
  #spent time in treatment ever AND hasn't drank in last 12 months
  #This would be who is sober today. 
 # sober_after_treatment <- recieved_treatment AND sober12months






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

subset_nsduh2015 <- PUF2015_102016 %>%
  select(txevrrcvd, alclottm, sexage, newrace2, sexrace, eduhighcat, ireduhighst2, al30est, alcus30d, alcbng30d, irpinc3, irfamin3, poverty3, coutyp2, alcwd2sx,alcemopb) 
# Who spent time in drug treatment ever? (TX01)		TXEVRRCVD	1=yes
#recieved_treatment <- nsduh2015 %>%
 # select(Variables/columns you want to view) %>%
  #filter(txevrrcvd == 1)


# Find out who spent no time getting or drinking alcohol in past 12 months
# (DRALC01) ALCLOTTM 
#83= Did not use or used < 6 dys Log asn 117
#93= Did not use past 12 mo or <6 dys 11935
#sober12months <-
  
  #spent time in treatment ever AND hasn't drank in last 12 months
  #This would be who is sober today. 
 # sober_after_treatment <- recieved_treatment AND sober12months




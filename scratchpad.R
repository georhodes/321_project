# scratch script for working through problems. 

#How do I import only specific variables with specific values? 
#How do I avoid the giant data table and only import relevent data? 

# Who spent time in drug treatment ever? (TX01)		TXEVRRCVD	1=yes
recieved_treatment <- nsduh2015 %>%
  select(Variables/columns you want to view) %>%
  filter(txevrrcvd == 1)


# Find out who spent no time getting or drinking alcohol in past 12 months
# (DRALC01) ALCLOTTM 
#83= Did not use or used < 6 dys Log asn 117
#93= Did not use past 12 mo or <6 dys 11935
sober12months <-
  
  #spent time in treatment ever AND hasn't drank in last 12 months
  #This would be who is sober today. 
  sober_after_treatment <- recieved_treatment AND sober12months

#changing all names of dataset to lowercase
names(nsduh2015) <- tolower(names(nsduh2015))



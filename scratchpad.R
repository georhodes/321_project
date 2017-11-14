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

# too much info, but gives codebook... for everything. 
#unclass(recovered_respondants)

#to find mode. Table and then look
table(recovered_respondants$irfamin3)
# can compare two arguments
table(data$variable1, data$variable2)

#find info about variables in my dataset
attributes(recovered_respondants)$val.lables[[]]

attributes(recovered_respondants)$var.labels[[]]

attributes(recovered_respondants)$label.table[[]]

# returns wrong info. shows key for 6th variable of total data, not subset.
# Surprised that this label info is seperate from the data. 
attributes(recovered_respondants)$label.table[6]

arranged_mutated_meaningless <- recovered_respondants %>%
  rank(eduhighcat, irfamin3) %>% mutate(
    edu2 = eduhighcat * 2,
    fam2 = irfamin3 *2
  )

#shows info about variables. 
glimpse()

# scratch script for working through problems. 

#How do I import only specific variables with specific values? 
#How do I avoid the giant data table and only import relevent data? 

# Who spent time in drug treatment ever? (TX01)		TXEVRRCVD	1=yes
#recieved_treatment <- nsduh2015 %>%
 # select(VariablesOrColumnsYouWantToView) %>%
  #filter(txevrrcvd == 1)


# Find out who spent no time getting or drinking alcohol in past 12 months
# (DRALC01) ALCLOTTM 
#83= Did not use or used < 6 dys Log asn 117
#93= Did not use past 12 mo or <6 dys 11935
#sober12months <-
  
  #spent time in treatment ever AND hasn't drank in last 12 months
  #This would be who is sober today. 
  #sober_after_treatment <- recieved_treatment AND sober12months

#changing all names of dataset to lowercase
#names(nsduh2015) <- tolower(names(nsduh2015))

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
#list levels of factor
level(data$variable)
# dimensions of an object
dim(object)
# class of an object (numeric, matrix, data frame, etc)
class(object)
# print mydata 
mydata
# print first 10 rows of mydata
head(mydata, n=10)
# print last 5 rows of mydata
tail(mydata, n=5)
# list the structure of mydata
str(mydata)

#Something is masking dplyr's select function, so I need to call it specifically.
project_variables <- recovered_respondants %>%
  dplyr::select(1,3:4,9, 12:15) 

#mutate using if (define recovered) = "recovered"
                  # else = "unrecovered"

hist_income <- ggplot(data, aes(y = ((..count..)/sum(..count..)), x = income)) +
  # default is stack, can do stat= identity or densisty
  geom_histogram(x= treated_sober == recovered, fill = "red", alpha = 0.2) + 
  geom_histogram(data = treated_sober == unrecovered, fill = "blue", alpha = 0.2) 


#cutting out parts of the VIS lab
#adds a column 16 of treated_sober recovered/niether
data_treated_sober <- subset_nsduh2015 %>% 
  mutate(treated_sober = if_else((treatment == 1 & alcohol == 93), 
                                 "recovered",
                                 "neither")
  )

#adds a column 16 of treated_drinking: drinking/neither
data_treated_drinking <- data_treated_sober %>% 
  mutate( treated_drinking = if_else((treatment == 1 & alcohol != (93) ), 
                                     "treated_drinking",
                                     "treated_drinking_FALSE")
  )

#adds a column 17 treated: treated/untreated. data becomes working dataset. 
data_treated <- data_treated_drinking %>% 
  mutate( treated = if_else((treatment == 1), 
                            "treated",
                            "untreated")
#Treated and Drinking True
data_count_treated_drinking_n <- data_count_not_recovered_n %>%
  mutate(treated_drinking_n = sum(treated_drinking == "treated_drinking"))

#Treated and Drinking False
data_count_not_treated_drinking_n <- data_count_treated_drinking_n %>%
  mutate(not_treated_drinking_n = sum(treated_drinking == "treated_drinking_FALSE"))

#Changing labels on graphs
p7 <- ggplot(airquality, aes(x = Ozone)) +
  geom_histogram(aes(y = ..count..), binwidth = 5) +
  scale_x_continuous(name = "Mean ozone in\nparts per billion") +
  scale_y_continuous(name = "Count")
p7
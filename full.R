

# # Install and load appropriate libraries
# #install.packages("dplyr")
# library("dplyr")
# #install.packages("readr")
# library("readr")
# #install.packages("haven")
# library("haven")
# library("tibble")
# 
# 
# #load National Survey on Drug Abuse and Mental Health and create object nsduh2015.
# load("/Users/george/Documents/School/UW/SOC321/honors_thesis/321_project/NSDUH-2015-survey-data.rda")
# names(PUF2015_102016) <- tolower(names(PUF2015_102016))
# 
# #subset to relevant variables. 
# subset_nsduh2015 <- PUF2015_102016 %>%
#   select(questid2, filedate, txevrrcvd, alclottm, catag6, irsex, newrace2, irmaritstat, eduhighcat,
#          al30est, alcbng30d, irpinc3, irfamin3, poverty3, coutyp2) 
# #rename columns
# colnames(subset_nsduh2015) <- c("ident", "date", "treatment", "alcohol", "age", "sex", "race", "marital", "edu", 
#                                 "drink30", "binge30", "income", "famincome", "poverty", "countytype")
# 
# # Who spent time in drug treatment ever? (TX01)		TXEVRRCVD	1=yes
# #recieved_treatment <- nsduh2015 %>%
#  # select(Variables/columns you want to view) %>%
#   #filter(txevrrcvd == 1)
# #recovered_respondants set to those that have recieved treatment AND not drank in last 12 months.
# recovered_respondants <- subset_nsduh2015 %>%
#   filter(treatment == 1 & alcohol == 93)
#   # 925 observations. 
# 
# # Find out who spent no time getting or drinking alcohol in past 12 months
# # (DRALC01) ALCLOTTM 
# #83= Did not use or used < 6 dys Log asn 117
# #93= Did not use past 12 mo or <6 dys 11935
# #sober12months <-
#   
#   #spent time in treatment ever AND hasn't drank in last 12 months
#   #This would be who is sober today. 
#  # sober_after_treatment <- recieved_treatment AND sober12months
# 
# 
# #Creating groups of variables
# alc_variables <- recovered_respondants %>%
#   select(1,4, 10, 11)
# alc_variables[1:10,]
# 
# income_variables <- recovered_respondants %>%
#   select(1,12:13)
# 
# demog_variables <- recovered_respondants %>%
#   select(1,5:8, 15)
# 
# #selects ident, treatment, alcohol, edu, income, famincome, poverty, and county type. 
# #Something is masking dplyr's select function, so I need to call it specifically.
# project_variables <- recovered_respondants %>%
#   dplyr::select(1,3:4,9, 12:15) 
# 
# #adds column 16 of recovered/unrecovered
# data_recovered <- subset_nsduh2015 %>% 
#   mutate( if_else((treatment == 1 & alcohol == 93), 
#                   "recovered",
#                   "unrecovered")
#          )
# #Adds column 17 of treated/untreated.
# data <- data_recovered %>% 
#   mutate( if_else((treatment == 1), 
#                   "treated",
#                   "untreated")
#         )
# 
# #adds a column 16 of treated_sober recovered/niether
# data_treated_sober <- subset_nsduh2015 %>% 
#   mutate( if_else((treatment == 1 & alcohol == (93|83)), 
#                   "recovered",
#                   "neither")
#   )
# 
# #rename new column
# colnames(data_treated_sober) <- c("ident", "date", "treatment", "alcohol", "age", "sex", "race", "marital", "edu", 
#                                   "drink30", "binge30", "income", "famincome", "poverty", "countytype",
#                                   "treated_sober")
# 
# #adds a column 16 of treated_drinking: drinking/neither
# data_treated_drinking <- data_treated_sober %>% 
#   mutate( if_else((treatment == 1 & alcohol != (83|93) ), 
#                   "drinking",
#                   "neither")
#   )
# #rename new column
# colnames(data_treated_drinking) <- c("ident", "date", "treatment", "alcohol", "age", "sex", "race", "marital", "edu", 
#                                      "drink30", "binge30", "income", "famincome", "poverty", "countytype",
#                                      "treated_sober", "treated_drinking")
# 
# #adds a column 17 treated: treated/untreated. data becomes working dataset. 
# data_treated <- data_treated_drinking %>% 
#   mutate( if_else((treatment == 1), 
#                   "treated",
#                   "untreated")
#   )
# 
# #adds name to new column. 
# colnames(data_treated) <- c("ident", "date", "treatment", "alcohol", "age", "sex", "race", "marital", "edu", 
#                             "drink30", "binge30", "income", "famincome", "poverty", "countytype",
#                             "treated_sober", "treated_drinking", "treated")
# 
# data_count_recovered_n <- data_treated %>%
#   mutate(recovered_n = sum(treated_sober == "recovered"))
# 
# data_count_not_recovered_n <- data_count_recovered_n %>%
#   mutate(not_recovered_n = sum(treated_sober == "neither"))
# 
# data_count_treated_drinking_n <- data_count_not_recovered_n %>%
#   mutate(treated_drinking_n = sum(treated_drinking == "drinking"))
# 
# data_count_not_treated_drinking_n <- data_count_treated_drinking_n %>%
#   mutate(not_treated_drinking_n = sum(treated_drinking == "neither"))
# #create sum of recovered. variable is the same for every row. 
# #group by income
# # create proportion recovered. # of peolpe in income/ total. 
# #Plot that variable .
# #rename last two columns "recovered" and "treated" to make usable
# 
#                  
# 
# #mutate using if (define recovered) = "recovered"
# # else = "unrecovered"
# 
# library(ggplot2)
# library(dplyr)
# library(scales)
# library(ggthemes)
# 
# 
# #load National Survey on Drug Abuse and Mental Health and create object nsduh2015.
# load("/Users/george/Documents/School/UW/SOC321/honors_thesis/honors_thesis/NSDUH-2015-survey-data.rda")
# 
# #change all variable names to all lowercase for ease of calling.
# names(PUF2015_102016) <- tolower(names(PUF2015_102016))
# 
# #subset to relevant variables. 
# subset_nsduh2015 <- PUF2015_102016 %>%
#   select(questid2, filedate, txevrrcvd, alclottm, catag6, irsex, newrace2, irmaritstat, eduhighcat,
#          al30est, alcbng30d, irpinc3, irfamin3, poverty3, coutyp2) 
# #rename columns
# colnames(subset_nsduh2015) <- c("ident", "date", "treatment", "alcohol", "age", "sex", "race", "marital", "edu", 
#                                 "drink30", "binge30", "income", "famincome", "poverty", "countytype")
# 
# #adds column 16 (treated_sober) with recovered, treated_drinking, and untreated 
# data_treated_sober <- subset_nsduh2015 %>%
#   mutate(treated_sober = ifelse((treatment == 1 & alcohol == 93), "recovered",
#                                 ifelse(treatment ==1 & alcohol != 93, "treated_drinking",
#                                        "untreated")))
# 
# #adding columns that count
# # Treated and Sober "recovered"
# data_count_recovered_n <- data_treated_sober %>%
#   mutate(recovered_n = sum(treated_sober == "recovered"))
# 
# #Treated and Sober "treated_drinking""
# data_count_not_recovered_n <- data_count_recovered_n %>%
#   mutate(treated_drinking_n = sum(treated_sober == "treated_drinking"))
# 
# count_untreated_n <- data_count_not_recovered_n %>%
#   mutate(untreated_n= sum(treated_sober == "untreated"))
# 
# data_clean <- count_untreated_n
# 
# #creates subet of only those "recovered"
# recovered_respondants <- data_clean %>%
#   filter(treatment == 1 & alcohol == 93)
# 
# #creates subset of only those "treated drinking"
# treated_drinking_respondants <- data_clean %>%
#   filter(treated_sober == "treated_drinking")
# 
# untreated_respondants <- data_clean %>%
#   filter(treated_sober == "untreated")
# 
# project_variables <- recovered_respondants %>%
#   dplyr::select(ident, treatment, alcohol, edu, income, famincome, poverty, countytype)

library(ggplot2)
library(dplyr)
library(scales)
library(ggthemes)


#load National Survey on Drug Abuse and Mental Health and create object nsduh2015.
load("/Users/george/Documents/School/UW/SOC321/honors_thesis/honors_thesis/NSDUH-2015-survey-data.rda")

#change all variable names to all lowercase for ease of calling.
names(PUF2015_102016) <- tolower(names(PUF2015_102016))

#subset to relevant variables. 
subset_nsduh2015 <- PUF2015_102016 %>%
  select(questid2, filedate, txevrrcvd, alclottm, catag6, irsex, newrace2, irmaritstat, eduhighcat,
         al30est, alcbng30d, irpinc3, irfamin3, poverty3, coutyp2) 
#rename columns
colnames(subset_nsduh2015) <- c("ident", "date", "treatment", "alcohol", "age", "sex", "race", "marital", "edu", 
                                "drink30", "binge30", "income", "famincome", "poverty", "countytype")

#adds column 16 (treated_sober) with recovered, treated_drinking, and untreated 
data_treated_sober <- subset_nsduh2015 %>%
  mutate(treated_sober = ifelse((treatment == 1 & alcohol == 93), "recovered",
                                ifelse(treatment ==1 & alcohol != 93, "treated_drinking",
                                       "untreated")))

#adding columns that count
# Treated and Sober "recovered"
data_count_recovered_n <- data_treated_sober %>%
  mutate(recovered_n = sum(treated_sober == "recovered"))

#Treated and Sober "treated_drinking""
data_count_not_recovered_n <- data_count_recovered_n %>%
  mutate(treated_drinking_n = sum(treated_sober == "treated_drinking"))

count_untreated_n <- data_count_not_recovered_n %>%
  mutate(untreated_n= sum(treated_sober == "untreated"))

data_clean <- count_untreated_n

#creates subet of only those "recovered"
recovered_respondants <- data_clean %>%
  filter(treatment == 1 & alcohol == 93)

#creates subset of only those "treated drinking"
treated_drinking_respondants <- data_clean %>%
  filter(treated_sober == "treated_drinking")

untreated_respondants <- data_clean %>%
  filter(treated_sober == "untreated")

#VIS
#following attempt to rename the values of data_clean$income failed to produce histogram
#x needs to be continuous, not discrete
data_clean$income[data_clean$income == 1] <- "0-9,999"
data_clean$income[data_clean$income == 2] <- "10,000-19,999"
data_clean$income[data_clean$income == 3] <- "20,000-29,999"
data_clean$income[data_clean$income == 4] <- "30,000-39,999"
data_clean$income[data_clean$income == 5] <- "40,000-49,999"
data_clean$income[data_clean$income == 6] <- "50,000-74,999"
data_clean$income[data_clean$income == 7] <- "75,000 +"

data_clean$famincome[data_clean$income == 1] <- "0-9,999"
data_clean$famincome[data_clean$income == 2] <- "10,000-19,999"
data_clean$famincome[data_clean$income == 3] <- "20,000-29,999"
data_clean$famincome[data_clean$income == 4] <- "30,000-39,999"
data_clean$famincome[data_clean$income == 5] <- "40,000-49,999"
data_clean$famincome[data_clean$income == 6] <- "50,000-74,999"
data_clean$famincome[data_clean$income == 7] <- "75,000 +"

hist_income <- ggplot(data_clean, 
                      mapping = aes(x = income, color = treated_sober), stat = "count", breaks = seq(0, 7, by = 1)) +
  geom_bar(aes(y = ((..count..)/sum(..count..))), position = "dodge", alpha = 0.5) +
  geom_bar(data = recovered_respondants, 
           aes(y = ((..count..)/sum(..count..))), position = "dodge", alpha = 0.5) +
  geom_bar(data = treated_drinking_respondants, 
           aes(y = ((..count..)/sum(..count..))), position = "dodge",alpha = 0.5) +
  scale_y_continuous(labels = percent) +
  labs(title = "Individual Income Distribution", x = "Annual income($)", y = "Percent of population in income bracket")

#Great. Just need to dodge. 
# can merge data set. use .id to call individual. .id saves which table data came from. 
#one of last merges we covered in lecture. Use BIND. Row Bind. BOOM. bind_rows. 
# .id = name of table. Then specify color. then position = "dodge"



hist_income

# make color an attribute. Take out of ggplot. add fill = red for each geom_bar. 
hist_famincome <- ggplot(data_clean, 
                         mapping = aes(x = famincome, color = treated_sober)) +
  geom_bar(aes(y = ((..count..)/sum(..count..))), position = "dodge", alpha = 0.5) +
  geom_bar(data = recovered_respondants, 
           aes(y = ((..count..)/sum(..count..))), position = "dodge", alpha = 0.5) +
  geom_bar(data = treated_drinking_respondants, 
           aes(y = ((..count..)/sum(..count..))), position = "dodge",alpha = 0.5)+
  scale_y_continuous(labels = percent) +
  labs(title = "Family Income Distribution", x = "Annual income($)", y = "Percent of population in income bracket")
geom_density()

hist_famincome

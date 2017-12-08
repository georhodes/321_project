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

#renaming income and and famincome catagories. 
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

# Chi-sq test
chitest <- chisq.test(data_clean$income, data_clean$treated_sober, correct=FALSE)
chitest

chitest <- chisq.test(data_clean$treated_sober, data_clean$income, correct=FALSE)
chitest

#correlation tesst for continuous numeric data. x and y both continuous.
corobject <- cor(data_clean$edu, data_clean$income)
corobject


relabeling as factors and then ordering. 
a$edu <- as.factor(a$edu)
> levels(a$edu)
[1] "12-17 YO"        "AA/some college" "college grad"    "highschool"      "less than HS"   
> a$edu <- factor(a$edu, levels(a$edu)[c(1,5,4,3,2)])


data_clean <- within(data_clean$edu, 
                   Position <- factor(Position, 
                                      levels=(data_clean$edu)[c(1,5,4,3,2)])
hist_edu <- ggplot(a, 
                   mapping = aes(x = edu, color = treated_sober)) +
  geom_bar(data = untreated_respondents, 
           aes(y = ((..count..)/sum(..count..))), position = "dodge", alpha = 0.5) +
  geom_bar(data = recovered_respondents, 
           aes(y = ((..count..)/sum(..count..))), position = "dodge", alpha = 0.5) +
  geom_bar(data = treated_drinking_respondents, 
           aes(y = ((..count..)/sum(..count..))), position = "dodge",alpha = 0.5)+
  scale_y_continuous(labels = percent) +
  
  labs(title = "Education distribution", x = "Highest Level of Education completed", y = "Percent of population in education level") +
  facet_grid(.~treated_sober) +
  theme(axis.text.x = element_text(angle = 80, hjust = .4, vjust = .45))

hist_edu



#row bind these subsets
# data_binded <- bind_rows(untreated_respondents, treated_drinking_respondents, recovered_respondents, .id = "treated_sober")

# data_binded <- bind_rows("untreated" = untreated_respondents, "treated_drinking" = treated_drinking_respondents, "recovered" = recovered_respondents, .id = "groups")

#following attempt to rename the values of data_clean$income failed to produce histogram
#x needs to be continuous, not discrete
# data_clean$income[data_clean$income == 1] <- "0-10"
# data_clean$income[data_clean$income == 2] <- "10-20"
# data_clean$income[data_clean$income == 3] <- "20-30"
# data_clean$income[data_clean$income == 4] <- "30-40"
# data_clean$income[data_clean$income == 5] <- "40-50"
# data_clean$income[data_clean$income == 6] <- "50-75"
# data_clean$income[data_clean$income == 7] <- "75+"
# 
# data_clean$famincome[data_clean$famincome == 1] <- "0-10"
# data_clean$famincome[data_clean$famincome == 2] <- "10-20"
# data_clean$famincome[data_clean$famincome == 3] <- "20-30"
# data_clean$famincome[data_clean$famincome == 4] <- "30-40"
# data_clean$famincome[data_clean$famincome == 5] <- "40-50"
# data_clean$famincome[data_clean$famincome == 6] <- "50-75"
# data_clean$famincome[data_clean$famincome == 7] <- "75 +"
# #
# data_clean$edu[data_clean$edu == 1] <- "less than HS"
# data_clean$edu[data_clean$edu == 2] <- "highschool"
# data_clean$edu[data_clean$edu == 3] <- "AA/some college"
# data_clean$edu[data_clean$edu == 4] <- "college grad"
# data_clean$edu[data_clean$edu == 5] <- "12-17 YO"

\pagebreak

```{r, echo = FALSE, message = FALSE}
#Making Mosaic Plot
#clean data more, print out table, print mosaic plot
mosaic_total_edu <- data_clean %>%#KEY POPULATION
  select(treated_sober, edu)  #Key VARIABLES

CrossTable(table(mosaic_total_edu),
           digits = 2,
           prop.r = TRUE,
           prop.c = TRUE,
           prop.t = FALSE,
           prop.chisq = TRUE)

mosaicplot(table(mosaic_total_edu), shade = TRUE, las = 2)
chitest1 <- chisq.test(data_clean$treated_sober, data_clean$edu, correct=FALSE)
chitest


```

\pagebreak

```{r, echo = FALSE, message = FALSE}

#clean data more, print out table, print mosaic plot
mosaic_total_famincome <- data_clean %>% #KEY POPULATION
  select(treated_sober, income) #KEY VARIABLES

CrossTable(table(mosaic_total_famincome),
           digits = 2,
           prop.r = TRUE,
           prop.c = TRUE,
           prop.t = FALSE,
           prop.chisq = TRUE)

mosaicplot(table(mosaic_total_famincome), shade = TRUE, las = 2)
chitest1 <- chisq.test(data_clean$treated_sober, data_clean$edu, correct=FALSE)
chitest

```

\pagebreak

```{r, echo = FALSE, message = FALSE}

#clean data more, print out table, print mosaic plot
mosaic_recovered <- recovered_respondents %>% #KEY POPULATION
  select(income, edu) #KEY VARIABLES

CrossTable(table(mosaic_recovered), 
           digits = 2,
           prop.r = TRUE,
           prop.c = TRUE,
           prop.t = FALSE,
           prop.chisq = TRUE)

mosaicplot(table(mosaic_recovered), shade = TRUE)
```

\pagebreak

```{r, echo = FALSE, message = FALSE}

#clean data more, print out table, print mosaic plot
mosaic_treated_drinking <- treated_drinking_respondents %>%#KEY POPULATION
  select(income, edu) #KEY VARIABLES

CrossTable(table(mosaic_treated_drinking), 
           digits = 2,
           prop.r = TRUE,
           prop.c = TRUE,
           prop.t = FALSE,
           prop.chisq = TRUE)

mosaicplot(table(mosaic_treated_drinking), shade = TRUE)
```

\pagebreak

```{r, echo = FALSE, message = FALSE}

#clean data more, print out table, print mosaic plot
mosaic_no_treatment <- untreated_respondents %>%  #KEY POPULATION
  select (income, edu)  #KEY VARIABLES

CrossTable(table(mosaic_no_treatment), 
           digits = 2,
           prop.r = TRUE,
           prop.c = TRUE,
           prop.t = FALSE,
           prop.chisq = TRUE)

mosaicplot(table(mosaic_no_treatment), shade = TRUE)
```
\pagebreak
```{r, echo = FALSE, message = FALSE}

plot_binge_income <- ggplot(data = treated_respondents,
                            aes(x = income, 
                                y = binge30)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Binge Drinking Days of Last 30 Against Individual Income", x = "Annual income($1000's)", y = "Number of days Binge Drank in past 30") +
  theme_economist()
#scale_x_discrete(labels = c( "1" = "0-10", "2" = "10-20", "3" = "20-30", "4" = "30-40", "5" = "40-50", "6" = "50-75", "7" = "75+"))
plot_binge_income

lm_binge_income <- lm(binge30 ~ income, data = treated_respondents)
summary(lm_binge_income)
lm_binge_income <- lm(binge30 ~ factor(income), data = treated_respondents)
summary(lm_binge_income)

plot_binge_famincome <- ggplot(data = treated_respondents,
                               aes(x = famincome, 
                                   y = binge30)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)

plot_binge_famincome

lm_binge_famincome <- lm(binge30 ~ factor(famincome), data = treated_respondents)
summary(lm_binge_famincome)

lm_binge_famincome <- lm(binge30 ~ famincome, data = treated_respondents)
summary(lm_binge_famincome)

```


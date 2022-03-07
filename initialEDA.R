#crimedata <- read.csv("data/dc-crimes-search-results.csv")

#getting the data for 2020 and 2021
crimedata2 <- crimedata[crimedata$YEAR == '2020', ]
crimedata3 <- crimedata[crimedata$YEAR == '2021', ]

#putting the data for 2020 and 2021 in one database
crimedata4 <- rbind(crimedata2, crimedata3)

#figuring out which method of categorization would have the least number of missing values

table(is.na(crimedata4$YEAR)) #no missing values
table(is.na(crimedata4$DISTRICT)) #195 missing values
table(is.na(crimedata4$WARD)) #27 missing values
table(is.na(crimedata4$VOTING_PRECINCT)) #no missing values
table(is.na(crimedata4$offensegroup)) #no missing values
table(is.na(crimedata4$offense.text)) #no missing values
table(is.na(crimedata4$SHIFT)) #no missing values
table(is.na(crimedata4$OFFENSE)) #no missing values
table(is.na(crimedata4$METHOD)) #no missing values

#after we divide up the data according to the years we'd like to use, 
#we need to figure out which variable to use as an aggregator  
#since the observations are currently individual incidents and we'd need to group them to run tests

library(dplyr)

ward8 <- subset(crimedata4[crimedata4$WARD == 8, ])
ward7 <- subset(crimedata4[crimedata4$WARD == 7, ])
ward6 <- subset(crimedata4[crimedata4$WARD == 6, ])
ward5 <- subset(crimedata4[crimedata4$WARD == 5, ])
ward4 <- subset(crimedata4[crimedata4$WARD == 4, ])
ward3 <- subset(crimedata4[crimedata4$WARD == 3, ])
ward2 <- subset(crimedata4[crimedata4$WARD == 2, ])
ward1 <- subset(crimedata4[crimedata4$WARD == 1, ])

#looking at the number of observations for the years 2020 and 2021 for no real reason
table(ward8$YEAR == 2020) #2020 - 2474, 2021 - 2930
table(ward7$YEAR == 2020) #2020 - 3466, 2021 - 3595
table(ward6$YEAR == 2020)
table(ward5$YEAR == 2020)
table(ward4$YEAR == 2020)
table(ward3$YEAR == 2020)
table(ward2$YEAR == 2020)
table(ward1$YEAR == 2020)

#we can't really compare the changes across the wards unless we look at the area 
#and population of the wards since the observations themselves are incidents  
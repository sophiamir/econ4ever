crimedata <- read.csv("data/dc-crimes-search-results.csv")

#getting the data for 2020 and 2021
crimedata2 <- crimedata[crimedata$YEAR == '2020', ]
crimedata3 <- crimedata[crimedata$YEAR == '2021', ]

#changing the blank spaces to NAs
crimedata2[crimedata2 == "" | crimedata2 == " "] <- NA
crimedata3[crimedata3 == "" | crimedata3 == " "] <- NA

#figuring out which date data to use 
#year 2020
table(is.na(crimedata2$END_DATE)) #22425 missing values
table(is.na(crimedata2$START_DATE)) #1 missing value
table(is.na(crimedata2$REPORT_DAT)) #0 missing values

#year 2021
table(is.na(crimedata3$REPORT_DAT)) #0 missing values
table(is.na(crimedata3$END_DATE)) #2649 missing values
table(is.na(crimedata3$START_DATE)) #3 missing values

#based on the number of missing values we should use the variable REPORT_DAT as the date

#putting the data for 2020 and 2021 in one database
crimedata4 <- rbind(crimedata2, crimedata3)

#converting REPORT_DAT from character to date format

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
table(ward6$YEAR == 2020) #2020 - 3186, 2021 - 3797
table(ward5$YEAR == 2020) #2020 - 3584, 2021 - 4413
table(ward4$YEAR == 2020) #2020 - 2130，2021 - 2578
table(ward3$YEAR == 2020) #2020 - 1240，2021 - 1441
table(ward2$YEAR == 2020) #2020 - 3486，2021 - 5361
table(ward1$YEAR == 2020) #2020 - 2860，2021 - 4197

#we can't really compare the changes across the wards unless we look at the area 
#and population of the wards since the observations themselves are incidents  

#remove unuseful columns
modified_crime = subset(crimedata,select = c(2,9,10,11,12,23,26,27))

#descriptive stats
summary1 <- summary(modified_crime)
summary1
install.packages("vtable")
library(vtable)
table1 <- sumtable(modified_crime)

#relationship between ward and OFFENSE
two_way = table(modified_crime$WARD, modified_crime$OFFENSE)
two_way
prop.table(two_way)#cell percentages
prop.table(two_way,1)#row percentages
prop.table(two_way,2)#column percentages
chisq.test(two_way)
#Since the p value is less than 0.05, we reject the null hypothesis that ward(geographical area) is not associated with offense type.

#types of crimes happen during the day/evening/midnight
two_way1 = table(modified_crime$SHIFT,modified_crime$OFFENSE)
two_way1
prop.table(two_way1,1)#row percentages
#During the day, the most frequent crime is theft for auto. The most frequent crime in the evening is theft for other things except autos. In the midnight, the most frequent crime is
#theft for other things except autos.
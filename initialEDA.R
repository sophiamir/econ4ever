
#crimedata <- read.csv("data/dc-crimes-search-results.csv")

crimedata[crimedata == "" | crimedata == " "] <- NA

#subsetting the database to the data we want
crimedata1 <- crimedata[, c(2,6,9,10,11,12,23,26,27)]

#converting REPORT_DAT from character to date format 
crimedata1$REPORT_DAT <- as.Date(crimedata1$REPORT_DAT, "%m/%d/%Y")


#getting the data for 2020 and 2021
crimedata2 <- crimedata[crimedata$YEAR == '2020', ]
crimedata3 <- crimedata[crimedata$YEAR == '2021', ]


table(crimedata1$offensekey)
table(crimedata1$offense.text)
table(crimedata1$OFFENSE)


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

crimedata4$REPORT_DAT <- as.Date(crimedata4$REPORT_DAT, "%m/%d/%Y")

#now we need to figure out how to convert the date into just months or create a new variable for just months


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

table(ward6$YEAR == 2020)
table(ward5$YEAR == 2020)
table(ward4$YEAR == 2020)
table(ward3$YEAR == 2020)
table(ward2$YEAR == 2020)
table(ward1$YEAR == 2020)

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


#What types of offenses are more common at different times of year?
  

monthdata <- as.data.frame(month(as.POSIXlt(crimedata1$REPORT_DAT, format="%m/%d/%Y")))

offensedata <- as.data.frame(as.factor(crimedata1$OFFENSE))

setnames(monthdata, 'month(as.POSIXlt(crimedata1$REPORT_DAT, format = "%m/%d/%Y"))', 'month')
setnames(offensedata, 'as.factor(crimedata1$OFFENSE)', "offense")


#graphs of incidences for each ward

ward1 %>% ggplot(aes(x = as.factor(OFFENSE), fill = SHIFT)) +
  geom_histogram(binwidth = 1, stat = "count") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.grid.major.y = element_line(color = "light pink"),
        #  panel.grid.minor.y = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = c(seq(0,5000, by=500),9999)) +labs(x = "Type of Offense", fill = "Time", y = "Frequency", title = "Types of Crimes Common at Different Times of Day in Ward 1")


ward2 %>% ggplot(aes(x = as.factor(OFFENSE), fill = SHIFT)) +
  geom_histogram(binwidth = 1, stat = "count") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.grid.major.y = element_line(color = "light blue"),
        #  panel.grid.minor.y = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = c(seq(0,5000, by=500),9999)) +
  labs(x = "Type of Offense", fill = "Time", y = "Frequency", title = "Types of Crimes Common at Different Times of Day in Ward 2")


ward3 %>% ggplot(aes(x = as.factor(OFFENSE), fill = SHIFT)) +
  geom_histogram(binwidth = 1, stat = "count") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.grid.major.y = element_line(color = "light green"),
        #  panel.grid.minor.y = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = c(seq(0,2000, by=200),9999)) +
  labs(x = "Type of Offense", fill = "Time", y = "Frequency", title = "Types of Crimes Common at Different Times of Day in Ward 3")


ward4 %>% ggplot(aes(x = as.factor(OFFENSE), fill = SHIFT)) +
  geom_histogram(binwidth = 1, stat = "count") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.grid.major.y = element_line(color = "gold"),
        #  panel.grid.minor.y = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = c(seq(0,2000, by=200),9999)) +
  labs(x = "Type of Offense", fill = "Time", y = "Frequency", title = "Types of Crimes Common at Different Times of Day in Ward 4")


ward5 %>% ggplot(aes(x = as.factor(OFFENSE), fill = SHIFT)) +
  geom_histogram(binwidth = 1, stat = "count") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.grid.major.y = element_line(color = "turquoise"),
        #  panel.grid.minor.y = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = c(seq(0,3000, by=300),9999)) +
  labs(x = "Type of Offense", fill = "Time", y = "Frequency", title = "Types of Crimes Common at Different Times of Day in Ward 5")


ward6 %>% ggplot(aes(x = as.factor(OFFENSE), fill = SHIFT)) +
  geom_histogram(binwidth = 1, stat = "count") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.grid.major.y = element_line(color = "purple"),
        #  panel.grid.minor.y = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = c(seq(0,4000, by=400),9999)) +
  labs(x = "Type of Offense", fill = "Time", y = "Frequency", title = "Types of Crimes Common at Different Times of Day in Ward 6")


ward7 %>% ggplot(aes(x = as.factor(OFFENSE), fill = SHIFT)) +
  geom_histogram(binwidth = 1, stat = "count") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.grid.major.y = element_line(color = "lime green"),
        #  panel.grid.minor.y = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = c(seq(0,4000, by=300),9999)) +
  labs(x = "Type of Offense", fill = "Time", y = "Frequency", title = "Types of Crimes Common at Different Times of Day in Ward 7")


ward8 %>% ggplot(aes(x = as.factor(OFFENSE), fill = SHIFT)) +
  geom_histogram(binwidth = 1, stat = "count") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.grid.major.y = element_line(color = "sky blue"),
        #  panel.grid.minor.y = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = c(seq(0,4000, by=300),9999)) +
  labs(x = "Type of Offense", fill = "Time", y = "Frequency", title = "Types of Crimes Common at Different Times of Day in Ward 8")


# total property incidences for both years 
wardcrime21 <- subset(crimedata1,YEAR==2021)
wardcrime20 <- subset(crimedata1,YEAR==2020)
wardcrime21$totalproperty <- wardcrime21$`property|burglary`+wardcrime21$`property|motor vehicle theft`+wardcrime21$`property|theft f/auto`+wardcrime21$`property|theft/other`
wardcrime20$totalproperty <- wardcrime20$`property|burglary`+wardcrime20$`property|motor vehicle theft`+wardcrime20$`property|theft f/auto`+wardcrime20$`property|theft/other`
cor.test(wardcrime20$totalproperty,wardcrime20$totalviolent)

# total violent incidences for both years 
wardcrime21$totalviolent <- wardcrime21$`violent|assault w/dangerous weapon`+wardcrime21$`violent|homicide`+wardcrime21$`violent|robbery`+wardcrime21$`violent|sex abuse`
wardcrime20$totalviolent <- wardcrime20$`violent|assault w/dangerous weapon`+wardcrime20$`violent|homicide`+wardcrime20$`violent|robbery`+wardcrime20$`violent|sex abuse`

# cor test with Pearson
cor.test(wardcrime21$totalproperty,wardcrime21$totalviolent,method=c("pearson"))
cor.test(wardcrime20$totalproperty,wardcrime20$totalviolent,method=c("pearson"))

# cor test with Spearman
cor.test(wardcrime21$totalproperty,wardcrime21$totalviolent,method=c("spearman"))
cor.test(wardcrime20$totalproperty,wardcrime20$totalviolent,method=c("spearman"))

#Is the method of the crime (gun, knife, others) dependent on the offense type?
chisq.test(crime$METHOD,crime$offensegroup)

#and population of the wards since the observations themselves are incidents  


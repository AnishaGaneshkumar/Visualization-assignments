

# ANISHA GANESHKUMAR, SECTION 01, IE6600
library(dplyr)
nydata<-read.csv("NYPD_Motor_Vehicle_Collisions_-_Crashes.csv",sep=",", stringsAsFactors = FALSE)

# Question 1
ans1 <- summarise(group_by(nydata,BOROUGH),sum_injured = sum(NUMBER.OF.PERSONS.INJURED, na.rm = TRUE))
ans1 

# Question 2
ans2 <- summarise(group_by(nydata,BOROUGH),sum_killed = sum(NUMBER.OF.PERSONS.KILLED, na.rm = TRUE))
ans2

# Question 3
df1 <-filter(nydata,BOROUGH == "BRONX")
df2 <- summarise(group_by(df1,LOCATION), sum_injured = sum(NUMBER.OF.PERSONS.INJURED, na.rm = TRUE))
df2 <- arrange(df2,desc(sum_injured))
ans3 <- df2[1:10,]
ans3 

# Question 4
nydatacopy <- nydata
library(stringr)
nydatacopy$BOROUGH <- str_replace_na(nydatacopy$BOROUGH, replacement = "NA")
nydatacopy$ADDRESS <- str_c(nydatacopy$BOROUGH,",",nydatacopy$ZIP.CODE,",",nydatacopy$ON.STREET.NAME)
nydatacopy$ZIP.CODE <- str_replace_na(nydatacopy$ZIP.CODE, replacement = "NA")
nydata$ADDRESS <- str_c(nydatacopy$BOROUGH,",",nydatacopy$ZIP.CODE,",",nydatacopy$ON.STREET.NAME)
nydata

# Question 5
df3 <- filter(nydata,BOROUGH =="MANHATTAN")
df4 <- summarize(group_by(df3,TIME), avg_injured = mean(NUMBER.OF.PERSONS.INJURED))
df4 <- arrange(df4,desc(avg_injured))
ans5 <- df4[1:10,]
ans5

# Question 6
library(lubridate)
typeof(nydata$DATE)
nydata$DATE <- mdy(nydata$DATE)
nydata$DATE <- as_date(nydata$DATE)
nydata$YEAR <- year(nydata$DATE)
ans6 <- summarize(group_by(nydata,YEAR),sum.injured = sum(NUMBER.OF.PERSONS.INJURED, na.rm = TRUE),sum.killed = sum(NUMBER.OF.PERSONS.KILLED, na.rm = TRUE))
ans6

# Question 7
library(readr)
crime <- read_csv("tmpanqa9zt3.csv")
crimedata <- crime[,c(3,5)]
a <- summarize(group_by(crimedata,OFFENSE_CODE_GROUP,DISTRICT),n = n())
a <- na.omit(a)
dis<-unique(a$DISTRICT)
off <-unique(a$OFFENSE_CODE_GROUP)
ma <- matrix(0, nrow = length(off), ncol = length(dis), byrow = TRUE)
colnames(ma)<- dis
rownames(ma)<- off
a.mtx <- as.matrix(a)
ma[a.mtx[,1:2]]<-a.mtx[,3]
View(ma)





# ANISHA GANESHKUMAR, SECTION 01, IE6600
library(ggplot2)
library(dplyr)
airline_delay <- read.csv("~/Visualization/R/airline_delay.csv", stringsAsFactors=FALSE)

# Question 1
airline_delay[is.na(airline_delay)] <- 0
df1 <- summarise(group_by(airline_delay,carrier_name), arr_delay = sum(arr_delay, na.rm = TRUE))
ans1<-ggplot(df1,aes(x=reorder(carrier_name,arr_delay),y =arr_delay))+geom_bar(stat = "identity")+coord_flip()+labs(x="Carrier Name",y="Total Arrival Delay")
ans1

# Question 2
df2 <- summarise(group_by(airline_delay,carrier_name,year), arr_delay = sum(arr_delay,na.rm = TRUE))
ans2<-ggplot(df2, aes(x=reorder(factor(carrier_name),arr_delay),y=arr_delay,fill=factor(year))) +geom_bar(stat="identity") +coord_flip()+labs(x="Carrier Name",y="Total Arrival Delay",fill="Year")
ans2

# Question 3
df3 <-summarise(group_by(airline_delay,airport,year),arr_delay = sum(arr_delay,na.rm = TRUE))
df3 <-filter(df3,airport=="SFO"|airport=="ORD"|airport=="LGA"|airport=="LAX"|airport=="JFK"|airport=="EWR"|airport=="DFW"|airport=="DEN"|airport=="BOS"|airport=="ATL")
ans3<-ggplot(df3, aes(year,airport,fill= arr_delay))+geom_tile()+labs(x="Year",y="Airport Name",fill= "Total Arrival Delay")
ans3

# Question 4
d4 <- summarise(group_by(airline_delay,year), Delay_mins = sum(carrier_delay,na.rm = TRUE))
d4$Delay_Type <- "Carrier Delay"
d5 <- summarise(group_by(airline_delay,year), Delay_mins = sum(late_aircraft_delay,na.rm = TRUE))
d5$Delay_Type <- "Late Aircraft Delay"
d6 <- rbind(d4,d5)
ans4<-ggplot(d6, aes(x=year, y=Delay_mins, group=Delay_Type, color=Delay_Type)) +geom_line()+geom_point()+ggtitle("Carrier v/s Late Aircraft Delay")+labs(y="Mins")
ans4

# Question 5
df5 <- filter(airline_delay,airport=="BOS"|airport=="ATL"|airport=="JFK")
df5 <- summarise(group_by(df5,airport,arr_delay))
ans5plot1 <- ggplot(df5, aes(x=factor(airport), y=arr_delay)) + geom_boxplot()+labs(x="Airport",y="Arriving Delay in Mins")+ggtitle("Boxplot")
ans5plot1
ans5plot2 <- ggplot(df5, aes(x=factor(airport), y=arr_delay)) + geom_violin()+labs(x="Airport",y="Arriving Delay in Mins")+ggtitle("Violin")
ans5plot2
ans5plot3 <- ggplot(df5, aes(x = arr_delay, fill=airport))+geom_density(alpha=0.5)+ggtitle("Density Plot")+labs(x="Arriving Delay(Mins)",y="Density")
ans5plot3


# Question 6

enplanements <- read_excel("C:/Users/anish/Downloads/cy17-commercial-service-enplanements.xlsx")
x = summarise(group_by(airline_delay,airport), arr_delay = sum(arr_delay,na.rm = TRUE))
y = select(enplanements,Locid,`CY 17 Enplanements`)
names(y)= c("airport","Enplanements")
df6 <-merge(x,y,by="airport")
ggplot(df6,aes(df6$Enplanement,df6$arr_delay))+geom_point()+labs(x="Enplanement",y="Arrival Delay")
library(scales)
ggplot(df6, aes(x = df6$Enplanements, y = df6$arr_delay)) + geom_point() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+ labs(x="Enplanement",y="Arrival Delay")



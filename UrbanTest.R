#Kris Stevens
#Urban Submission

library(ggplot2)
library(grid)
library(ggthemes)
library(RColorBrewer)

#Read in Data
df<-read.csv(file="C:/temp/Urban/urban.csv", stringsAsFactors = FALSE)

#Subset data
df1<- df[,c(6:13,18,23)]

#Create new rows for calculations
df1$Population<-rowSums(df1[,1:8])
df1$TotalDist<-df1$Population*df1$dis_rating_above_6

#Aggregate data
df2<-(df1[10:12])
df2<-aggregate(. ~ school_name, data=df2, FUN=sum)

#Create an average distance for each school
df2$AvgDist <- df2$TotalDist/df2$Population

#Drop unneeded columns
df2<-df2[,c(1,4)]
colnames(df2)<- c("School", "AVGDistance")

#Plot the results
ggplot(data=df2, aes(x=reorder(School, -AVGDistance), y=AVGDistance)) +
  xlab("School")+
  geom_bar(stat="identity", fill="#1696d2",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
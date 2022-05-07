# Packages installation

install.packages("ggplot2")
install.packages("dplyr")
install.packages("caret")
install.packages('ggExtra')
install.packages('ggcorrplot')
install.packages('GGally')
install.packages('corrgram')
install.packages('DataExplorer')
install.packages('waffle')


install.packages('gridExtra')
install.packages('alluvial')
install.packages('extrafont')
install.packages('plyr')
install.packages('plotly')

# Importing Libraries
library(plyr)
library(dplyr)
library(plotly)
library(knitr)

library(gridExtra)
library(alluvial)
library(extrafont)
library(waffle)
library(ggplot2)
library(viridis)
library(timetk)
library(tidyr)
library(dplyr)
library(ggExtra)
library(ggcorrplot)
library(GGally)
library(corrgram)
library(DataExplorer)


# Loading the dataset
student <- read.csv(file = "/Users/mamadouourydiallo/Downloads/student.csv", sep = ",", stringsAsFactors = T)
student

# summary  of the table
summary(student)

# checking the summary of the missing value for each column.
sum(is.na(student))

apply(student, 2, function(col)sum(is.na(col))/length(col))



# School and gender
con_3 <- ggplot(student, aes(x=Dalc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("School")+
  ggtitle("Workday alcohol consumption per school and sex")

con_4 <- ggplot(student, aes(x=Walc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Weekend alcohol consumption")+
  ylab("School")+
  ggtitle("Weekend alcohol consumption per school and sex")

grid.arrange(con_3,con_4, nrow=2)


# Alcohol and grades

#windowsFonts(FontAwesome=windowsFont("FontAwesome"))

alcohol.d <- as.data.frame(table(student$Dalc))
par.d <- as.numeric(alcohol.d$Freq)
names(par.d) <- alcohol.d$Var1
par.d <- round(par.d/10)

waffle.col <- c("#00d27f","#adff00","#f9d62e","#fc913a","#ff4e50")

con_1 <- waffle(par.d, rows=5, 
             #use_glyph="glass", 
             size=2, 
             title = "Workday alcohol consumption among students",
             glyph_size=8,
             xlab="1 glass == 10 students",
             colors=waffle.col,
             legend_pos= "top"
)

alcohol.w <- as.data.frame(table(student$Walc))
par.w <- as.numeric(alcohol.w$Freq)
names(par.w) <- alcohol.w$Var1
par.w <- round(par.w/10)

con_2 <- waffle(par.w, rows=5, 
             #use_glyph="glass", 
             size=2, 
             title = "Weekend alcohol consumption among students",
             glyph_size=8,
             xlab="1 glass == 10 students",
             colors=waffle.col,
             legend_pos= "top"
)

grid.arrange(con_1,con_2, nrow=2)


# Age
ggplot(student, aes(x=(as.numeric(age))))+ geom_histogram(fill="green", colour="black",binwidth=1) +
  facet_grid(Dalc ~ .)+ggtitle("Distribution based on Age and levels of Workday Consumption")

# Distribution based on Age and levels of alcohol consumption
ggplot(student, aes(x=(as.numeric(age))))+ geom_histogram(fill="blue", colour="black",binwidth=1) +
  facet_grid(Walc ~ .)+ggtitle("Distribution based on Age and levels of Weekend Consumption")

# Distribution based on Age and Whether of not they are in a relationship

ggplot(student, aes(x=(as.numeric(age))))+ geom_histogram(fill="red", colour="black",binwidth=1) +
  facet_grid(romantic ~ .)+ggtitle("Distribution based on Age and Whether of not they are in a relationship")

# Status of the health
ggplot(student, aes(x=(as.numeric(age))))+ geom_histogram(fill="red", colour="black",binwidth=1) +
  facet_grid(health ~ .)+ggtitle("Distribution based on Age and Health")

# Gender
student %>% group_by(sex) %>% summarise(n=n()) %>%
  ggplot(aes(x=sex,y=n))+ geom_bar(stat="identity")

# Sex identity
iden <- student %>% group_by(sex,Dalc) %>% summarise(n=n())

acty <- ddply(iden, "sex", transform,
            percent = n / sum(n) * 100)
ggplot(acty, aes(x=sex, y=percent, fill=Dalc)) +
  geom_bar(stat="identity")

# Sex Wal
iden <- student %>% group_by(sex,Walc) %>% summarise(n=n())
iden$Walc <- as.factor(iden$Walc)

acty <- ddply(iden, "sex", transform,
            percent = n / sum(n) * 100)
ggplot(acty, aes(x=sex, y=percent, fill=Walc)) +
  geom_bar(stat="identity")

# Health and Grades

student %>% mutate(avg_grade=G3,health=as.factor(health)) %>% group_by(health,sex) %>% summarise(n=mean(avg_grade)) %>%
  ggplot(aes(x=sex,y=n,fill=health))+geom_bar(stat="identity",position = "dodge")+ggtitle("Health and Grades")

# Do romantic relationships affect grades?
student %>% group_by(romantic) %>% summarise(n=mean(G3,na.rm=T)) %>%
  ggplot(aes(x=romantic,y=n))+geom_bar(stat="identity")

# Distrubution of the socre

ggplot(student, aes(x=G3, fill=romantic)) +
  geom_histogram(position="identity", alpha=0.4,binwidth=1.0)

# Romantic Relationship,Grades and Gender
student %>% group_by(sex,romantic) %>% summarise(n=mean(G3,na.rm=T)) %>%
  ggplot(aes(x=sex, y=n, fill=romantic)) +
  geom_bar(position="dodge",stat="identity")+ggtitle("Romantic Relationship,Grades and Gender")


# Activities and Romantic Relationships
temp <- student %>% group_by(romantic,activities) %>% summarise(n=n()) 
Acty <- ddply(temp, "activities", transform,
            percent_weight = n / sum(n) * 100)
ggplot(Acty, aes(x=activities, y=percent_weight, fill=romantic)) +
  geom_bar(stat="identity")+geom_text(aes(label=percent_weight), vjust=1.5, colour="white",
                                      position=position_dodge(.9), size=3)+ggtitle("Activities and Romantic Relationships")



# plotting the percentage sales for both female and male

moth<-student %>% filter(sex=="M")%>% group_by(romantic,activities) %>% summarise(n=n()) %>%
  ddply("activities",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=activities,y=percent,fill=romantic))+
  geom_bar(stat="identity")+ggtitle("Male Students")+geom_text(aes(label=percent), vjust=1.5, colour="white")

fat<-student %>% filter(sex=="F")%>% group_by(romantic,activities) %>% summarise(n=n()) %>%
  ddply("activities",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=activities,y=percent,fill=romantic))+
  geom_bar(stat="identity")+ggtitle("Female Students")+geom_text(aes(label=percent), vjust=1.5, colour="white")

grid.arrange(moth,fat)

# Average Number of Hours spent for studying
student %>% group_by(romantic) %>% summarise(mean_hour=mean(studytime,na.rm=T)) %>%
  ggplot(aes(x=romantic,y=mean_hour))+geom_bar(stat="identity")+ ggtitle("Mean Number of Hours spent for studying")


# Percentage of Students of each category who have romantic relationships
student %>% group_by(romantic,goout) %>% summarise(n=n()) %>%
  ddply("goout",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=goout,y=percent,fill=romantic))+
  geom_bar(stat="identity")+ggtitle("Percentage of Students of each category who have romantic relationships")


# Parent’s Education

student %>% group_by(Medu,higher) %>% summarise(n=n()) %>%
  ddply("Medu",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=Medu,y=percent,fill=higher))+
  geom_bar(stat="identity")+ggtitle("Percentage of students who have plans for higher education vs Mother's Education Levels")


student %>% group_by(Fedu,higher) %>% summarise(n=n()) %>%
  ddply("Fedu",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=Fedu,y=percent,fill=higher))+
  geom_bar(stat="identity")+ggtitle("Percentage of students who have plans for higher education vs Father's Education Levels")

# Prents Eduaction affection on students levels
student %>% mutate(failures=as.factor(failures))%>% group_by(Medu,failures) %>% summarise(n=n()) %>%
  ddply("Medu",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=Medu,y=percent,fill=failures))+
  geom_bar(stat="identity")+ggtitle("Number of failures(%) and education level of the mother")

student %>% mutate(failures=as.factor(failures))%>% group_by(Fedu,failures) %>% summarise(n=n()) %>%
  ddply("Fedu",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=Fedu,y=percent,fill=failures))+
  geom_bar(stat="identity")+ggtitle("Number of failures(%) and education level of the father")

# Internet Access(%) and education level of the mother
student %>% group_by(Medu,internet) %>% summarise(n=n()) %>%
  ddply("Medu",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=Medu,y=percent,fill=internet))+
  geom_bar(stat="identity")+ggtitle("Internet Access(%) and education level of the mother")


student %>% group_by(Fedu,internet) %>% summarise(n=n()) %>%
  ddply("Fedu",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=Fedu,y=percent,fill=internet))+
  geom_bar(stat="identity")+ggtitle("Internet Access(%) and education level of the father")


# Work Day Alcohol Consumption and education level of the mother 
student %>% group_by(Medu,Dalc) %>% summarise(n=n()) %>%
  ddply("Medu",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=Medu,y=percent,fill=Dalc))+
  geom_bar(stat="identity")+ggtitle("Work Day Alcohol Consumption and education level of the mother")

student %>% group_by(Fedu,Dalc) %>% summarise(n=n()) %>%
  ddply("Fedu",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=Fedu,y=percent,fill=Dalc))+
  geom_bar(stat="identity")+ggtitle("Work Day Alcohol Consumption and education level of the father")

# consumption of alcohool based on the weekend
student %>%mutate(Walc=as.factor(Walc)) %>% group_by(Medu,Walc) %>% summarise(n=n()) %>%
  ddply("Medu",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=Medu,y=percent,fill=Walc))+
  geom_bar(stat="identity")+ggtitle("Weekend Alcohol Consumption and education level of the mother")


student %>%mutate(Walc=as.factor(Walc)) %>% group_by(Fedu,Walc) %>% summarise(n=n()) %>%
  ddply("Fedu",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=Fedu,y=percent,fill=Walc))+
  geom_bar(stat="identity")+ggtitle("Weekend Alcohol Consumption and education level of the father")

# Parents Education Level
student %>% mutate(if_same=ifelse(Fedu==Medu,"Yes","No")) %>% group_by(if_same) %>% summarise(n=n()) %>%
  ggplot(aes(x=if_same,y=n))+geom_bar(stat="identity")+ggtitle("Number of Parents who have/do not have same educational levels") + geom_text(aes(label=n), vjust=1.5, colour="white")

# Parents Different Eduaction Leveles

MotFat_1 <- student %>% mutate(if_less=ifelse(Medu<Fedu,"Less Than","No")) %>% filter(if_less!="No") %>% select(Medu,Fedu,if_less)
MotFat_2 <- student %>% mutate(if_less=ifelse(Medu>Fedu,"Greater Than","No")) %>% filter(if_less!="No") %>% select(Medu,Fedu,if_less)

temp <- rbind(MotFat_1,MotFat_2)

temp %>% group_by(if_less) %>% summarise(n=n()) %>%
  ggplot(aes(x=if_less,y=n)) +geom_bar(stat="identity")+geom_text(aes(label=n), vjust=1.5, colour="white")+ggtitle("Number of Cases where the mother has higher/lower education level")









install.packages(c("tidyverse","haven","readxl","dslabs","plyr","descriptr","expss","dplyr","tidyr"))
library(tidyverse); library(haven); library(readxl); library(dslabs);library(plyr);library(descriptr);library(expss);library(dplyr);library(tidyr)
library(psych)
library(stats)
library(ggplot2)

#Preparing the DataSet

survey <-read_xlsx("raw_data_mps_group_lead_5.xlsx") # Reading the Excel File into R (deleted the first row and adjusted the variable name)
save("survey", file="survey.RData") #saveing the files and converting them into RData files
load(file="survey.RData") # Loading the prepared data (the data frame “survey”)
survey <- filter(survey, QUO==1 ) # only use data with quota of 100%
survey <- filter(survey, DUR>120, DUR<1600) # only use data with duration bigger then 120 sec. and smaller then 1600 sec.
id <- data.frame(ID= seq(from=1, to=368)) # creating an ID column with numbers from 1 to 369
survey <- bind_cols(id, survey) # Appending “id” to “survey” as a new column
survey[2:4] <- list(NULL)# deleting the unnecessary columns (wrong ID (ID_W), Date (DAT), Confirmation (CON))
survey[48:50] <- list(NULL)# deleting further unnecessary columns (Anonymous (ANO), Completed (COM), Finished (FIN))
survey[49:65] <- list(NULL)# deleting further unnecessary columns (no adjusted names in the Excel sheet, including Quota)

survey$OCC1[survey$OCC1==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected
survey$OCC2[survey$OCC2==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected
survey$OCC3[survey$OCC3==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected
survey$OCC4[survey$OCC4==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected
survey$OCC5[survey$OCC5==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected
survey$OCC6[survey$OCC6==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected
survey$OCC7[survey$OCC7==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected

survey$HOU1[survey$HOU1==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected
survey$HOU2[survey$HOU2==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected
survey$HOU3[survey$HOU3==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected
survey$HOU4[survey$HOU4==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected
survey$HOU5[survey$HOU5==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected
survey$HOU6[survey$HOU6==-1]<-0 #we are changing the multiple choice values from -1 to 0 when they are not selected

survey=survey %>% mutate(ITK3 = case_when(ITK3 == 1 ~ 5,
                                   ITK3 == 2 ~ 4,
                                   ITK3 == 3 ~ 3,
                                   ITK3 == 4 ~ 2,
                                   ITK3 == 5 ~ 1))
survey=survey %>% mutate(ITK4 = case_when(ITK4 == 1 ~ 5,
                                    ITK4 == 2 ~ 4,
                                    ITK4 == 3 ~ 3,
                                    ITK4 == 4 ~ 2,
                                    ITK4 == 5 ~ 1))

survey = apply_labels(survey, ITU1="I am willing to use MPS.", ITU2="I use MPS on a daily basis.", ITU3="I would encourage others to use MPS.", ITU4="I think that it is valuable to me to make mobile payments.", USE1="I perceive MPS as useful.", USE2="Using MPS is more convenient compared to paying by cash or card.", USE3="Using MPS accelerates the payment process for me.", USE4="By using MPS, I have more control over my payments.", EOU1="I would find MPS easy to use.", EOU2="I would find it easy to learn how to use MPS.", EOU3="I would find using MPS clear and understandable.", EOU4="I would have easy access to a mobile device that allows me to use MPS.", SOI1="Using MPS improves my social status.", SOI2="I am more likely to use MPS if someone I trust recommends them.", SOI3="The more I see people using MPS, the more likely it is that I will use them too.", SOI4="MPS align with the values of my social group.", ITK1="I have the skills necessary to use MPS.", ITK2="I could install and set up MPS if there is no one around to assist me.", ITK3="Completing a mobile payment is so complicated that it is difficult to understand how it works.", ITK4="I prefer paying by cash or card over MPS as it does not require additional knowledge.", TRU1="I have trust in using MPS.", TRU2="I have trust that my data will stay protected while using MPS.", TRU3="I trust that MPS are secure enough for me to use.", TRU4="I trust my MPS provider to ensure fast and secure transactions.", RIS1="I am concerned about losing money while using MPS.", RIS2="I am afraid that the usage of MPS might lead to overspending.", RIS3="I am worried about potential malfunctions while using MPS.", RIS4="I am concerned about the potential exposure of my personal data while using MPS.", GEN="Gender", AGE="Age in years", EDU="Highest level of education", OCC1="Unemployed", OCC2="Employment (private sector)", OCC3="Employment (public sector)", OCC4="Self-employment", OCC5="Retired", OCC6="In education/training/student", OCC7="Other employment", INC="Monthly net income", HOU1="Alone", HOU2="With partner", HOU3="With parents", HOU4="With children", HOU5="Shared flat", HOU6="Other", MUN="Size of municipality", DUR="Duration of completion in seconds") # changing the headlines

save("survey", file="survey.RData") #saveing the file

#Start with the Analysis

#Check for Relations
alpha(survey[,c("ITK1","ITK2","ITK3","ITK4")])
alpha(survey[,c("ITU1","ITU2","ITU3","ITU4")])
alpha(survey[,c("ITU1","ITU2","ITU3","ITU4","ITK1","ITK2","ITK3","ITK4","EDU")])

#Adding Means of all ITUs and ITKs
survey$ITU <- with(survey, (ITU1 + ITU2 + ITU3 + ITU4) / 4)
survey$ITK <- with(survey,(ITK1+ITK2+ITK3+ITK4)/4)

#Analysing th Intention to Use MPS

#Hystograms of ITU
ggplot(survey,aes(x=ITU))+geom_histogram(bins=30)+
  geom_vline(aes(xintercept = mean(ITU)),color = "red",linetype = "dashed", size = 1)+
  geom_vline(aes(xintercept = median(ITU)),color = "green",linetype = "dashed", size = 1)+
  ggtitle("Histogram of Intention to Use") +
  xlab("Intention to Use") +
  ylab("Frequency")

table(survey$ITU1)
summary(survey$ITU1)
ggplot(survey,aes(x=ITU1))+geom_histogram(bins=10)+
  geom_vline(xintercept = mean(survey$ITU1),color = "red",linetype = "dashed", size = 1)+
  geom_vline(xintercept = median(survey$ITU1),color = "green",linetype = "dashed", size = 1)+
  ggtitle("Histogram of ITU1") +
  xlab("IT Knowledge") +
  ylab("Frequency")

table(survey$ITU2)
summary(survey$ITU2)
ggplot(survey,aes(x=ITU2))+geom_histogram(bins=10)+
  geom_vline(xintercept = mean(survey$ITU2),color = "red",linetype = "dashed", size = 1)+
  geom_vline(xintercept = median(survey$ITU2),color = "green",linetype = "dashed", size = 1)+
  ggtitle("Histogram of ITU2") +
  xlab("IT Knowledge") +
  ylab("Frequency")

table(survey$ITU3)
summary(survey$ITU3)
ggplot(survey,aes(x=ITU3))+geom_histogram(bins=10)+
  geom_vline(xintercept = mean(survey$ITU3),color = "red",linetype = "dashed", size = 1)+
  geom_vline(xintercept = median(survey$ITU3),color = "green",linetype = "dashed", size = 1)+
  ggtitle("Histogram of ITU3") +
  xlab("IT Knowledge") +
  ylab("Frequency")

table(survey$ITU4)
summary(survey$ITU4)
ggplot(survey,aes(x=ITU4))+geom_histogram(bins=10)+
  geom_vline(xintercept = mean(survey$ITU4),color = "red",linetype = "dashed", size = 1)+
  geom_vline(xintercept = median(survey$ITU4),color = "green",linetype = "dashed", size = 1)+
  ggtitle("Histogram of ITU4") +
  xlab("IT Knowledge") +
  ylab("Frequency")

#Analysing the ITK
summary(survey$ITK)
ggplot(survey,aes(x=ITK))+geom_histogram(bins=30)+
  geom_vline(aes(xintercept = mean(ITK)),color = "red",linetype = "dashed", size = 1)+
  geom_vline(aes(xintercept = median(ITK)),color = "green",linetype = "dashed", size = 1)+
  ggtitle("Histogram of IT Knowledge") +
  xlab("IT Knowledge") +
  ylab("Frequency")

table(survey$ITK1)
summary(survey$ITK1)
ggplot(survey,aes(x=ITK1))+geom_histogram(bins=10)+
  geom_vline(xintercept = mean(survey$ITK1),color = "red",linetype = "dashed", size = 1)+
  geom_vline(xintercept = median(survey$ITK1),color = "green",linetype = "dashed", size = 1)+
  ggtitle("Histogram of ITK1") +
  xlab("IT Knowledge") +
  ylab("Frequency")

table(survey$ITK2)
summary(survey$ITK2)
ggplot(survey,aes(x=ITK2))+geom_histogram(bins=10)+
  geom_vline(xintercept = mean(survey$ITK2),color = "red",linetype = "dashed", size = 1)+
  geom_vline(xintercept = median(survey$ITK2),color = "green",linetype = "dashed", size = 1)+
  ggtitle("Histogram of ITK2") +
  xlab("IT Knowledge") +
  ylab("Frequency")

table(survey$ITK3)
summary(survey$ITK3)
ggplot(survey,aes(x=ITK3))+geom_histogram(bins=10)+
  geom_vline(xintercept = mean(survey$ITK3),color = "red",linetype = "dashed", size = 1)+
  geom_vline(xintercept = median(survey$ITK3),color = "green",linetype = "dashed", size = 1)+
  ggtitle("Histogram of ITK3") +
  xlab("IT Knowledge") +
  ylab("Frequency")

table(survey$ITK4)
summary(survey$ITK4)
ggplot(survey,aes(x=ITK4))+geom_histogram(bins=10)+
  geom_vline(xintercept = mean(survey$ITK4),color = "red",linetype = "dashed", size = 1)+
  geom_vline(xintercept = median(survey$ITK4),color = "green",linetype = "dashed", size = 1)+
  ggtitle("Histogram of ITK4") +
  xlab("IT Knowledge") +
  ylab("Frequency")

#Making Frequency Distributons, Create Boxplots and calculate the correlations
x1=addmargins(table(survey$ITU3,survey$ITK2))
x1
y1=round(addmargins(proportions(table(survey$ITU3,survey$ITK2))*100),1)
y1
cor(survey$ITU3,y=survey$ITK2,method="pearson")

x2=addmargins(table(survey$ITU4,survey$ITK4))
x2
y2=round(addmargins(proportions(table(survey$ITU3,survey$ITK2))*100),1)
y2
cor(survey$ITU4,y=survey$ITK4,method="pearson")

df<-data.frame(ITK1=survey$ITK1,ITU=survey$ITU)
box_box <- ggplot(df, aes(x = ITK1, y = ITU, group = ITK1)) +
  geom_boxplot(outlier.colour="red", outlier.shape=5,
               outlier.size=2, medcol="red") +
  xlab("The participant has the skills necessary to use MPS") + ylab("Intention to Use")
box_box+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
  )+scale_color_grey()+scale_fill_grey()+ theme_classic()

cor(survey$ITK1,y=survey$ITU,method="pearson")

p <- ggplot(data=survey) + geom_point(aes(x=ITU, y=ITK)) + xlab("Intention to Use") +
  ylab("IT Knowledge") 
lm_fit <- lm(ITK ~ ITU, data = survey)
p + geom_abline(intercept = coef(lm_fit)[1], slope = coef(lm_fit)[2], color = "red",linetype = 1,size=1)+theme(
  panel.background = element_rect(fill='transparent'), #transparent panel bg
)+ theme_classic()

p=ggplot(survey, aes(ITU, ITK)) + 
  geom_bin2d() +
  xlab("Intention to Use")+
  ylab("IT Knowledge")+
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
p+ geom_abline(intercept = coef(lm_fit)[1], slope = coef(lm_fit)[2], color = "red",linetype = 1,size=1.3)

cor(survey$ITU,y=survey$ITK,method="pearson")


#Creating Dataset with only people that have at least a Bachelors degree
survey2<- filter(survey,EDU!=7)

#Labeling the EDU variable
survey2$EDU<- ordered(survey2$EDU, levels=c(1,2,3,4,5,6,7), labels=c("Prim. EDU","CVT","Sec. EDU","Bachelor","Master","PHD","Other"))

#Analysing the new created DataSet
x4=addmargins(table(survey2$ITU1,survey2$EDU))
x4
y4=round(addmargins(proportions(table(survey2$ITU1,survey2$EDU))*100),1)
y4
cor(survey2$ITU1,y=survey2$EDU,method="pearson")

x5=addmargins(table(survey2$ITU2,survey2$EDU))
x5
y5=round(addmargins(proportions(table(survey2$ITU2,survey2$EDU))*100),1)
y5
cor(survey2$ITU2,y=survey2$EDU,method="pearson")

x6=addmargins(table(survey2$ITU4,survey2$EDU))
x6
y6=round(addmargins(proportions(table(survey2$ITU4,survey2$EDU))*100),1)
y6
cor(survey2$ITU4,y=survey2$EDU,method="pearson")


df1<-data.frame(ITU1=survey$ITU1,AGE=survey$AGE)
box_box <- ggplot(df1, aes(x = ITU1, y = AGE, group = ITU1)) +
  geom_boxplot(outlier.colour="red", outlier.shape=5,
               outlier.size=2, medcol="red") +
  xlab("The Participant is willing to use MPS") + ylab("AGE")
box_box+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
  )+ theme_classic()


df2<-data.frame(ITU2=survey$ITU2,AGE=survey$AGE)
box_box <- ggplot(df2, aes(x = ITU2, y = AGE, group = ITU2)) +
  geom_boxplot() +
  xlab("ITU2") + ylab("AGE")
box_box

df3<-data.frame(ITU4=survey$ITU4,AGE=survey$AGE)
box_box <- ggplot(df3, aes(x = ITU4, y = AGE, group = ITU4)) +
  geom_boxplot() +
  xlab("ITU4") + ylab("AGE")
box_box

p1 <- ggplot(data=survey) + geom_point(aes(x=ITU, y=AGE)) + xlab("Intention to Use") +
  ylab("AGE") +
  ggtitle("Scatterplot of Intention to Use and Age")
lm_fit <- lm(AGE ~ ITU, data = survey)
p1 + geom_abline(intercept = coef(lm_fit)[1], slope = coef(lm_fit)[2], color = "red")+ theme_classic()

cor(survey$ITU,y=survey$AGE,method="pearson")

survey2$ITU123 <- with(survey2, (ITU1 + ITU2 + ITU3) / 3)







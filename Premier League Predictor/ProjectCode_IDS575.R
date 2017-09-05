### IDS 575 Project
### Team :Adrash Tomar,Debdeep Dhar,Mihir Singh ,Sandeep G,Sudhir S B

### Please find 9 datasets - 1 (s1) is the overall dataset that we modified a little and then worked upon. The modification is mentioned in the report.
##                         - 2 (s2) is the dataset pertaining to the top 6 teams that has been provided and can also be genrated through the code too. This datset has been used for the models later alot
###                        - The rest 6 (manutd, mancity, liverpool, tot, arsenal, chelsea) are exclusive datasets of the top 6 teams that have been used in the modelling part and further.
### In addition to the above table, there will also be an s2_alter table that would be generated as a scaled dataset of the numeric variables of s2. It has also been provided for reference.
## Libraries to load
library(dplyr)
library(lubridate)
library(fmsb)
library(car)
library(zoo)
library(psych)
library(ROCR)
library(forecast)
## EDA
rm(list=ls())
setwd("/Users/mihirsingh/Documents/IDS 575/Project/Dataset")
save(list = ls(), file = "IDS575Project.Rdata")
load("IDS575Project.Rdata")
s1<- readr::read_csv(file.choose())
## Select the file data with standings.csv. This is the combined(5 seasons combined) and modified file (modified to represent home and away match for every team)
head(s1,100)

#Checking the NULLS 
which(is.na(s1))
colSums(is.na(s1))

attach(s1)
str(s1)
dim(s1)
summary(s1)

#Datatype conversion
s1$Match <- as.factor(s1$Match)
s1$target <- ifelse(s1$FTR == "W","1","0")
s1$target <- as.factor(s1$target)
s1$HTR <- as.factor(s1$HTR)

#Relationship among variables

hist(FTTG)  #  multimodal
plot(density(FTTG))
hist(HTTG)  #  multimodal
plot(density(HTTG))

hist(TS)
plot(density(TS))  #normal

hist(OS)           #normal
plot(density(OS)) 

hist(TST)      #normal
plot(density(TST)) 

hist(OST)      #slightly skewed
plot(density(OST)) 

hist(TF)      #normal
plot(density(TF))

hist(OF)      #normal
plot(density(OF))

hist(TC)      #slightly skewed
plot(density(TC))

hist(OC)      #slightly skewed
plot(density(OC))

hist(TY)     #highly skewed
plot(density(TY))

hist(OY)     #highly skewed
plot(density(TY))

#Filter data for top 6 and worked on it
s2=s1 %>% filter(Team =="Arsenal" | Team =="Man United" | Team =="Liverpool"| Team =="Chelsea"| Team =="Man City" | Team =="Tottenham")
nrow(s1)/nrow(s2)

#Bivariate 
#Categorical-Categorical

x1 <- xtabs(~target+Opponent.Cat, data=s2)
plot(x1, main="Result by opponentcat", sub="s2", col="darkgreen")

x2 <- xtabs(~target+Match, data=s2)
plot(x2, main="Result by home/away", sub="s2", col="darkgreen")

x3 <- xtabs(~target+HTR, data=s2)
plot(x3, main="Final Result by half time score", sub="s2", col="darkgreen")


#Bivariate statistics for Numeric variables


cor.test(FTTG,TST)         #0.4100111
cor.test(FTTG,TS)          #0.2137871
cor.test(HTTG,TST)         #0.2260952
cor.test(HTTG,TS)         #0.05235252
cor.test(FTTG,TF)         #-0.08672609 
cor.test(FTTG,TC)         #-0.08363878

cor.test(TS,TC)  #0.5138017
cor.test(TST,TC)  #0.316158



#Bivariate statistics for Cat-Numeric variables

describeBy(TST,target)  
boxplot(TST~target, data=s2, main="Results by Shots on target", 
        xlab="Target", ylab="TST", col=c("orange", "lightblue4"))

describeBy(TY,target)  
boxplot(TY~target, data=s2, main="Result pattern by yellow cards", 
        xlab="Target", ylab="TY", col=c("orange", "lightblue4"))

describeBy(OY,target)  
boxplot(OY~target, data=s2, main="Result pattern by opponent yellow cards ", 
        xlab="target", ylab="OY", col=c("orange", "lightblue4"))

describeBy(TF,target)  
boxplot(TF~target, data=s2, main="Result pattern by Free Kicks ", 
        xlab="target", ylab="TF", col=c("orange", "lightblue4"))

describeBy(OF,target)  
boxplot(OF~target, data=s2, main="Result pattern by opp free kicks ", 
        xlab="target", ylab="opp free kicks", col=c("orange", "lightblue4"))





#Data structure
str(s2)
colnames(s2)


s2$Match <- as.factor(s2$Match)
s2$Date<-mdy(s2$Date)
s2$target<-as.factor(s2$target)
min(s2$TC)
max(s2$TC)
s2[which.max(s2$TC),]#just checking

#CHi-squared for the cards columns
t1<-table(s2$target,s2$TR)
chisq.test(t1)#shows that red cards have an effect on result!
t2<-table(s2$target,s2$TY)
chisq.test(t2)#shows that these have an effect

#transform these two variables to categorical
s2$R<-factor(s2$TR)
levels(s2$R)<-list(No=c('0'),Yes=c('1','2'))
#Lets perform the chi-sq again
t3<-table(s2$target,s2$R)
chisq.test(t3)#P value reduces further
s2$Y<-factor(s2$TY)
levels(s2$Y)<-list(Low=c('0','1','2'),High=c('3','4','5','6','7','8','9'))
#Lets perform the chi-sq again
t4<-table(s2$target,s2$Y)
chisq.test(t4)#P value reduces further
colnames(s2)

#Lets do this for the opponent columns as well
#CHi-squared for the cards columns
t5<-table(s2$target,s2$OR)
chisq.test(t5)#shows that red cards have an effect on result!
t6<-table(s2$target,s2$OY)
chisq.test(t6)#shows that yellow cards have an effect

#transform these two variables to categorical
s2$O_R<-factor(s2$OR)
levels(s2$O_R)<-list(No=c('0'),Yes=c('1','2'))

#Lets perform the chi-sq again
t7<-table(s2$target,s2$O_R)
chisq.test(t7)#P value reduces further
s2$O_Y<-factor(s2$OY)
levels(s2$O_Y)<-list(Low=c('0','1','2'),High=c('3','4','5','6','7','8','9'))
#Lets perform the chi-sq again
t8<-table(s2$target,s2$O_Y)
chisq.test(t8)#P value reduces further
colnames(s2)
s2[,1] <- NULL

#VIF
variableSet <- target ~ TS+OS+TST+OST+TF+OF+TC+OC
log_1=glm(formula = variableSet,family = "binomial",data = s2) #AIC 1257
summary(log_1)
sqrt(vif(log_1))>2 #no multicollinearity


# Basically, there were three sets of model that were made - mixed model and ingame model, pre-game model.
# All models were made and used to predict the probabilities of winning of our 6 teams of interest
# Appropriate cut-off probabilities were chosen for each of the models that gave the most accurate reponse.
# Finally, the standings were calculated.

# Building the mixed model
diffmodels = function(data,variableSet){
  library(car)
  set.seed(1234)
  ind = sample(2,nrow(data), replace=TRUE,prob=c(0.75,0.25))
  trainData = data[ind==1,]
  testData = data[ind==2,]
  m = glm(formula = variableSet,family = "binomial",data = trainData)
  print("The summary of the model")
  print(summary(m))
  print("the dispersion is")
  print(deviance(m)/df.residual(m))
  print("The durbin Watson test")
  print(durbinWatsonTest(m))
  pred_log_2 = predict(m, newdata = testData ,type = "response")
  roc_log_2=prediction(pred_log_2,testData$target)
  rocr_perf_log2=performance(roc_log_2,'tpr','fpr')
  plot(rocr_perf_log2,colorize = TRUE, text.adj = c(-0.2,1.7))
  print("The AUC is")
  print(performance(roc_log_2,"auc"))
  pred_log_reg_orig=predict(m,newdata = testData,type = "response")
  acc_log_orig=table(testData$target,pred_log_reg_orig>0.5)
  pred_log_reg_orig_1=predict(m,newdata = trainData,type = "response")
  acc_log_orig_1=table(trainData$target,pred_log_reg_orig_1>0.5)
  print("Training accuracy")
  print(sum(diag(acc_log_orig_1))/sum(acc_log_orig_1))
  print("Testing accuracy")
  print(sum(diag(acc_log_orig))/sum(acc_log_orig))
  return(m)
}
#Making model without variable reduction
variableSetall = target ~ TS+OS+TST+OST+TF+OF+TC+OC+Match+`Opponent Cat`+R+Y+O_R+O_Y
modelallvariable = diffmodels(s2,variableSetall)

#After removing non-significant variables
variableSetsig = target ~ OS+TST+OST+OF+TC+OC+Match+`Opponent Cat`+O_R
modelwithsigvariable = diffmodels(s2,variableSetsig)

#doing a pca analysis
s2_alter <- data.frame(scale(s2[,c(13:20)]))
pca1 <- prcomp(s2_alter)
summary(pca1) #5 pcas-84.2%
plot(pca1, type="l", main="PCA and Captured Variance in the scaled data", ylim=c(0,9), pch=20)
fa.parallel(s2_alter, fa="pc", n.iter=100)
s2.new<-cbind(s2,pca1$x[,1:5])
variableSetpca <- target ~ PC1+PC2+PC3+PC5+Match+`Opponent Cat`+R+Y+O_R+O_Y
modelwithpca = diffmodels(s2.new,variableSetpca)

#factor analysis
library(psych)
View(s2_alter)
s2_alter 
fa.parallel(s2_alter, fa="fa", n.iter=100, show.legend=F)
s2fac <- factanal(s2_alter, 3) 
s2fac$loadings
s2_fac <- s2_alter
s2_fac$factor1 <- s2_alter$OS+s2_alter$OST
s2_fac$factor2 <- s2_alter$TS+s2_alter$TC
s2_fac$factor3 <- s2_alter$TST
s2_factornew <- cbind(s2,s2_fac)

#Factor Analysis models
variableSetfa = target ~ factor1+factor2+factor3+Match+`Opponent Cat`+R+Y+O_R+O_Y
modelwithfa = diffmodels(s2_factornew,variableSetfa)

#chcking for interaction on final model
variableSetinteraction = target ~ OS+OST+OF+TST+TC+OC+Match+`Opponent Cat`:O_R
modelwithinteraction = diffmodels(s2,variableSetinteraction) #interactions didn't bring improvement

### Performing the Time-Series on the data
## Time Series and forecasting was done for all the data and was fed in manually for the games for which the prediction has not been made across all the six teams for whic the analysis is being carried out.

data=s2
nrow(data)

#_____________________
#LIVERPOOL
#_____________________
library(dplyr)
liv<-data %>% filter(Team=="Liverpool")
nrow(liv)
str(liv)
##Some Data type conversions
liv$Date<-ymd(liv$Date)
##Since we will need just the numeric values to forecast we shall not do 
##anymore data conversion as the numeric ones are correctly defined
liv<-liv%>%arrange(Date)
write.csv(liv,"Liv.csv")

#Total Shots
ts1<-ts(liv$TS,start=c(2012,1),freq=12)
plot(ts1)
ts1_arima<- auto.arima(ts1)# may take a while 
plot(forecast(ts1_arima,h=6)) 

#Opponent Shots
#Total Shots
ts2<-ts(liv$OS,start=c(2012,1),freq=12)
plot(ts2)
ts2_arima<- auto.arima(ts2)# may take a while 
plot(forecast(ts2_arima,h=6)) 

#Total shots on target
ts3<-ts(liv$TST,start=c(2012,1),freq=12)
plot(ts3)
ts3_arima<- auto.arima(ts3)# may take a while 
plot(forecast(ts3_arima,h=6)) 

#Opponent total shots on target
ts4<-ts(liv$OST,start=c(2012,1),freq=12)
plot(ts4)
ts4_arima<- auto.arima(ts4)# may take a while 
plot(forecast(ts4_arima,h=6)) 

#Team free kicks
ts5<-ts(liv$TF,start=c(2012,1),freq=12)
plot(ts5)
ts5_arima<- auto.arima(ts5)# may take a while 
plot(forecast(ts5_arima,h=6)) 

#Opponent Team free kicks
ts6<-ts(liv$OF,start=c(2012,1),freq=12)
plot(ts6)
ts6_arima<- auto.arima(ts6)# may take a while 
plot(forecast(ts6_arima,h=6)) 

#Team Corners
ts7<-ts(liv$TC,start=c(2012,1),freq=12)
plot(ts7)
ts7_arima<- auto.arima(ts7)# may take a while 
plot(forecast(ts7_arima,h=6)) 

#Opponent Corners
ts8<-ts(liv$OC,start=c(2012,1),freq=12)
plot(ts8)
ts8_arima<- auto.arima(ts8)# may take a while 
plot(forecast(ts8_arima,h=6)) 

#Team Yellow cards
ts9<-ts(liv$TY,start=c(2012,1),freq=12)
plot(ts9)
ts9_arima<- auto.arima(ts9)# may take a while 
plot(forecast(ts9_arima,h=6)) 

#Opponent Team Yellow cards
ts10<-ts(liv$OY,start=c(2012,1),freq=12)
plot(ts10)
ts10_arima<- auto.arima(ts10)# may take a while 
plot(forecast(ts10_arima,h=6)) 

#Team Red Cards
ts11<-ts(liv$TR,start=c(2012,1),freq=12)
plot(ts11)
ts11_arima<- auto.arima(ts11)# may take a while 
plot(forecast(ts11_arima,h=6)) 

#Opponent Team Red Cards
ts12<-ts(liv$OR,start=c(2012,1),freq=12)
plot(ts12)
ts12_arima<- auto.arima(ts12)# may take a while 
plot(forecast(ts12_arima,h=6)) 

## Now we have manually entered these values into the excel sheet
##we no longer need these forecasts so will be reqriting over these variables for ease of reuse of code


#_____________________
#TOTTENHAM HOTSPURS
#_____________________

tot<-data %>% filter(Team=="Tottenham")
nrow(tot)
str(tot)
##Some Data type conversions
tot$Date<-ymd(tot$Date)
##Since we will need just the numeric values to forecast we shall not do 
##anymore data conversion as the numeric ones are correctly defined
tot<-tot%>%arrange(Date)
write.csv(tot,"Tottenham.csv")

#Total Shots
ts1<-ts(tot$TS,start=c(2012,1),freq=12)
plot(ts1)
ts1_arima<- auto.arima(ts1)# may take a while 
plot(forecast(ts1_arima,h=6)) 

#Opponent Shots
#Total Shots
ts2<-ts(tot$OS,start=c(2012,1),freq=12)
plot(ts2)
ts2_arima<- auto.arima(ts2)# may take a while 
plot(forecast(ts2_arima,h=6)) 

#Total shots on target
ts3<-ts(tot$TST,start=c(2012,1),freq=12)
plot(ts3)
ts3_arima<- auto.arima(ts3)# may take a while 
plot(forecast(ts3_arima,h=6)) 

#Opponent total shots on target
ts4<-ts(tot$OST,start=c(2012,1),freq=12)
plot(ts4)
ts4_arima<- auto.arima(ts4)# may take a while 
plot(forecast(ts4_arima,h=6)) 

#Team free kicks
ts5<-ts(tot$TF,start=c(2012,1),freq=12)
plot(ts5)
ts5_arima<- auto.arima(ts5)# may take a while 
plot(forecast(ts5_arima,h=6)) 

#Opponent Team free kicks
ts6<-ts(tot$OF,start=c(2012,1),freq=12)
plot(ts6)
ts6_arima<- auto.arima(ts6)# may take a while 
plot(forecast(ts6_arima,h=6)) 

#Team Corners
ts7<-ts(tot$TC,start=c(2012,1),freq=12)
plot(ts7)
ts7_arima<- auto.arima(ts7)# may take a while 
plot(forecast(ts7_arima,h=6)) 

#Opponent Corners
ts8<-ts(tot$OC,start=c(2012,1),freq=12)
plot(ts8)
ts8_arima<- auto.arima(ts8)# may take a while 
plot(forecast(ts8_arima,h=6)) 

#Team Yellow cards
ts9<-ts(tot$TY,start=c(2012,1),freq=12)
plot(ts9)
ts9_arima<- auto.arima(ts9)# may take a while 
plot(forecast(ts9_arima,h=6)) 

#Opponent Team Yellow cards
ts10<-ts(tot$OY,start=c(2012,1),freq=12)
plot(ts10)
ts10_arima<- auto.arima(ts10)# may take a while 
plot(forecast(ts10_arima,h=6)) 

#Team Red Cards
ts11<-ts(tot$TR,start=c(2012,1),freq=12)
plot(ts11)
ts11_arima<- auto.arima(ts11)# may take a while 
plot(forecast(ts11_arima,h=6)) 

#Opponent Team Red Cards
ts12<-ts(tot$OR,start=c(2012,1),freq=12)
plot(ts12)
ts12_arima<- auto.arima(ts12)# may take a while 
plot(forecast(ts12_arima,h=6)) 

#_____________________
#MANCHESTER CITY
#_____________________

mc<-data %>% filter(Team=="Man City")
nrow(mc)
str(mc)
##Some Data type conversions
mc$Date<-ymd(mc$Date)
##Since we will need just the numeric values to forecast we shall not do 
##anymore data conversion as the numeric ones are correctly defined
mc<-mc%>%arrange(Date)
write.csv(mc,"ManCity.csv")

#Total Shots
ts1<-ts(mc$TS,start=c(2012,1),freq=12)
plot(ts1)
ts1_arima<- auto.arima(ts1)# may take a while 
plot(forecast(ts1_arima,h=7)) 

#Opponent Shots
#Total Shots
ts2<-ts(mc$OS,start=c(2012,1),freq=12)
plot(ts2)
ts2_arima<- auto.arima(ts2)# may take a while 
plot(forecast(ts2_arima,h=7)) 

#Total shots on target
ts3<-ts(mc$TST,start=c(2012,1),freq=12)
plot(ts3)
ts3_arima<- auto.arima(ts3)# may take a while 
plot(forecast(ts3_arima,h=7)) 

#Opponent total shots on target
ts4<-ts(mc$OST,start=c(2012,1),freq=12)
plot(ts4)
ts4_arima<- auto.arima(ts4)# may take a while 
plot(forecast(ts4_arima,h=7)) 

#Team free kicks
ts5<-ts(mc$TF,start=c(2012,1),freq=12)
plot(ts5)
ts5_arima<- auto.arima(ts5)# may take a while 
plot(forecast(ts5_arima,h=7)) 

#Opponent Team free kicks
ts6<-ts(mc$OF,start=c(2012,1),freq=12)
plot(ts6)
ts6_arima<- auto.arima(ts6)# may take a while 
plot(forecast(ts6_arima,h=7)) 

#Team Corners
ts7<-ts(mc$TC,start=c(2012,1),freq=12)
plot(ts7)
ts7_arima<- auto.arima(ts7)# may take a while 
plot(forecast(ts7_arima,h=7)) 

#Opponent Corners
ts8<-ts(mc$OC,start=c(2012,1),freq=12)
plot(ts8)
ts8_arima<- auto.arima(ts8)# may take a while 
plot(forecast(ts8_arima,h=7)) 

#Team Yellow cards
ts9<-ts(mc$TY,start=c(2012,1),freq=12)
plot(ts9)
ts9_arima<- auto.arima(ts9)# may take a while 
plot(forecast(ts9_arima,h=7)) 

#Opponent Team Yellow cards
ts10<-ts(mc$OY,start=c(2012,1),freq=12)
plot(ts10)
ts10_arima<- auto.arima(ts10)# may take a while 
plot(forecast(ts10_arima,h=7)) 

#Team Red Cards
ts11<-ts(mc$TR,start=c(2012,1),freq=12)
plot(ts11)
ts11_arima<- auto.arima(ts11)# may take a while 
plot(forecast(ts11_arima,h=7)) 

#Opponent Team Red Cards
ts12<-ts(mc$OR,start=c(2012,1),freq=12)
plot(ts12)
ts12_arima<- auto.arima(ts12)# may take a while 
plot(forecast(ts12_arima,h=7)) 

#_____________________
#MANCHESTER UNITED
#_____________________

mu<-data %>% filter(Team=="Man United")
nrow(mu)
str(mu)
##Some Data type conversions
mu$Date<-ymd(mu$Date)
##Since we will need just the numeric values to forecast we shall not do 
##anymore data conversion as the numeric ones are correctly defined
mu<-mu%>%arrange(Date)
write.csv(mu,"ManUnited.csv")

#Total Shots
ts1<-ts(mu$TS,start=c(2012,1),freq=12)
plot(ts1)
ts1_arima<- auto.arima(ts1)# may take a while 
plot(forecast(ts1_arima,h=8)) 

#Opponent Shots
#Total Shots
ts2<-ts(mu$OS,start=c(2012,1),freq=12)
plot(ts2)
ts2_arima<- auto.arima(ts2)# may take a while 
plot(forecast(ts2_arima,h=8)) 

#Total shots on target
ts3<-ts(mu$TST,start=c(2012,1),freq=12)
plot(ts3)
ts3_arima<- auto.arima(ts3)# may take a while 
plot(forecast(ts3_arima,h=8)) 

#Opponent total shots on target
ts4<-ts(mu$OST,start=c(2012,1),freq=12)
plot(ts4)
ts4_arima<- auto.arima(ts4)# may take a while 
plot(forecast(ts4_arima,h=8)) 

#Team free kicks
ts5<-ts(mu$TF,start=c(2012,1),freq=12)
plot(ts5)
ts5_arima<- auto.arima(ts5)# may take a while 
plot(forecast(ts5_arima,h=8)) 

#Opponent Team free kicks
ts6<-ts(mu$OF,start=c(2012,1),freq=12)
plot(ts6)
ts6_arima<- auto.arima(ts6)# may take a while 
plot(forecast(ts6_arima,h=8)) 

#Team Corners
ts7<-ts(mu$TC,start=c(2012,1),freq=12)
plot(ts7)
ts7_arima<- auto.arima(ts7)# may take a while 
plot(forecast(ts7_arima,h=8)) 

#Opponent Corners
ts8<-ts(mu$OC,start=c(2012,1),freq=12)
plot(ts8)
ts8_arima<- auto.arima(ts8)# may take a while 
plot(forecast(ts8_arima,h=8)) 

#Team Yellow cards
ts9<-ts(mu$TY,start=c(2012,1),freq=12)
plot(ts9)
ts9_arima<- auto.arima(ts9)# may take a while 
plot(forecast(ts9_arima,h=8)) 

#Opponent Team Yellow cards
ts10<-ts(mu$OY,start=c(2012,1),freq=12)
plot(ts10)
ts10_arima<- auto.arima(ts10)# may take a while 
plot(forecast(ts10_arima,h=8)) 

#Team Red Cards
ts11<-ts(mu$TR,start=c(2012,1),freq=12)
plot(ts11)
ts11_arima<- auto.arima(ts11)# may take a while 
plot(forecast(ts11_arima,h=8)) 

#Opponent Team Red Cards
ts12<-ts(mu$OR,start=c(2012,1),freq=12)
plot(ts12)
ts12_arima<- auto.arima(ts12)# may take a while 
plot(forecast(ts12_arima,h=8))

#_____________________
#ARSENAL
#_____________________

ars<-data %>% filter(Team=="Arsenal")
nrow(ars)
str(ars)
##Some Data type conversions
ars$Date<-mdy(ars$Date)
##Since we will need just the numeric values to forecast we shall not do 
##anymore data conversion as the numeric ones are correctly defined
ars<-ars%>%arrange(Date)
write.csv(ars,"C:\\Users\\debde\\Desktop\\IDS 575\\Project\\ars.csv")

#Total Shots
ts1<-ts(ars$TS,start=c(2012,1),freq=12)
plot(ts1)
ts1_arima<- auto.arima(ts1)# may take a while 
plot(forecast(ts1_arima,h=7)) 

#Opponent Shots
#Total Shots
ts2<-ts(ars$OS,start=c(2012,1),freq=12)
plot(ts2)
ts2_arima<- auto.arima(ts2)# may take a while 
plot(forecast(ts2_arima,h=7)) 

#Total shots on target
ts3<-ts(ars$TST,start=c(2012,1),freq=12)
plot(ts3)
ts3_arima<- auto.arima(ts3)# may take a while 
plot(forecast(ts3_arima,h=7)) 

#Opponent total shots on target
ts4<-ts(ars$OST,start=c(2012,1),freq=12)
plot(ts4)
ts4_arima<- auto.arima(ts4)# may take a while 
plot(forecast(ts4_arima,h=7)) 

#Team free kicks
ts5<-ts(ars$TF,start=c(2012,1),freq=12)
plot(ts5)
ts5_arima<- auto.arima(ts5)# may take a while 
plot(forecast(ts5_arima,h=7)) 

#Opponent Team free kicks
ts6<-ts(ars$OF,start=c(2012,1),freq=12)
plot(ts6)
ts6_arima<- auto.arima(ts6)# may take a while 
plot(forecast(ts6_arima,h=7)) 

#Team Corners
ts7<-ts(ars$TC,start=c(2012,1),freq=12)
plot(ts7)
ts7_arima<- auto.arima(ts7)# may take a while 
plot(forecast(ts7_arima,h=7)) 

#Opponent Corners
ts8<-ts(ars$OC,start=c(2012,1),freq=12)
plot(ts8)
ts8_arima<- auto.arima(ts8)# may take a while 
plot(forecast(ts8_arima,h=7)) 

#Team Yellow cards
ts9<-ts(ars$TY,start=c(2012,1),freq=12)
plot(ts9)
ts9_arima<- auto.arima(ts9)# may take a while 
plot(forecast(ts9_arima,h=7)) 

#Opponent Team Yellow cards
ts10<-ts(ars$OY,start=c(2012,1),freq=12)
plot(ts10)
ts10_arima<- auto.arima(ts10)# may take a while 
plot(forecast(ts10_arima,h=7)) 

#Team Red Cards
ts11<-ts(ars$TR,start=c(2012,1),freq=12)
plot(ts11)
ts11_arima<- auto.arima(ts11)# may take a while 
plot(forecast(ts11_arima,h=7)) 

#Opponent Team Red Cards
ts12<-ts(ars$OR,start=c(2012,1),freq=12)
plot(ts12)
ts12_arima<- auto.arima(ts12)# may take a while 
plot(forecast(ts12_arima,h=7)) 

#_____________________
#CHELSEA
#_____________________


library(dplyr)
che<-data %>% filter(Team=="Chelsea")
nrow(che)
str(che)
##Some Data type conversions
che$Date<-ymd(che$Date)
##Since we will need just the numeric values to forecast we shall not do 
##anymore data conversion as the numeric ones are correctly defined
che<-che%>%arrange(Date)
write.csv(che,"Chelsea.csv")

#Total Shots
ts1<-ts(che$TS,start=c(2012,1),freq=12)
plot(ts1)
ts1_arima<- auto.arima(ts1)# may take a while 
plot(forecast(ts1_arima,h=7)) 

#Opponent Shots
#Total Shots
ts2<-ts(che$OS,start=c(2012,1),freq=12)
plot(ts2)
ts2_arima<- auto.arima(ts2)# may take a while 
plot(forecast(ts2_arima,h=7)) 

#Total shots on target
ts3<-ts(che$TST,start=c(2012,1),freq=12)
plot(ts3)
ts3_arima<- auto.arima(ts3)# may take a while 
plot(forecast(ts3_arima,h=7)) 

#Opponent total shots on target
ts4<-ts(che$OST,start=c(2012,1),freq=12)
plot(ts4)
ts4_arima<- auto.arima(ts4)# may take a while 
plot(forecast(ts4_arima,h=7)) 

#Team free kicks
ts5<-ts(che$TF,start=c(2012,1),freq=12)
plot(ts5)
ts5_arima<- auto.arima(ts5)# may take a while 
plot(forecast(ts5_arima,h=7)) 

#Opponent Team free kicks
ts6<-ts(che$OF,start=c(2012,1),freq=12)
plot(ts6)
ts6_arima<- auto.arima(ts6)# may take a while 
plot(forecast(ts6_arima,h=7)) 

#Team Corners
ts7<-ts(che$TC,start=c(2012,1),freq=12)
plot(ts7)
ts7_arima<- auto.arima(ts7)# may take a while 
plot(forecast(ts7_arima,h=7)) 

#Opponent Corners
ts8<-ts(che$OC,start=c(2012,1),freq=12)
plot(ts8)
ts8_arima<- auto.arima(ts8)# may take a while 
plot(forecast(ts8_arima,h=7)) 

#Team Yellow cards
ts9<-ts(che$TY,start=c(2012,1),freq=12)
plot(ts9)
ts9_arima<- auto.arima(ts9)# may take a while 
plot(forecast(ts9_arima,h=7)) 

#Opponent Team Yellow cards
ts10<-ts(che$OY,start=c(2012,1),freq=12)
plot(ts10)
ts10_arima<- auto.arima(ts10)# may take a while 
plot(forecast(ts10_arima,h=7)) 

#Team Red Cards
ts11<-ts(che$TR,start=c(2012,1),freq=12)
plot(ts11)
ts11_arima<- auto.arima(ts11)# may take a while 
plot(forecast(ts11_arima,h=7)) 

#Opponent Team Red Cards
ts12<-ts(che$OR,start=c(2012,1),freq=12)
plot(ts12)
ts12_arima<- auto.arima(ts12)# may take a while 
plot(forecast(ts12_arima,h=7)) 
#========Time series forecast ends=========================

#============================================================
# From the forecast above, the values were obtained for the future games that had to be used for prediction
# Using mixed model for predcition of the final results.
# The dataset was dvided for each of the 6 teams and used upon separately
# This dataset has also been provided
manutd <- readr::read_csv(file.choose()) ## use the csv Man United - checked
arsenal <- readr::read_csv(file.choose()) ## use the csv Arsenal-checked
mancity <- readr::read_csv(file.choose()) ## use the csv mc
liverpool <- readr::read_csv(file.choose()) ## use the csv Liverpool-Checked
chelsea <- readr::read_csv(file.choose()) ## use the csv Chelsea-checked
tot <- readr::read_csv(file.choose()) ## use the csv tot
# The above part can be skipped if the Rdata has been loaded

#Setting format
setformat = function(teams)
{
  teams$Match <- as.factor(teams$Match)
  teams$Date <- mdy(teams$Date)
  teams$Team <- as.factor(teams$Team)
  teams$Opponent.Team <- as.factor(teams$Opponent.Team)
  teams$Opponent.Cat <- as.factor(teams$Opponent.Cat)
  teams$FTR <- as.factor(teams$FTR)
  teams$HTR <- as.factor(teams$HTR)
  teams$Referee <- as.factor(teams$Referee)
  teams$target <- as.factor(teams$target)
  teams$R <- as.factor(teams$R)
  teams$Y <- as.factor(teams$Y)
  teams$O_R <- as.factor(teams$O_R)
  teams$O_Y <- as.factor(teams$O_Y)
  return(teams)
}

manutd = setformat(manutd)
arsenal = setformat(arsenal)
mancity = setformat(mancity)
liverpool = setformat(liverpool)
chelsea = setformat(chelsea)
tot = setformat(tot)

#cleaning NA
cleanNA = function(team){
  a = which(rowSums(is.na(team))==NCOL(team))
  team = team[-a,]
  return(team)
}
tot = cleanNA(tot)

modelwithsigvariable #chosen model

# Predicitng probabilities of win
pred = function(model,team){
  a = data.frame(predict(model, team, type = "response"))
  team = cbind(team,a)
  names(team)[ncol(team)] <-"probability"
  return(team)
}



manutd = pred(modelwithsigvariable,manutd)
arsenal = pred(modelwithsigvariable,arsenal)
liverpool = pred(modelwithsigvariable,liverpool)
mancity = pred(modelwithsigvariable,mancity)
tot = pred(modelwithsigvariable,tot)
chelsea = pred(modelwithsigvariable,chelsea)


#checking for the most appropriate cutoff value of probability
accuracy = function(team,value){
  a <- table(team$target,team$probability>value)
  print(a)
  print("The accuracy is")
  print (sum(diag(a))/sum(a))
}


accuracy(manutd,0.45) #0.45
accuracy(liverpool,0.6) #0.6
accuracy(mancity,0.4) #0.4
accuracy(tot,0.5) #0.5
accuracy(chelsea,0.45) #0.45
accuracy(arsenal,0.5) #0.5

# Predicting response
result = function(team,cutoff){
  team$result <- ifelse(team$probability<cutoff,"0","1")
  return(team)
}

manutd = result(manutd,0.45) 
arsenal = result(arsenal,0.5)
mancity = result(mancity,0.4)
tot = result(tot,0.5)
chelsea = result(chelsea,0.45)
liverpool = result(liverpool,0.6)

## In addition to the mixed model, we made models based on in-game and pre-game statistics
## building the in game model.
#VIF test
#FTG and OTG not to be included
variableSet <- target ~ TS+OS+TST+OST+TF+OF+TC+OC
log_1=glm(formula = variableSet,family = "binomial",data = s2) #AIC 1257
summary(log_1)
sqrt(vif(log_1))>2 #no multicollinearity

#Making model without variable reduction
variableSet2 = target ~ TS+OS+TST+OST+TF+OF+TC+OC+R+Y+O_R+O_Y
ingameall = diffmodels(s2, variableSet2)



#After removing non-significant variables
variableSet3 = target ~ TST+OST+OF+TC+OC+Y+O_R

ingame = diffmodels(s2, variableSet3)



#doing a pca analysis
s2_alter <- data.frame(scale(s2[,c(13:20)]))
pca1 <- prcomp(s2_alter)
summary(pca1) #5 pcas-84.2%
plot(pca1, type="l", main="PCA and Captured Variance in the scaled data", ylim=c(0,9), pch=20)
fa.parallel(s2_alter, fa="pc", n.iter=100)
s2.new<-cbind(s2,pca1$x[,1:5]) ## taking upto 90% of the cumilative proportion of variance
View(s2.new)

#building model
variableSet4 <- target ~ R+Y+O_R+O_Y+PC1+PC2+PC3+PC3+PC4+PC5
ingamepca = diffmodels(s2.new, variableSet4)


### Logistic Regression with significant variables and significant PC's
variableSet5 <- target ~ O_R+Y+PC5+PC1

ingame2 = diffmodels(s2.new, variableSet5)


#factor analysis
View(s2_alter)
s2_fa <- s2_alter 
s2_fa$Match <- s2$Match
s2_fa$Date <- s2$Date
s2_fa$Team <- s2$Team
s2_fa$Opponent.Team <- s2$Opponent.Team
s2_fa$Opponent.Cat <- s2$Opponent.Cat
View(s2_fa)
fa.parallel(s2_alter, fa="fa", n.iter=100, show.legend=F,main="Factor Analysis Scree plot")
s2fac <- factanal(s2_alter, 3) 
s2fac$loadings
View(s2fac)
s2_alter$f1=s2_alter$OS+s2_alter$OST
s2_alter$f2=s2_alter$TS
s2_alter$f3=s2_alter$TST
s2_fa_1 <- cbind(s2,s2_alter[,c(9:11)])

#============================================================
## Factor Anlaysis Model
variableSet6=target ~ f1+f2+f3

ingamefa = diffmodels(s2_fa_1, variableSet6)


## Result computation for the in game data
## Please drop the existing probability and result column by running the following code as the function would create these columns again for the new model
manutd$probability <- NULL
manutd$result <- NULL
arsenal$probability <- NULL
arsenal$result <- NULL
mancity$probability <- NULL
mancity$result <- NULL
liverpool$probability <- NULL
liverpool$result <- NULL
chelsea$probability <- NULL
chelsea$result <- NULL
tot$probability <- NULL
tot$result <- NULL


ingameall #chosen model

#Predicting probabilities of win
manutd = pred(ingameall,manutd)
arsenal = pred(ingameall,arsenal)
liverpool = pred(ingameall,liverpool)
mancity = pred(ingameall,mancity)
tot = pred(ingameall,tot)
chelsea = pred(ingame,chelsea)


#checking for the most appropriate cutoff value of probability
accuracy(manutd,0.45) #0.45
accuracy(liverpool,0.55) #0.55
accuracy(mancity,0.4) #0.4
accuracy(tot,0.55) #0.55
accuracy(chelsea,0.55) #0.55
accuracy(arsenal,0.5) #0.5

# Predicting response
manutd = result(manutd,0.45) 
arsenal = result(arsenal,0.5)
mancity = result(mancity,0.4)
tot = result(tot,0.55)
chelsea = result(chelsea,0.55)
liverpool = result(liverpool,0.55)



###Pre game model
vset = target~.
s2_pregame=s2[,c(1,5,21)]
pregame = diffmodels(s2_pregame,vset)

## Predicting the results for the pregame model
## Please drop the existing probability and result column by running the following code as the function would create these columns again for the new model
manutd$probability <- NULL
manutd$result <- NULL
arsenal$probability <- NULL
arsenal$result <- NULL
mancity$probability <- NULL
mancity$result <- NULL
liverpool$probability <- NULL
liverpool$result <- NULL
chelsea$probability <- NULL
chelsea$result <- NULL
tot$probability <- NULL
tot$result <- NULL


pregame #chosen model

#Predicting probabilities of win
manutd = pred(log_pre_game,manutd)
arsenal = pred(log_pre_game,arsenal)
liverpool = pred(log_pre_game,liverpool)
mancity = pred(log_pre_game,mancity)
tot = pred(log_pre_game,tot)
chelsea = pred(log_pre_game,chelsea)


#checking for the most appropriate cutoff value of probability
accuracy(manutd,0.5) #0.5
accuracy(liverpool,0.5) #0.5
accuracy(mancity,0.5) #0.5
accuracy(tot,0.5) #0.50
accuracy(chelsea,0.5) #0.50
accuracy(arsenal,0.5) #0.5


# Predicting response
manutd = result(manutd,0.5) 
arsenal = result(arsenal,0.5)
mancity = result(mancity,0.5)
tot = result(tot,0.5)
chelsea = result(chelsea,0.5)
liverpool = result(liverpool,0.5)
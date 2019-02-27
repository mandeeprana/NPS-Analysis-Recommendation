


# clean and merge datasets 
#data.fname<-file.choose()
#OctData <- read.csv(data.fname,header=TRUE,stringsAsFactors = FALSE)
#View(OctData)
#data.fname<-file.choose()
#NovData <- read.csv(data.fname,header=TRUE,stringsAsFactors = FALSE)
#str(NovData)
#data.fname<-file.choose()
#DecData <- read.csv(data.fname,header=TRUE,stringsAsFactors = FALSE)
#str(DecData)
# select US hotels 
#readData$Country_PL
#JanData <- readData[readData$Country_PL == "United States" ,]
#str(JanData)

# delete the rows that did not have NPS_Type and LTR 
#unique(JanData$NPS_Type)
#JanData <- JanData[-which(JanData$NPS_Type == ""), ]
#str(JanData)
#sum(is.na(JanData$Likelihood_Recommend_H))
#str(JanData) # 23672 rows 
# replace NA with mean value of the same hotel 
#JanData$Guest_Room_H <- with(JanData, ave(Guest_Room_H, Hotel.Name.Long_PL, FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))

#sum(is.na(JanData$Guest_Room_H))

# tranquility 
#JanData$Tranquility_H <- with(JanData, ave(Tranquility_H, Hotel.Name.Long_PL,
#                                           FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
#sum(is.na(JanData$Tranquility_H))

# hotel condition 
#JanData$Condition_Hotel_H <- with(JanData, ave(Condition_Hotel_H, Hotel.Name.Long_PL,
#                                               FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
#sum(is.na(JanData$Condition_Hotel_H))


# Quality of customer service metric; value on a 1 to 10 scale
#JanData$Customer_SVC_H <- with(JanData, ave(Customer_SVC_H, Hotel.Name.Long_PL,
#                                            FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
#sum(is.na(JanData$Customer_SVC_H))

# Staff cared metric; value on a 1 to 10 scale
#JanData$Staff_Cared_H <- with(JanData, ave(Staff_Cared_H, Hotel.Name.Long_PL,
#                                          FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
#sum(is.na(JanData$Staff_Cared_H))

# Internet satisfaction 
#JanData$Internet_Sat_H <- with(JanData, ave(Internet_Sat_H, Hotel.Name.Long_PL,
#                                           FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
#sum(is.na(JanData$Internet_Sat_H))

# Quality of the check in process metric; value on a 1 to 10 scale
#JanData$Check_In_H <- with(JanData, ave(Check_In_H, Hotel.Name.Long_PL,
#                                       FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
#sum(is.na(JanData$Check_In_H))

# NPS goals 
#JanData$Check_In_H <- with(JanData, ave(Guest.NPS.Goal_PL, Hotel.Name.Long_PL,
#                                       FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
#sum(is.na(JanData$Guest.NPS.Goal_PL))
#JanData$Guest.NPS.Goal_PL
# check the new dataset 
#View(JanData)
#unique(JanData$Hotel.Name.Long_PL)


# replacing NAs in NPS goals with mean value   
#mean(JanData$Guest.NPS.Goal_PL,na.rm=TRUE) # 58 
#JanData$Guest.NPS.Goal_PL[is.na(JanData$Guest.NPS.Goal_PL)] <- mean(JanData$Guest.NPS.Goal_PL,na.rm=TRUE)

# merge and save the cleaned data 
#completeData <- rbind(OctData,NovData,DecData,JanData)


#str(completeData)
#write.csv(completeData,file = "completeData.csv", row.names = FALSE)
################################################################################################

#Reading the ready completeData csv file from above cleansing

# load the data
rm(list = ls()) 
#install.packages("data.table")
library(data.table)
setwd("/Users/manu/Desktop/687/Projects")
input <- "completeData.csv"
f1 <- fread(input, header = TRUE)
#data.fname<-file.choose()
#completeData <- read.csv(data.fname,header=TRUE,stringsAsFactors = FALSE)
str(f1)
f2 <- data.frame(f1$GP_Tier_H,f1$Guest.NPS.Goal_PL,f1$City_PL,f1$Country_PL,f1$NPS_Type,
                 f1$Likelihood_Recommend_H,f1$Guest_Room_H,f1$Tranquility_H,f1$Condition_Hotel_H,
                 f1$Customer_SVC_H,f1$Staff_Cared_H,f1$Internet_Sat_H,f1$Check_In_H,f1$Bell.Staff_PL,
                 f1$Convention_PL,f1$Dry.Cleaning_PL,f1$Fitness.Center_PL,f1$Business.Center_PL,
                 f1$Golf_PL,f1$Laundry_PL,f1$Limo.Service_PL,f1$Restaurant_PL,f1$Shuttle.Service_PL,
                 f1$Spa_PL,f1$Valet.Parking_PL,f1$Hotel.Name.Long_PL,f1$Property.Latitude_PL,f1$Property.Longitude_PL,
                 f1$Class_PL,f1$Relationship_PL,f1$Booking_Channel,f1$Internet_Dissat_Lobby_H,f1$Internet_Dissat_Slow_H,
                 f1$Internet_Dissat_Expensive_H,f1$Internet_Dissat_Connectivity_H,f1$Internet_Dissat_Billing_H,f1$Internet_Dissat_Billing_H,
                 f1$Internet_Dissat_Wired_H,f1$Room_Dissat_Internet_H,f1$Status_H,f1$Room_Type_H,
                 f1$Language_H,f1$Age_Range_H,f1$Gender_H,f1$Guest_Country_H, f1$Brand_PL, f1$POV_CODE_C, f1$State_PL)
View(f2)
##############################################
#business question 1
#1. How is the performance of Hotels in USA overall?Plot it using Map. 
# descriptive statistics 
# which hotels have low LTR 
#install.packages('gglot2')
library(ggplot2)
LTRhotel <- aggregate(f2$f1.Likelihood_Recommend_H,list(f2$f1.Hotel.Name.Long_PL),mean)
NPS_hotel_name <-aggregate(f2$f1.Likelihood_Recommend_H,list(f2$f1.State_PL,f2$f1.Property.Latitude_PL,f2$f1.Property.Longitude_PL),
                           FUN=function(x){
                             y <- (sum(x[x>=9])-sum(x[x<=6]))*100/sum(x);
                             return(y)})
View(NPS_hotel_name)

NPS_hotel_name$Group.1<-tolower(NPS_hotel_name$Group.1)
names(NPS_hotel_name)<-c('State_PL','Lat_PL','Long_PL','NPS')
#meanNPS <- NPS_hotel_name

#meanNPS <- aggregate(meanNPS$NPS, list(meanNPS$State_PL, meanNPS$Lat_PL, meanNPS$Long_PL), mean)
                                       
#install.packages("openintro")
library(openintro)
us=map_data("state")
us

map.HotColor<- ggplot(NPS_hotel_name,aes(map_id=NPS_hotel_name$State_PL))
map.HotColor<-map.HotColor+geom_map(map=us,aes(fill=NPS_hotel_name$NPS))
map.HotColor<-map.HotColor+expand_limits(x=NPS_hotel_name$Long_PL,y=NPS_hotel_name$Lat_PL)
map.HotColor<-map.HotColor+coord_map()+ggtitle("NPS Distribution")
map.HotColor

#------------ US Hotels performance -----------------#
NPS_us <- aggregate(f2$f1.Likelihood_Recommend_H,list(f2$f1.Hotel.Name.Long_PL),
                         FUN=function(x){
                           y <- (sum(x[x>=9])-sum(x[x<=6]))*100/sum(x);
                           return(y)})

View(NPS_us)

NPSgoal_us <- aggregate(f2$f1.Guest.NPS.Goal_PL,list(f2$f1.Hotel.Name.Long_PL),mean)
NPSdiff_us <- NPS_us$x-NPSgoal_us$x
NPSdf_us <- data.frame(NPS_us$Group.1,NPS_us$x,NPSgoal_us$x,NPSdiff_us)
colnames(NPSdf_us) <- c("hotel_name","NPS","NPS_goal","NPS-NPS_goal")
View(NPSdf_us)

library(ggplot2)
us_performance <- ggplot(NPSdf_us, aes(x=NPS, y=NPS-NPS_goal)) +
  geom_point(aes(color=NPS-NPS_goal)) +
  scale_colour_gradient(low = "red")+
  lims(x=c(25,100),y=c(-50,50)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = 68) + geom_hline(yintercept = 0) 
us_performance

# LTR distribution 
hist(LTRhotel$x,
     main= "Distribution of Mean LTR among 396 Hotels",
     xlab= "LTR",
     col="grey",
     xlim=c(6,10),
     breaks = 14)
# NPS distribution 
#hist(NPS_hotelName$x,
#     main= "Distribution of Mean NPS among 396 Hotels",
#     xlab= "LTR",
#     col="grey",
#     breaks = 14)

library(lattice)
# Relationship of the hotel with Hyatt corporation: no significant difference
f2$f1.Relationship_PL
aggregate(f2$f1.Likelihood_Recommend_H,list(f2$f1.Relationship_PL),mean)
bwplot(f1.Likelihood_Recommend_H ~ f1.Relationship_PL, data=f2)

#brands
unique(f2$f1.Brand_PL)
aggregate(f2$f1.Likelihood_Recommend_H,list(f2$f1.Brand_PL),mean)
bwplot(f2$f1.Likelihood_Recommend_H ~ f2$f1.Brand_PL, data=f2)

# gender 
f2$f1.Gender_H
aggregate(f2$f1.Likelihood_Recommend_H,list(f2$f1.Gender_H),mean)
bwplot(f1.Likelihood_Recommend_H ~ f1.Gender_H, data=f2)

NPS_gender <-aggregate(f2$f1.Likelihood_Recommend_H,list(f2$f1.Gender_H),
                       FUN=function(x){
                         y <- (sum(x[x>=9])-sum(x[x<=6]))*100/sum(x);
                         return(y)})
Promoter_gender <-aggregate(f2$f1.Likelihood_Recommend_H,list(f2$f1.Gender_H),
                            FUN=function(x){
                              y <- sum(x[x>=9])*100/sum(x);
                              return(y)})

View(NPS_gender)
View(Promoter_gender)

# Defined booking channel as per the NPS analysis
# digital channels and GDS have a lower LTR
f2$f1.Booking_Channel
aggregate(f2$f1.Likelihood_Recommend_H,list(f2$f1.Booking_Channel),mean)
bwplot(f1.Likelihood_Recommend_H ~ f1.Booking_Channel, data=f2)


# which gender is more likely to recommend based on LTR
table(f2$f1.Gender_H)
slices <- c(2189,99033,111782,4109)*100/nrow(f2)
lbls <- c("blank", "female","male","perfer not answer")
pie(slices,labels = lbls, main="Guests' Gender")
aggregate(f2$f1.Likelihood_Recommend_H,list(f2$f1.Gender_H),mean)
bwplot(f1.Likelihood_Recommend_H ~ f1.Gender_H, data=f2)

# travel type - more number of business people are LTR, Leisure people are more ltr
table(f2$f1.POV_CODE_C)
slices <- c(183904,33209)*100/nrow(f2)
lbls <- c("business", "leisure")
pie(slices,labels = lbls, main="Guests travel type")
aggregate(f2$f1.Likelihood_Recommend_H,list(f2$f1.POV_CODE_C),mean)
bwplot(f1.Likelihood_Recommend_H ~ f1.POV_CODE_C, data=f2)

#----------------BUSINESS QUESTION 2-----------------------#
# business question 2: Performance of hotel brands in entire USA
# brand 

NPS_brands <-aggregate(f2$f1.Likelihood_Recommend_H,list(f2$f1.Brand_PL),
                       FUN=function(x){
                         y <- (sum(x[x>=9])-sum(x[x<=6]))*100/sum(x);
                         return(y)})
names(NPS_brands) <- c("Brands","NPS") 
#View(NPS_brands)
library(ggplot2)
g <- ggplot(NPS_brands, aes(x= NPS_brands$Brands, y = NPS_brands$NPS, fill = NPS_brands$Brands))+ geom_bar(stat="identity")
g


# Best perfomring is Park Hyatt and least performing is Hyatt Regency(this brand because has more data)
nrow(f2[f2$f1.Brand_PL=="Hyatt Regency", ])
nrow(f2[f2$f1.Brand_PL=="Hyatt", ])
nrow(f2[f2$f1.Brand_PL=="Park Hyatt", ])

# we selected Hyatt Regency because it is least perfomring and has more data
# performance of Hyatt Regency in USA overall

# Hyatt Regency performance 
#average of each hotels of regency brand
HyattRegency <-f2[f2$f1.Brand_PL=="Hyatt Regency", ]
LTR_regency <- aggregate(HyattRegency$f1.Likelihood_Recommend_H,list(HyattRegency$f1.Hotel.Name.Long_PL),mean)
NPS_regency <- aggregate(HyattRegency$f1.Likelihood_Recommend_H,list(HyattRegency$f1.Hotel.Name.Long_PL),
                         FUN=function(x){
                           y <- (sum(x[x>=9])-sum(x[x<=6]))*100/sum(x);
                           return(y)})

View(NPS_regency)

NPSgoal_regency <- aggregate(HyattRegency$f1.Guest.NPS.Goal_PL,list(HyattRegency$f1.Hotel.Name.Long_PL),mean)
NPSdiff_regency <- NPS_regency$x-NPSgoal_regency$x
NPSdf_regency <- data.frame(NPS_regency$Group.1,NPS_regency$x,NPSgoal_regency$x,NPSdiff_regency)
colnames(NPSdf_regency) <- c("hotel_name","NPS","NPS_goal","NPS-NPS_goal")
View(NPSdf_regency)

# plotting all the regency hotels in USA
# underperformer: The Concourse Hotel at Los Angeles Intl Airport 
#NPSdf_regency[index,]
library(ggplot2)
regency_performance <- ggplot(NPSdf_regency, aes(x=NPS, y=NPS-NPS_goal)) +
  geom_point(aes(color=NPS-NPS_goal)) +
  scale_colour_gradient(low = "red")+
  lims(x=c(25,100),y=c(-50,50)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = 68) + geom_hline(yintercept = 0) 
regency_performance

# mean of NPS of entire regency hotels in USA
mean(NPSdf_regency$NPS)
#number fo regency hotel in USA that is not performing well
sum(NPSdf_regency$`NPS-NPS_goal`<0)
# index of least performing hotel that is concourse hotel LA
index <-which(NPSdf_regency$`NPS-NPS_goal`<0)

# All rengency hotels in USA are performing well except for Concourse International LA

#-----------FOCUSSING ON CONCOURSE HOTEL ONLY (ONLY FOR REPORT BOOK)------------------#

# focus on this hotel 
HyattRegency_losAngeles <- HyattRegency[HyattRegency$f1.Hotel.Name.Long_PL=="The Concourse Hotel at Los Angeles Intl Airport",]
View(HyattRegency_losAngeles)

# gender distribution
table(HyattRegency_losAngeles$f1.Gender_H)

# age range distribution
table(HyattRegency_losAngeles$f1.Age_Range_H)

# distribution of LTR - ?
hist(HyattRegency_losAngeles$f1.Likelihood_Recommend_H)

# purpose - Concourse has more number of business travellers and they are less likely to recommend
table(HyattRegency_losAngeles$f1.POV_CODE_C)
aggregate(HyattRegency_losAngeles$f1.Likelihood_Recommend_H,list(HyattRegency_losAngeles$f1.POV_CODE_C),mean)

# guest's origin 
HyattRegency_losAngeles$f1.Guest_Country_H
aggregate(HyattRegency_losAngeles$f1.Likelihood_Recommend_H,list(HyattRegency_losAngeles$f1.Guest_Country_H),mean)

#------------BUSIENSS QUESTION 3 FOR CONCOURSE ONLY----------------#
# how to know why they're not satisfied?: guest_room, Customer_SVC and hotel_condition are significant customer opinions for Concourse
m1_losAngeles <- lm(formula = f1.Likelihood_Recommend_H ~ f1.Guest_Room_H+ f1.Tranquility_H+ f1.Condition_Hotel_H+
                      f1.Customer_SVC_H+ f1.Staff_Cared_H, data = HyattRegency_losAngeles)
summary(m1_losAngeles)

# guest_room, Customer_SVC and hotel_condition are significant: wide range of opinions 
hist(HyattRegency_losAngeles$f1.Guest_Room_H)
hist(HyattRegency_losAngeles$f1.Condition_Hotel_H)
hist(HyattRegency_losAngeles$f1.Customer_SVC_H)

#------------ BUSIENSS QUESTION 3 FOR HYATT REGENCY BRAND --------------------#
#View(HyattRegency)
#View(f1)
m1_hyattReg <- lm(formula = f1.Likelihood_Recommend_H ~ f1.Guest_Room_H+ f1.Tranquility_H+ f1.Condition_Hotel_H+
                    f1.Customer_SVC_H+ f1.Staff_Cared_H+f1.Internet_Sat_H+f1.Check_In_H , data = HyattRegency)
summary(m1_hyattReg) 

#Check_In is not significant
m2_hyattReg <- lm(formula = f1.Likelihood_Recommend_H ~ f1.Guest_Room_H+  f1.Condition_Hotel_H+
                    f1.Customer_SVC_H+ f1.Staff_Cared_H, data = HyattRegency)
summary(m2_hyattReg) 
#Internet is not significant
# m3_hyattReg <- lm(formula = f1.Likelihood_Recommend_H ~ f1.Guest_Room_H+ f1.Tranquility_H+ f1.Condition_Hotel_H+
#                     f1.Customer_SVC_H+ f1.Staff_Cared_H , data = HyattRegency)
# summary(m3_hyattReg) 

# m4_hyattReg <- lm(formula = f1.Likelihood_Recommend_H ~ f1.Guest_Room_H+ f1.Tranquility_H+ f1.Condition_Hotel_H+
#                     f1.Customer_SVC_H +f1.Internet_Sat_H+f1.Check_In_H , data = HyattRegency)
# summary(m4_hyattReg) 
# 
# m5_hyattReg <- lm(formula = f1.Likelihood_Recommend_H ~ f1.Guest_Room_H+ f1.Condition_Hotel_H+ f1.Staff_Cared_H +f1.Customer_SVC_H  , data = HyattRegency)
# summary(m5_hyattReg)

# Guest_Room is important because it is giving 0.5292 which is the highest value compared to the other 2
m3_hyattReg <- lm(formula = f1.Likelihood_Recommend_H ~ f1.Guest_Room_H, data = HyattRegency)
summary(m3_hyattReg) 

plot( HyattRegency$f1.Guest_Room_H, HyattRegency$f1.Likelihood_Recommend_H) + abline(lm(f1.Likelihood_Recommend_H ~ f1.Guest_Room_H, data= HyattRegency))

m4_hyattReg <- lm(formula = f1.Likelihood_Recommend_H ~  f1.Condition_Hotel_H,data = HyattRegency)
summary(m4_hyattReg) 

m5_hyattReg <- lm(formula = f1.Likelihood_Recommend_H ~  f1.Customer_SVC_H, data = HyattRegency)
summary(m5_hyattReg) 


 # abline(lm(f1.Likelihood_Recommend_H ~ f1.Condition_Hotel_H, data= HyattRegency))
 # abline(lm(f1.Likelihood_Recommend_H ~ f1.Customer_SVC_H, data= HyattRegency))

#R-square: All 0.6735
#less effect:
#f1.Check_In_H : 0.675
#f1.Internet_Sat_H: 0.673
#f1.Staff_Cared_H: 0.6708
#f1.Tranquility_H: 0.6722

#more effect:
#f1.Customer_SVC_H: 0.6199
#f1.Condition_Hotel_H:0.6566
#f1.Guest_Room_H: 0.6299

#----------BUSINESS QUESTION 4 FOR CONCOURSE AND REGENCY BRAND-----------------#


HyattRegency_validation <- HyattRegency[,c("f1.Likelihood_Recommend_H",
                                           "f1.Guest_Room_H","f1.Condition_Hotel_H", "f1.Customer_SVC_H")]
HyattRegency_validation <- HyattRegency[,c("f1.Likelihood_Recommend_H",
                                           "f1.Guest_Room_H","f1.Condition_Hotel_H", "f1.Customer_SVC_H","f1.Staff_Cared_H","f1.Tranquility_H","f1.Internet_Sat_H")]
HyattRegency_validation <- HyattRegency[,c("f1.Likelihood_Recommend_H",
                                           "f1.Guest_Room_H")]
nrow(HyattRegency)
nrows <- nrow(HyattRegency_validation)
Na <- sum(is.na(HyattRegency_validation))
Na

jl <- sum(is.na(HyattRegency_validation$f1.Guest_Room_H))
jl
HyattRegency_validation <- na.omit(HyattRegency_validation)

random.indexes <- sample(1:15000,replace=FALSE)
#View(random.indexes)
nrows <- 15000
# create a 2/3 cutpoint and round the number
cutpoint<-floor(nrows/4*3)

# check the 2/3 cutpoint
cutpoint

# create train data set, which contains the first 2/3 of overall data
mydata.train <-HyattRegency_validation[random.indexes[1:cutpoint],]
mydata.train
nrow(mydata.train)
# check the train dataset
str(mydata.train)

# create test data, which contains the left 1/3 of the overall data
mydata.test <-HyattRegency_validation[random.indexes[(cutpoint+1):nrows],]
mydata.test <- na.omit(mydata.test)
mydata.train <- na.omit(mydata.train)

X <- sum(is.na(mydata.train))
X
Y <- sum(is.na(mydata.test))
Y

nrow(mydata.test)
nrow(mydata.train)
# check the test dataset
str(mydata.test)
library(kernlab)

#install.packages("kernlab")
#install.packages("e1071")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("caret")
#install.packages("Metrics")

library("Metrics")
library("kernlab")
library("e1071")
library("ggplot2")
library("gridExtra")
library("caret")

#model.svm = svm(Likelihood_Recommend_H ~.,data = mydata.train)
# 
# model.svm = svm(Likelihood_Recommend_H ~ Condition_Hotel_H,data = mydata.train)
# 
# model.svm = svm(Likelihood_Recommend_H ~ Condition_Hotel_H+
#                   Guest_Room_H,data = mydata.train)
# 
# model.svm = svm(Likelihood_Recommend_H ~ Condition_Hotel_H+
#                   Guest_Room_H+
#                   Customer_SVC_H,data = mydata.train)

#Run this model only
# model.svm = svm(f1.Likelihood_Recommend_H ~ f1.Guest_Room_H ,data = mydata.train)
# 
# 
# model.svm
# 
# predicted_svm <-predict(model.svm, mydata.test)
# predicted_svm
# table(predicted_svm)
# actual <- mydata.test$f1.Likelihood_Recommend_H
# var <- actual - predicted_svm
# View(predicted_svm)
# confusionMatrix(mydata.train$f1.Likelihood_Recommend_H, predicted_svm)
# scatterchartgraph1 <- ggplot(data = mydata.test,aes(x=mydata.test$f1.Likelihood_Recommend_H, y=mydata.test$f1.Condition_Hotel_H+f1.Customer_SVC_H+f1.Guest_Room_H+f1.Staff_Cared_H)) + geom_point(aes(size= var,	color= var))
# scatterchartgraph1

######################
#KSVM
modelksvm <-ksvm(f1.Likelihood_Recommend_H ~ f1.Guest_Room_H + f1.Condition_Hotel_H + f1.Customer_SVC_H+f1.Staff_Cared_H+f1.Tranquility_H,
                 data=mydata.train,
                 kernel="rbfdot",
                 kpar="automatic",
                 C=3,
                 cross=20,
                 prob.model=TRUE)

modelksvm
unique(f2$f1.Likelihood_Recommend_H)

predictedksvm <-predict(modelksvm,mydata.test)
predictedksvm
mydata.test
nrow(mydata.test)

mydata.test
nrow(mydata.test)

E <- sum(is.na(predictedksvm))
E

table(predictedksvm)
results <- table(predictedksvm, mydata.test$f1.Likelihood_Recommend_H)

nrow(predictedksvm)
totalcorrect = results[1,1]+results[2,2]
totalintest = nrow(mydata.test)
totalcorrect/totalintest

#table(predictedksvm, mydata.test$f1.Condition_Hotel_H)
table(predictedksvm, mydata.test$f1.Guest_Room_H)
#table(predictedksvm, mydata.test$f1.Staff_Cared_H)
#table(predictedksvm, mydata.test$f1.Customer_SVC_H)


# check the predicted value
predictedksvm
# scatterchartgraphksvm = ggplot(data = mydata.test,aes(x=mydata.test$f1.Guest_Room_H, y=mydata.test$f1.Condition_Hotel_H)) + geom_point(aes(size= predictedksvm,	color= mydata.test$f1.Likelihood_Recommend_H))
# 
# scatterchartgraphksvm





#-------- based on graph accuracy ------------#
# on the 3 significant factors
svmOutput <- svm(f1.Likelihood_Recommend_H ~ f1.Guest_Room_H + f1.Condition_Hotel_H + f1.Customer_SVC_H,
                 data=mydata.train)
svmOutput

svmpred <- predict(svmOutput,mydata.test)
svmpred
table(svmpred)
actual <- mydata.test$f1.Likelihood_Recommend_H
actual
length(actual)
length(svmpred)
var2 <- as.numeric(actual) - as.numeric(svmpred)
View(var2)
var2 <- na.omit(var2)
hist(var2, main = "Histogram of SVM Model on 3 significant factors")

# on all the factors

svmOutputall <- svm(f1.Likelihood_Recommend_H ~ f1.Guest_Room_H + f1.Condition_Hotel_H + f1.Customer_SVC_H+f1.Staff_Cared_H+f1.Tranquility_H+f1.Internet_Sat_H,
                 data=mydata.train)
svmOutputall

svmpred <- predict(svmOutputall,mydata.test)
svmpred
table(svmpred)
actual <- mydata.test$f1.Likelihood_Recommend_H
actual
length(actual)
length(svmpred)
var2 <- as.numeric(actual) - as.numeric(svmpred)
View(var2)
var2 <- na.omit(var2)
hist(var2, main = "Histogram of SVM Model on all factors")

# on guest room
svmOutputguest <- svm(f1.Likelihood_Recommend_H ~ f1.Guest_Room_H,
                    data=mydata.train)
svmOutputguest

svmpred <- predict(svmOutputguest,mydata.test)
svmpred
table(svmpred)
actual <- mydata.test$f1.Likelihood_Recommend_H
actual
length(actual)
length(svmpred)
var2 <- as.numeric(actual) - as.numeric(svmpred)
View(var2)
var2 <- na.omit(var2)
hist(var2, main = "Histogram of SVM Model on GuestRoom")




ksvmOutput_Likelihood <- ksvm(f1.Likelihood_Recommend_H ~ f1.Guest_Room_H + f1.Condition_Hotel_H + f1.Customer_SVC_H,
                                
                              data=mydata.train,
                              kernel = "rbfdot", 
                              kpar="automatic", 
                              C = 5, 
                              cross = 20,
                              prob.model = TRUE)

ksvmOutput_Likelihood
#Histogram
ksvmpredLR <- predict(ksvmOutput_Likelihood,mydata.test)
ksvmpredLR

actual <- mydata.test$NPS_Type
View(actual)
View(ksvmpredLR)
Kvar2 <- abs(as.numeric(actual) - as.numeric(ksvmpredLR))
Kvar2
Kvar2 <- na.omit(Kvar2)
View(Kvar2)
hist(Kvar2, main = "Histogram of KSVM Model")




#----------BUSINESS QUESTION 5 FOR REGENCY BRAND-----------------#

# facilities: all blank!
library(arules)
library(arulesViz)
# how about the Regency in the US? 
facilities_us <- HyattRegency[,c( "f1.Bell.Staff_PL",
                                  "f1.Convention_PL",
                                  "f1.Dry.Cleaning_PL",
                                  "f1.Fitness.Center_PL","f1.Business.Center_PL","f1.Golf_PL","f1.Laundry_PL","f1.Restaurant_PL",
                                  "f1.Shuttle.Service_PL","f1.Spa_PL","f1.Valet.Parking_PL","f1.Limo.Service_PL","f1.NPS_Type")]
View(facilities_us)
# change character to factor 
for (i in 1:ncol(facilities_us)){
  if(is.character(facilities_us[,i])){
    facilities_us[,i]=factor(facilities_us[,i])
  }
}
View(facilities_us)

ruleset_promoters <- apriori(facilities_us, parameter = list(support=0.3,confidence=0.65),
                             appearance = list(default="lhs", rhs=("f1.NPS_Type=Promoter")))
inspect(ruleset_promoters)
summary(ruleset_promoters)
plot(ruleset_promoters)
goodrules_promoters <-ruleset_promoters[quality(ruleset_promoters)$lift > 1.02] 
# convention and spa; convention,spa and restrautant 
inspect(goodrules_promoters)
# 
# ruleset_passive <- apriori(facilities_us, parameter = list(support=0.01,confidence=0.9),
#                               appearance = list(default="lhs", rhs=("f1.NPS_Type=Passive")))
# inspect(ruleset_passive)
# summary(ruleset_passive)
# plot(ruleset_passive)
# goodrules_passive <-ruleset_passive[quality(ruleset_passive)$lift > 1.02] 
# # convention and spa; convention,spa and restrautant 
# inspect(goodrules_passive)
# 
# # how to set support and confidence? 
# ruleset_detractors <- apriori(facilities_us, parameter = list(support=0.1,confidence=0.1),
#                               appearance = list(default="lhs", rhs=("f1.NPS_Type=Detractor")))
# inspect(ruleset_detractors)
# summary(ruleset_detractors)
# plot(ruleset_detractors)
# goodrules_detractors <-ruleset_detractors[quality(ruleset_detractors)$lift > 1.02] 
# # convention and spa; convention,spa and restrautant 
# inspect(goodrules_detractors)
# 
# hist(HyattRegency$f1.Likelihood_Recommend_H)
# unique(HyattRegency$f1.Hotel.Name.Long_PL)
# aggregate(HyattRegency$f1.Likelihood_Recommend_H,list(HyattRegency$f1.City_PL),mean)




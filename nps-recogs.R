
# load the data 
data.fname<-file.choose()
completeData <- read.csv(data.fname,header=TRUE,stringsAsFactors = FALSE)
str(completeData)
# to check whether there is no other country other than USA
unique(completeData$Country_PL)
# we selected below three because they are least performing brands
# rows = 15427 Grand Hyatt
completeData[completeData$Brand_PL=="Grand Hyatt", ]
completeData$Guest.NPS.Goal_PL
# rows = 15687
completeData[completeData$Brand_PL=="Hyatt", ]
# rows = 82887, so we selected Hyatt Regency because it has more data
HyattRegency <-completeData[completeData$Brand_PL=="Hyatt Regency", ]

# Hyatt Regency performance 
#average of each hotels of regency brand
LTR_regency <- aggregate(HyattRegency$Likelihood_Recommend_H,list(HyattRegency$Hotel.Name.Long_PL),mean)
NPS_regency <- aggregate(HyattRegency$Likelihood_Recommend_H,list(HyattRegency$Hotel.Name.Long_PL),
                          FUN=function(x){
                            y <- (sum(x[x>=9])-sum(x[x<=6]))*100/sum(x);
                            return(y)})
str(NPS_regency)
NPSgoal_regency <- aggregate(HyattRegency$Guest.NPS.Goal_PL,list(HyattRegency$Hotel.Name.Long_PL),mean)
NPSdiff_regency <- NPSgoal_regency$x-NPS_regency$x
NPSdf_regency <- data.frame(NPS_regency$Group.1,NPS_regency$x,NPSgoal_regency$x,NPSdiff_regency)
colnames(NPSdf_regency) <- c("hotel_name","NPS","NPS_goal","difference")
View(NPSdf_regency)
mean(NPSdf_regency$NPS)
sum(NPSdf_regency$difference>0)
index <-which(NPSdf_regency$difference>0)

# question 3 for Regency 
regency_opinion <- lm(formula = Likelihood_Recommend_H ~ Guest_Room_H+  Condition_Hotel_H+
            Customer_SVC_H, data = HyattRegency)
summary(regency_opinion)

# underperformer: The Concourse Hotel at Los Angeles Intl Airport 
NPSdf_regency[index,]
library(ggplot2)
regency_performance <- ggplot(NPSdf_regency, aes(x=NPS, y=difference)) +
  geom_point(aes(color=difference)) +
  scale_colour_gradient(low = "red")+
  lims(x=c(25,100),y=c(-50,50)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = 68) + geom_hline(yintercept = 0) 
regency_performance

# focus on this hotel 
HyattRegency_losAngeles <- HyattRegency[HyattRegency$Hotel.Name.Long_PL=="The Concourse Hotel at Los Angeles Intl Airport",]
str(HyattRegency_losAngeles)
View(HyattRegency_losAngeles)
# gender
table(HyattRegency_losAngeles$Gender_H)
# age
table(HyattRegency_losAngeles$Age_Range_H)
# distribution of LTR
hist(HyattRegency_losAngeles$Likelihood_Recommend_H)
# purpose 
table(HyattRegency_losAngeles$POV_CODE_C)
aggregate(HyattRegency_losAngeles$Likelihood_Recommend_H,list(HyattRegency_losAngeles$POV_CODE_C),mean)
# guest's origin
HyattRegency_losAngeles$Guest_Country_H
aggregate(HyattRegency_losAngeles$Likelihood_Recommend_H,list(HyattRegency_losAngeles$Guest_Country_H),mean)

# how to know why they're not satisfied?: guest_room and hotel_condition significant 
m1_losAngeles <- lm(formula = Likelihood_Recommend_H ~ Guest_Room_H+ Tranquility_H+ Condition_Hotel_H+
            Customer_SVC_H+ Staff_Cared_H + F.B_Overall_Experience_H , data = HyattRegency_losAngeles)
summary(m1_losAngeles)

# guest_room and hotel_condition significant: wide range of opinions 
hist(HyattRegency_losAngeles$Guest_Room_H)
hist(HyattRegency_losAngeles$Condition_Hotel_H)

# facilities: all blank!
library(arules)
library(arulesViz)
HyattRegency_losAngeles$Bell.Staff_PL
HyattRegency_losAngeles$Convention_PL
HyattRegency_losAngeles$NPS_Type
facilities <- HyattRegency_losAngeles[,c( "Bell.Staff_PL",
                                        "Convention_PL",
                                        "Dry.Cleaning_PL",
"Fitness.Center_PL","Business.Center_PL","Golf_PL","Laundry_PL","Restaurant_PL",
"Shuttle.Service_PL","Spa_PL","Valet.Parking_PL","Limo.Service_PL","NPS_Type")]
str(facilities)
# how about the Regency in the US? 
facilities_us <- HyattRegency[,c( "Bell.Staff_PL",
                                             "Convention_PL",
                                             "Dry.Cleaning_PL",
                                             "Fitness.Center_PL","Business.Center_PL","Golf_PL","Laundry_PL","Restaurant_PL",
                                             "Shuttle.Service_PL","Spa_PL","Valet.Parking_PL","Limo.Service_PL","NPS_Type")]
# change character to factor 
for (i in 1:ncol(facilities_us)){
  if(is.character(facilities_us[,i])){
    facilities_us[,i]=factor(facilities_us[,i])
  }
}
str(facilities_us)

ruleset_promoters <- apriori(facilities_us, parameter = list(support=0.1,confidence=0.5),
                   appearance = list(default="lhs", rhs=("NPS_Type=Promoter")))
inspect(ruleset_promoters)
summary(ruleset_promoters)
plot(ruleset_promoters)
goodrules_promoters <-ruleset_promoters[quality(ruleset_promoters)$lift > 1.04] # convention and spa; convention,spa and restrautant 
inspect(goodrules_promoters)

# how to set support and confidence? 
ruleset_detractors <- apriori(facilities_us, parameter = list(support=0.02,confidence=0.8),
                             appearance = list(default="lhs", rhs=("NPS_Type=Passive")))
inspect(ruleset_detractors)
summary(ruleset_detractors)
plot(ruleset_detractors)
goodrules_promoters <-ruleset_promoters[quality(ruleset_promoters)$lift > 1.04] # convention and spa; convention,spa and restrautant 
inspect(goodrules_promoters)

hist(HyattRegency$Likelihood_Recommend_H)
LosAngeles <-HyattRegency[HyattRegency$City_P== "Los Angeles",]
unique(HyattRegency$Hotel.Name.Long_PL)
unique(LosAngeles$Hotel.Name.Long_PL)
aggregate(HyattRegency$Likelihood_Recommend_H,list(HyattRegency$City_PL),mean)
nrow(completeData[completeData$Brand_PL=="Grand Hyatt", ])
nrow(completeData[completeData$Brand_PL=="Hyatt", ])
nrow(completeData[completeData$Brand_PL=="Hyatt Regency", ])
#########################################################################
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
#replace NA with mean value of the same hotel 
JanData$Guest_Room_H <- with(JanData, ave(Guest_Room_H, Hotel.Name.Long_PL,
                                          FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))

sum(is.na(JanData$Guest_Room_H))

# tranquility 
JanData$Tranquility_H <- with(JanData, ave(Tranquility_H, Hotel.Name.Long_PL,
                                           FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
sum(is.na(JanData$Tranquility_H))

# hotel condition 
JanData$Condition_Hotel_H <- with(JanData, ave(Condition_Hotel_H, Hotel.Name.Long_PL,
                                               FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
sum(is.na(JanData$Condition_Hotel_H))


# Quality of customer service metric; value on a 1 to 10 scale
JanData$Customer_SVC_H <- with(JanData, ave(Customer_SVC_H, Hotel.Name.Long_PL,
                                            FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
sum(is.na(JanData$Customer_SVC_H))

# Staff cared metric; value on a 1 to 10 scale
JanData$Staff_Cared_H <- with(JanData, ave(Staff_Cared_H, Hotel.Name.Long_PL,
                                           FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
sum(is.na(JanData$Staff_Cared_H))

# Internet satisfaction 
JanData$Internet_Sat_H <- with(JanData, ave(Internet_Sat_H, Hotel.Name.Long_PL,
                                            FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
sum(is.na(JanData$Internet_Sat_H))

# Quality of the check in process metric; value on a 1 to 10 scale
JanData$Check_In_H <- with(JanData, ave(Check_In_H, Hotel.Name.Long_PL,
                                        FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
sum(is.na(JanData$Check_In_H))

# NPS goals 
JanData$Check_In_H <- with(JanData, ave(Guest.NPS.Goal_PL, Hotel.Name.Long_PL,
                                        FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
sum(is.na(JanData$Guest.NPS.Goal_PL))
JanData$Guest.NPS.Goal_PL
# check the new dataset 
View(JanData)
unique(JanData$Hotel.Name.Long_PL)


# replacing NAs in NPS goals with mean value   
mean(JanData$Guest.NPS.Goal_PL,na.rm=TRUE) # 58 
JanData$Guest.NPS.Goal_PL[is.na(JanData$Guest.NPS.Goal_PL)] <- mean(JanData$Guest.NPS.Goal_PL,na.rm=TRUE)

# merge and save the cleaned data 
completeData <- rbind(OctData,NovData,DecData,JanData)
str(completeData)
write.csv(completeData,file = "completeData.csv", row.names = FALSE)
################################################################################################


# discriptive statistics 
# which hotels have low LTR 
LTRhotel <- aggregate(completeData$Likelihood_Recommend_H,list(completeData$Hotel.Name.Long_PL),mean)

# LTR distribution 
hist(LTRhotel$x,
     main= "Distribution of Mean LTR among 396 Hotels",
     xlab= "LTR",
     col="grey",
     xlim=c(6,10),
     breaks = 14)
# NPS distribution 
hist(NPS_hotelName$x,
     main= "Distribution of Mean NPS among 396 Hotels",
     xlab= "LTR",
     col="grey",
     breaks = 14)

# Relationship of the hotel with Hyatt corporation: no significant difference
completeData$Relationship_PL
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Relationship_PL),mean)
bwplot(Likelihood_Recommend_H ~ Relationship_PL, data=completeData)

# brand 
unique(completeData$Brand_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Brand_PL),mean)
bwplot(Likelihood_Recommend_H ~ Brand_PL, data=completeData)
# gender 
completeData$Gender_H
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Gender_H),mean)
bwplot(Likelihood_Recommend_H ~ Gender_H, data=completeData)


NPS_gender <-aggregate(completeData$Likelihood_Recommend_H,list(completeData$Gender_H),
                          FUN=function(x){
                            y <- (sum(x[x>=9])-sum(x[x<=6]))*100/sum(x);
                            return(y)})
Promoter_gender <-aggregate(completeData$Likelihood_Recommend_H,list(completeData$Gender_H),
                            FUN=function(x){
                              y <- sum(x[x>=9])*100/sum(x);
                              return(y)})

View(Promoter_gender)
# Defined booking channel as per the NPS analysis
completeData$Booking_Channel
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Booking_Channel),mean)
library(lattice)
bwplot(Likelihood_Recommend_H ~ Booking_Channel, data=completeData)# digital channels and GDS have a lower LTR

# gender
table(completeData$Gender_H)
slices <- c(2189,99033,111782,4109)*100/nrow(completeData)
lbls <- c("blank", "female","male","perfer not answer")
pie(slices,labels = lbls, main="Guests' Gender")
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Gender_H),mean)
bwplot(Likelihood_Recommend_H ~ Gender_H, data=completeData)

# travel type 
table(completeData$POV_CODE_C)
slices <- c(183904,33209)*100/nrow(completeData)
lbls <- c("business", "leisure")
pie(slices,labels = lbls, main="Guests travel type")
aggregate(completeData$Likelihood_Recommend_H,list(completeData$POV_CODE_C),mean)
bwplot(Likelihood_Recommend_H ~ POV_CODE_C, data=completeData)

# internet dissatifaction  
# customers with opinions that are internt billing high have a lower LTR 
completeData$Internet_Dissat_Billing_H
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Internet_Dissat_Billing_H),mean)  
bwplot(Likelihood_Recommend_H ~ Internet_Dissat_Billing_H, data=completeData) 

# guests with opinions(yes or no) on internet speed have a lower LTR 
completeData$Internet_Dissat_Slow_H
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Internet_Dissat_Slow_H),mean) 
bwplot(Likelihood_Recommend_H ~ Internet_Dissat_Slow_H, data=completeData) 

# room internet has a significant impact on LTR 
completeData$Room_Dissat_Internet_H
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Room_Dissat_Internet_H),mean)
bwplot(Likelihood_Recommend_H ~ Room_Dissat_Internet_H, data=completeData)

# status of the survey, not significant 
completeData$Status_H
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Status_H),mean)
bwplot(Likelihood_Recommend_H ~ Status_H, data=completeData)

# GP tier of the guest, blank,gold and plat have a lower LTR 
completeData$GP_Tier_H
aggregate(completeData$Likelihood_Recommend_H,list(completeData$GP_Tier_H),mean)
bwplot(Likelihood_Recommend_H ~ GP_Tier_H, data=completeData)

# room type, there is difference, how to visualize? 
completeData$ROOM_TYPE_CODE_C
aggregate(completeData$Likelihood_Recommend_H,list(completeData$ROOM_TYPE_CODE_C),mean)

# French,Japanese and spanish surveys have a low LTR 
completeData$Language_H
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Language_H),mean)
bwplot(Likelihood_Recommend_H ~ Language_H, data=completeData)

# guest age: 26~55 has a lower LTR 
completeData$Age_Range_H
LTR_age <-aggregate(completeData$Likelihood_Recommend_H,list(completeData$Age_Range_H),mean)
bwplot(Likelihood_Recommend_H ~ Age_Range_H, data=completeData)

# guest age distribution 
age <- c(length(completeData$Age_Range_H),
         sum(completeData$Age_Range_H=="18-25"),
         sum(completeData$Age_Range_H=="26-35"),
                                              sum(completeData$Age_Range_H=="36-45"),
                                                        sum(completeData$Age_Range_H=="46-55"),
                                                            sum(completeData$Age_Range_H=="56-65"),
                                                                sum(completeData$Age_Range_H=="66-75"),
                                                                    sum(completeData$Age_Range_H=="76+"))
LTR_age$age <-age
LTR_age <-LTR_age[-1,]
counts <- table (completeData$Age_Range_H)
barplot(counts,
        main="Guest Age Distribution",
        xlab="Age")

# guests from some countries have a much lower to LTR 
unique(completeData$GUEST_COUNTRY_R)
LTR_country <-aggregate(completeData$Likelihood_Recommend_H,list(completeData$GUEST_COUNTRY_R),mean)
View(LTR_country)
lower_country <- LTR_country$x<8
sum(lower_country)

# 86% come from the US 
which.max(table(completeData$GUEST_COUNTRY_R))
max(table(completeData$GUEST_COUNTRY_R))/nrow(completeData)

# LTR vs. room type 
completeData$ROOM_TYPE_CODE_R
aggregate(completeData$Likelihood_Recommend_H,list(completeData$ROOM_TYPE_CODE_R),mean)




library(arules)
library(arulesViz)

ruleset1 <- apriori(completeData, parameter = list(support=0.005,confidence=0.5),
                   appearance = list(default="lhs", rhs=("Bell.Staff_PL==Y")))
inspect(ruleset1)

# LTR vs.bell staff: "Y" a little bit lower 
unique(completeData$Bell.Staff_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Bell.Staff_PL),mean)
bwplot(Likelihood_Recommend_H ~ Bell.Staff_PL, data=completeData)

# LTR vs. Convention_PL: "Y" a little bit lower 
unique(completeData$Convention_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Convention_PL),mean)
bwplot(Likelihood_Recommend_H ~ Convention_PL, data=completeData)


# LTR vs. Dry-Cleaning_PL: "Y" a little bit lower 
unique(completeData$Dry.Cleaning_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Dry.Cleaning_PL),mean)
bwplot(Likelihood_Recommend_H ~ Dry.Cleaning_PL, data=completeData)

# LTR vs. Fitness Center_PL: "Y" a little bit lower 
unique(completeData$Fitness.Center_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Fitness.Center_PL),mean)
bwplot(Likelihood_Recommend_H ~ Fitness.Center_PL, data=completeData)

# LTR vs. Business Center_PL: both "Y" and "N" lower 
unique(completeData$Business.Center_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Business.Center_PL),mean)
bwplot(Likelihood_Recommend_H ~ Business.Center_PL, data=completeData)

# LTR vs. Golf_PL: "N" lower 
unique(completeData$Golf_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Golf_PL),mean)
bwplot(Likelihood_Recommend_H ~ Golf_PL, data=completeData)

# LTR vs. Laundry_PL: "N"  and "Y" lower 
unique(completeData$Laundry_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Laundry_PL),mean)
bwplot(Likelihood_Recommend_H ~ Laundry_PL, data=completeData)

# LTR vs. Limo.Service_PL: "N"  and "Y" lower 
unique(completeData$Limo.Service_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Limo.Service_PL),mean)
bwplot(Likelihood_Recommend_H ~ Limo.Service_PL, data=completeData)

# LTR vs. Restaurant_PL: "Y" lower 
unique(completeData$Restaurant_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Restaurant_PL),mean)
bwplot(Likelihood_Recommend_H ~ Restaurant_PL, data=completeData)

# LTR vs. Shuttle Service_PLL: "N"  and "Y" lower 
unique(completeData$Shuttle.Service_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Shuttle.Service_PL),mean)
bwplot(Likelihood_Recommend_H ~ Shuttle.Service_PL, data=completeData)

# LTR vs. Spa_PL: "N"  and "Y" lower 
unique(completeData$Spa_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Spa_PL),mean)
bwplot(Likelihood_Recommend_H ~ Spa_PL, data=completeData)

# LTR vs. Valet Parking_PL: "Y" lower
unique(completeData$Valet.Parking_PL)
aggregate(completeData$Likelihood_Recommend_H,list(completeData$Valet.Parking_PL),mean)
bwplot(Likelihood_Recommend_H ~ Valet.Parking_PL, data=completeData)

aggregate(completeData$Guest.NPS.Goal_PL,list(completeData$Brand_PL),mean)
NPS_brand <-aggregate(completeData$Likelihood_Recommend_H,list(completeData$Brand_PL),
                          FUN=function(x){
                            y <- (sum(x[x>=9])-sum(x[x<=6]))*100/sum(x);
                            return(y)})
nrow(completeData[completeData$Brand_PL=="Hyatt",])
nrow(completeData[completeData$Brand_PL=="Hyatt Regency",])
nrow(completeData[completeData$Brand_PL=="Park Hyatt",])
nrow(completeData[completeData$Brand_PL=="Andaz",])
nrow(completeData[completeData$Brand_PL=="Grand Hyatt",])
nrow(completeData[completeData$Brand_PL=="Hyatt House",])
nrow(completeData[completeData$Brand_PL=="Hyatt Place",])

LTR_hotelName <-aggregate(completeData$Likelihood_Recommend_H,list(completeData$Hotel.Name.Long_PL),mean)
NPS_hotelName <-aggregate(completeData$Likelihood_Recommend_H,list(completeData$Hotel.Name.Long_PL),
                          FUN=function(x){
                            y <- (sum(x[x>=9])-sum(x[x<=6]))*100/sum(x);
                          return(y)})
str(NPS_hotelName)
LTR_goal <- aggregate(completeData$Guest.NPS.Goal_PL,list(completeData$Hotel.Name.Long_PL),mean)
NPSdiff <- LTR_goal$x-NPS_hotelName$x
NPSdf <- data.frame(NPS_hotelName$Group.1,NPS_hotelName$x,LTR_goal$x,NPSdiff)
colnames(NPSdf) <- c("hotel_name","NPS","NPS_goal","difference")
View(NPSdf)
mean(NPSdf$NPS)
sum(NPSdf$difference>0)
index <-which(NPSdf$difference>0)
low <- NPSdf[index,]
underperformers <- completeData[completeData$Hotel.Name.Long_PL== low$hotel_name,]
str(underperformers)
hist(underperformers$Likelihood_Recommend_H)

library(ggplot2)
p<-ggplot(NPSdf, aes(x=NPS, y=difference)) +
  geom_point(aes(color=difference)) +
  scale_colour_gradient(low = "red")+
  lims(x=c(25,100),y=c(-50,50)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = 73) + geom_hline(yintercept = 0) 
p


# guests travel type 
sum(completeData$POV_CODE_C == "BUSINESS")

# business: 84.7%
percentage <- sum(completeData$POV_CODE_C == "BUSINESS")/length(completeData$POV_CODE_C) 
percentage 

# distribution of LTR 
LTR <- completeData$Likelihood_Recommend_H
hist(LTR)

# promotors: 78%
per_promotors <- sum(LTR[LTR>=9])/sum(LTR)
per_promotors

# detractors: 5.6% 
per_detractors <- sum(LTR[LTR<=6])/sum(LTR)
per_detractors

# 72 
NPS <-per_promotors-per_detractors 
NPS

# identify the factors correlating to NPS

# Quality of customer service: 0.485
m1 <- lm(formula = Likelihood_Recommend_H ~ Customer_SVC_H, data = completeData )
summary(m1)

# Quality of customer service + Internet satisfaction:0.4909
m2 <-lm(formula = Likelihood_Recommend_H ~ Customer_SVC_H + Internet_Sat_H, data = completeData )
summary(m2)

# Tranquility metric, value on a 1 to 10 scale: 0.185
m3 <- lm(formula = Likelihood_Recommend_H ~ Tranquility_H, data = completeData )
summary(m3)

# Overall satisfaction metric, R-squared: 0.8222
m4 <- lm(formula = Likelihood_Recommend_H ~ Overall_Sat_H, data = completeData )
summary(m4)

# Guest room satisfaction 0.5405
m5 <- lm(formula = Likelihood_Recommend_H ~ Guest_Room_H, data = completeData )
summary(m5)

# Condition of hotel;0.5081
m6 <- lm(formula = Likelihood_Recommend_H ~ Condition_Hotel_H, data = completeData )
summary(m6)

# Staff cared metric: 0.2096
m7 <- lm(formula = Likelihood_Recommend_H ~ Staff_Cared_H, data = completeData)
summary(m7)

# Internet dissatisfaction, 0.1728,  only room internet dissatisfaction has a significant impact 
HyattRegency$Internet_Dissat_Billing_H
HyattRegency$Internet_Dissat_Connectivity_H
HyattRegency$Internet_Dissat_Expensive_H
HyattRegency$Internet_Dissat_Lobby_H
HyattRegency$Internet_Dissat_Other_H
HyattRegency$Internet_Dissat_Slow_H
HyattRegency$Internet_Dissat_Wired_H
HyattRegency$Room_Dissat_Internet_H
internet_us <- HyattRegency[,c( "Internet_Dissat_Billing_H",
                                  "Internet_Dissat_Connectivity_H",
                                  "Internet_Dissat_Expensive_H",
                                  "Internet_Dissat_Lobby_H","Internet_Dissat_Other_H","Internet_Dissat_Slow_H","Internet_Dissat_Wired_H","TV_Internet_General_H",
                                  "Room_Dissat_Internet_H","Likelihood_Recommend_H")]
str(internet_us)
# change character to factor 
for (i in 1:ncol(internet_us)){
  if(is.character(internet_us[,i])){
    internet_us[,i]=factor(internet_us[,i])
  }
}
str(internet_us)

internetDissatifaction <- lm(formula = internet_us$Likelihood_Recommend_H ~ internet_us$Internet_Dissat_Billing_H+
                             internet_us$Internet_Dissat_Connectivity_H+
                             internet_us$Internet_Dissat_Expensive_H+
                             internet_us$Internet_Dissat_Lobby_H+
                             internet_us$Internet_Dissat_Other_H+
                             internet_us$Internet_Dissat_Slow_H+
                             internet_us$Internet_Dissat_Wired_H+
                             internet_us$Room_Dissat_Internet_H,data = internet_us)
summary(internetDissatifaction) 
internetDissatifaction1 <- lm(formula = internet_us$Likelihood_Recommend_H ~  internet_us$Room_Dissat_Internet_H,data = internet_us )
summary(internetDissatifaction1)
# Quality of the check-in process, 0.02453
m9 <- lm(formula = Likelihood_Recommend_H ~ Check_In_H, data = completeData)
summary(m9)

# all the 8 columns: 0.6477
m22 <- lm(formula = Likelihood_Recommend_H ~
            Internet_Sat_H+
            Staff_Cared_H+
            Condition_Hotel_H+
            Guest_Room_H+
            Tranquility_H+
            Customer_SVC_H+
            F.B_Overall_Experience_H + Check_In_H, data = HyattRegency)
summary(m22)

# 6 columns: 0.6729
Regency_factor<- lm(formula = Likelihood_Recommend_H ~ Guest_Room_H+ Condition_Hotel_H+
            Customer_SVC_H+ Staff_Cared_H , data = HyattRegency)
summary(Regency_factor)
# Number of times guest visited an F&B outlet in the hotel: 0.0001568
m10 <- lm(formula = Likelihood_Recommend_H ~ F.B_FREQ_H, data = completeData)
summary(m10)

# Overal F&B experience metric; value on a 1 to 10 scale, 0.2299
m11<- lm(formula = Likelihood_Recommend_H ~ F.B_Overall_Experience_H, data = completeData)
summary(m11)



################################
# other incentives
################################

# Average daily rate on eff_date: very low 
m11<- lm(formula = Likelihood_Recommend_H ~ average_daily_rate_CC, data = completeData )
summary(m11)

# Gold passport award redemption category: very low 
m12<- lm(formula = Likelihood_Recommend_H ~ Award.Category_PL, data = completeData )
summary(m12)

# time period between booking and check-in: very low 
reservationTime <- as.numeric(as.Date(completeData$ARRIVAL_DATE_R))-as.numeric(as.Date(completeData$RESERVATION_DATE_R))
completeData$resevation <- reservationTime 

m13<- lm(formula = Likelihood_Recommend_H ~ resevation, data = completeData )
summary(m13)


# region has no significant impact on LTR 
tapply(completeData$Likelihood_Recommend_H,completeData$US.Region_PL,mean)
bwplot(Likelihood_Recommend_H ~ US.Region_PL, data=completeData)


# business users satisfaction vs. internet, 0.03533
businessData <- completeData[completeData$POV_CODE_C=="BUSINESS",]
m14 <- lm(formula = Likelihood_Recommend_H ~ Internet_Sat_H, data = businessData)
summary(m14)

# business users satisfaction vs. meeting space,0.00144 
m15 <- lm(formula = Likelihood_Recommend_H ~ Total.Meeting.Space_PL, data = businessData)
summary(m15)

# elevator: 0.001773
m16 <- lm(formula = Likelihood_Recommend_H ~ Elevators_PL, data = businessData)
summary(m16)


# geology
completeData$state <- tolower(completeData$State_PL)
unique(completeData$state)
library(ggplot2)

install.packages("devtools")
devtools::install_github("wmurphyrd/fiftystater")
library(fiftystater)
data("fifty_states")

p <- ggplot(fifty_states, aes(map_id = id)) + 
  # map points to the fifty_states shape data
  geom_map(fill="white",color="black",map = fifty_states) +
  geom_point(aes(x=completeData$Property.Longitude_PL,y=completeData$Property.Latitude_PL))+
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  theme(legend.position = "bottom")
p
p + fifty_states_inset_boxes()


# question 4: validation of question 3 
# choose row index randomly with sample() function 
HyattRegency_validation <- HyattRegency[,c("Likelihood_Recommend_H",
                                            "Condition_Hotel_H",
                                            "Guest_Room_H",
                                            "Staff_Cared_H",
                                            "Customer_SVC_H")]
nrows <- nrow(HyattRegency_validation)
nrows
random.indexes <- sample(1:nrows,replace=FALSE)
View(random.indexes)

# create a 2/3 cutpoint and round the number 
cutpoint<-floor(nrows/3*2)

# check the 2/3 cutpoint 
cutpoint

# create train data set, which contains the first 2/3 of overall data 
mydata.train <-HyattRegency_validation[random.indexes[1:cutpoint],]

# check the train dataset
str(mydata.train)

# create test data, which contains the left 1/3 of the overall data 
mydata.test <-HyattRegency_validation[random.indexes[(cutpoint+1):nrows],]

# check the test dataset
str(mydata.test)
library(kernlab)
model.ksvm <-ksvm(Likelihood_Recommend_H ~
                    Condition_Hotel_H+
                    Guest_Room_H+
                    Customer_SVC_H+
                    Staff_Cared_H,
                  data=mydata.train,
                  kernel="rbfdot",
                  kpar="automatic",
                  C=1,
                  cross=3,
                  prob.model=TRUE)

# check the model 
predicted_ksvm <-predict(model.ksvm,
                              mydata.test,
                              type="votes")

# check the prediced value 
predicted_ksvm

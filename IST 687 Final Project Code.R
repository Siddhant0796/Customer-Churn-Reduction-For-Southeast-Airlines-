# IST 687, Standard Homework Heading
#
# Student name: Liya Zhou, Siyun Ding, Renjie Yin, Haolin Liu, Siddhant Prashant Bandiwadekar 
# Homework number: Final Project
# Date due: 12/10/2019
#
# Attribution statement: 1 (choose the statements that are true)
# 1. I did this work by myself, with help from the book and the professor

# Run these three functions to get a clean test of homework code
dev.set(dev.next())
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!


########################################Data Acquisition, Cleansing, Transformation, Munging##############################################
##########################################################################################################################################

############# DATA EXTRACTION #################
#read JSON document and save as a data frame
library(jsonlite)
df <- jsonlite::fromJSON("C:/Users/yinre/Desktop/IST 687/Final Project/fall2019-survey-M07.json")

################### DATA PREPROCESSING #####################
# delete variables that will not be used in data analysis
df <- df[,-11]

# find out NA's
summary(df)

# change integrate data into numeric ones
df$Age <- as.numeric(df$Age)
df$Price.Sensitivity <- as.numeric(df$Price.Sensitivity)
df$Year.of.First.Flight <- as.numeric(df$Year.of.First.Flight)
df$Flights.Per.Year <- as.numeric(df$Flights.Per.Year)
df$Shopping.Amount.at.Airport <- as.numeric(df$Shopping.Amount.at.Airport)
df$Eating.and.Drinking.at.Airport <- as.numeric(df$Eating.and.Drinking.at.Airport)
df$Scheduled.Departure.Hour <- as.numeric(df$Scheduled.Departure.Hour)
df$Departure.Delay.in.Minutes <- as.numeric(df$Departure.Delay.in.Minutes)
df$Arrival.Delay.in.Minutes <- as.numeric(df$Arrival.Delay.in.Minutes)
df$Flight.time.in.minutes <- as.numeric(df$Flight.time.in.minutes)
df$Flight.Distance <- as.numeric(df$Flight.Distance)

# judge data distribution--not normal distribution
hist(df$Departure.Delay.in.Minutes)
hist(df$Arrival.Delay.in.Minutes)
hist(df$Flight.time.in.minutes)
# fill Na's with median
df[is.na(df$Departure.Delay.in.Minutes),"Departure.Delay.in.Minutes"] <- median(df$Departure.Delay.in.Minutes, na.rm = T)
df[is.na(df$Arrival.Delay.in.Minutes),"Arrival.Delay.in.Minutes"] <- median(df$Arrival.Delay.in.Minutes, na.rm = T)
df[is.na(df$Flight.time.in.minutes),"Flight.time.in.minutes"] <- median(df$Flight.time.in.minutes, na.rm = T)

# delete the state abbreviation in destination and origin city
library(stringr)
df$Destination.City <- gsub("\\,", "", str_extract_all(df$Destination.City, ".*\\,"))
df$Origin.City <- gsub("\\,", "", str_extract_all(df$Origin.City, ".*\\,"))

# extract flight month
df$Flight.Month <- as.numeric(gsub("\\/", "", str_extract_all(df$Flight.date, "^.{1,2}\\/")))
df$Flight.Year <- 2014
# delete flight date
df <- df[,-15]
library(ggplot2)
ggplot(df, aes(x = Partner.Code, y = frequency(df$Partner.Code), fill = Partner.Name))+ geom_col()

# delete Partner Code since each code correspond to the company.
df <- df[, -15]

# add a new variable of the length of year that a customer have taken the flight
df$Year.Lenghth <- df$Flight.Year-df$Year.of.First.Flight
# delete the used variables
df <- df[, -c(7,31)]

# classify the likelihood recommend
df$recommend.level[which(df$Likelihood.to.recommend > 8)] <- "yes"
df$recommend.level[which(df$Likelihood.to.recommend <= 8)] <- "no"





##################################################DESCRIPTIVE STATISTICS & VISUALIZATION########################################
################################################################################################################################
# read updated data frame
library(jsonlite)
df <- jsonlite::fromJSON("~/Desktop/df.json")
df_customer <- df[, c(3,4,5,6,7,9,10,11,12,29,8,23,30)]
View(df_customer)

################ Visualization #############
library(ggplot2)
# explore the relationship between likelihood and loyalty
ggplot(data = df, aes(x = Likelihood.to.recommend, y = Loyalty)) + geom_boxplot() + facet_wrap(~ Likelihood.to.recommend, scales="free")
# As we can see, with the growth of likelihood to recommend, the average of loyalty is increasing. Withe the growth of likelihood, volatility of lotalty is relatively greater

# explore class, travel type and status distribution
ggplot(df_customer, aes(x = Class)) + geom_bar(width = 2/3) # most economic
ggplot(df_customer, aes(x = Type.of.Travel)) + geom_bar(width = 2/3) # most for business travel
ggplot(df_customer, aes(x = Airline.Status)) + geom_bar(width = 2/3) # most blue
ggplot(df_customer, aes(x = Class)) + 
  geom_bar(aes(fill = Airline.Status)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# explore year length, age, gendar and price sensitive
ggplot(df_customer, aes(x = Age, y = Price.Sensitivity)) + 
  geom_col(aes(col = Gender))
# Female is more sensitive to price than male. People above 80 and between 30 to 50 seem more sensitive.

# explore flight per year, year length and airline status
ggplot(df_customer, aes(x=Flights.Per.Year)) +
  geom_density(aes(fill=Airline.Status), position = "fill")

# explore price sensitivity and loyalty or likelihood
df_customer %>%
  ggplot() +
  aes(x = Price.Sensitivity) +
  geom_bar(aes(fill = recommend.level))

# explore shopping amount, eating & drinking amount, class and travel type
df_customer$Departure.Delay.in.Minutes <- df$Departure.Delay.in.Minutes
ggplot(df_customer, aes(x = Class, y = Shopping.Amount.at.Airport)) +
  geom_col(aes(color = Type.of.Travel))

ggplot(df_customer, aes(x = Class, y = Shopping.Amount.at.Airport)) +
  geom_col(aes(color = Type.of.Travel))

ggplot(df_customer[df_customer$Shopping.Amount.at.Airport<200 & df_customer$Eating.and.Drinking.at.Airport<200,], aes(x = Eating.and.Drinking.at.Airport, y = Shopping.Amount.at.Airport)) +
  geom_point(cex = 2, aes(alpha = Airline.Status, color = Type.of.Travel, shape = Class))

############################################ linear model #######################################################################################
df_lm <- df_customer
lmodel0 <- lm(formula = Loyalty ~., data = df_lm[,c(-12,-13)])
summary(lmodel0)
lmodel1 <- lm(formula = Likelihood.to.recommend ~ ., data = df_lm[,c(-11,-13)])
summary(lmodel1)
lmodel2 <- lm(formula = recommend.level ~ ., data = df_lm[,c(-11,-12)])

# select remarkable variable 
lmodel2 <- lm(formula = Likelihood.to.recommend ~ Airline.Status + Age + Flights.Per.Year + Type.of.Travel + Shopping.Amount.at.Airport + Eating.and.Drinking.at.Airport + Departure.Delay.in.Minutes, data = df_customer)
summary(lmodel2)

lmodel3 <- lm(formula = Likelihood.to.recommend ~ Age, data = df_customer)
summary(lmodel3)




############ SVM to predict loyalty ####################
install.packages("kernlab")
library(kernlab)

df_svm <- df_customer[,c(-12,-14)]
df_svm$Loyalty[df_svm$Loyalty>=0.5] <- "loyal"
df_svm$Loyalty[df_svm$Loyalty != "loyal"] <- "not loyal"

df_svm$randIndex <- sample(1:dim(df_svm)[1])

cutPoint2_3 <- floor(2 * dim(df_svm)[1]/3)
trainData <- df_svm[df_svm$randIndex[1:cutPoint2_3],]
testData <- df_svm[df_svm$randIndex[(cutPoint2_3+1):dim(df_svm)[1]],]

install.packages("caret")
library(caret)
svmmodel <- ksvm(Loyalty ~ ., data = trainData[,-12], kernel = "rbfdot", kpar = "automatic", 
                 C = 5, cross = 3, prob.model = TRUE)
svmPred <- predict(svmmodel, testData)

svmPred <- as.factor(svmPred)
testData$Loyalty <- as.factor(testData$Loyalty)
install.packages("e1071")
library(e1071)
confusionMatrix(svmPred,testData$Loyalty)

svmmodel2 <- ksvm(recommend.level ~ ., data = trainData[,-11], kernel = "rbfdot", kpar = "automatic", 
                  C = 5, cross = 3, prob.model = TRUE)
svmPred2 <- predict(svmmodel2, testData)

svmPred2 <- as.factor(svmPred2)
testData$recommend.level <- as.factor(testData$recommend.level)
confusionMatrix(svmPred2,testData$recommend.level)

################# cart tree to predict recommend #################
install.packages("tree")
library(tree)
cartTree <- train(recommend.level ~., data = trainData[,-11], method = "treebag", preProc = c("center","scale"))
preoutput <- predict(cartTree, testData)
preoutput <- as.factor(preoutput)
testData$recommend.level <- as.factor(testData$recommend.level)
confusionMatrix(preoutput,testData$recommend.level)









##################################USE OF MODELING TECHNIQUES & VISUALIZATION######################################################
##################################################################################################################################
# SVM: Delay and flight time in minutes that may lead to the change of loyalty/likelihood to recommend
dfnew <- df
View(dfnew)

dfnew$True.Arrival.Delay <- dfnew$Arrival.Delay.in.Minutes
dfnew$True.Arrival.Delay[which(df$Arrival.Delay.in.Minutes < 6)] <- 0
trainindex <- sample(c(1,2), nrow(dfnew),replace= T,prob = c(0.67,0.33))
traindata <- dfnew[trainindex==1,]
testdata <- dfnew[trainindex==2,]
svmOutput1 <- ksvm(Loyalty ~ Departure.Delay.in.Minutes+True.Arrival.Delay+Flight.time.in.minutes,
                   data=traindata,kernel="rbfdot", kpar="automatic",C=40,cross=4, prob.model=TRUE)
svmOutput1

svmresult1 <- predict(svmOutput,testdata,type="votes")
View(svmresult1)
comparable1 <- (testdata[,8]-svmresult)<0.8&(testdata[,8]-svmresult)>-0.7
result1 <- table(comparable)
result1
accuraryratio1 <- result[2]/(sum(result))
accuraryratio1

svmOutput2 <- ksvm(Likelihood.to.recommend ~ Departure.Delay.in.Minutes+True.Arrival.Delay+
                     Flight.time.in.minutes+Flight.Distance,
                   data=traindata,kernel="rbfdot", kpar="automatic",C=40,cross=4, prob.model=TRUE)
svmOutput2

svmresult2 <- predict(svmOutput,testdata,type="votes")
View(svmresult2)
comparable2 <- (testdata[,25]-svmresult)<6.5&(testdata[,25]-svmresult)>3.5
result2 <- table(comparable)
result2
accuraryratio2 <- result[2]/(sum(result))
accuraryratio2

# correlation in Departure delay,loyalty and LTR
Plot_Dep.Delay <- ggplot(df,aes(x = Departure.Delay.in.Minutes,y = Loyalty))+
  geom_boxplot(aes(fill=recommend.level),position = 'dodge')+
  ggtitle('Loyalty versus Departure delay')
Plot_Dep.Delay

# correlation in Arrival delay, loyalty and LTR
Plot_Arr.Delay <- ggplot(df,aes(x = Arrival.Delay.in.Minutes, y = Loyalty))+
  geom_boxplot(aes(fill=recommend.level),position = 'dodge')+
  ggtitle('Loyalty versus Arrival delay')
Plot_Arr.Delay

# correlation in Flight time, loyalty and LTR
Plot_F.time <- ggplot(df,aes(x = Flight.time.in.minutes, y = Loyalty))+
  geom_spot(aes(fill=recommend.level),position = 'dodge')+
  ggtitle('Loyalty versus Flight time')
Plot_F.time

# correlation in Flight distance, loyalty and LTR
Plot_F.distance <- ggplot(df,aes(x = Flight.Distance, y = Loyalty))+
  geom_boxplot(aes(fill=recommend.level),position = 'dodge')+
  ggtitle('Loyalty versus Flight distance')
Plot_F.distance

################################ Association rules #########################
install.packages("arules")
install.packages("arulesViz")
install.packages("arules")
install.packages("arulesViz")
library("arules")
library(arulesViz)
df_customer$Old <- "adult"
df_customer$Old[which(df_customer$Age <= 21)] <- "young"
df_customer$Old[which(df_customer$Age >= 60)] <- "old"

# find out the consumption relationship
df_rule_con <- df_customer[,c(1,3,5,6,7,8,9,10,15)]

df_rule_con$shopping[df_rule_con$Shopping.Amount.at.Airport != 0] <- "yes"
df_rule_con$shopping[df_rule_con$Shopping.Amount.at.Airport == 0] <- "no"

df_rule_con$eating[df_rule_con$Eating.and.Drinking.at.Airport != 0] <- "yes"
df_rule_con$eating[df_rule_con$Eating.and.Drinking.at.Airport == 0] <- "no"

df_rule_con$Flights.Freq <- "med"
df_rule_con$Flights.Freq[which(df_rule_con$Flights.Per.Year <= 10)] <- "low"
df_rule_con$Flights.Freq[which(df_rule_con$Flights.Per.Year >= 30)] <- "high"

df_rule_con$Year.length[which(df_rule_con$Year.Lenghth < 6)] <- "new"
df_rule_con$Year.length[which(df_rule_con$Year.Lenghth >= 6)] <- "regular"

df_rule_con <- df_rule_con[,c(-3,-5,-6,-8)]

df_rule_con <- as(df_rule_con, "transactions")
ruleset_con1 <- apriori(df_rule_con[,-7], parameter = list(support = 0.005, confidence = 0.5), 
                        appearance = list(default="lhs", rhs=("shopping=yes")))
ruleset_con2 <- apriori(df_rule_con[,-6], parameter = list(support = 0.005, confidence = 0.5), 
                        appearance = list(default="lhs", rhs=("eating=yes")))
plot(ruleset_con1, jitter = 0)
inspect(ruleset_con1[which.max(quality(ruleset_con1)$lift)])
plot(ruleset_con2, jitter = 0)
inspect(ruleset_con2[which.max(quality(ruleset_con2)$lift)])

# find out the most related factor to recommendation
df_rule_r <- df_customer

df_rule_r$Price.Sensitivity <- as.factor(df_rule_r$Price.Sensitivity)

ggplot(aes(x=Flights.Per.Year), data = df_rule_r) + geom_histogram(col="black")
df_rule_r$Flights.Freq <- "med"
df_rule_r$Flights.Freq[which(df_rule_r$Flights.Per.Year <= 10)] <- "low"
df_rule_r$Flights.Freq[which(df_rule_r$Flights.Per.Year >= 30)] <- "high"

ggplot(aes(x=df_rule_r$Year.Lenghth), data = df_rule_r) + geom_histogram(col="black")
df_rule_r$Year.length[which(df_rule_r$Year.Lenghth < 6)] <- "new"
df_rule_r$Year.length[which(df_rule_r$Year.Lenghth >= 6)] <- "regular"

df_rule_r$delay[which(as.numeric(df_rule_r$Departure.Delay.in.Minutes) == 0)] <- "on time"
df_rule_r$delay[is.na(df_rule_r$delay)] <- "delay"

df_rule_r <- df_rule_r[,c(-2,-5,-7,-8,-10,-11,-12,-14)]

df_rule_r <- as(df_rule_r, "transactions")
ruleset_recom <- apriori(df_rule_r, parameter = list(support = 0.005, confidence = 0.5), 
                         appearance = list(default="lhs", rhs=("recommend.level=yes")))
plot(ruleset_recom)
inspect(ruleset_recom)
inspect(ruleset_recom[which.max(quality(ruleset)$lift)])

# find out relationship to loyalty
df_rule_l <- df_customer
df_rule_l$Loyalty[df_rule_l$Loyalty>=0.5] <- "loyal"
df_rule_l$Loyalty[df_rule_l$Loyalty != "loyal"] <- "not loyal"

df_rule_l$Price.Sensitivity <- as.factor(df_rule_l$Price.Sensitivity)

ggplot(aes(x=df_rule_l$Flights.Per.Year), data = df_rule_l) + geom_histogram(col="black")
df_rule_l$Flights.Freq <- "med"
df_rule_l$Flights.Freq[which(df_rule_l$Flights.Per.Year <= 10)] <- "low"
df_rule_l$Flights.Freq[which(df_rule_l$Flights.Per.Year >= 30)] <- "high"

ggplot(aes(x=df_rule_l$Year.Lenghth), data = df_rule_l) + geom_histogram(col="black")
df_rule_l$Year.length[which(df_rule_l$Year.Lenghth < 6)] <- "new"
df_rule_l$Year.length[which(df_rule_l$Year.Lenghth >= 6)] <- "regular"

str(df_rule_l$Departure.Delay.in.Minutes)
df_rule_l$delay[which(as.numeric(df_rule_l$Departure.Delay.in.Minutes) == 0)] <- "on time"
df_rule_l$delay[is.na(df_rule_l$delay)] <- "delay"
df_rule_l <- df_rule_l[,c(-2,-5,-7,-8,-10,-12,-13,-14)]

df_rule_l <- as(df_rule_l, "transactions")
ruleset_loyal <- apriori(df_rule_l, parameter = list(support = 0.005, confidence = 0.5), 
                         appearance = list(default="lhs", rhs=("Loyalty=loyal")))
inspect(ruleset_loyal)
inspect(ruleset_loyal[which.max(quality(ruleset_loyal)$lift)])

df_flight <- df
df_flight$Departure.Delay <- df_flight$Departure.Delay.in.Minutes
df_flight$Departure.Delay[which(df_flight$Departure.Delay.in.Minutes > 0)] <- "yes"
df_flight$Departure.Delay[which(df_flight$Departure.Delay.in.Minutes == 0)] <- "no"

df_flight$Arrival.Delay <- df_flight$Arrival.Delay.in.Minutes
df_flight$Arrival.Delay[which(df_flight$Arrival.Delay.in.Minutes > 0)] <- "yes"
df_flight$Arrival.Delay[which(df_flight$Arrival.Delay.in.Minutes == 0)] <- "no"

df_rule <- df_flight[,c(2,22,33,34,35)]
df_rule <- data.frame(df_flight$Origin.City,df_flight$Flight.cancelled,df_flight$Departure.Delay,df_flight$Arrival.Delay,df_flight$recommend.level)
df_rule <- as(df_rule, "transactions")
ruleset <- apriori(df_rule, 
                   parameter = list(support = 0.005, confidence = 0.5), 
                   appearance = list(default="lhs", rhs=("recommend.level=yes")))
plot(ruleset,jitter = 0)
inspect(ruleset)
inspect(ruleset[which.max(quality(ruleset)$lift)])

df_rule_l <- df_flight
df_rule_l$Loyalty[df_rule_l$Loyalty>=0.5] <- "loyal"
df_rule_l$Loyalty[df_rule_l$Loyalty != "loyal"] <- "not loyal"
df_rule_l <- df_rule_l[,c(2,9,22,33,34,35)]
df_rule_l <- as(df_rule_l, "transactions")
colnames(df_rule_l$Loyalty) <- "loyalty"
ruleset_l <- apriori(df_rule_l, parameter = list(support = 0.005, confidence = 0.5), 
                     appearance = list(default="lhs", rhs=("Loyalty = loyal")))

# flight from or to which place has possibility to cancel or delay
possible_cancel <- df_flight %>%
  filter(Flight.cancelled == 'Yes') %>%
  group_by(Origin.City) %>%
  summarise(count = n())
possible_D.Delay <- df_flight %>%
  filter(Departure.Delay == 'yes') %>%
  group_by(Origin.City) %>%
  summarise(count = n())
possible_A.Delay <- df_flight %>%
  filter(Arrival.Delay == 'yes') %>%
  group_by(Origin.City) %>%
  summarise(count = n())
possible_cancel <- data.frame(possible_cancel)
possible_cancel$count <- sort(possible_cancel$count,decreasing = TRUE)
possible_D.Delay <- data.frame(possible_D.Delay)
possible_D.Delay$count <- sort(possible_D.Delay$count,decreasing = TRUE)
possible_A.Delay <- data.frame(possible_A.Delay)
possible_A.Delay$count <- sort(possible_A.Delay$count,decreasing = TRUE)
head(possible_cancel,10)
head(possible_D.Delay,10)
head(possible_A.Delay,10)





#################################################################Partner analysis#####################################################################################
######################################################################################################################################################################

a<-unique(df$Partner.Name)
View(a)

states <- map_data("state")

MAP<- ggplot(data = states) +
  borders("state",colour = "black",fill = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE) 
MAP


EF <- subset(df,Partner.Name=="EnjoyFlying Air Services")
EFmap <- MAP +
  geom_curve(data=EF,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="black",
             size=0.70,
             curvature=0.1) +
  geom_point(data=EF,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  geom_point(data=EF,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12))+ 
  ggtitle("Enjoyflying") 

EFmap



##################################################################################


SE <- subset(df,Partner.Name=="Southeast Airlines Co.")
SEmap <- MAP+
  geom_curve(data=SE,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=SE,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=enjoyflying,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("Southeast Airlines Co.") 
SEmap

####################################################################################


#Cheapseats Airlines Inc.


CA <- subset(df,Partner.Name=="Cheapseats Airlines Inc.")

CAmap <- MAP+
  geom_curve(data=CA,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=CA,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=CA,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("Cheapseats Airlines Inc.") 

CAmap


#######################################################################################
#GoingNorth Airlines Inc.

GN <- subset(df,Partner.Name=="GoingNorth Airlines Inc.")

GNmap <- MAP+
  geom_curve(data=GN,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=GN,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=GN,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("GoingNorth Airlines Inc.") 
GNmap

#####################################################################################
#FlyToSun Airlines Inc.

FA <- subset(df,Partner.Name=="FlyToSun Airlines Inc.")

FAmap <- MAP+
  geom_curve(data=FA,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=FA,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=FA,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("FlyToSun Airlines Inc.") 
FAmap

#################################################################################


#Paul Smith Airlines Inc.

PS <- subset(df,Partner.Name=="Paul Smith Airlines Inc.")

PSmap <- MAP+
  geom_curve(data=PS,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=PS,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=PS,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("Paul Smith Airlines Inc") 

PSmap

###############################################################################

#OnlyJets Airlines Inc.

OJ <- subset(df,Partner.Name=="OnlyJets Airlines Inc.")
OJmap <- MAP+
  geom_curve(data=OJ,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=OJ,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=OJ,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("OnlyJets Airlines Inc.") 

OJmap

###############################################################################

#Sigma Airlines Inc.

SA <- subset(df,Partner.Name=="Sigma Airlines Inc.")
SAmap <- MAP+
  geom_curve(data=SA,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=SA,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=SA,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("Sigma Airlines Inc.") 

SAmap


##############################################################################

#Northwest Business Airlines Inc.


NBA <- subset(df,Partner.Name=="Northwest Business Airlines Inc.")
NBAmap <- MAP+
  geom_curve(data=NBA,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=NBA,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=NBA,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("Northwest Business Airlines Inc.") 

NBAmap


############################################################################


#FlyFast Airways Inc.


FF <- subset(df,Partner.Name=="FlyFast Airways Inc.")
FFmap <- MAP+
  geom_curve(data=FF,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=FF,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=FF,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("FlyFast Airways Inc.") 

FFmap


############################################################################


#Oursin Airlines Inc.


OA <- subset(df,Partner.Name=="Oursin Airlines Inc.")
OAmap <- MAP+
  geom_curve(data=OA,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=OA,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=OA,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("Oursin Airlines Inc.")

OAmap


#####################################################################

#FlyHere Airways


FH <- subset(df,Partner.Name=="FlyHere Airways")
FHmap <- MAP+
  geom_curve(data=FH,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=FH,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=FH,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("FlyHere Airways")

FHmap

#####################################################################


#Cool&Young Airlines Inc.

CN <- subset(df,Partner.Name=="Cool&Young Airlines Inc.")
CNmap <- MAP+
  geom_curve(data=CN,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=CN,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=CN,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme( axis.line = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks = element_blank(),
         plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("Cool&Young Airlines Inc.") 
CNmap


######################################################################

#West Airways Inc.


WA <- subset(df,Partner.Name=="West Airways Inc.")
WAmap <- MAP+
  geom_curve(data=WA,
             aes(x=olong,y=olat,xend=dlong,yend=dlat),
             color="#17202A",
             size=0.70,
             curvature=0.1) +
  geom_point(data=WA,
             aes(x=olong,y=olat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue" ) +
  geom_point(data=WA,
             aes(x=dlong,y=dlat),
             size = 2.5, 
             alpha = 1, 
             na.rm = T,
             shape = 20, 
             colour = "blue"
  ) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12)) + 
  ggtitle("West Airways Inc.")


WAmap

######################################################################

unique(df$Type.of.Travel)
unique(df$Partner.Name)

stat1 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="Cheapseats Airlines Inc.") 
One<-nrow(stat1)

stat2 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="GoingNorth Airlines Inc.") 
Two<-nrow(stat2)

stat3 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="FlyToSun Airlines Inc.") 
Three<-nrow(stat3)

stat4 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="Paul Smith Airlines Inc.") 
Four <- nrow(stat4)

stat5 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="OnlyJets Airlines Inc.") 
Five <- nrow(stat5)

stat6 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="Sigma Airlines Inc.") 
Six <-nrow(stat6)

stat7 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="Northwest Business Airlines Inc.") 
Seven <-nrow(stat7)

stat8 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="FlyFast Airways Inc.") 
Eight <-nrow(stat8)

stat9 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="Oursin Airlines Inc.") 
Nine <-nrow(stat9)

stat10 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="Southeast Airlines Co.") 
Ten <-nrow(stat10)

stat11 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="FlyHere Airways") 
Eleven <-nrow(stat11)

stat12 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="EnjoyFlying Air Services") 
Twelve <-nrow(stat12)

stat13 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="Cool&Young Airlines Inc.") 
Thirteen <-nrow(stat13)

stat14 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Business travel",(df$Partner.Name)=="West Airways Inc.") 
Fourteen <-nrow(stat14)


Business.Travel <- c(One,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Eleven,Twelve,Thirteen,Fourteen)
View(Business.Travel)

#################################################################################################################

stat11 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="Cheapseats Airlines Inc.") 
One1<-nrow(stat11)

stat21 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="GoingNorth Airlines Inc.") 
Two1<-nrow(stat21)

stat31 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="FlyToSun Airlines Inc.") 
Three1<-nrow(stat31)

stat41 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="Paul Smith Airlines Inc.") 
Four1 <- nrow(stat41)

stat51 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="OnlyJets Airlines Inc.") 
Five1 <- nrow(stat51)

stat61 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="Sigma Airlines Inc.") 
Six1 <-nrow(stat61)

stat71 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="Northwest Business Airlines Inc.") 
Seven1 <-nrow(stat71)

stat81 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="FlyFast Airways Inc.") 
Eight1 <-nrow(stat81)

stat91 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="Oursin Airlines Inc.") 
Nine1 <-nrow(stat91)

stat101 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="Southeast Airlines Co.") 
Ten1 <-nrow(stat101)

stat111 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="FlyHere Airways") 
Eleven1 <-nrow(stat111)

stat121 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="EnjoyFlying Air Services") 
Twelve1 <-nrow(stat121)

stat131 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="Cool&Young Airlines Inc.") 
Thirteen1 <-nrow(stat131)

stat141 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Personal Travel",(df$Partner.Name)=="West Airways Inc.") 
Fourteen1 <-nrow(stat141)

Personal.Travel <- c(One1,Two1,Three1,Four1,Five1,Six1,Seven1,Eight1,Nine1,Ten1,Eleven1,Twelve1,Thirteen1,Fourteen1)
View(Personal.Travel)

#######################################################################################################

stat111 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="Cheapseats Airlines Inc.") 
One11<-nrow(stat111)

stat211 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="GoingNorth Airlines Inc.") 
Two11<-nrow(stat211)

stat311 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="FlyToSun Airlines Inc.") 
Three11<-nrow(stat311)

stat411 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="Paul Smith Airlines Inc.") 
Four11 <- nrow(stat411)

stat511 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="OnlyJets Airlines Inc.") 
Five11 <- nrow(stat511)

stat611 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="Sigma Airlines Inc.") 
Six11 <-nrow(stat611)

stat711 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="Northwest Business Airlines Inc.") 
Seven11 <-nrow(stat711)

stat811 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="FlyFast Airways Inc.") 
Eight11 <-nrow(stat811)

stat911 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="Oursin Airlines Inc.") 
Nine11 <-nrow(stat911)

stat101 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="Southeast Airlines Co.") 
Ten11 <-nrow(stat101)

stat1111 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="FlyHere Airways") 
Eleven11 <-nrow(stat1111)

stat1211 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="EnjoyFlying Air Services") 
Twelve11 <-nrow(stat1211)

stat1311 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="Cool&Young Airlines Inc.") 
Thirteen11 <-nrow(stat1311)

stat1411 <- df %>%
  filter(str_trim(df$Type.of.Travel)=="Mileage tickets",(df$Partner.Name)=="West Airways Inc.") 
Fourteen11 <-nrow(stat1411)

Mileage.Tickets <- c(One11,Two11,Three11,Four11,Five11,Six11,Seven11,Eight11,Nine11,Ten11,Eleven11,Twelve11,Thirteen11,Fourteen11)
View(Mileage.Tickets)

CustAnalysis <- data.frame(Business.Travel,Personal.Travel,Mileage.Tickets)
View(CustAnalysis)

unique(df$Partner.Name)
Names <- c("Cheapseats Airlines Inc.",
           "GoingNorth Airlines Inc.",
           "FlyToSun Airlines Inc." ,
           "Paul Smith Airlines Inc." ,
           "OnlyJets Airlines Inc.",
           "Sigma Airlines Inc." ,
           "Northwest Business Airlines Inc.",
           "FlyFast Airways Inc.",
           "Oursin Airlines Inc." ,
           "Southeast Airlines Co.",
           "FlyHere Airways",
           "EnjoyFlying Air Services",
           "Cool&Young Airlines Inc.",
           "Cool&Young Airlines Inc.")

CustAnalysis <- data.frame(Names,Business.Travel,Personal.Travel,Mileage.Tickets)
View(CustAnalysis)          


row.names(CustAnalysis[which.max(CustAnalysis$Business.Travel),])          
row.names(CustAnalysis[which.max(CustAnalysis$Personal.Travel),])            
row.names(CustAnalysis[which.max(CustAnalysis$Mileage.Tickets),]) 


row.names(CustAnalysis[which.min(CustAnalysis$Business.Travel),])   
row.names(CustAnalysis[which.min(CustAnalysis$Personal.Travel),])            
row.names(CustAnalysis[which.min(CustAnalysis$Mileage.Tickets),]) 







##################################################################Feedback Analysis-Text Mining#######################################################################
######################################################################################################################################################################
#Loading the Package
library(NLP)
library(tm)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(RColorBrewer)
library(wordcloud)


####################Loyalty#########################
#Delete the rows with positive loyalty
df1<-filter(df,df$Loyalty<=0)
#Extracting the free Text
Comment<-df1$freeText
#Cleaning the NA in Text
Comment<-na.omit(Comment)
words.vec <- VectorSource(c(Comment))
# Make a corpus of web data
words.corpus <- Corpus(words.vec)
#To lower
words.corpus <- tm_map(words.corpus,content_transformer(tolower))
#removePunctuation
words.corpus <- tm_map(words.corpus,removePunctuation)
#removeNumbers
words.corpus <- tm_map(words.corpus, removeNumbers)
#Words no nned to be used
otherwords<-c("now","well","however","always","way","much","take","took","can","poor","better","even","flying","didnt","bad","dont","due","never","flew","airline","flight","southeast","service","good","one","Always","also","just","best","nice","great","first","will","many","ever","fly","get","got","really","like","worst")
#Taking out stop words
words.corpus <- tm_map(words.corpus, removeWords,c(stopwords("english"),otherwords))


#Create a document-term-matrix
tdm <- TermDocumentMatrix(words.corpus)
#coerce text data back into a plain data matrix
m<-as.matrix(tdm)
wordCounts<-rowSums(m)
wordCounts<-sort(wordCounts,decreasing=TRUE)



#Create a word cloud
cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
wordcloud(cloudFrame$word,cloudFrame$freq,max.words=150, random.order=FALSE, rot.per=0.15)


#Creating new dataframe with 50 most frequent words
CF25<-head(cloudFrame,25)
CF25<-arrange(CF25,desc(freq))
#Plot word frequencies
CF25%>% ggplot(aes(x = reorder(CF25$word,-CF25$freq), y = CF25$freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 25 Feature Words based on Negative Loyalty", x = "Word", y= "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

####################NPS#########################
#Delete the rows with positive loyalty
df2<-filter(df,df$Likelihood.to.recommend<7)
#Extracting the free Text
Comment2<-df2$freeText
#Cleaning the NA in Text
Comment2<-na.omit(Comment2)

words.vec2 <- VectorSource(c(Comment2))
# Make a corpus of web data
words.corpus2 <- Corpus(words.vec2)
#To lower
words.corpus2 <- tm_map(words.corpus2,content_transformer(tolower))
#removePunctuation
words.corpus2 <- tm_map(words.corpus2,removePunctuation)
#removeNumbers
words.corpus2 <- tm_map(words.corpus2, removeNumbers)
#Taking out stop words
words.corpus2 <- tm_map(words.corpus2, removeWords,c(stopwords("english"),otherwords))



#Create a document-term-matrix
tdm2 <- TermDocumentMatrix(words.corpus2)
#coerce text data back into a plain data matrix
m2<-as.matrix(tdm2)
wordCounts2<-rowSums(m2)
wordCounts2<-sort(wordCounts2,decreasing=TRUE)


#Create a word cloud
cloudFrame2<-data.frame(word=names(wordCounts2),freq=wordCounts2)
wordcloud(cloudFrame2$word,cloudFrame2$freq,max.words=150, random.order=FALSE, rot.per=0.15, color= "blue")


#Creating new dataframe with 50 most frequent words
CF252<-head(cloudFrame2,25)
CF252<-arrange(CF252,desc(freq))
#Plot word frequencies
CF252%>% ggplot(aes(x = reorder(CF252$word,-CF252$freq), y = CF252$freq)) +
  geom_bar(stat = "identity",color="blue") +
  labs(title = "Top 25 feature words based on Low NPS", x = "Word", y= "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 




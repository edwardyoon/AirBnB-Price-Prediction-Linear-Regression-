library(forecast)
library(leaps)
library(ggplot2)
library(gplots)
library(dplyr)
library(dummies)


#Loading the datset in R

Airbnb1 <- read.csv("Airbnb_train_new.csv")
View(Airbnb1)

summary(Airbnb1$price)

mean(Airbnb1[,"price"])


###Code for removing the outliers
Airbnb <- Airbnb1[(Airbnb1$price > 0 & Airbnb1$price < 701.9095),]
Air <- Airbnb1[(Airbnb1$price > 0 & Airbnb1$price < 701.9095),]
View(Airbnb)


#code for imputing the mean for the missing values
Airbnb = transform(Airbnb, bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm=TRUE), bathrooms))
Airbnb = transform(Airbnb, host_response_rate = ifelse(is.na(host_response_rate), mean(host_response_rate, na.rm=TRUE), host_response_rate))
Airbnb = transform(Airbnb, review_scores_rating = ifelse(is.na(review_scores_rating), mean(review_scores_rating, na.rm=TRUE), review_scores_rating))
Airbnb = transform(Airbnb, bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm=TRUE), bedrooms))
Airbnb = transform(Airbnb, beds = ifelse(is.na(beds), mean(beds, na.rm=TRUE), beds))


summary(Airbnb)
View(Airbnb)



# Creating dummy variables
Airbnb <- cbind(Airbnb, dummy(Airbnb$room_type, sep="_"))
Airbnb <- cbind(Airbnb, dummy(Airbnb$city, sep="_"))
Airbnb <- cbind(Airbnb, dummy(Airbnb$bed_type, sep="_"))
Airbnb <- cbind(Airbnb, dummy(Airbnb$cancellation_policy, sep="_"))
View(Airbnb)
Airbnb <- Airbnb[,-2]
Airbnb <- Airbnb[,-4]
Airbnb <- Airbnb[,-4]
Airbnb <- Airbnb[,-5]
View(Airbnb)




# partition data into test and training data
set.seed(1)  # set seed for reproducing the partition
numberOfRows <- nrow(Airbnb)
train.index <- sample(numberOfRows, numberOfRows*0.6)  
train_part <- Airbnb[train.index,]
test_part <- Airbnb[-train.index,]

View(train_part)
View(test_part)



###############################Training model Final #####################################

#R-squared:  0.3639, Residual standard error: 89.21
m1 <- lm(price ~ accommodates + bathrooms + cleaning_fee + host_response_rate + instant_bookable + number_of_reviews + review_scores_rating,data=train_part)
summary(m1)

#R-squared:  0.02799, Residual standard error: 89.21
m2 <- lm(price ~ Airbnb_PulloutSofa + Airbnb_Futon + Airbnb_Real_Bed + Internet + Patio.or.balcony + Smart.lock + Hair.dryer + Extra.pillows.and.blankets + Elevator.in.building + property_type_Train + property_type_Treehouse + property_type_Vacation.home + property_type_Villa + property_type_Yurt + property_type_Loft + property_type_Camper.RV + property_type_Bungalow, data=train_part)
summary(m2)

#R-squared:  0.3776, Residual standard error: 88.3
#Added Property type variables with m1
m3 <- lm(price ~ accommodates + bathrooms + cleaning_fee + host_response_rate + instant_bookable + number_of_reviews + review_scores_rating + property_type_Apartment	+ property_type_Boat + property_type_Boutique.hotel	+ property_type_Bungalow + property_type_Cabin + property_type_Camper.RV + 	property_type_Casa.particular + property_type_Castle + property_type_Cave	+ property_type_Chalet + property_type_Condominium	+ property_type_Dorm + property_type_Earth.House + 	property_type_Guest.suite + property_type_Guesthouse + property_type_Hostel	+ property_type_House	+ property_type_Hut	+ property_type_In.law + property_type_Island + property_type_Lighthouse + property_type_Loft + property_type_Other	+ property_type_Parking.Space + property_type_Serviced.apartment + property_type_Tent + property_type_Timeshare + property_type_Tipi + property_type_Townhouse + property_type_Train + property_type_Treehouse + property_type_Vacation.home	+ property_type_Villa + property_type_Yurt, data=train_part)
summary(m3)

#Removed insignificant variables from Model 3
#R-squared:  0.377, Residual standard error: 88.32
m4 <- lm(price ~ accommodates + bathrooms + cleaning_fee + host_response_rate + instant_bookable + number_of_reviews + review_scores_rating + property_type_Apartment + property_type_Boat + property_type_Boutique.hotel	+ property_type_Bungalow + property_type_Cabin + property_type_Condominium	+ property_type_Dorm +  	property_type_Guest.suite + property_type_Guesthouse + property_type_Hostel	+ property_type_House	+ property_type_In.law  +  property_type_Loft + property_type_Other	+ property_type_Timeshare,data=train_part)
summary(m4)

#Added the amenities with the model 1
#R-squared:  0.4683, Residual standard error:  81.59
m5 <- lm(price ~ bedrooms + beds + Air.conditioning + BBQ.grill + Buzzer.wireless.intercom + Cable.TV + Coffee.maker + Indoor.fireplace + Dishwasher + Gym + Kitchen + Washer + Airbnb_Entire_home_apt + Airbnb_Private_room + Airbnb_PulloutSofa + Airbnb_Futon + Airbnb_Real_Bed + Internet + Patio.or.balcony + Smart.lock + Hair.dryer + Extra.pillows.and.blankets + Elevator.in.building, data=train_part) 
summary(m5)

#Removed the insignificant variables from model 5
#Added the significant variables from model 1
#R-squared:  0.5064, Residual standard error:  78.61
m6 <- lm(price ~ bedrooms + beds + Air.conditioning + BBQ.grill + Buzzer.wireless.intercom + Cable.TV + Indoor.fireplace + Dishwasher + Gym + Kitchen + Washer + Airbnb_Entire_home_apt + Airbnb_Private_room + Internet + Smart.lock + Hair.dryer + Elevator.in.building + accommodates + bathrooms + cleaning_fee + host_response_rate + instant_bookable + number_of_reviews + review_scores_rating , data=train_part) 
summary(m6)

#Added amenties with Room Type and its location
#R-squared:  0.4814, Residual standard error:  80.56
m7 <- lm(price ~ Airbnb_Entire_home_apt + Airbnb_Private_room + Airbnb_Boston + Airbnb_Chicago + Airbnb_LA + Airbnb_SF + Coffee.maker + bedrooms + Airbnb_PulloutSofa + Air.conditioning + Extra.pillows.and.blankets + Buzzer.wireless.intercom + Patio.or.balcony + Airbnb_Futon, data=train_part)
summary(m7)


#R-squared:  0.543, Residual standard error:  75.64
M8 <- lm (price ~ bedrooms + beds + Air.conditioning + Buzzer.wireless.intercom + Cable.TV + Indoor.fireplace + Gym + Kitchen + Washer + Airbnb_Entire_home_apt + Airbnb_Private_room + accommodates + bathrooms + cleaning_fee + host_response_rate + instant_bookable + number_of_reviews + review_scores_rating + Airbnb_Boston + Airbnb_Chicago + Airbnb_LA + Coffee.maker + Airbnb_moderate, data=train_part) 
summary(M8)


M9 <- lm (price ~ Airbnb_Boston + Airbnb_Chicago + Airbnb_LA + Airbnb_SF,data=train_part)
summary(M9)

#######################Best models###################################################################
#Added significant amenities with number_of_reviews + review_scores_rating 
#R-squared:  0.5064, Residual standard error:  78.61
BM1 <- lm (price ~ bedrooms + beds + Air.conditioning + BBQ.grill + Buzzer.wireless.intercom + Cable.TV + Indoor.fireplace + Dishwasher + Gym + Kitchen + Washer + Airbnb_Entire_home_apt + Airbnb_Private_room + Internet + Smart.lock + Hair.dryer + Elevator.in.building + accommodates + bathrooms + cleaning_fee + host_response_rate + instant_bookable + number_of_reviews + review_scores_rating , data=train_part) 
summary(BM1)

#R-squared:  0.5426, Residual standard error:  75.68
#Added location other than SF
#removed Hair.dryer  from model 1
BM2 <- lm (price ~ bedrooms + beds + Air.conditioning + BBQ.grill + Buzzer.wireless.intercom + Cable.TV + Indoor.fireplace + Dishwasher + Gym + Kitchen + Washer + Airbnb_Entire_home_apt + Airbnb_Private_room + Internet + Smart.lock + Elevator.in.building + accommodates + bathrooms + cleaning_fee + host_response_rate + instant_bookable + number_of_reviews + review_scores_rating + Airbnb_Boston + Airbnb_Chicago + Airbnb_LA, data=train_part) 
summary(BM2)


#Removed Internet,smartlock,elevator in building from model2 
#Added significant property types 
#R-squared: 0.548,Residual standard error: 75.24 
BM3<- lm (price ~ accommodates + bathrooms + cleaning_fee + host_response_rate + instant_bookable + number_of_reviews + review_scores_rating + bedrooms + beds + Air.conditioning + BBQ.grill + Buzzer.wireless.intercom + Cable.TV + Coffee.maker + Indoor.fireplace + Dishwasher + Gym + Kitchen + Washer + Airbnb_Entire_home_apt + Airbnb_Private_room + Airbnb_Boston + Airbnb_Chicago + Airbnb_LA + Airbnb_moderate +  property_type_Apartment + property_type_Boat + property_type_Boutique.hotel + property_type_Dorm  + property_type_Hostel + property_type_In.law  +  property_type_Loft + property_type_Other	+ property_type_Timeshare,data=train_part)
summary(BM3)



####Predicting the values using Best model BM1##################
predicted_value<-predict(BM1,test_part)
summary(predicted_value)
View(predicted_value)

accuracy(predicted_value, test_part$price)



####Predicting the values using Best model BM2##################
predicted_value<-predict(BM2,test_part)
summary(predicted_value)
View(predicted_value)

accuracy(predicted_value, test_part$price)



####Predicting the values using Best model BM3##################
predicted_value<-predict(BM3,test_part)
summary(predicted_value)
View(predicted_value)

accuracy(predicted_value, test_part$price)


#######################################Code for drawing box plots####################################

boxplot(price ~ accommodates, data = train_part, xlab = "Accommodates",
        ylab = "Price", main = "Accommodates vs Price")
boxplot(price ~ bedrooms, data = train_part, xlab = "Bedrooms",
        ylab = "Price", main = "Bedrooms vs Price")
boxplot(price ~ beds, data = train_part, xlab = "beds",
        ylab = "Price", main = "beds vs Price")
boxplot(price ~ city, data = Air, xlab = "City",
        ylab = "Price", main = "city vs Price")


#############################################Scatter Plots##############################################################

attach(train_part)
plot(review_scores_rating, price, main="Price vs review_scores_rating", 
     xlab="review_scores_rating", ylab="price", pch=19)
plot(number_of_reviews, price, main="Price vs number_of_reviews", 
     xlab="number_of_reviews", ylab="price", pch=19)
plot(accommodates, price, main="Price vs accommodates", 
     xlab="accommodates", ylab="price", pch=19)
plot(bedrooms, price, main="Price vs bedrooms", 
     xlab="bedrooms", ylab="price", pch=19)



##############Code for the correlation Matrix#############################################################
Corr1<-select(train_part,price,accommodates,bathrooms,cleaning_fee,host_response_rate,instant_bookable,
              number_of_reviews,review_scores_rating,bedrooms,beds,Air.conditioning,BBQ.grill,
              Buzzer.wireless.intercom,Cable.TV,Coffee.maker, Indoor.fireplace,Dishwasher,Gym,
              Kitchen,Washer,Airbnb_Entire_home_apt,Airbnb_Private_room,
              Airbnb_Boston,Airbnb_Chicago,Airbnb_LA,Airbnb_moderate, property_type_Apartment,
              property_type_Boat,property_type_Boutique.hotel,property_type_Dorm ,property_type_Hostel,
              property_type_In.law ,property_type_Loft,property_type_Other,property_type_Timeshare)
View(Corr1)
heatmap.2(cor(Corr1), Rowv=FALSE,Colv = FALSE, dendrogram = "none", cellnote=round(cor(Corr1),1),key= FALSE, trace= 'none', margins= c(10,10))


################################################################################################



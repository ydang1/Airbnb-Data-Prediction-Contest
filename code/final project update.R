###final Project###

setwd("C:/Data mining/Project Materials")
airbnb_train_x <-read.csv("airbnb_train_x.csv", header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)
airbnb_test <- read.csv("airbnb_test_x.csv",na.strings=" ")
airbnb_train_y <- read.csv("airbnb_train_y.csv")


airbnb_updated <-airbnb_train_x[-c(16246, 30584, 47615, 56281, 65792, 72540, 75208, 92585, 96068,
                                 90609, 13584, 24317, 44690, 51371, 76218, 85311, 29879, 92028, 63486),
                              -c(19,20,24,30,31,32,43,51,53,62,63,66,67,70)] [,37:56]

library(xlsx)
library(faraway)
attach(airbnb_updated)
library(modeest)
library(RTextTools)
library(maxent)
library(tm)
library(ggplot2)
library(ROCR)

#check out the missing value across all the variables##
as.matrix(sapply(airbnb_updated, function(x) sum(is.na(x))))




#Cleaning latitude
sum(is.na(latitude))
print(latitude)



#Cleaning license
for (i in 1:nrow(airbnb_updated))  {
    license[i] <-
    ifelse(is.na(license[i]),"f",license[i])
}

sum(is.na(license))

for (i in 1:nrow(airbnb_updated)) {
    license[i]<-
    ifelse(grepl("pending", license[i]),"f",license[i])
}

for (i in 1:nrow(airbnb_updated))  {
    license[i] <-
    ifelse(license[i]=="f",license[i],"t")
}
print(license)


#Cleaning Longitude
sum(is.na(longitude))
print(longitude)


#Cleaning market
market <-as.matrix(airbnb_updated$market)
for (i in 1:nrow(market)) {
  market[i] <- 
    ifelse(is.na(market[i]),"All", market[i])
}
print(market)


#cleaning maximum_nights
mode <-mfv(maximum_nights)
for(i in 1:nrow(airbnb_updated)) {
    maximum_nights[i] <-
    ifelse(is.na(maximum_nights[i]),mode, maximum_nights[i])
}
mean <-mean(maximum_nights)
sd <- sqrt(sum((maximum_nights-mean)^2)/(length(maximum_nights)-1))
maximum_nights <- ((maximum_nights-mean)/sd)
print(maximum_nights)
 


#minimum_nights
rm(mean,sd) 
sum(is.na(minimum_nights))
  mode <- mfv(minimum_nights)
  for (i in 1:nrow(airbnb_updated)) {
      minimum_nights[i] <-
      ifelse(is.na(minimum_nights[i]),mode,minimum_nights[i])
  }
minimum_nights <- ifelse(minimum_nights==100000000,mode,minimum_nights)

mean <-mean(minimum_nights)
sd <- sqrt(sum((minimum_nights-mean)^2)/(length(minimum_nights)-1))
minimum_nights <- ((minimum_nights-mean)/sd)
print(minimum_nights)


# price
sum(is.na(price))
mean1 <-mean(price,na.rm=TRUE)
mode1<-mfv(price)
for (i in 1:nrow(airbnb_updated)) {
    price[i]<- 
    ifelse(is.na(price[i]),mode1, price[i])
}

print(price)


#monthly_price
for(i in 1:nrow(airbnb_updated)) {
    monthly_price[i] <-
    ifelse(is.na(monthly_price[i]), price[i]*30, monthly_price[i])
}
sum(is.na(monthly_price))
print(monthly_price)


#weekly_price
for(i in 1:nrow(airbnb_updated))  {
  weekly_price[i] <-
    ifelse(is.na(weekly_price[i]),price[i]*7, weekly_price[i])
}
print(weekly_price)



#square_feet
rm(mean)
mean <-mean(square_feet, na.rm=TRUE)
for (i in 1:nrow(airbnb_updated)) {
    square_feet[i] <-
    ifelse(is.na(square_feet[i]), mean, square_feet[i])
}
print(square_feet)


#security_deposit
for (i in 1:nrow(airbnb_updated)) {
    security_deposit[i]<-
    ifelse(is.na(security_deposit[i]),0,security_deposit[i])
}
print(security_deposit)



#property_type
table(property_type)
for (i in 1:nrow(airbnb_updated)) {
    property_type[i] <-
    ifelse(is.na(property_type[i]),"Apartment",property_type[i])
}
print (property_type)



#require_guest_phone_verfication
for (i in 1:nrow(airbnb_updated))  {
  require_guest_phone_verification[i] <-
  ifelse(is.na(require_guest_phone_verification[i]),"f",require_guest_phone_verification[i])
}
sum(is.na(require_guest_phone_verification))
print(require_guest_phone_verification)


#requires_liscense
for (i in 1:nrow(airbnb_updated))  {
  requires_license[i] <-
    ifelse(is.na(requires_license[i]),"f",requires_license[i])
}
sum(is.na(requires_license))
print(requires_license)



#room_type
for (i in 1:nrow(airbnb_updated)) {
     room_type[i] <-
    ifelse(is.na(room_type[i]), "f", room_type[i])
}
sum(is.na(room_type))
print(room_type)



#note
for(i in 1:nrow(airbnb_updated)) {
   notes[i] <-
  ifelse(is.na(notes[i]),"f","t")
}
print(notes)


#neighborhood_overview
sum(is.na(neighborhood_overview))
neighborhood_overview <-as.character(neighborhood_overview)

#build tdm term document matrix
matrix= create_matrix(neighborhood_overview,language="english",
        removeSparseTerms=0.95, removeStopwords=TRUE, removePunctuation = TRUE,
        removeNumbers=TRUE, stemWords=TRUE, stripWhitespace=TRUE,
        toLower=TRUE)

mat_neigbor <-as.matrix(matrix)
mat.frequent <- as.matrix(rowSums(mat))
neighborhood_overview <- mat[]



#inspect frequent words
term.freq <- colSums(mat)
term.freq <- sort(term.freq, decreasing = TRUE)
df2 <- data.frame(term=names(term.freq),freq=term.freq)
df2 <- transform(df2, term=reorder(term, freq))
ggplot(df2,aes(x=term, y=freq))+geom_bar(stat="identity")+xlab("terms")+ylab("Count")+coord_flip()

for (i in 1:nrow(airbnb_updated)) {
     neighborhood_overview[i] <- 
    ifelse((grepl("neighborhood",neighborhood_overview[i])|(grepl("restaurants",neighborhood_overview[i]))|
          (grepl("park",neighborhood_overview[i]))|(grepl("walk",neighborhood_overview[i]))|
          (grepl("away",neighborhood_overview[i]))),"1","0")
}

print(neighborhood_overview)

#transit
transit <-as.character(transit)

#build tdm term document matrix
matrix= create_matrix(transit,language="english",
                      removeSparseTerms=0.95, removeStopwords=TRUE, removePunctuation = TRUE,
                      removeNumbers=TRUE, stemWords=TRUE, stripWhitespace=TRUE,
                      toLower=TRUE)

mat_transit <-as.matrix(matrix)

#inspect frequent words
term.freq <- colSums(mat)
df2 <- data.frame(term=names(term.freq),freq=term.freq)
df2 <- transform(df2, term=reorder(term, freq))
ggplot(df2,aes(x=term, y=freq))+geom_bar(stat="identity")+xlab("terms")+ylab("Count")+coord_flip()

for (i in 1:nrow(airbnb_updated)) {
  transit[i] <- 
    ifelse((grepl("bus",transit[i])|(grepl("walk",transit[i]))|
              (grepl("away",transit[i]))|(grepl("minutes",transit[i]))|
              (grepl("street",transit[i]))),"1","0")
}

print(transit)


#state

for  (i in 1:nrow(airbnb_updated))   {
  state[i] <-
    ifelse(is.na(state[i]), airbnb_train$city_name[i],state[i])
}

print(state)



library(RTextTools)
amenity <-airbnb_train_x [-c(16246, 30584, 47615, 56281, 65792, 72540, 75208, 92585, 96068,
                                       90609, 13584, 24317, 44690, 51371, 76218, 85311, 29879, 92028, 63486),4]

matrixA= create_matrix(amenity,language="english",
                      removeSparseTerms=0.95, removeStopwords=TRUE, removePunctuation = TRUE,
                      removeNumbers=TRUE, stemWords=TRUE, stripWhitespace=TRUE,
                      toLower=TRUE)

matA <-as.matrix(matrixA)


airbnb_final <- read.csv("airbnb_final.csv")
airbnb_final <- airbnb_final[,-c(1,25,46,33,28,40)]
summary(airbnb_final)
airbnb_final <- airbnb_final[,-c(41,33)]
airbnb_final <- cbind(matA,airbnb_final,mat_neigbor,mat_transit)[,-c(136,201)]


airbnb_final[, "high_booking_rate"] <- as.factor(airbnb_final[, "high_booking_rate"])
airbnb_final[, "host_has_profile_pic"] <- as.factor(airbnb_final[, "host_has_profile_pic"])
airbnb_final[, "host_identity_verified"] <- as.factor(airbnb_final[, "host_identity_verified"])
airbnb_final[, "host_is_superhost"] <- as.factor(airbnb_final[, "host_is_superhost"])
airbnb_final[, "instant_bookable"] <- as.factor(airbnb_final[, "instant_bookable"])
airbnb_final[, "no_guest_requirement"] <- as.factor(airbnb_final[, "no_guest_requirement"])



test_final <- read.csv("test_final.csv")

test_final[, "host_has_profile_pic"] <- as.factor(test_final[, "host_has_profile_pic"])
test_final[, "host_identity_verified"] <- as.factor(test_final[, "host_identity_verified"])
test_final[, "host_is_superhost"] <- as.factor(test_final[, "host_is_superhost"])
test_final[, "instant_bookable"] <- as.factor(test_final[, "instant_bookable"])
test_final[, "no_guest_requirement"] <- as.factor(test_final[, "no_guest_requirement"])
test_final[, "is_business_travel_ready"] <- as.factor(test_final[, "is_business_travel_ready"])


#change level for is_business_travel_ready
test_final$is_business_travel_ready <- as.character(test_final$is_business_travel_ready)
for (i in 1:nrow(test_final))   {
  test_final$is_business_travel_ready[i] <-
    ifelse(test_final$is_business_travel_ready[i]=="1","t","f")
}
print(test_final$is_business_travel_ready)


train1 <- readRDS("train_final.rds")
test1 <- readRDS("test_final.rds")
attach(train1)


attach(airbnb_final)
###prediction start####
##randomly partition the data into 30% validation data and 70% training data
set.seed(12345)
validation_instn <- sample(nrow(airbnb_final), 0.3*nrow(airbnb_final))
airbnb_validation <-airbnb_final[validation_instn,]
airbnb_train <- airbnb_final[-validation_instn,]


##output the baseline
baseline=sum(ifelse(airbnb_validation$high_booking_rate==0,1,0))/nrow(airbnb_validation)


##logistic regression
logistic <- glm(high_booking_rate~.,data = airbnb_train,family = "binomial")
summary(logistic)

log_pred_valid <- predict(logistic, newdata=airbnb_validation, type = "response")
pred <- prediction(log_pred_valid,airbnb_validation$high_booking_rate)
acc.perf = performance(pred,measure="acc")
plot(acc.perf,ylim=c(0,1))
best= which.max(slot(acc.perf,"y.values")[[1]]) 
max.acc=slot(acc.perf,"y.values")[[1]][best] 
max.cutoff=slot(acc.perf,"x.values")[[1]][best]
max.acc
max.cutoff

log.pred <- ifelse(log_pred_valid>=0.5,1,0)

##Accuracy
sum(ifelse(airbnb_validation$high_booking_rate== log.pred,1,0))/nrow(airbnb_validation) 



log_test_probs=predict(logistic,newdata=test1,type = "response")
log_test <- ifelse(log_test_probs>max.cutoff,1,0)


###nerual network###
library(neuralnet)
n <- names(airbnb_train)
f <- as.formula(paste("high_booking_rate ~", paste(n[n!= "high_booking_rate"], collapse = " + ")))
nn <- neuralnet(f,data=data.matrix(airbnb_train),hidden=c(10,7,3), linear.output=FALSE)
plot(nn)
mypredict <- compute(nn,data.matrix(airbnb_validation))
predicted <- ifelse(mypredict>0.5,1,0)
sum(ifelse(airbnb_validation$high_booking_rate==mypredict,1,0))/nrow(airbnb_validation)

###random Forests in R
library(randomForest)
RF <- randomForest(as.factor(high_booking_rate) ~ .,airbnb_train_new, ntree=500, norm.votes=FALSE, do.trace=10,importance=TRUE)
summary(RF)

rf_pred_valid <- predict(RF, newdata=airbnb_validation, type = "response")
table(airbnb_validation$high_booking_rate, rf_pred_valid)
accuracy_rf_valid = sum(ifelse(airbnb_validation$high_booking_rate == rf_pred_valid, 1, 0)) /
  nrow(airbnb_validation) 
print(accuracy_rf_valid)

test_final <- read.csv("test_final.csv")
test_final <- test_final[,-c(41,33)]


amenity_test <-airbnb_test [-c(1539,775,10274),4]

matrix1= create_matrix(amenity_test,language="english",
                       removeSparseTerms=0.95, removeStopwords=TRUE, removePunctuation = TRUE,
                       removeNumbers=TRUE, stemWords=TRUE, stripWhitespace=TRUE,
                       toLower=TRUE)

mat1 <-as.matrix(matrix1)

#neighborhood_overview_test
neighborhood_overview_test <-as.character(airbnb_test [-c(1539,775,10274),52])

#build tdm term document matrix
matrix2= create_matrix(neighborhood_overview_test,language="english",
                      removeSparseTerms=0.95, removeStopwords=TRUE, removePunctuation = TRUE,
                      removeNumbers=TRUE, stemWords=TRUE, stripWhitespace=TRUE,
                      toLower=TRUE)

mat_neigbor_test <-as.matrix(matrix2)

#transit_test
transit_test <-as.character(airbnb_test [-c(1539,775,10274),68])

#build tdm term document matrix
matrix3= create_matrix(transit_test,language="english",
                      removeSparseTerms=0.95, removeStopwords=TRUE, removePunctuation = TRUE,
                      removeNumbers=TRUE, stemWords=TRUE, stripWhitespace=TRUE,
                      toLower=TRUE)

mat_transit_test <-as.matrix(matrix3)

test_final <- cbind(mat1,test_final,mat_neigbor_test,mat_transit_test)[,-94]
test_final$cafes <- airbnb_train$cafes[1:12205]
test_final$music <- airbnb_train$music[1:12205]
test_final$drive <- airbnb_train$drive[1:12205]
test_final <- test_final[,-c(198,199)]

a <- read.csv("airbnb_train.csv")
b <- read.csv("test_final.csv")

c <- rbind(a,b)
test_final <- c[69988:82192,]
airbnb_train_new <-cbind(c[1:69987,],airbnb_train$high_booking_rate)
RF_test_probs=predict(RF,newdata=test_final,type = "response")
RF_test <- ifelse(RF_test_probs>0.5,1,0)




###Bagging in R
library(randomForest)
library(ISLR)

bag.mod <- randomForest(high_booking_rate~.,data=airbnb_final, subset=airbnb_train, mtry=20, importance=TRUE)
bag_preds <- predict(bag.mod, newdata=airbnb_validation)
bag_acc <- sum(ifelse(bag_preds==airbnb_validation$high_booking_rate),1,o)/nrow(airbnb_validation)

#Boosting in R
library(gbm)
airbnb_train <- data.frame(airbnb_train)
high_booking_rate <- as.factor(high_booking_rate)

boost.mod <- gbm(high_booking_rate~.,data=airbnb_train, distribution="bernoulli", n.trees=100, interaction.depth=1)

boost_preds <- predict(boost.mod, newdata=airbnb_validation, type="response", n.trees=100)
boots_class <- ifelse(boost_preds >0.5, 1,0)
boost_acc <- sum(ifelse(boost_class=airbnb_validation$high_booking_rate),1,0)/nrow(airbnb_validation)




## Ridge regression##

ridge.pred <- predict(airbnb_ridge, s=best.lambda, type="coefficients") 
pred.ridge <- prediction(ridge.pred, airbnb_validation$high_booking_rate)

log_pred_valid <- predict(logistic, newdata=airbnb_validation, type = "response")
pred <- prediction(log_pred_valid,airbnb_validation$high_booking_rate)
acc.perf = performance(pred,measure="acc")
plot(acc.perf,ylim=c(0,1))
best= which.max(slot(acc.perf,"y.values")[[1]]) 
max.acc=slot(acc.perf,"y.values")[[1]][best] 
max.cutoff=slot(acc.perf,"x.values")[[1]][best]


sqrt(mean((airbnb_validation$high_booking_rate-ridge.pred)^2))
tree_preds5 <- predict(airbnb.tree, newdata=airbnb_validation)
pred.ridge <- prediction(ridge.pred, airbnb_validation$high_booking_rate)


acc.perf = performance(pred,measure="acc")
plot(acc.perf,ylim=c(0,1))
best= which.max(slot(acc.perf,"y.values")[[1]]) 
max.acc_ridge=slot(acc.perf,"y.values")[[1]][best] 
max.cutoff_ridge=slot(acc.perf,"x.values")[[1]][best]
max.acc_ridge
max.cutoff_ridge


##randomly partition the data into 30% validation data and 70% training data
set.seed(12345)
validation_instn <- sample(nrow(train1), 0.3*nrow(train1))
airbnb_validation <-train1[validation_instn,]
airbnb_train <- train1[-validation_instn,]




##LASSO Regression model
library(MASS)
library(glmnet)
library(Matrix)
library(foreach)
airbnb_lasso <- glmnet(data.matrix(airbnb_train[,-109]), airbnb_train$high_booking_rate, family="binomial", alpha = 1) 

airbnb_lasso.cv=cv.glmnet(data.matrix(airbnb_train[,-109]),airbnb_train$high_booking_rate, family="binomial", alpha = 1)

best.lambda <- airbnb_lasso.cv$lambda.min 
lasso.pred <- predict(airbnb_lasso,s=best.lambda,newx = (data.matrix(airbnb_validation[,-109])), type="response")
predict(airbnb_lasso,s=best.lambda,type="coefficients") 
lasso.pred <- ifelse(lasso.pred>=0.5,1,0)

##Accuracy
sum(ifelse(airbnb_validation$high_booking_rate== lasso.pred,1,0))/nrow(airbnb_validation) 





lasso_pred <- prediction(lasso.pred,test1$high_booking_rate)
acc.perf = performance(lasso_pred,measure="acc")
plot(acc.perf,ylim=c(0,1))
best= which.max(slot(acc.perf,"y.values")[[1]]) 
max.acc=slot(acc.perf,"y.values")[[1]][best] 
max.cutoff=slot(acc.perf,"x.values")[[1]][best]
max.acc
max.cutoff





log_test_probs=predict(logistic,newdata=test1,type = "response")
log_test <- ifelse(log_test_probs>max.cutoff,1,0)





train1=data.matrix(train1)
train1=data.frame(train1)
train1$high_booking_rate=train1$high_booking_rate-1


#Classification Tree
library(tree)
airbnb.tree=tree(high_booking_rate~., data=airbnb_train)
summary(airbnb.tree)
plot(airbnb.tree)
text(airbnb.tree)



#tree prediction with all the nodes
tree_preds5 <- predict(airbnb.tree, newdata=airbnb_validation)
tree_prob5 <- tree_preds5[,2]
pred5 <- prediction(tree_prob5, airbnb_validation$high_booking_rate)

tree.pred <- ifelse(tree_preds5>=0.5,1,0)
table(tree.pred[,2], airbnb_validation$high_booking_rate)
tree_acc_5 <- sum(ifelse(airbnb_validation$high_booking_rate== tree.pred[,2],1,0))/nrow(airbnb_validation) 
print(tree_acc_5)



##Prun the tree node 2
airbnb_pruned2=prune.tree(airbnb.tree, best=2)
summary(airbnb_pruned2)
plot(airbnb_pruned2)
text(airbnb_pruned2)

tree_preds2 <- predict(airbnb_pruned2, newdata=airbnb_validation)


tree_prob2 <- tree_preds2[,2]
pred2 <- prediction(tree_prob2, airbnb_validation$high_booking_rate)
acc.perf = performance(pred2,measure="acc")
plot(acc.perf,ylim=c(0,1))
best= which.max(slot(acc.perf,"y.values")[[1]]) 
max.acc2=slot(acc.perf,"y.values")[[1]][best] 
max.cutoff2=slot(acc.perf,"x.values")[[1]][best]
max.acc2
max.cutoff2

##Prun the tree node 3
airbnb_pruned3=prune.tree(airbnb.tree, best=3)
summary(airbnb_pruned3)
plot(airbnb_pruned3)
text(airbnb_pruned3)

tree_preds3 <- predict(airbnb_pruned3, newdata=airbnb_validation)
tree_prob3 <- tree_preds3[,2]
pred3 <- prediction(tree_prob3, airbnb_validation$high_booking_rate)
acc.perf = performance(pred3,measure="acc")
plot(acc.perf,ylim=c(0,1))
best= which.max(slot(acc.perf,"y.values")[[1]]) 
max.acc3=slot(acc.perf,"y.values")[[1]][best] 
max.cutoff3=slot(acc.perf,"x.values")[[1]][best]
max.acc3
max.cutoff3


##Prun the tree node 4
airbnb_pruned4=prune.tree(airbnb.tree, best=4)
summary(airbnb_pruned4)
plot(airbnb_pruned4)
text(airbnb_pruned4)

tree_preds4 <- predict(airbnb_pruned4, newdata=airbnb_validation)
tree_prob4 <- tree_preds4[,2]
pred4 <- prediction(tree_prob4, airbnb_validation$high_booking_rate)
acc.perf = performance(pred4,measure="acc")
plot(acc.perf,ylim=c(0,1))
best= which.max(slot(acc.perf,"y.values")[[1]]) 
max.acc4=slot(acc.perf,"y.values")[[1]][best] 
max.cutoff4=slot(acc.perf,"x.values")[[1]][best]
max.acc4
max.cutoff4


log_pred_valid <- predict(logistic, newdata=airbnb_validation, type = "response")
pred <- prediction(log_pred_valid,airbnb_validation$high_booking_rate)
acc.perf = performance(pred,measure="acc")
plot(acc.perf,ylim=c(0,1))
best= which.max(slot(acc.perf,"y.values")[[1]]) 
max.acc=slot(acc.perf,"y.values")[[1]][best] 
max.cutoff=slot(acc.perf,"x.values")[[1]][best]


tree_pred_n <- predict(airbnb.pruned_n, newdata=airbnb_validation)
classification_n <- ifelse(tree_pred_n>0.5,1,0) 
accuracy_n=sum(ifelse(airbnb_validation$high_booking_rate==classification_n[,2],1,0))/nrow(airbnb_validation) 



###Forward Selection
airbnb_all <- glm(high_booking_rate~.,data= airbnb_train,family = "binomial")
airbnb_null <- glm(high_booking_rate~1,data= airbnb_train,family = "binomial")

forward_model <- step(airbnb_null, scope=list(upper=airbnb_all), direction="forward")

forward.pred <- predict(forward_model, newdata = airbnb_validation, type= "response")

###Backward Eliminate
backward_model <- step(airbnb_all)
summary(backward_model)
backward.pred <- predict(backward_model,newdata=airbnb_validation, type="response")


###a stepwise procedure starting with forward selection
forward_model_both <- step(airbnb_null, scope=list(upper=airbnb_all), direction="both")
summary(forward_model_both)

backwardboth.pred <-predict(backward_model_both, newdata=airbnb_validation, type="response")



##Ridge Regression model
set.seed(12345)
library(glmnet)
library(Matrix)
library(foreach)

airbnb_ridge <- glmnet(data.matrix(airbnb_train),airbnb_train$high_booking_rate, family="binomial", alpha = 0) 
airbnb_ridge.cv <- cv.glmnet(data.matrix(airbnb_train), airbnb_train$high_booking_rate, family= "binomial", alpha=0) 
plot(airbnb_ridge.cv)
best.lambda <- airbnb_ridge.cv$lambda.mintest_new 

test_new <- read.csv("test_new.csv")
test_new <- test_new[,28:47]
write.csv(test_new, "test_new.csv")
test_new <- read.csv("test_new.csv")
test_news <- test_new[,-c(4,11,13,18)]



test <- read.csv("test.csv")
test1 <- read.csv("test1.csv")













##KNN

library(class)

colnames(airbnb)
airbnb_train$high_booking_rate <- as.numeric(airbnb_train$high_booking_rate) 
train.X=airbnb_train
valid.X=airbnb_validation 
train.Y=airbnb_train$high_booking_rate
valid.Y=airbnb_validation$high_booking_rate
3,5,10,15,20,25,30,35,40
knn.pred_valid_n<- knn(train.X,valid.X,train.Y-1,k=n) 
table(valid.Y, knn.pred_valid_n) 


acc_knn_valid_n <- sum(ifelse(airbnb_validation$high_booking_rate==knn.pred_valid_n,1,0))/nrow(airbnb_validation) 




####################
del_row=c(1539,775,10274)


library(R.utils)
insert(log_test, del_row, values=c(0,0,0))
write.csv(log_test, "log_test.csv")


train1 <- readRDS("train_final.rds")
test1 <- readRDS("test_final.rds")


##randomly partition the data into 30% validation data and 70% training data
set.seed(12345)
validation_instn <- sample(nrow(train1), 0.3*nrow(train1))
airbnb_validation <-train1[validation_instn,]
airbnb_train <- train1[-validation_instn,]



#Boosting in R
library(gbm)
airbnb_train[, "license"] <- as.factor(airbnb_train[, "license"])
boost.mod <- gbm(high_booking_rate~.,data=airbnb_train, distribution="bernoulli", n.trees=100, interaction.depth=4)

boost_preds <- predict(boost.mod, newdata=airbnb_validation, type="response", n.trees=100)
boosts_class <- ifelse(boost_preds >0.5, 1,0)
boost_acc <- sum(ifelse(boosts_class==airbnb_validation$high_booking_rate),1,0)/nrow(airbnb_validation)



















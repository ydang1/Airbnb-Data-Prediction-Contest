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

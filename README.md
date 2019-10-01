# Project Description
To predict one of two attributes of an Airbnb.com listing that a listing owner might want to know: high_booking_rate (how popular the listing is), and review_scores_rating (what the average visitor rating for the listing is). The dataset has been provided and it has about 70 features for each listing. 

# DataSet
Train and test dataset have been provided:
1.	airbnb_train_x.csv: features for the training instances.
2.	airbnb_train_y.csv: labels for the training instances.
3.	airbnb_test_x.csv: features for the test instances. Your goal is to make predictions for the instances in the test set.

Target variables:
1.	high_booking_rate is binary. The winning team will have the binary predictions with the highest accuracy. 
2.	review_scores_rating is numeric. The winning team will have the numerical predictions with the lowest RMSE.

Our team is a 4-member team and we choose the high_booking_rate as our target variables.The goal for this project is to acheieve the accuracy score as high as possible. There could be a lot of ways to achieve the highest accuracy, for example, choose the proper model by model comparison, include as many features as we want. Including as many samples as we want. So it is important to use the proper way to achieve the goal. 

# Prerequistes
Tools:
R studio: https://www.rstudio.com/products/rstudio/download/

# Running the tests
## 1. Data exploration and data processing
- Highly correlated variables:

The content of variable “city” and “city_name” are quite close except that “city” sometimes referred to a more specific district and more customized. There were also several variables that were describing locations. In these cases, we only chose variables that were more complete and more standardized in formats among similar variables.

- Text variables

The dataset contained several text variables such as “amenities” and “transit” and “description”. Since customers were open to enter everything they like into these variables, it is tricky to handle these variables. We believed that when customers are searching for rooms, they must care about amenities. As “amenities” came in a relatively standardized format, we constructed a document-term matrix which included amenities with sparsity over 5%.

- Boolean variables

Any observations with a non-blank “host_verifications” were regarded as having a verification. The same method was also applied to “house_rules”.

- Category variables

We divided the variable “access” into three levels “Entire”, “Partial” and “Unknown”. If the text contains certain words, we assign it to “Entire” or “Partial”, otherwise it is “Unknown”. Text variables other than the above four were deleted.

- Missing values

For the “host_response_rate”, we filled with the mean of the existed numbers which is 0.96. And we inserted the blank of “host_response_time” with the mode “within an hour”.

## 2. Model Approach

We combined the given train independent variables with given dependent variable. We set the seed to 12345 and randomly partitioned the given train data into 30% validation data and 70% remaining(training) data. The baseline we used was 75.11%, which is the percentage of the majority group (Not High Booking Rate) in the validation data. We used a cutoff of 0.5 for prediction classification and calculated the accuracy of all models on the validation data and compare them with the baseline. If the model's accuracy was higher than the baseline, it would a good model for our further model selection.
```
airbnb <-readRDS("~/Desktop/train.rds")
set.seed(12345)
validation_instn <- sample(nrow(airbnb), 0.3 * nrow(airbnb))
airbnb_validation <- airbnb[validation_instn, ]
airbnb_train <- airbnb[-validation_instn, ]
```

## 3. Model Selection
Among those models with a higher accuracy than baseline, we chose the one with the highest accuracy on the validation data for interpretation purpose. Due to the factor that some models are hard to understand and interpret, we also chose one method with more detailed information, which is able to best interpret the relationships between features and booking rate. That is, we can get the exact coefficient not only for numerical variables but also for each level of categorical factors.

- ### Logistic Regression
Our initial attempt on logistic regression did not include any variable derived from text mining. The model reached an accuracy of 78.6% for the validation data. After we added the matrix of amenities, the accuracy of predicting the validation data increased to 79.6%. This confirmed our expectation that amenities is a major concern when people book their rooms. However as we added other variables derived from text mining, we didn’t see an increase in accuracy.
```
logistic <- glm(high_booking_rate ~ ., data = airbnb_train, family = "binomial")
summary(logistic)
log_pred_valid <- predict(logistic, newdata=airbnb_validation, type = "response")
log_pre <- ifelse(log_pred_valid>0.5,1,0)
table(airbnb_validation$high_booking_rate, log_pre)
accuracy_log_valid = sum(ifelse(airbnb_validation$high_booking_rate == log_pre, 1, 0)) / nrow(airbnb_validation)
```
![](final%20report/logistic%201.png)
![](final%20report/logsitic%202.png)
From the summary, we can see that customers more care about the convenience and comfort of a room. Room seekers tend to book rooms which have temperature control, elevator and can check in quickly. As we expected, variable “country” is not significant because almost all observations are in the US. Customers also don’t seem to care about hosts’ license and profile picture. Though they don’t quite care about the maximum nights, they do need to consider the minimum nights required to book a room. Logistic model also suggests that the most popular room_type is the entire home. If a host becomes a super host and does more verifications on the website, their effort will more likely to get payback. All these information are critically for us to help with those low booking rate listings to improve.

- ### Classification Tree
![](final%20report/tree%203.png)

Our prediction accuracy for decision tree is 76.97%. However, after we run the model, the tree only selects 3 variables, “availability_60”, “host_repsonse_time”, “minimum_nights” and eliminates all the other variables. According to the dendrogram, any Airbnb availability less than half day will predict the book rate as 0, any Airbnb availability more than half and has a certain amount of host_reponse _time will predict high booking rate as 0. For availability_60 less than 56.5 and minimum nights less than 1.5 will predict high booking rate as 1. And other will be 0. And any availability larger than 56.5 will predict high booking rate as 0. One potential reason for the model eliminates a lot of variables is because most of the variables are highly correlated with each other, and decision tree recognized the highly involved interaction of different variables so that it deletes the correlated one but leave the most significant one. However, since this model eliminates most of the variables, it ends up with very low accuracy so that we decide to not use decision tree anyway.

- ### Ridge Regression
We also applied a ridge regression with training data if there is high correlation variables in our dataset.Opposed to LASSO regression, Ridge wouldn’t take away any variables from the model. The significant coefficients are those separate far from zero, so we chose those variables whose absolute coefficients’ value are above 0.3.
```
library(glmnet)
ridge <- glmnet(data.matrix(airbnb_train[,c(1:105)]),airbnb_train$high_booking_rate,
family="binomial",alpha=0)
ridge.cv <- cv.glmnet(data.matrix(airbnb_train[,c(1:105)]),airbnb_train$high_booking_rate,
family="binomial",alpha=0)
best.lambda <- ridge.cv$lambda.min
predict(ridge,s=best.lambda,type="coefficients")
ridge.pred <- predict(ridge, s=best.lambda,newx = data.matrix(airbnb_validation[,c(1:105)]),
type="response")
class_ridge <- ifelse(ridge.pred>0.5,1,0)
accuracy_ridge <- sum(ifelse(airbnb_validation$high_booking_rate==class_ridge,1,0))/
nrow(airbnb_validation)
```
![](final%20report/ridge%201.png)
Accordingly, we draw a conclusion that the significant variables were elevator building, heating, host_acceptance_rate, host_is_superhost, host_response_time, instant_bookable, and requires_license.we found that an increase in host_acceptance_rate would increase the probability of high booking rate. In addition, compared with the listings without elevator and heating, the listings have these amenities tend to have high book rate. And compared with the listings don’t require licenses, the listings require license has less probability of having high book rate. What’s more, the probability of having high book rate would also increase owing to a super host, a shorter response time, or instant bookable.
- issues
we translated all variables into a numerical format which resulted in a difficulty in interpreting the categorical variables. What’s more, with this model, our team discovered an accuracy of only 78.65% on our validation data. Although this was higher than the validation baseline of 75.11%, it was much lower than boosting model we tested, so ultimately we do not recommend a Ridge model for this problem.

- ### LASSO Regression
LASSO performs both regularization and variable selection, which can improve the prediction accuracy and enhance interpretability of model. It is supposed to be a better model for interpretation purpose. We used the training data to train the LASSO model. We do not need a separate validation data split here since LASSO uses cross-validation for their model choices.
Any variable not be taken away can be considered significant in LASSO.
```
lasso <- glmnet(data.matrix(airbnb_train[,c(1:105)]),airbnb_train$high_booking_rate,family = "binomial",alpha = 1)
lasso.cv <- cv.glmnet(data.matrix(airbnb_train[,c(1:105)]),airbnb_train$high_booking_rate,
family="binomial",alpha=1)
best.lambda <- lasso.cv$lambda.min
predict(lasso,s=best.lambda,type="coefficients")
lasso.pred <- predict(lasso,s=best.lambda,newx=data.matrix(airbnb_validation[,c(1:105)]),
type="response")
class_lasso <- ifelse(lasso.pred>0.5,1,0)
accuracy_lasso <- sum(ifelse(airbnb_validation$high_booking_rate==class_lasso,1,0))/
nrow(airbnb_validation)
```
According to the output, only breakfast, dryer, oven, maximum_nights, minimum_nights, and monthly_price are excluded in this model. It indicates that these features are not useful when evaluating the importance and determining the correlation between high booking rate and each feature. Among the significant variables, country, host_acceptance_rate, host_is_superhost, host_response_rate,host_response_time, instant_bookable, license, and reuqires_license seem to have more influences on predicting a high booking rate listing, since their absolute values of the coefficient are higher than others.It should be noted with this model, our team discovered an accuracy of only 78.75% on our validation data. Although this was higher than the validation baseline of 75.11%, the prediction accuracy of this model was lower than boosting model we tested.

- ### Random Forests
Since the random forests can avoid overfitting and reduce variance compared to a classification tree, we also tried the random forests model for prediction purpose. We used the training data to train the model.
```
library(randomForest)
RF <- randomForest (high_booking_rate~.,airbnb_train,ntree=500,norm.votes=FALSE, do.trace=10, importance=TRUE)
summary(RF)
imp <- importance(RF)
impvar <- imp[order(imp[, 3], decreasing=TRUE),]
rf_pred_valid <- predict(RF, newdata=airbnb_validation, type = "response")
table(airbnb_validation$high_booking_rate, rf_pred_valid)
accuracy_rf_valid = sum(ifelse(airbnb_validation$high_booking_rate == rf_pred_valid, 1, 0)) / nrow(airbnb_validation)
```
![](final%20report/random%20forest%201.png)

According to the output, by sorting variable importance from high to low, city_name, minimun_nighs, availability_365, hosting_listings_count, and cleaning fee are the top 5 important variables for this model to predict whether an Airbnb listing will be a high booking rate. With this model, our team discovered an accuracy of only 83.66% on our validation data. Although this was higher than the validation baseline of 75.11%.
- issue
since it is hard for this model to interpret the relationships between each variable and high booking rate, so ultimately we do not recommend a Random Forest model for this problem.

- ### Support Vector Machine (SVM)
We also tried Support Vector Machine since it works on both linear and non-linear classification that might improve the our prediction accuracy. We used the training data to train the Support Vector Machine model.
```
library(e1071)
svm <- svm(high_booking_rate~. , data=airbnb_train, scale = TRUE)
summary(svm)
w = t(svm$coefs) %*% svm$SV
svm_pred_valid <- predict(svm, newdata=airbnb_validation, type = "response")
table(airbnb_validation$high_booking_rate, svm_pred_valid)
accuracy_svm_valid = sum(ifelse(airbnb_validation$high_booking_rate == svm_pred_valid, 1, 0)) /nrow(airbnb_validation)
```
![](final%20report/SVM%201.png)
According to the output, based on the training data set, the SVM identified 26421 support vectors. By extracting variable weights from this SVM model, beds, host_is_superhost, cleaning_fee, instant_bookable, and maximum_nights are the top 5 relatively important variables this model used to separate the data set since they have larger absolute vector weights. With this model, our team discovered an accuracy of only 81.16% on our validation data.

- ### XGboosting
In order to increase the accuracy, we employed boosting with our training dataset. According to the output, the variables are listed with the relative influence.
```
library(gbm)
boosting <- gbm(high_booking_rate~.,data=airbnb_train,distribution = "bernoulli",n.tree=10000,
interaction.depth = 5,shrinkage = 0.05)
boosting_pred <- predict(boosting,newdata=airbnb_validation,type="response",n.tree=10000)
class_boost <- ifelse(boosting_pred>0.5,1,0)
accuracy_boost <- sum(ifelse(airbnb_validation$high_booking_rate==class_boost,1,0))/nrow(airbnb_validation)
summary(boosting)
```
![](final%20report/boosting%201.png)
The longitude, availibity_30, first_review, latitude and minimum nights are the top five significant in predicting the high book rate listing. Conversely, the hosting, kid-friendly, translation missing, wireless intercom and country are uselessly indicated by this model. With this model, our team discovered an accuracy of 85.08% on our validation data, which is the highest accuracy among all the model we used. Thus, we recommended a boosting model only for the purpose of prediction.

# Concolusion
It turns out that the optimal prediction model is boosting model with the highest accuracy of 85.026% for the testing dataset. And we chose logistic regression as our interpretation model since it explains the information we need more detailed.

# Interesting findings
After analyzing this data through a logistic model, we found quite a few factors which are significant variables related to high booking rate. In general, we found that listings with the lower price, lower cleaning fee, lower minimum_nights are more likely to have high booking rates. Listings in big cities such as Boston, New York City and Washington, DC are more highly booked than other cities on average. In order to increase booking rate, hosts may better become a superhost, reply to potential clients as much as they can and collect more verified information. Listings with amenities which bring guests more convenience and provide more comfort utilities may also help to increase the booking rate. For example, heater, air conditioning, and hot water make guests comfortable during their stay. Self-check and shampoo give travelers convenience.

# Suggestions for Airbnb hosts
- Use logistic regression to detect the relation between each variable and high_booking_rate. Give suggestions according to the relationship between high book rate and factors to hosts who do not have high booking rate.
- Use the boosting model to predict which listing would be a high booking rate home and rearrange the listing’s priority in search results.
- If two models have any inconsistent prediction, use the boosting model as the final decision. Meanwhile, check the variables in the logistic model to see if there are enough variables to explain the book rate and whether the variables are unbiased.

# Future Imporvement
- build a base model with small amount of features and test the model performance rather than use all the features togehters. It might save more time during the model fit process
- Consider check multicollinearity before fit all data to decision tree, otherwise fit the data to decision tree is meaningless. 
- Try PCA to conduct the dimension reduction to see if it will increase the peformance when train the model. 
- Swith from R to Python in the future for building machine learning models to see if it is much efficient than R.

# Contribution


































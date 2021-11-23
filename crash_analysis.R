## This project attempts to explain the total number of crashes at intersections across Texas in 2010-2019 ##
## The objective of the project: 1. Use count-based regression models to examine the effects of different explanatory variables on the outcome variable: Total number of crashes at intersections ##
## 2. Use tree-based machine learning models to predict the number of crashes at intersections ##
## 3. Evaluate the performance of the machine learning models ##
library(haven)
library(pscl)
library(dplyr)
crash<-read.csv("C:/Users/theod/Desktop/kockelman/tx_crash.csv")

## Standardize the selected variables ##
crash_std <- as.data.frame(scale(crash[c("sidewalk_lenght_150ft_ft","lanes_major","lanes_minor","median_major","median_minor","on_system","median_width_ft_major","median_width_ft_minor",
                                         "lane_width_ft_major","lane_width_ft_minor","shoulder_width_ft_major","shoulder_width_ft_minor","aadt_lane_major","aadt_lane_minor",
                                         "truck_perc_major","truck_perc_minor","tot_WMT_sqmi","tot_WMT_pop","tot_WMT","speed_lim_mph_major","speed_lim_mph_minor",
                                         "f_local_major","f_local_minor","f_collector_major","f_collector_minor","f_arterial_major","f_arterial_minor",
                                         "a_rural","a_small_urban","a_urbanized","signalized_ind","approaches","dist_near_school_mi","dist_near_hops_mi","transit_ind","transit_stops_025mi_count",
                                         "pop_den","employ_den","cen_tr_income","cen_tr_age","county_rain")]))
crash_ind <- data.frame(crash[c("tot_crash_count")])
## Bind the columns of the two dataset ##
crash_std <- dplyr::bind_cols(crash_std, crash_ind)
## Remove missing values in the combined dataset ##
crash_std <- na.omit(crash_std)

## Zero-inflated negative binomial regression ##
zinfneg_3 <- zeroinfl(tot_crash_count ~ sidewalk_lenght_150ft_ft + lanes_major + lanes_minor + median_major + median_minor + median_width_ft_major + median_width_ft_minor
                      + lane_width_ft_major + lane_width_ft_minor + shoulder_width_ft_major + shoulder_width_ft_minor + on_system + aadt_lane_major + aadt_lane_minor 
                      + truck_perc_major + truck_perc_minor + tot_WMT_sqmi + tot_WMT_pop + tot_WMT + speed_lim_mph_major + speed_lim_mph_minor 
                      + f_local_major + f_local_minor + f_collector_major + f_collector_minor + f_arterial_major + f_arterial_minor 
                      + a_rural + a_small_urban + a_urbanized + signalized_ind + approaches + dist_near_school_mi + dist_near_hops_mi + transit_ind + transit_stops_025mi_count
                      + pop_den + employ_den + cen_tr_income + cen_tr_age + county_rain,
                      data = crash_std, dist = "negbin")
summary(zinfneg_3)

## Create a dataset without standardizing the selected variables ##
crash_data <- as.data.frame(crash[c("sidewalk_lenght_150ft_ft","lanes_major","lanes_minor","median_major","median_minor","on_system","median_width_ft_major","median_width_ft_minor",
                                    "lane_width_ft_major","lane_width_ft_minor","shoulder_width_ft_major","shoulder_width_ft_minor","aadt_lane_major","aadt_lane_minor",
                                    "truck_perc_major","truck_perc_minor","tot_WMT_sqmi","tot_WMT_pop","tot_WMT","speed_lim_mph_major","speed_lim_mph_minor",
                                    "f_local_major","f_local_minor","f_collector_major","f_collector_minor","f_arterial_major","f_arterial_minor",
                                    "a_rural","a_small_urban","a_urbanized","signalized_ind","approaches","dist_near_school_mi","dist_near_hops_mi","transit_ind","transit_stops_025mi_count",
                                    "pop_den","employ_den","cen_tr_income","cen_tr_age","county_rain","tot_crash_count")])
crash_data <- na.omit(crash_data)

## Descriptive statistics ##
## Barplot, percentages of intersections with 1. No crash, and 2. Crashes ##
crash_data$label <- NA
crash_data$label[which(crash_data$tot_crash_count==0)] <- 0
crash_data$label[which(crash_data$tot_crash_count!=0)] <- 1
bar <- prop.table(table(crash_data$label))*100
barplot(bar, ylim=c(0,80), horiz=FALSE, names.arg = c("No crash", "Crash"), col = c("darkblue","red"), cex.names = 1.8, cex.axis = 1.5, cex.lab = 1.2, ylab = "Percentage (%)")

## Among intersections where crashes occurred, I create 7 categories for crash counts ##
cr <- crash_data[crash_data$label==1,]
cr$count <- NA
cr$count[which(cr$tot_crash_count>=1 & cr$tot_crash_count<=5)] <- 1
cr$count[which(cr$tot_crash_count>=6 & cr$tot_crash_count<=10)] <- 2
cr$count[which(cr$tot_crash_count>=11 & cr$tot_crash_count<=20)] <- 3
cr$count[which(cr$tot_crash_count>=21 & cr$tot_crash_count<=30)] <- 4
cr$count[which(cr$tot_crash_count>=31 & cr$tot_crash_count<=40)] <- 5
cr$count[which(cr$tot_crash_count>=41 & cr$tot_crash_count<=50)] <- 6
cr$count[which(cr$tot_crash_count>=51)] <- 7
## Pie chart for the percentage of each category ##
slices <- c(108051,30985,20512,8221,4709,2891,8758)
lbls <- c("1-5","6-10","11-20","21-30","31-40","41-50","51 or above")
pct <- round(slices/sum(slices)*100,1)
colors <- c("red","orange","yellow","green","blue","pink","purple")
pie(slices, labels = paste(pct, "%", sep = ""), main = "Total Crashes at Each Intersection in 2010-19",col = colors, radius = 1.3)
legend("topright", lbls, cex = 0.8,
       fill = colors)

## Scatter plot of total crash count against aadt (annual average distance travelled) ##
cr$sum_aadt <- NA
cr$sum_aadt <- cr$aadt_lane_major + cr$aadt_lane_minor
plot(cr$sum_aadt, cr$tot_crash_count, xlab = "Annual Average Distance Travelled", ylab = "Total Crash Count")

## Compare total crash counts between the signalized and non-signalized intersections ##
signal <- filter(crash_data, signalized_ind==1)
nosignal <- filter(crash_data, signalized_ind==0)
signal$count <- NA
signal$count[which(signal$tot_crash_count>=1 & signal$tot_crash_count<=5)] <- 1
signal$count[which(signal$tot_crash_count>=6 & signal$tot_crash_count<=10)] <- 2
signal$count[which(signal$tot_crash_count>=11 & signal$tot_crash_count<=20)] <- 3
signal$count[which(signal$tot_crash_count>=21 & signal$tot_crash_count<=30)] <- 4
signal$count[which(signal$tot_crash_count>=31 & signal$tot_crash_count<=40)] <- 5
signal$count[which(signal$tot_crash_count>=41 & signal$tot_crash_count<=50)] <- 6
signal$count[which(signal$tot_crash_count>=51)] <- 7
table(signal$count)
nosignal$count <- NA
nosignal$count[which(nosignal$tot_crash_count>=1 & nosignal$tot_crash_count<=5)] <- 1
nosignal$count[which(nosignal$tot_crash_count>=6 & nosignal$tot_crash_count<=10)] <- 2
nosignal$count[which(nosignal$tot_crash_count>=11 & nosignal$tot_crash_count<=20)] <- 3
nosignal$count[which(nosignal$tot_crash_count>=21 & nosignal$tot_crash_count<=30)] <- 4
nosignal$count[which(nosignal$tot_crash_count>=31 & nosignal$tot_crash_count<=40)] <- 5
nosignal$count[which(nosignal$tot_crash_count>=41 & nosignal$tot_crash_count<=50)] <- 6
nosignal$count[which(nosignal$tot_crash_count>=51)] <- 7
table(nosignal$count)
## Create stacked percentage bar plots ##
library(ggplot2)
sign <- c(rep("No signal" , 7) , rep("Signalized" , 7))
crash_ct <- rep(c("1-5" , "6-10" , "11-20", "21-30", "31-40", "41-50", "51 or above") , 2)
crash_ct = factor(crash_ct, levels = c("51 or above","41-50" , "31-40", "21-30", "11-20", "6-10", "1-5"))
value <- c(107491,30059,18472,6326,3052,1551,2823,560,926,2040,1895,1657,1340,5935)
sign_data <- data.frame(sign,crash_ct,value)
ggplot(sign_data, aes(fill=crash_ct, y=value, x=sign)) + 
  geom_bar(position="fill", stat="identity") +
  ylab("Percentage") +
  labs(fill = "Total crash count") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size=25),
        axis.title.y = element_text(size=15),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=25),
        legend.text = element_text(size=20)) 

## Similarly, create stacked percentage plots for intersections with different numbers of lanes ##
two_lane <- filter(crash_data, lanes_major %in% c(1,2))
four_lane <- filter(crash_data, lanes_major %in% c(3,4))
six_lane <- filter(crash_data, lanes_major %in% c(5,6))
eight_lane <- filter(crash_data, lanes_major %in% c(7,8))

two_lane$count <- NA
two_lane$count[which(two_lane$tot_crash_count>=1 & two_lane$tot_crash_count<=5)] <- 1
two_lane$count[which(two_lane$tot_crash_count>=6 & two_lane$tot_crash_count<=10)] <- 2
two_lane$count[which(two_lane$tot_crash_count>=11 & two_lane$tot_crash_count<=20)] <- 3
two_lane$count[which(two_lane$tot_crash_count>=21 & two_lane$tot_crash_count<=30)] <- 4
two_lane$count[which(two_lane$tot_crash_count>=31 & two_lane$tot_crash_count<=40)] <- 5
two_lane$count[which(two_lane$tot_crash_count>=41 & two_lane$tot_crash_count<=50)] <- 6
two_lane$count[which(two_lane$tot_crash_count>=51)] <- 7
table(two_lane$count)
four_lane$count <- NA
four_lane$count[which(four_lane$tot_crash_count>=1 & four_lane$tot_crash_count<=5)] <- 1
four_lane$count[which(four_lane$tot_crash_count>=6 & four_lane$tot_crash_count<=10)] <- 2
four_lane$count[which(four_lane$tot_crash_count>=11 & four_lane$tot_crash_count<=20)] <- 3
four_lane$count[which(four_lane$tot_crash_count>=21 & four_lane$tot_crash_count<=30)] <- 4
four_lane$count[which(four_lane$tot_crash_count>=31 & four_lane$tot_crash_count<=40)] <- 5
four_lane$count[which(four_lane$tot_crash_count>=41 & four_lane$tot_crash_count<=50)] <- 6
four_lane$count[which(four_lane$tot_crash_count>=51)] <- 7
table(four_lane$count)
six_lane$count <- NA
six_lane$count[which(six_lane$tot_crash_count>=1 & six_lane$tot_crash_count<=5)] <- 1
six_lane$count[which(six_lane$tot_crash_count>=6 & six_lane$tot_crash_count<=10)] <- 2
six_lane$count[which(six_lane$tot_crash_count>=11 & six_lane$tot_crash_count<=20)] <- 3
six_lane$count[which(six_lane$tot_crash_count>=21 & six_lane$tot_crash_count<=30)] <- 4
six_lane$count[which(six_lane$tot_crash_count>=31 & six_lane$tot_crash_count<=40)] <- 5
six_lane$count[which(six_lane$tot_crash_count>=41 & six_lane$tot_crash_count<=50)] <- 6
six_lane$count[which(six_lane$tot_crash_count>=51)] <- 7
table(six_lane$count)
eight_lane$count <- NA
eight_lane$count[which(eight_lane$tot_crash_count>=1 & eight_lane$tot_crash_count<=5)] <- 1
eight_lane$count[which(eight_lane$tot_crash_count>=6 & eight_lane$tot_crash_count<=10)] <- 2
eight_lane$count[which(eight_lane$tot_crash_count>=11 & eight_lane$tot_crash_count<=20)] <- 3
eight_lane$count[which(eight_lane$tot_crash_count>=21 & eight_lane$tot_crash_count<=30)] <- 4
eight_lane$count[which(eight_lane$tot_crash_count>=31 & eight_lane$tot_crash_count<=40)] <- 5
eight_lane$count[which(eight_lane$tot_crash_count>=41 & eight_lane$tot_crash_count<=50)] <- 6
eight_lane$count[which(eight_lane$tot_crash_count>=51)] <- 7
table(eight_lane$count)

lane <- c(rep("1-2", 7) , rep("3-4", 7), rep("5-6", 7), rep("7-8", 7))
crash_ct <- rep(c("1-5" , "6-10" , "11-20", "21-30", "31-40", "41-50", "51 or above") , 4)
crash_ct = factor(crash_ct, levels = c("51 or above","41-50" , "31-40", "21-30", "11-20", "6-10", "1-5"))
value <- c(94434,20274,9743,2964,1458,762,1582,12466,9369,9124,4425,2687,1749,5333,1141,1333,1633,825,559,377,1813,10,9,12,7,5,3,30)
lane_data <- data.frame(lane,crash_ct,value)
ggplot(lane_data, aes(fill=crash_ct, y=value, x=lane)) + 
  geom_bar(position="fill", stat="identity") +
  xlab("Number of major lanes") +
  ylab("Percentage") +
  labs(fill = "Total crash count") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size=25),
        axis.title.y = element_text(size=15),
        axis.text.y = element_text(size=15),
        legend.title = element_text(size=25),
        legend.text = element_text(size=20)) 

## The dataset has 41 features and the outcome is the total number of crashes at intersections ##
crash_data <- crash_data[,1:42]

## Model 1: Random forest regression ##
set.seed(1234)
library(ranger)
library(caret)
## Divide the dataset into 70% training data and 30% test data ##
indexes = createDataPartition(crash_data$tot_crash_count, p = .7, list = F)
train = crash_data[indexes, ]
test = crash_data[-indexes, ]
## ranger is a R package that conducts random forest regression quickly ##
## Train the model ##
crash.rf <- ranger(tot_crash_count ~ ., data = train, num.trees = 500,
                   importance = "impurity", verbose = T)
print(crash.rf)
## Compute the predictions of the model, and calculate MSE, MAE, and RMSE ##
pred_y = predict(crash.rf, test)
mse = mean((test$tot_crash_count - pred_y$predictions)^2)
mae = caret::MAE(test$tot_crash_count, pred_y$predictions)
rmse = caret::RMSE(test$tot_crash_count, pred_y$predictions)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

## Compute feature importance, examining which features exert higher weights on the predictions ##
crash.rf$variable.importance/max(crash.rf$variable.importance)
library(vip)
vi(crash.rf, scale=T)
vip(crash.rf, num_features = 20, scale=T)


## Model 2: Xgboost ##
set.seed(1234)
indexes_2 = createDataPartition(crash_data$tot_crash_count, p = .7, list = F)
train_2 = crash_data[indexes_2, ]
test_2 = crash_data[-indexes_2, ]
train_x = data.matrix(train_2[,-42])
train_y = train_2[,42]
test_x = data.matrix(test_2[,-42])
test_y = test_2[,42]

library(xgboost)
## In XGBoost, the training and test data have to be in the DMatrix class ##
xgb.train = xgb.DMatrix(data = train_x, label = train_y)
xgb.test = xgb.DMatrix(data = test_x, label = test_y)
## Train the model ##
crash.xgb = xgboost(data = xgb.train, max_depth = 6, nrounds = 500, eta = 0.1, verbose = 2)
print(crash.xgb)
## Compute the predictions of the model, and calculate MSE, MAE, and RMSE ##
pred_y = predict(crash.xgb, xgb.test)
mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

## Compute R-squared ##
res <- test_y - pred_y
1 - var(res) / var(test_y)
## Compute feature importance ##
importance_matrix <- xgb.importance(model = crash.xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance", top_n = 20)


## Model 3: Light GBM ##
set.seed(1234)
indexes_3 = createDataPartition(crash_data$tot_crash_count, p = .7, list = F)
train_3 = crash_data[indexes_3, ]
test_3 = crash_data[-indexes_3, ]
train_x = data.matrix(train_3[,-42])
train_y = train_3[,42]
test_x = data.matrix(test_3[,-42])
test_y = test_3[,42]

library(lightgbm)
## In Light GBM, the data has to be in a lgb.Dataset object ##
dtrain <- lgb.Dataset(train_x, label = train_y)
## Specify the hyperparameters ##
train_params <- list(
  num_leaves = 6L,
  learning_rate = 0.1,
  objective = "regression",
  nthread = 2L
)
## Train the model ##
crash.bst <- lgb.train(
  data = dtrain,
  params = train_params,
  nrounds = 1000L,
  verbose = 1L
)
## Compute the predictions of the model, and calculate MSE, MAE, and RMSE ##
pred_y <- predict(crash.bst, test_x)
mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

## R-squared ##
res <- test_y - pred_y
1 - var(res) / var(test_y)
## Feature importance
tree_imp <- lgb.importance(crash.bst)
lgb.plot.importance(tree_imp, top_n = 20, measure = "Gain")


## Model 4: XBART ##
set.seed(1234)
indexes_4 = createDataPartition(crash_data$tot_crash_count, p = .7, list = F)
train_4 = crash_data[indexes_4, ]
test_4 = crash_data[-indexes_4, ]
train_x = data.matrix(train_4[,-42])
train_y = train_4[,42]
test_x = data.matrix(test_4[,-42])
test_y = test_4[,42]

library(dbarts)
## BART consumes a lot of memory, so we have to raise the memory limit to allow enough memory space ##
memory.limit(size=20000)
## Record the time taken for model training ##
time = proc.time()
xbart.train <- bart(train_x, train_y, verbose = T, ntree = 100, keeptrees = T)
time = proc.time() - time
print(time)
## Compute the predictions ##
pred_y <- predict(xbart.train, newdata=test_x, type = "response")

## Evaluate the performance metrics ##
mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

## R-squared
res <- test_y - pred_y
1 - var(res) / var(test_y)

## Since the dataset is imbalanced (About 70% of the intersections have 0 crashes), I try to balance the dataset by undersampling intersections with 0 crashes, and oversampling intersections with non-zero crashes ##
## The balanced dataset should have about 50% zero-crash intersections and 50% non-zero-crash intersections ##
## Data over and undersampling ##
library(ROSE)
crash_data$label <- NA
crash_data$label[which(crash_data$tot_crash_count==0)] <- 0
crash_data$label[which(crash_data$tot_crash_count!=0)] <- 1
crash_balance <- ovun.sample(label ~ ., data = crash_data, method = "both", seed = 1234)$data
table(crash_balance$label)
crash_balance <- crash_balance[,1:42]
crash_balance <- na.omit(crash_balance)

set.seed(1234)
library(caret)

## Model 1: Random Forest regression ##
library(ranger)
indexes = createDataPartition(crash_balance$tot_crash_count, p = .7, list = F)
train = crash_balance[indexes, ]
test = crash_balance[-indexes, ]
crash.rf <- ranger(tot_crash_count ~ ., data = train, num.trees = 500,
                   importance = "impurity", verbose = T)
print(crash.rf)

pred_y = predict(crash.rf, test)
mse = mean((test$tot_crash_count - pred_y$predictions)^2)
mae = caret::MAE(test$tot_crash_count, pred_y$predictions)
rmse = caret::RMSE(test$tot_crash_count, pred_y$predictions)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

crash.rf$variable.importance/max(crash.rf$variable.importance)
library(vip)
vi(crash.rf, scale=T)
vip(crash.rf, num_features = 20, scale=T)

## Model 2: Xgboost ##
set.seed(1234)
indexes_2 = createDataPartition(crash_balance$tot_crash_count, p = .7, list = F)
train_2 = crash_balance[indexes_2, ]
test_2 = crash_balance[-indexes_2, ]
train_x = data.matrix(train_2[,-42])
train_y = train_2[,42]
test_x = data.matrix(test_2[,-42])
test_y = test_2[,42]

library(xgboost)
xgb.train = xgb.DMatrix(data = train_x, label = train_y)
xgb.test = xgb.DMatrix(data = test_x, label = test_y)
crash.xgb = xgboost(data = xgb.train, max_depth = 6, nrounds = 500, eta = 0.1, verbose = 2)
print(crash.xgb)

pred_y = predict(crash.xgb, xgb.test)
mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

## R-squared
res <- test_y - pred_y
1 - var(res) / var(test_y)

importance_matrix <- xgb.importance(model = crash.xgb)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance", top_n = 20)


## Model 3: Light GBM ##
set.seed(1234)
indexes_3 = createDataPartition(crash_balance$tot_crash_count, p = .7, list = F)
train_3 = crash_balance[indexes_3, ]
test_3 = crash_balance[-indexes_3, ]
train_x = data.matrix(train_3[,-42])
train_y = train_3[,42]
test_x = data.matrix(test_3[,-42])
test_y = test_3[,42]

library(lightgbm)
dtrain <- lgb.Dataset(train_x, label = train_y)

train_params <- list(
  num_leaves = 6L,
  learning_rate = 0.1,
  objective = "regression",
  nthread = 2L
)
crash.bst <- lgb.train(
  data = dtrain,
  params = train_params,
  nrounds = 1000L,
  verbose = 1L
)

pred_y <- predict(crash.bst, test_x)
mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

## R-squared
res <- test_y - pred_y
1 - var(res) / var(test_y)

tree_imp <- lgb.importance(crash.bst)
lgb.plot.importance(tree_imp, top_n = 20, measure = "Gain")


## Model 4: XBART ##
set.seed(1234)
indexes_4 = createDataPartition(crash_balance$tot_crash_count, p = .7, list = F)
train_4 = crash_balance[indexes_4, ]
test_4 = crash_balance[-indexes_4, ]
train_x = data.matrix(train_4[,-42])
train_y = train_4[,42]
test_x = data.matrix(test_4[,-42])
test_y = test_4[,42]

library(dbarts)
memory.limit(size=15000)
time = proc.time()
xbart.train <- bart(train_x, train_y, verbose = T, ntree = 100, keeptrees = T)
time = proc.time() - time
print(time)

pred_y <- predict(xbart.train, newdata=test_x, type = "response")

## Evaluate the performance metrics ##
mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

## R-squared
res <- test_y - pred_y
1 - var(res) / var(test_y)

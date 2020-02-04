Sys.setenv(LANG = "en")

#sets proper directory
setwd("C:\\Users\\Michalina\\Desktop\\Machine Learning\\nowe_projekt")

#installs all the needed packages
source("technicals_10.11.R")

#reads dataset, saves as data.frame
data <- read_csv("CarPrice_Assignment.csv")
data <- as.data.frame(data)

#create new variable
data[,"CarBrand"] <- word(data[,"CarName"],1)
sum(is.na(data))

source("data_preparation_11.12_small.R")
data_final <- data_preparation(data)

str(data_final)
data_final <- data_final[,-c(1,3)]

#####plot of dependent variable
d <- ggplot(data_final, aes(x=price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#11CF1D") + 
  geom_vline(aes(xintercept=mean(price)),
             color="#1142CF", linetype="dashed", size=1) +
  labs(title='Density of price') +
  theme(legend.title = element_blank())
print(d)

#plots of independent variables
source('plots.R')
create_plot(data_final)

p3 <- ggplot(data_final, aes(x=data_final[,25], y=price, color=data_final[,25])) +
  geom_boxplot() +
  labs(title=paste('Boxplot of', colnames(data_final)[25]), x=colnames(data_final)[25], y='price') +
  theme(legend.title = element_blank())
p3 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
c <- ggplot(data_final, aes(factor(data_final[,25]))) + geom_bar(fill='#F08080') +
  labs(title=paste(colnames(data_final)[25],'frequency'), x=colnames(data_final)[25]) +
  theme(legend.title = element_blank())
c + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####
continuous_variables <- c(
  "wheelbase","carlength","carwidth",
  "carheight","curbweight",
  "enginesize","boreratio",
  "stroke","compressionratio",
  "horsepower","peakrpm","citympg","highwaympg", "price"
)

categorical_variables <- c(
  "fueltype","aspiration","doornumber","carbody","drivewheel","enginelocation",
  "enginetype","cylindernumber","fuelsystem","CarBrand", "symboling"
)

#correlations' analyses
source('correlations_11.12.R')

#pearson for continuous variables
correlations(data_final,continuous_variables,1)

#goodman/kruskal lambda for categorical variables
correlations(data_final,categorical_variables,0)


####encoding 
data_final_enc <- as.data.frame(one_hot(as.data.table(data_final)))

data_final_enc_cont <- as.data.frame(one_hot(as.data.table(data_final[,continuous_variables])))

data_final_enc_cat <- as.data.frame(one_hot(as.data.table(data_final[,categorical_variables])))

##standarization
data_final_enc_cont <- robustscale(data_final_enc_cont)

data_final_enc <- cbind(data_final_enc_cat, data_final_enc_cont$data)

#changing name of columns
data_final_enc <- data_final_enc %>% 
  rename(
    symboling_neg_1 = 'symboling_-1',
    symboling_neg_2 = 'symboling_-2'
  )

##split into train and test
set.seed(12345)
indice = sample(seq_len(nrow(data_final_enc)), size = floor(0.7*nrow(data_final_enc)))

train = data_final_enc[indice,]
test = data_final_enc[-indice,]

nrow(test)/nrow(data_final_enc)
nrow(train)/nrow(data_final_enc)

c_5 = trainControl(method = "cv", number = 5)

#######Linear regression
source('regressionMetrics.R')

modelform <- price~.
set.seed(12345)
linear <- train(modelform, data = train,
                method = "lm",
                metric = "RMSE",
                trControl = c_5)

linear$results
summary(linear)

predictions_l <- predict(linear, test)

linear_m <- regressionMetrics(real=test$price, predicted=predictions_l)
linear_m

set.seed(12345)
model1 <- lm(price~., data=train)
step.model <- stepAIC(model1, direction='both', trace=TRUE)

modelformula <- as.formula(step.model$call)

set.seed(12345)
linear_2 <- train(modelformula, data = train,
                method = "lm",
                metric = "RMSE",
                trControl = c_5)

linear_2$results
summary(linear_2)

predictions_l_2 <- predict(linear_2, test)

linear_m_2 <- regressionMetrics(real=test$price, predicted=predictions_l_2)
linear_m_2

#########Lasso and Ridge 
lambda <- 10^seq(-3, 3, length = 100)
set.seed(12345)
ridge <- train(
  price ~., data = train, method = "glmnet",
  trControl = c_5,
  tuneGrid = expand.grid(alpha = 0, lambda = lambda), 
  standardize = FALSE
)

ridge$bestTune$lambda

predictions_r_tr <- ridge %>% predict(train) #, s=ridge$bestTune$lambda)
ridge_m_tr <- regressionMetrics(real=train$price, predicted=predictions_r_tr)
ridge_m_tr

predictions_r <- ridge %>% predict(test) #, s=ridge$bestTune$lambda)
ridge_m_tst <- regressionMetrics(real=test$price, predicted=predictions_r)
ridge_m_tst
####################
set.seed(12345)
lasso <- train(
  price ~., data = train, method = "glmnet",
  trControl = c_5,
  tuneGrid = expand.grid(alpha = 1, lambda = lambda),
  standardize = FALSE
)

lasso$bestTune$lambda

predictions_ls_tr <- lasso %>% predict(train)#, s=lasso$bestTune$lambda)
lasso_m_tr <- regressionMetrics(real=train$price, predicted=predictions_ls_tr)
lasso_m_tr

predictions_ls <- lasso %>% predict(test)#, s=lassso$bestTune$lambda)
lasso_m_tst <- regressionMetrics(real=test$price, predicted=predictions_ls)
lasso_m_tst

##############Tree regression
set.seed(12345)
cars.tree <- tree(price ~ ., train)

summary(cars.tree)

plot(cars.tree)
text(cars.tree, pretty = 0)

set.seed(12345)
cars.tree_r <- rpart(price~., data=train, cp=0.01)
rpart.plot(cars.tree_r, box.palette="RdBu", shadow.col="gray", nn=TRUE)


cars.cv <- cv.tree(cars.tree, K = 5)
plot(cars.cv$size, cars.cv$dev, type = 'b')

best.size <- cars.cv$size[which(cars.cv$dev==min(cars.cv$dev))]
best.size

set.seed(12345)
cars.prune <- prune.tree(cars.tree, best = best.size)

# prunned tree on the plot
plot(cars.prune)
text(cars.prune, pretty = 0)

# and finally we can build the prediction 
set.seed(12345)
cars.pred <- predict(cars.tree, newdata = test)

# visualising prediction 
#carS.test <- data_final_enc[-train, "price"]
plot(cars.pred, test$price, ylab="Real", xlab='Predicted', main='Real vs predicted price (test data)',
     col='#34495e')
abline(0, 1, col='#f1c40f')

cars.pred_tr <- predict(cars.tree, newdata = train)
tree_m_tr <- regressionMetrics(real=train$price, predicted=cars.pred_tr)
tree_m_tr

tree_m_tst <- regressionMetrics(real=test$price, predicted=cars.pred)
tree_m_tst


set.seed(12345)
cars.tree_2 <- tree(price ~ ., train, mindev=0.001)
cars.tree_2_r <- rpart(price~., data=train, cp=0.001)
rpart.plot(cars.tree_2_r, box.palette="RdBu", shadow.col="gray", nn=TRUE)

summary(cars.tree_2)

plot(cars.tree_2)
text(cars.tree_2, pretty = 0)


cars.cv_2 <- cv.tree(cars.tree_2, K = 5)
plot(cars.cv_2$size, cars.cv_2$dev, type = 'b')

best.size_2 <- cars.cv_2$size[which(cars.cv_2$dev==min(cars.cv_2$dev))]
best.size_2

set.seed(12345)
cars.prune_2 <- prune.tree(cars.tree_2, best = best.size_2[length(best.size_2)])

plot(cars.prune_2)
text(cars.prune_2, pretty = 0)

cars.pred_2 <- predict(cars.tree_2, newdata = test)

# visualising prediction 
#carS.test <- data_final_enc[-train, "price"]
plot(cars.pred_2, test$price, ylab="Real", xlab='Predicted', main='Real vs predicted price (test data)',
     col='#34495e')
abline(0, 1, col='#f1c40f')

cars.pred_2_tr <- predict(cars.tree_2, newdata = train)
tree_m_2_tr <- regressionMetrics(real=train$price, predicted=cars.pred_2_tr)
tree_m_2_tr

tree_m_2_tst <- regressionMetrics(real=test$price, predicted=cars.pred_2)
tree_m_2_tst

#############kNN regression
set.seed(12345)
knn1 <- 
  train(price~., 
        data = train, 
        method = "knn",
        trControl = c_5, 
        standarize = FALSE,
        metric="RMSE")

knn1

knn1$finalModel
knn1$finalModel$k
knn1$bestTune$k

plot(knn1)

#knnPredict <- predict(knn1,newdata = train, s=knn1$bestTune$k)

predictions_knn_tr <- knn1 %>% predict(train)#, s=knn1$bestTune$k)
knn_m_tr <- regressionMetrics(real=train$price, predicted=predictions_knn_tr)
knn_m_tr

predictions_knn <- knn1 %>% predict(test)#, s=knn1$bestTune$k)
knn_m_tst <- regressionMetrics(real=test$price, predicted=predictions_knn)
knn_m_tst


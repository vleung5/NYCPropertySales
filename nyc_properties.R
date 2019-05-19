
setwd("C:\\Users\\Owner\\Capstone")

if(!require(data.table)) install.packages("data.table")
if(!require(tidyr)) install.packages("tidyr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2",dependencies=TRUE)
if(!require(lubridate)) install.packages("lubridate")
if(!require(corrplot)) install.packages("corrplot")
if(!require(caret)) install.packages("caret")
if(!require(gbm)) install.packages("gbm")
if(!require(glmnet)) install.packages("glmnet")
if(!require(MASS)) install.packages("MASS")
if(!require(randomForest)) install.packages("randomForest")

library(data.table)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(corrplot)
library(caret)
library(gbm)
library(glmnet)
library(MASS)
library(Rborist)
library(randomForest)

#Data Import

#I downloaded the dataset from Kaggle and read it as a data frame using the fread() function.
#The NYC Property Sales Data has 84548 observations and 22 variables. It has property sales data 
#of each of the 5 boroughs in NYC - Manhattan, the Bronx, Queens, Brookyln and Staten Island.

nyc_prop <- as_data_frame(fread("nyc-rolling-sales.csv"))
class(nyc_prop)
#[1] "tbl_df"     "tbl"        "data.frame"

dim(nyc_prop)
#[1] 84548    22

names(nyc_prop)
#[1] "V1"                             "BOROUGH"                        "NEIGHBORHOOD"                   "BUILDING CLASS CATEGORY"       
#[5] "TAX CLASS AT PRESENT"           "BLOCK"                          "LOT"                            "EASE-MENT"                     
#[9] "BUILDING CLASS AT PRESENT"      "ADDRESS"                        "APARTMENT NUMBER"               "ZIP CODE"                      
#[13] "RESIDENTIAL UNITS"              "COMMERCIAL UNITS"               "TOTAL UNITS"                    "LAND SQUARE FEET"              
#[17] "GROSS SQUARE FEET"              "YEAR BUILT"                     "TAX CLASS AT TIME OF SALE"      "BUILDING CLASS AT TIME OF SALE"
#[21] "SALE PRICE"  


#Data Cleaning

# Removed unnecessary columns 'V1' and 'EASE-MENT' 
nyc_property <- as_data_frame(nyc_prop[,-1])
nyc_property <- nyc_property[,c(-7)]
names(nyc_property)


head(nyc_property)

# Separated the columns in 'BUILDING CLASS CATEGORY'
nyc_property <- nyc_property %>%
  separate(col = "BUILDING CLASS CATEGORY", into = c("BUILDING CLASS CATEGORY NUMBER",
                                                     "BUILDING CLASS CATEGORY"), sep = 3) %>%
  separate(col = "SALE DATE", into = c("SALE DATE", "TIME"), sep = " ")


unique(nyc_property$'BUILDING CLASS CATEGORY NUMBER')
unique(nyc_property$'BUILDING CLASS CATEGORY')

# Removing the Duplicates in the data frame, nyc_property 
nyc_property %>% filter(duplicated(nyc_property) == TRUE) %>% nrow()
#[1] 765
head(nyc_property %>% filter(duplicated(nyc_property) == TRUE))
nyc_property <- unique(nyc_property)


lapply(nyc_property, class)
names(nyc_property)
head(nyc_property)

# Data Type conversions to the Data set
#fac <- c(1,2,3,4,5,8,11,18,19)
fac <- c(1,2,3,4,5,8,11,18,19)
nyc_property <- nyc_property %>% mutate_at(fac, funs(factor(.)))
levels(nyc_property$BOROUGH) <- c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")


num <- c(15,16,17,20)
nyc_property <- nyc_property %>% mutate_at(num, funs(as.numeric(.)))
#Warning in evalq(as.numeric(`LAND SQUARE FEET`), <environment>) :
#  NAs introduced by coercion
#Warning in evalq(as.numeric(`GROSS SQUARE FEET`), <environment>) :
#  NAs introduced by coercion
#Warning in evalq(as.numeric(`SALE PRICE`), <environment>) :
#  NAs introduced by coercion

#chr <- c(6,7)
chr <- c(6,7)
nyc_property  <- nyc_property %>% mutate_at(chr, funs(as.character(.)))

nyc_property$'SALE DATE' <- ymd(nyc_property$'SALE DATE')
nyc_property$'SALE YEAR' <- as.factor(format(nyc_property$'SALE DATE',"%Y"))
nyc_property$'SALE MONTH' <- as.factor(format(nyc_property$'SALE DATE',"%m"))

colSums(is.na(nyc_property))
#LAND SQUARE FEET  has 26054 
#GROSS SQUARE FEET has 27385
#SALE PRICE has 14176

colSums(nyc_property == 0)

#Let's remove NA or else we will get the following error later when training:
#Warning in storage.mode(v) <- "double" : NAs introduced by coercion
#Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
#  NA/NaN/Inf in 'y'
sum(is.na(nyc_property$'SALE PRICE'))
#[1] 14176
nyc_property %>% filter(nyc_property$'SALE PRICE' == 0 | is.na(nyc_property$'SALE PRICE')) %>% nrow()
#[1] 24188
#Remove rows with no sale price
nyc_property <- nyc_property %>% filter((!nyc_property$'SALE PRICE' == 0) & !is.na(nyc_property$'SALE PRICE'))



dim(nyc_property)

#Do the same with land square feet and gross square feet
nyc_property <- nyc_property %>% filter((!nyc_property$'LAND SQUARE FEET' == 0) & !is.na(nyc_property$'LAND SQUARE FEET'))
nyc_property <- nyc_property %>% filter((!nyc_property$'GROSS SQUARE FEET' == 0) & !is.na(nyc_property$'GROSS SQUARE FEET'))
nyc_property <- nyc_property %>% filter((!nyc_property$'ZIP CODE' == 0) & !is.na(nyc_property$'ZIP CODE'))
nyc_property <- nyc_property %>% filter((!nyc_property$'TOTAL UNITS' == 0) & !is.na(nyc_property$'TOTAL UNITS'))
nyc_property <- nyc_property %>% filter((!nyc_property$'YEAR BUILT' == 0) & !is.na(nyc_property$'YEAR BUILT'))

#Let's get the building age instead of the year built, it is more practical and clearer to understand.
nyc_property <- nyc_property %>% 
  mutate('BUILDING AGE' = 2017 - nyc_property$'YEAR BUILT')
#which(names(nyc_property) %in% c("YEAR BUILT"))

#remove year built
nyc_property <- subset(nyc_property, select = -c(which(names(nyc_property) %in% c("YEAR BUILT"))))

nyc_property <- subset(nyc_property, select = -c(which(names(nyc_property) %in% c("SALE DATE"))))
nyc_property <- subset(nyc_property, select = -c(which(names(nyc_property) %in% c("TIME"))))
nyc_property <- subset(nyc_property, select = -c(which(names(nyc_property) %in% c("ADDRESS","APARTMENT NUMBER"))))


#filter out outliers for sale price
 boxplot(nyc_property$'SALE PRICE')
 outliers = boxplot(nyc_property$'SALE PRICE', plot=FALSE)$out
 nyc_property[nyc_property$'SALE PRICE' %in% outliers,]
 nyc_property <- nyc_property %>% filter(nyc_property$'SALE PRICE' %in% outliers )


#Do a log transformation on this
hist(nyc_property$'SALE PRICE')
nyc_property <- nyc_property %>% mutate_at(c(which(names(nyc_property) %in% c("SALE PRICE"))),funs(log10(.)))
mean(nyc_property$'SALE PRICE')
hist(nyc_property$'SALE PRICE')

summary(nyc_property)

names(nyc_property)

#Normalising and Transforming Numerical columns

#nyc_property %>% mutate_at(c(12,13,14),funs(. + 1)) 
#nyc_property %>% mutate_at(c(15,16),funs(. + 1)) 
#nyc_property %>% mutate_at(c(25),funs(. + 1)) 
nyc_property %>% mutate_at(c(which(names(nyc_property) %in% c("RESIDENTIAL UNITS","COMMERCIAL UNITS","TOTAL UNITS"))),funs(. + 1)) 
nyc_property %>% mutate_at(c(which(names(nyc_property) %in% c("LAND SQUARE FEET", "GROSS SQUARE FEET"))),funs(. + 1)) 
nyc_property %>% mutate_at(c(which(names(nyc_property) %in% c("BUILDING AGE"))),funs(. + 1)) 


hist(nyc_property$'RESIDENTIAL UNITS')
nyc_property <- nyc_property %>% mutate_at(c(which(names(nyc_property) %in% c("RESIDENTIAL UNITS"))),funs(log1p(.))) 
hist(nyc_property$'RESIDENTIAL UNITS')

hist(nyc_property$'COMMERCIAL UNITS')
nyc_property <- nyc_property %>% mutate_at(c(which(names(nyc_property) %in% c("COMMERCIAL UNITS"))),funs(log1p(.))) 
hist(nyc_property$'COMMERCIAL UNITS')

hist(nyc_property$'TOTAL UNITS')
nyc_property <- nyc_property %>% mutate_at(c(which(names(nyc_property) %in% c("TOTAL UNITS"))),funs(log10(.))) 
hist(nyc_property$'TOTAL UNITS')

hist(nyc_property$'LAND SQUARE FEET')
nyc_property <- nyc_property %>% mutate_at(c(which(names(nyc_property) %in% c("LAND SQUARE FEET"))),funs(log10(.))) 
hist(nyc_property$'LAND SQUARE FEET')

hist(nyc_property$'GROSS SQUARE FEET')
nyc_property <- nyc_property %>% mutate_at(c(which(names(nyc_property) %in% c("GROSS SQUARE FEET"))),funs(log10(.))) 
hist(nyc_property$'GROSS SQUARE FEET')


#split my data set into an 80-20% training and test set.
set.seed(23)
index <- sample(nrow(nyc_property),nrow(nyc_property)*0.80)
nyc_property.train <- nyc_property[index,]
nyc_property.test<- nyc_property[-index,]


nzv <- nearZeroVar(nyc_property.train)
image(matrix(1:784 %in% nzv, 28, 28))
col_index <- setdiff(1:ncol(nyc_property.train), nzv)
length(col_index)


#Let's plot the correlations
nyc_property.train %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

#It looks like year built and building age doesn't have good correlations.


nyc_property.train %>% 
  select('BOROUGH', 'NEIGHBORHOOD','RESIDENTIAL UNITS','COMMERCIAL UNITS','TOTAL UNITS', 
         'LAND SQUARE FEET','GROSS SQUARE FEET', 'BUILDING AGE') %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")



RMSE <- function(true_sale_price, predicted_sale_price){
  sqrt(mean((true_sale_price - predicted_sale_price)^2))
}

names(nyc_property.train)
names(nyc_property.test)
lapply(nyc_property.train, class)


# There are neighborhood in test but not in train and will throw the following error.  So let's add this to the train data.
# Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#   factor NEIGHBORHOOD has new levels ARROCHAR, BULLS HEAD, CONCORD, GRANT CITY, MARINERS HARBOR, MORRIS PARK/VAN NEST, SOUTH BEACH, SOUTH OZONE PARK, WOODLAWN

#neighorhood in test but not in train?
nyc_property.train %>% filter(`NEIGHBORHOOD` == "ARROCHAR") #0 row
nyc_property.test %>% filter(`NEIGHBORHOOD` == "ARROCHAR") #1 row

missing_data <- nyc_property.test %>% 
  anti_join(nyc_property.train, by = "NEIGHBORHOOD")

nyc_property.train <- rbind(nyc_property.train, missing_data)

#same with building class category number
# Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#   factor BUILDING CLASS CATEGORY NUMBER has new levels 05 , 06 
missing_data <- nyc_property.test %>% 
  anti_join(nyc_property.train, by = "BUILDING CLASS CATEGORY NUMBER")
nyc_property.train <- rbind(nyc_property.train, missing_data)

missing_data <- nyc_property.test %>% 
  anti_join(nyc_property.train, by = "BLOCK")
nyc_property.train <- rbind(nyc_property.train, missing_data)

missing_data <- nyc_property.test %>% 
  anti_join(nyc_property.train, by = "LOT")
nyc_property.train <- rbind(nyc_property.train, missing_data)

missing_data <- nyc_property.test %>% 
  anti_join(nyc_property.train, by = "BUILDING CLASS CATEGORY")
nyc_property.train <- rbind(nyc_property.train, missing_data)

missing_data <- nyc_property.test %>% 
  anti_join(nyc_property.train, by = "ZIP CODE")
nyc_property.train <- rbind(nyc_property.train, missing_data)

missing_data <- nyc_property.test %>% 
  anti_join(nyc_property.train, by = "BUILDING CLASS AT TIME OF SALE")
nyc_property.train <- rbind(nyc_property.train, missing_data)


Sys.time()
# "2019-05-17 07:50:15 EDT"
# 

# m <- lm(nyc_property.train$'SALE PRICE' ~ nyc_property.train$'BOROUGH' +
#                        nyc_property.train$'NEIGHBORHOOD' +
#                        nyc_property.train$'BUILDING CLASS CATEGORY NUMBER' +
#                        nyc_property.train$'BUILDING CLASS CATEGORY' +
#                        nyc_property.train$'TAX CLASS AT PRESENT'+
#                        #block, lot
#                        nyc_property.train$'BLOCK'+
#                        nyc_property.train$'LOT'+
#                        nyc_property.train$'BUILDING CLASS AT PRESENT' +
#                        nyc_property.train$'ZIP CODE' +
#                        #residential units, commercial units
#                        nyc_property.train$'COMMERCIAL UNITS' +
#                        nyc_property.train$'RESIDENTIAL UNITS' +
#                        nyc_property.train$'TOTAL UNITS' +
#                        nyc_property.train$'LAND SQUARE FEET' +
#                        nyc_property.train$'GROSS SQUARE FEET' +
#                        nyc_property.train$'BUILDING AGE' +
#                        nyc_property.train$'TAX CLASS AT TIME OF SALE' +
#                        nyc_property.train$'BUILDING CLASS AT TIME OF SALE' +
#                        #SALE PRICE
#                        nyc_property.train$'SALE YEAR' +
#                        nyc_property.train$'SALE MONTH', data = nyc_property.train)
# 
# m <- lm(`SALE PRICE` ~ `BOROUGH` +
#           `NEIGHBORHOOD` +
#           `BUILDING CLASS CATEGORY NUMBER` +
#           `BUILDING CLASS CATEGORY` +
#           `TAX CLASS AT PRESENT`+
#           #block, lot
#           `BLOCK`+
#           `LOT`+
#           `BUILDING CLASS AT PRESENT` +
#           `ZIP CODE` +
#           #residential units, commercial units
#           `COMMERCIAL UNITS` +
#           `RESIDENTIAL UNITS` +
#           `TOTAL UNITS` +
#           `LAND SQUARE FEET` +
#           `GROSS SQUARE FEET` +
#           `BUILDING AGE` +
#           `TAX CLASS AT TIME OF SALE` +
#           `BUILDING CLASS AT TIME OF SALE` +
#           #SALE PRICE
#           `SALE YEAR` +
#           `SALE MONTH`, data = nyc_property.train)

names(nyc_property.train)
m <- lm(nyc_property.train$'SALE PRICE' ~ . - `SALE PRICE`, data = nyc_property.train)


Sys.time()
#"2019-05-17 07:51:46 ED"T

predict_model_lm_m <- predict(m, nyc_property.test)
# Warning message:
#   In predict.lm(m, nyc_property.test) :
#   prediction from a rank-deficient fit may be misleading
length(m$coefficients) > m$rank
#this means there are collinear covariates, maybe total units?
# library(Matrix)
# cat(rankMatrix(nyc_property.train), "\n")
# cat(rankMatrix(nyc_property.test), "\n")
# install.packages("car")
# library(car)
# vif(lm(nyc_property.train$'SALE PRICE' ~ . - `SALE PRICE`, data = nyc_property.train))
# alias( lm(nyc_property.train$'SALE PRICE' ~ . - `SALE PRICE`, data = nyc_property.train) )
# Error in vif.default(lm(nyc_property.train$"SALE PRICE" ~ . - `SALE PRICE`,  : 
#                           there are aliased coefficients in the model


model_1_rmse_m <- RMSE(predict_model_lm_m, nyc_property.test$'SALE PRICE')
model_1_rmse_m


rmse_results <- data_frame(method = "LM", RMSE = model_1_rmse_m)
rmse_results %>% knitr::kable()



#GBM Gradient Boosting Machine
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1) , predFixed = c(10, 15, 35))

names(nyc_property.train)
lapply(nyc_property.train, class)

Sys.time()
# train_gbm <-  train(nyc_property.train[,c(1,2,12,13,14,15,16,17,23,24)], nyc_property.train$'SALE PRICE',
#                     method = "gbm",
#                     trControl = control)

# train_gbm <-  train(nyc_property.train$'SALE PRICE' ~ . ,nyc_property.train,
#                     method = "gbm",
#                     trControl = control)
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
#   contrasts can be applied only to factors with 2 or more levels

# train_gbm <-  train(nyc_property.train[,c(1,2,12,13,14,15,16,17,23,24)], nyc_property.train$'SALE PRICE',
#                                         method = "gbm",
#                                         trControl = control)

Sys.time()
#Setting row names on a tibble is deprecated.
#if moving from data.frame to data.table, there will be no more warnings.

# ggplot(train_gbm)
# #RMSE goes down when max tree depth is at 2
# Sys.time()
# predictions_gbm_model1 <- predict(train_gbm, nyc_property.test)
# gbm_model_1_rmse <-RMSE(predictions_gbm_model1, nyc_property.test$'SALE PRICE')
# gbm_model_1_rmse
# #0.8787105
# Sys.time()
# 
# rmse_results <- bind_rows(rmse_results,
#                           data_frame(method="GBM model 1",  
#                                      RMSE = gbm_model_1_rmse ))
# rmse_results %>% knitr::kable()


lapply(nyc_property.train, class)

#try this way:
gbm.fit <- gbm(
  formula = `SALE PRICE` ~ `BOROUGH` +
    `NEIGHBORHOOD` +
    `BUILDING CLASS CATEGORY NUMBER` +
    `BUILDING CLASS CATEGORY` +
    `TAX CLASS AT PRESENT`+
    #block, lot
    #`BLOCK`+
    #`LOT`+
    `BUILDING CLASS AT PRESENT` +
    `ZIP CODE` +
    #residential units, commercial units
    `COMMERCIAL UNITS` +
    `RESIDENTIAL UNITS` +
    `TOTAL UNITS` +
    `LAND SQUARE FEET` +
    `GROSS SQUARE FEET` +
    `BUILDING AGE` +
    `TAX CLASS AT TIME OF SALE` +
    `BUILDING CLASS AT TIME OF SALE` +
    #SALE PRICE
    `SALE YEAR` +
    `SALE MONTH`, 
  distribution = "gaussian",
  data = nyc_property.train,
  n.trees = 1000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)

#This doesnt work because it only accept variables in numeric, ordered or factor (not characters)
# gbm.fit <- gbm(
#   formula = nyc_property.train$'SALE PRICE' ~ . - `SALE PRICE`,
#   distribution = "gaussian",
#   data = nyc_property.train,
#   n.trees = 10000,
#   interaction.depth = 1,
#   shrinkage = 0.001,
#   cv.folds = 5,
#   n.cores = NULL, # will use all cores by default
#   verbose = FALSE
# )  
# Error in checkForRemoteErrors(val) : 
#   5 nodes produced errors; first error: variable 6: BLOCK is not of type numeric, ordered, or factor.

Sys.time()

gbm.fit
#The best cross-validation iteration was 7407

# get MSE and compute RMSE
gbm_model_rmse <- sqrt(min(gbm.fit$cv.error))
gbm_model_rmse
#[1] 0.2496879
Sys.time()
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="GBM model",  
                                     RMSE = gbm_model_rmse ))
rmse_results %>% knitr::kable()



#Let's try random forest again
names(nyc_property.train)
lapply(nyc_property.train, class)
# 
# rs <- Rborist(nyc_property.train[,col_index], nyc_property.train$`SALE PRICE`)

#check level of borough in both test and train

X = nyc_property.train[,names(nyc_property.train)!="SALE PRICE"]
y = nyc_property.train$"SALE PRICE"
dim(X)
dim(y)
class(y)
names(nyc_property.train)
rf_model <- randomForest(X,y)
# Error in randomForest.default(X, y) : 
#   Can not handle categorical predictors with more than 53 categories.

#make illegal variable names to legal names.
nyc_property.train_new <- nyc_property.train
names(nyc_property.train_new) <- make.names(names(nyc_property.train_new))

nyc_property.test_new <- nyc_property.test
names(nyc_property.test_new) <- make.names(names(nyc_property.test_new))

names(Filter(is.factor, nyc_property.train_new))
length(unique(nyc_property.train_new$BOROUGH))
length(unique(nyc_property.train_new$NEIGHBORHOOD))
length(unique(nyc_property.train_new$BUILDING.CLASS.CATEGORY.NUMBER))
length(unique(nyc_property.train_new$BUILDING.CLASS.CATEGORY))
length(unique(nyc_property.train_new$TAX.CLASS.AT.PRESENT))
length(unique(nyc_property.train_new$BUILDING.CLASS.AT.PRESENT))
length(unique(nyc_property.train_new$ZIP.CODE))
length(unique(nyc_property.train_new$TAX.CLASS.AT.TIME.OF.SALE))
length(unique(nyc_property.train_new$BUILDING.CLASS.AT.TIME.OF.SALE))
length(unique(nyc_property.train_new$SALE.YEAR))
length(unique(nyc_property.train_new$SALE.MONTH))

rf_model <- randomForest(`SALE.PRICE` ~ 
                                                                              `BOROUGH`+
                                                                            #`NEIGHBORHOOD`, #no good
                                                                            `BUILDING.CLASS.CATEGORY.NUMBER` +
                                                                            `BUILDING.CLASS.CATEGORY` +
                                                                            `TAX.CLASS.AT.PRESENT`+
                                                                            `BLOCK`+
                                                                            `LOT`+
                                                                            #nyc_property.train$'BUILDING CLASS AT PRESENT' + #no good
                                                                            #nyc_property.train$'ZIP CODE' + #no sgood
                                                                            `COMMERCIAL.UNITS` +
                                                                            `RESIDENTIAL.UNITS` +
                                                                            `TOTAL.UNITS` +
                                                                            `LAND.SQUARE.FEET` +
                                                                            `GROSS.SQUARE.FEET` +
                                                                            `BUILDING.AGE` +
                                                                            `TAX.CLASS.AT.TIME.OF.SALE` +
                                                                            #nyc_property.train$'BUILDING CLASS AT TIME OF SALE' + #no good
                                                                            `SALE.YEAR`+
                                                                            `SALE.MONTH`,
                                                                            data=nyc_property.train_new, 
                                                                            importance=TRUE, na.action=na.omit)
# # Error in randomForest.default(m, y, ...) : 
# #   Can not handle categorical predictors with more than 53 categories.
# # removing those fields
# 
# Error in eval(predvars, data, env) : object 'SALE MONTH' not found
# solution: illegal variable names in dataframe.  use make.names into legal names.

rf_model
nyc_property.test
colSums(is.na(nyc_property.test))
predictions_rf_model1 <- predict(rf_model, nyc_property.test_new)
# Warning message:
# #   'newdata' had 589 rows but variables found have 2354 rows 
# Solution: it means there are issues matching variable names.  Name is them with ``.
rf_model_1_rmse <-RMSE(predictions_rf_model1, nyc_property.test_new$'SALE.PRICE')
rf_model_1_rmse
#[1] 0.1966702
Sys.time()

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="RF model 1",
                                     RMSE = rf_model_1_rmse ))
rmse_results %>% knitr::kable()

#ridge regression

#ridge needs the data in a matrix
# x=model.matrix(nyc_property.train$'SALE PRICE' ~ . - `SALE PRICE`,data=nyc_property.train) 
# x_test=model.matrix(nyc_property.test$'SALE PRICE' ~ . - `SALE PRICE`,data=nyc_property.test) 
# dim(nyc_property.train)
# dim(nyc_property.test)

x=model.matrix(nyc_property$'SALE PRICE' ~ . - `SALE PRICE`,data=nyc_property) 
y=nyc_property$'SALE PRICE'
set.seed(23)
index <- sample(nrow(x),nrow(x)*0.80)
mtx_train_x <- x[index,]
mtx_train_y <- y[index]
mtx_test_x<- x[-index,]
mtx_test_y <- y[-index]



ridge <- glmnet(mtx_train_x,mtx_train_y,alpha=0)
plot(ridge,xvar="lambda",label=TRUE)

cv.ridge=cv.glmnet(mtx_train_x,mtx_train_y,alpha=0)
plot(cv.ridge)

predictions_ridge_model <- predict(ridge, mtx_test_x)

ridge_model_1_rmse <-RMSE(predictions_ridge_model, mtx_test_y)
ridge_model_1_rmse
#[1] 0.3714356

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="GLM with Ridge Regression model",  
                                     RMSE = ridge_model_1_rmse ))
rmse_results %>% knitr::kable()



#lasso

lasso <- glmnet(mtx_train_x,mtx_train_y,alpha=1)
plot(lasso,xvar="lambda",label=TRUE)

cv.lasso=cv.glmnet(mtx_train_x,mtx_train_y,alpha=1)
plot(cv.lasso)


predictions_lasso_model <- predict(lasso, mtx_test_x)
lasso_model_1_rmse <-RMSE(predictions_lasso_model, mtx_test_y)
lasso_model_1_rmse
#[1] 0.2651776

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="GLM with LASSO Regression model",  
                                     RMSE = lasso_model_1_rmse ))
rmse_results %>% knitr::kable()

lam.best=lasso$lambda[order(lasso_model_1_rmse)[1]]
lam.best
#[1] 0.2923926
coef(lasso,s=lam.best)




#elastic net

elnet <- glmnet(mtx_train_x,mtx_train_y,alpha=0.5)
plot(elnet,xvar="lambda",label=TRUE)

cv.elnet=cv.glmnet(mtx_train_x,mtx_train_y,alpha=0.5)
plot(cv.elnet)

predictions_elnet_model <- predict(elnet, mtx_test_x)
elnet_model_1_rmse <-RMSE(predictions_elnet_model, mtx_test_y)
elnet_model_1_rmse
#[1] 0.2727173

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="GLM with Elastic Net Regression model",  
                                     RMSE = elnet_model_1_rmse ))
rmse_results %>% knitr::kable()

lam.best=lasso$lambda[order(lasso_model_1_rmse)[1]]
lam.best
#[1] 0.2923926
coef(lasso,s=lam.best)



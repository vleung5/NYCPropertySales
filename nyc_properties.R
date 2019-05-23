#setwd("C:\\Users\\Owner\\Capstone")

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


# dl <- tempfile()
# download.file("https://github.com/vleung5/NYCPropertySales/raw/master/nyc-property-sales.zip", dl)
# nyc_prop <- as_data_frame(read.csv(unzip(dl,"nyc-rolling-sales.csv")))
# unlink(dl)

download.file("https://github.com/vleung5/NYCPropertySales/raw/master/nyc-property-sales.zip", destfile="nyc-property-sales.zip")
unzip("nyc-property-sales.zip","nyc-rolling-sales.csv")
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


nyc_property.train %>% 
  dplyr::select(`BOROUGH`, `NEIGHBORHOOD`,`RESIDENTIAL UNITS`,`COMMERCIAL UNITS`,`TOTAL UNITS`, 
         `LAND SQUARE FEET`,`GROSS SQUARE FEET`, `BUILDING AGE`) %>% 
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

#Let's verify if there are neighorhoods in test but not in train.  We will put them in the training set.
nyc_property.train %>% filter(`NEIGHBORHOOD` == "ARROCHAR") #0 row
nyc_property.test %>% filter(`NEIGHBORHOOD` == "ARROCHAR") #1 row

nyc_property.temp <- nyc_property.test

nyc_property.test <- nyc_property.temp %>% 
  semi_join(nyc_property.train, by = "NEIGHBORHOOD") %>%
  semi_join(nyc_property.train, by = "BUILDING CLASS CATEGORY NUMBER") %>%
  semi_join(nyc_property.train, by = "BLOCK") %>%
  semi_join(nyc_property.train, by = "LOT") %>%
  semi_join(nyc_property.train, by = "BUILDING CLASS CATEGORY") %>%
  semi_join(nyc_property.train, by = "ZIP CODE") %>%
  semi_join(nyc_property.train, by = "BUILDING CLASS AT TIME OF SALE")

removed <- anti_join(nyc_property.temp, nyc_property.test)
nyc_property.train <- rbind(nyc_property.train, removed)

rm(nyc_property.temp, removed)



m <- lm(nyc_property.train$'SALE PRICE' ~ . - `SALE PRICE`, data = nyc_property.train)


predict_model_lm_m <- predict(m, nyc_property.test)
# Warning message:
#   In predict.lm(m, nyc_property.test) :
#   prediction from a rank-deficient fit may be misleading
length(m$coefficients) > m$rank
#This means there are collinear covariates. Using summary, we see that there are NAs for Estimate, Std. Error and t value.
#The warning may or may not be important but it's probably benign.  But we can do another LM model without these fields and compare results.
summary(m)

model_1_rmse_m <- RMSE(predict_model_lm_m, nyc_property.test$'SALE PRICE')
model_1_rmse_m

rmse_results <- data_frame(method = "LM", RMSE = model_1_rmse_m)
rmse_results %>% knitr::kable()



#LM removed fields

m <- lm(`SALE PRICE` ~ . - `NEIGHBORHOOD` - `BUILDING CLASS CATEGORY NUMBER` - `BUILDING CLASS CATEGORY` - `TAX CLASS AT PRESENT` -
          `BLOCK` - `BUILDING CLASS AT PRESENT` - `ZIP CODE` - `TAX CLASS AT TIME OF SALE` - `SALE PRICE`
          `BUILDING CLASS AT TIME OF SALE` - `SALE MONTH`
        
        , data = nyc_property.train)

predict_model_lm_m <- predict(m, nyc_property.test)


model_1_rmse_m <- RMSE(predict_model_lm_m, nyc_property.test$'SALE PRICE')
model_1_rmse_m

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="LM with selected columns",  
                                     RMSE = model_1_rmse_m ))
rmse_results %>% knitr::kable()



#GBM Gradient Boosting Machine

#clean up global data/variables before running this as it will run out of memory.
rm(m, model_1_rmse_m, predict_model_lm_m, outliers, nzv, fac, index, nyc_prop)

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

gbm.fit <- gbm(
  formula = `SALE PRICE` ~ . - `BLOCK` - `LOT` - `SALE PRICE`, 
  distribution = "gaussian",
  data = nyc_property.train,
  n.trees = 1000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)

gbm.fit
#The best cross-validation iteration was 7407

# get MSE and compute RMSE
gbm_model_rmse <- sqrt(min(gbm.fit$cv.error))
gbm_model_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="GBM model",  
                                     RMSE = gbm_model_rmse ))
rmse_results %>% knitr::kable()

rm(gbm.fit, gbm_model_rmse)

#Let's try random forest 

#Random forest have issues with the spaces in column names as they are illegal. 
#make illegal variable names to legal names.
nyc_property.train_new <- nyc_property.train
names(nyc_property.train_new) <- make.names(names(nyc_property.train_new))

nyc_property.test_new <- nyc_property.test
names(nyc_property.test_new) <- make.names(names(nyc_property.test_new))

#Random Forest also have issues with factors with more than 53 categories.  Let's identify them and exclude them.
#Error: Can not handle categorical predictors with more than 53 categories.
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


rf_model <- randomForest(`SALE.PRICE` ~ . - `NEIGHBORHOOD` - `BUILDING.CLASS.AT.PRESENT` - `ZIP.CODE` - `COMMERCIAL.UNITS`
                          - `BUILDING.CLASS.AT.TIME.OF.SALE` - `SALE.PRICE`,
                         data=nyc_property.train_new, 
                         importance=TRUE, na.action=na.omit)

predictions_rf_model1 <- predict(rf_model, nyc_property.test_new)

rf_model_1_rmse <-RMSE(predictions_rf_model1, nyc_property.test_new$'SALE.PRICE')
rf_model_1_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Random Forest model",
                                     RMSE = rf_model_1_rmse ))
rmse_results %>% knitr::kable()

rm(rf_model, predictions_rf_model1, rf_model_1_rmse)
rm(nyc_property.train_new, nyc_property.test_new)

#ridge regression
#ridge regession needs the data in a matrix
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

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="GLM with Ridge Regression model",  
                                     RMSE = ridge_model_1_rmse ))
rmse_results %>% knitr::kable()

rm(ridge, predictions_ridge_model, ridge_model_1_rmse, cv.ridge)

#lasso
lasso <- glmnet(mtx_train_x,mtx_train_y,alpha=1)
plot(lasso,xvar="lambda",label=TRUE)

cv.lasso=cv.glmnet(mtx_train_x,mtx_train_y,alpha=1)
plot(cv.lasso)


predictions_lasso_model <- predict(lasso, mtx_test_x)
lasso_model_1_rmse <-RMSE(predictions_lasso_model, mtx_test_y)
lasso_model_1_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="GLM with LASSO Regression model",  
                                     RMSE = lasso_model_1_rmse ))
rmse_results %>% knitr::kable()

lam.best=lasso$lambda[order(lasso_model_1_rmse)[1]]
lam.best

rm(lasso, predictions_lasso_model, lasso_model_1_rmse, lam.best, cv.lasso)

#elastic net
elnet <- glmnet(mtx_train_x,mtx_train_y,alpha=0.5)
plot(elnet,xvar="lambda",label=TRUE)

cv.elnet=cv.glmnet(mtx_train_x,mtx_train_y,alpha=0.5)
plot(cv.elnet)

predictions_elnet_model <- predict(elnet, mtx_test_x)
elnet_model_1_rmse <-RMSE(predictions_elnet_model, mtx_test_y)
elnet_model_1_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="GLM with Elastic Net Regression model",  
                                     RMSE = elnet_model_1_rmse ))
rmse_results %>% knitr::kable()

lam.best=elnet$lambda[order(elnet_model_1_rmse)[1]]
lam.best

rm(elnet, predictions_elnet_model, elnet_model_1_rmse, lam.best, cv.elnet)
rm(mtx_train_x, mtx_train_y, mtx_test_x, mtx_test_y, x, y)

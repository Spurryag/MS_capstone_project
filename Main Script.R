# Import packages------
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(summarytools)
library(randomForest)
library(caret)
library(DataExplorer)
library(xgboost)
library(Matrix)
library(vcd)
library(corrplot)
library(naniar)
library(ggthemes)
library(GGally)
library(missForest)
library(forestFloor)
#(arsenal)
#library(stargazer)

# Source the helper function file ----------
set.seed(12345)
source("datprep.R")

# Import data files------------
train_val <-
  read_csv("C:/Users/Sanil Purryag/Desktop/train_values.csv")
train_lab <-
  read_csv("C:/Users/Sanil Purryag/Desktop/train_labels.csv")
test_val <-
  read_csv("C:/Users/Sanil Purryag/Desktop/test_values.csv")

## Create complete training dataset
training <- merge(train_val, train_lab, by = "row_id")

# Exploratory Data analysis and Data Wrangling----------

## Generate summary page for numeric data
# view(dfSummary(training))
# view(dfSummary(test_val))

## Summary tables
# stargazer(training, type = "latex", title="Descriptive statistic for Training data", digits=1, out="Descriptive Stats for Training data", flip=FALSE)
# test_val <- as.data.frame(test_val)
# stargazer(test_val, type = "latex", title="Descriptive statistics for Testing data", digits=1, out="Descriptive Stats for Testing data", flip=FALSE)

## Missing value analysis indicates that bank_interest_rate, mm_interest_rate, mfi_interest_rate,
## other_fsp_interest_rate have very high percentages of NAs
# gg_miss_var(training) + labs(y = "Training dataset Missing Value Count")+ theme_economist(base_size = 7) + scale_fill_economist()
# gg_miss_var(test_val) + labs(y = "Testing dataset Missing Value Count")+ theme_economist(base_size = 7) + scale_fill_economist()

#Histogram of poverty prob
#adapted from: https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/histdens.html


# p7 <- ggplot(training, aes(x = poverty_probability)) +
#   geom_histogram(aes(fill = ..count..), binwidth = 0.05) +
#   scale_x_continuous(name = "Poverty Probability")+
#   scale_y_continuous(name = "Count") +
#   ggtitle("Frequency histogram of Poverty Probability") + theme_economist(base_size = 7)
# p7
#
# p8 <-
#   ggplot(training, aes(x=poverty_probability)) +
#   geom_density(alpha=.9, fill="#4169E1")+
#   ggtitle("Density plot of Poverty Probability")+ theme_economist(base_size = 7) + scale_fill_economist()
# p8


# Data Wrangling-------------

dat_class <-
  training[, sapply(training, class) %in% c('character', 'logical')]

dat_num <- training[, sapply(training, class) %in% c('numeric')]

potential_fact <-
  dat_num %>% select(
    education_level,
    share_hh_income_provided,
    num_times_borrowed_last_year,
    borrowing_recency,
    num_shocks_last_year,
    phone_technology,
    num_informal_institutions_last_year,
    phone_ownership
  )

## Keep the remaining numeric variables seperate for the training data
num_dat <-
  dat_num[,-which(
    names(dat_num) %in% c(
      "education_level",
      "share_hh_income_provided",
      "num_times_borrowed_last_year",
      "borrowing_recency",
      "num_shocks_last_year",
      "phone_technology",
      "num_informal_institutions_last_year",
      "phone_ownership"
    )
  )]

#Numerical correlation
#Plot pairs
# ggpairs(num_dat)
# #Plot matrix
# train_corr <- cor(num_dat)
# train_corr[train_corr >= 0.99] <- NA #drop perfect
# train_corr[train_corr <= -0.99] <- NA #drop perfect
# train_corr[abs(train_corr) < 0.5] <- NA # drop less than abs(0.5)
# train_corr <- na.omit(melt(train_corr)) # melt!
# train_corr[order(-abs(train_corr$value)), ] # sort
#
# # Categorical correlation
# # Initialize empty matrix to store coefficients
# empty_m <- matrix(
#   ncol = length(dat_class),
#   nrow = length(dat_class),
#   dimnames = list(names(dat_class),
#                   names(dat_class))
# )
# cor_matrix <- calculate_cramer(empty_m , dat_class)
# cor_matrix[cor_matrix >=  0.99] <- NA #drop perfect
# cor_matrix[cor_matrix == -0.99] <- NA #drop perfect
# cor_matrix[abs(cor_matrix) < 0.5] <- NA # drop less than abs(0.5)
# cat_corr <- na.omit(melt(cor_matrix)) # melt!
# cat_corr[order(-abs(cat_corr$value)), ] # sort


## Drop the variables with very high percentages of missing values from train and test dataset
training <-
  training[,-which(
    names(training) %in% c(
      "bank_interest_rate",
      "mm_interest_rate",
      "mfi_interest_rate",
      "other_fsp_interest_rate"
    )
  )]

test_val <-
  test_val[,-which(
    names(test_val) %in% c(
      "bank_interest_rate",
      "mm_interest_rate",
      "mfi_interest_rate",
      "other_fsp_interest_rate"
    )
  )]

## Prepare training and testing dataset
train_final <- datprep(training)
test_final <- datprep(test_val)

# ML model prep --------------------

## Create 80% train-dev split
index <-
  createDataPartition(y = train_final$poverty_probability,
                      p = 0.8,
                      list = FALSE)
train_df <- train_final[index, ]
dev_df <- train_final[-index, ]

## Keep the labels in the training set and dev set seperate
train_xlab <-
  train_df[, -which(names(train_df) %in% c("poverty_probability", "row_id"))]
train_ylab <- train_df$poverty_probability
dev_xlab <-
  dev_df[, -which(names(dev_df) %in% c("poverty_probability"))]
dev_ylab <- dev_df$poverty_probability

## Check for any remaining NAs

### Training dataset
#table(is.na(train_xlab)) # 377 missing values in training
#table(is.na(train_ylab)) # No missing in response

### Development dataset
#table(is.na(dev_xlab)) # 164  missing values in dev dataset
#table(is.na(dev_ylab)) # No missing in response

### Check for which variable there are missing values
#view(dfSummary(train_xlab))

#### Missing values appear to be in the education_level and share_hh_income_provided variables
#### Drop for the moment for ease of fitting

train_xlab <-
  train_df[, -which(
    names(train_df) %in% c(
      "education_level",
      "share_hh_income_provided",
      "poverty_probability",
      "row_id"
    )
  )]
dev_xlab <-
  dev_df[, -which(
    names(dev_df) %in% c(
      "education_level",
      "share_hh_income_provided",
      "poverty_probability"
    )
  )]

test_final <-
  test_final[, -which(names(test_final) %in% c("education_level", "share_hh_income_provided"))]

#Train on the whole dataset

train_complete_x <-
  train_final[, -which(
    names(train_final) %in% c(
      "education_level",
      "share_hh_income_provided",
      "poverty_probability",
      "row_id"
    )
  )]

train_complete_y <- train_final$poverty_probability

#table(is.na(test_final))
#view(dfSummary(train_xlab))

# RF Fit-------------------


### Fit the RF model for training dataset ---
modelRF <-
  randomForest(
    x = train_xlab,
    y = train_ylab,
    mtry = 41,
    importance = TRUE,
    keep.inbag=T
  )


### Plot variable importance
varImpPlot(modelRF, type = 2)
importance <- varImp(modelRF)

### Complete RF fit on the complete training dataset ---
modelRF_complete <-
  randomForest(
    x = train_complete_x,
    y = train_complete_y,
    mtry = 41,
    importance = TRUE,
    keep.inbag=T
  )

### Plot variable importance
varImpPlot(modelRF_complete, type = 2)
importance1 <- varImp(modelRF_complete)


#Remove unncessary variables from the data

train_xlab <-
  train_xlab[, -which(names(train_xlab) %in% c("employment_category_last_yearother", "employment_category_last_yearunemployed","religionN", "relationship_to_hh_headFather.Mother"))]
dev_xlab <-
  dev_xlab[, -which(names(dev_xlab) %in% c("employment_category_last_yearother","employment_category_last_yearunemployed","religionN", "relationship_to_hh_headFather.Mother"))]
train_complete_x<-
  train_complete_x[, -which(names(dev_df) %in% c("employment_category_last_yearother","employment_category_last_yearunemployed","religionN", "relationship_to_hh_headFather.Mother"))]


# RF Tuning-------------------

### Tuning RF mdodel
tune <- tuneRF(x = train_xlab,
               y = train_ylab)

### Tuning RF model on whole dataset

tune1 <- tuneRF(x = train_complete_x,
                y = train_complete_y)

### Plot OOB error for training data
plot(
  tune,
  type = "o",
  main = "RF Tuning result for split Training data",
  ylab = "OOB Error percentage",
  xlab = "Number of variables randomly sampled at each split",
  col = "cornflowerblue"
)

### Plot OOB error for whole training data

plot(
  tune1,
  type = "o",
  main = "RF Tuning result for original Training data",
  ylab = "OOB Error percentage",
  xlab = "Number of variables randomly sampled at each split",
  col = "cornflowerblue"
)


#### Current OOB tuning indicates that 41 is the mtry (post removal to accomodate 114 variables)



# Local Predictions-----------

#Train data RF
modelRF_tuned <-
  randomForest(
    x = train_xlab,
    y = train_ylab,
    mtry = 20,
    importance = TRUE,
    keep.inbag=T
  )

### Predict on train set
preds_train <- predict(modelRF_tuned, train_xlab)

### Compute R squared
rss <- sum((preds_train - train_ylab) ^ 2)  ## residual sum of squares
tss <- sum((train_ylab - mean(train_ylab)) ^ 2)  ## total sum of squares
rsq_train <- 1 - rss/tss

### Predict on development set
preds <- predict(modelRF_tuned, dev_xlab)
### Compute R squared
rss1 <- sum((preds - dev_ylab) ^ 2)  ## residual sum of squares
tss1 <- sum((dev_ylab - mean(dev_ylab)) ^ 2)  ## total sum of squares
rsq_dev <- 1 - rss1/tss1

### Plot sample tree
#getTree(modelRF_tuned, 4, labelVar = TRUE)

### Plot forest floor 
### Sourced from: https://stats.stackexchange.com/questions/21152/obtaining-knowledge-from-a-random-forest
#compute forestFloor object, often only 5-10% time of growing forest
ff = forestFloor(
  rf.fit = modelRF_tuned,       # mandatory
  X = train_xlab,              # mandatory
  calc_np = FALSE,    # TRUE or FALSE both works, makes no difference
  binary_reg = FALSE  # takes no effect here when rfo$type="regression"
)
#also a k-nearest neighbor fit is applied to evaluate goodness-of-fit - parameter =3 
Col=fcol(ff,3,orderByImportance=TRUE) #create color gradient see help(fcol)
plot(ff,col=Col,plot_GOF=TRUE) 

# Complete RF

### Complete RF fit on the complete training dataset ---
modelRF_complete_tuned <-
  randomForest(
    x = train_complete_x,
    y = train_complete_y,
    mtry = 20,
    importance = TRUE,
    keep.inbag=T
  )


### Predict on train set
preds_train_full <- predict(modelRF_complete_tuned, train_complete_x)

### Compute RMSE
caret::RMSE(preds_train_full, train_complete_y)

### Plot forest floor 
### Sourced from: https://stats.stackexchange.com/questions/21152/obtaining-knowledge-from-a-random-forest
#compute forestFloor object, often only 5-10% time of growing forest
ff1 = forestFloor(
  rf.fit = modelRF_complete_tuned,       # mandatory
  X = train_xlab,              # mandatory
  calc_np = FALSE,    # TRUE or FALSE both works, makes no difference
  binary_reg = FALSE  # takes no effect here when rfo$type="regression"
)
#also a k-nearest neighbor fit is applied to evaluate goodness-of-fit - parameter =3 
Col=fcol(ff1,3,orderByImportance=TRUE) #create color gradient see help(fcol)
plot(ff1,col=Col,plot_GOF=TRUE) 

# Test Dataset Predictions------------

## Predict RF on actual test data
finalpreds <- predict(modelRF, newdata = test_final)
write.csv(finalpreds, "RF_predictions.csv")

## Predict from full traning
finalpreds2 <- predict(modelRF_complete, newdata = test_final)
write.csv(finalpreds, "Full_RF_predictions.csv")

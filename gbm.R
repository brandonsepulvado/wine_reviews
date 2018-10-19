#####
##### gbm models
#####

### run after analyses_0.R file

### boosted trees; DOESN'T WORK
boost.caret <- train(points ~ .,
                     data = tree_train,
                     distribution = "gaussian",
                     method = "gbm",
                     ntree = 10)
# online forum says that these models cannot 
# have factor variables

# preprocess
library(vtreat)
treat_plan_train <- designTreatmentsN(tree_train, 
                                      colnames(tree_train),
                                      "points")
treat_plan_test <- designTreatmentsN(tree_test,
                                     colnames(tree_test),
                                     "points")
# get new var names
library(magrittr)
newvars_train <- treat_plan_train %>%
  use_series(scoreFrame) %>%
  select(varName, origName, code) %>%
  filter(code %in% c("clean", "lev")) %>%
  use_series(varName)
newvars_test <- treat_plan_test %>%
  use_series(scoreFrame) %>%
  select(varName, origName, code) %>%
  filter(code %in% c("clean", "lev")) %>%
  use_series(varName)

# new treated data frame
train_treat <- prepare(treat_plan_train, tree_train, 
                       varRestriction = newvars_train)
test_treat <- prepare(treat_plan_test, tree_test,
                      varRestriction = newvars_test)



# run boosted model
library(xgboost)
cv <- xgb.cv(data = as.matrix(train_treat), 
             label = tree_train$points,
             nrounds = 100,
             nfold = 5,
             objective = "reg:linear",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0    # silent
)

# Get the evaluation log 
elog <- cv$evaluation_log

# Determine and print how many trees minimize training and test error
elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   
            # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean))   
# find the index of min(test_rmse_mean)

# fit gradient boosting model
model_xgb <- xgboost(data = as.matrix(train_treat), 
                     label = tree_train$points,
                     nrounds = 100,
                     objective = "reg:linear",
                     eta = 0.3,
                     depth = 6,
                     verbose = 0)

# Make predictions 
# dframe_treat needs to be treated test
tree_test$pred <- predict(model_xgb, 
                          newdata =  as.matrix(test_treat))

# Plot predictions (on x axis) vs actual bike rental count
library(ggplot2)
ggplot(tree_test, aes(x = pred, y = points)) + 
  geom_point() + 
  geom_abline() # doesn't seem correct

tree_test %>%
  mutate(residuals = points - pred) %>%
  summarize(rmse = sqrt(mean(residuals^2)))



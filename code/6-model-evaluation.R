# load libraries
library(glmnetUtils)
library(tidyverse)
library(kableExtra)

# load test data
grad_test = read_csv("data/clean/grad_test.csv")

# load ridge fit object
load("results/ridge_fit.Rda")

# load lasso fit object
load("results/lasso_fit.Rda")

#load decision tree
load("results/tree_fit.Rda")


#find prediction values on test set using ridge regression
probabilities_r = predict(ridge_fit,               
                          newdata = grad_test,  
                          s = "lambda.1se",
                          type = "response") %>%
  as.numeric()                                  

#Calculate RMSE for ridge
test_error_r = sqrt(mean((probabilities_r-grad_test$grad_rate_combined)^2))

#make predictions on test using lasso regression
probabilities_l = predict(lasso_fit,                        
                          newdata = grad_test,
                          s = "lambda.1se",                 
                          type = "response") %>%           
  as.numeric()                                        

#evaluate RMSE for lasso
test_error_l = sqrt(mean((probabilities_l-grad_test$grad_rate_combined)^2))

#make predictions on test set using decision tree model
pred = predict(optimal_tree, newdata = grad_test)

#evaluate RMSE for decision tree
test_error_t = sqrt(mean((pred-grad_test$grad_rate_combined)^2))


#Pull errors into a tibble
test_errors = tibble(
  type = c("Ridge", "Lasso", "Tree"),
  test_error = c(test_error_r, test_error_l, test_error_t)
)

#If RMSE of train data < RMSE of test , then we overfit the model and underfit if the viceversa scenario happens

table = test_errors %>%
  kbl(caption = "Error Values", col.names = c("Type", "Test Error")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "HOLD_position")

table


# load libraries
library(tidyverse)
library(glmnetUtils)                   
source("code/functions/plot_glmnet.R")
library(kableExtra)

# read in the training data
grad_train = read_csv("data/clean/grad_train.csv")

###Linear regression
lm_fit = lm(grad_rate_combined ~ . - `district_cd` - `district_name` - `entity_name` -`entity_cd` -`year` -`county_cd` - `grad_rate_5` - `grad_rate_6` - `grad_rate_4`,
            data = grad_train)
summary(lm_fit)

#collect linear coefficients into a tibble
lm_coeffs = tibble(lm_coef = coef(lm_fit)[-1],
                   features = names(coef(lm_fit)[-1]))
lm_coeffs

#Make kable 
linear_regression_table = lm_coeffs %>%
  kbl(caption = "Linear Regression Coefficients", col.names = c("Feature", "Coefficient")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "HOLD_position")

###RIDGE
# run ridge regression with the following features
set.seed(3)
ridge_fit = cv.glmnet(grad_rate_combined ~ . -`county_name` - `district_cd` - `district_name` - `entity_name` -`entity_cd` -`year` -`county_cd` - `grad_rate_5` - `grad_rate_6` - `grad_rate_4`,
                      alpha = 0,
                      nfolds = 10,
                      data = grad_train)

#C-V plot
ridge_cv_plot = plot(ridge_fit)

ggsave(filename = "results/ridge-cv-plot.png", 
       plot = ridge_cv_plot, 
       device = "png", 
       width = 5, 
       height = 3)

#find lambda values
ridge_fit$lambda.1se
ridge_fit$lambda.min

#ridge coeff plot
ridge_coeff_p = plot_glmnet(ridge_fit, grad_train, features_to_plot = 6)

ggsave(filename = "results/ridge-coef-plot.png", 
       plot = ridge_coeff_p, 
       device = "png", 
       width = 5, 
       height = 3)

#collect ridge coefficients into a tibble
ridge_coefs = coef(ridge_fit, s = "lambda.1se")

ridge_coefs_df = as.data.frame(as.table(as.matrix(ridge_coefs)))

ridge_coefs_tibble = tibble(features = ridge_coefs_df$Var1,
                            coefficients = ridge_coefs_df$Freq)

#Make kable for ridge coeffs
ridge_regression_table = ridge_coefs_tibble %>%
  kbl(caption = "Ridge Regression Coefficients", col.names = c("Feature", "Coefficient")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "HOLD_position")


#save the ridge fit object for future
save(ridge_fit, file = "results/ridge_fit.Rda")

###LASSO
#run lasso regression
set.seed(3)
lasso_fit = cv.glmnet(grad_rate_combined ~ . -`county_name` - `district_cd` - `district_name` - `entity_name` -`entity_cd` -`year` -`county_cd` - `grad_rate_5` - `grad_rate_6` - `grad_rate_4`,   
                      alpha = 1,                 
                      nfolds = 10,               
                      data = grad_train)

#Lasso CV plot
plot(lasso_fit)

#find lambda values
lasso_fit$lambda.1se
log(lasso_fit$lambda.min)

#Lasso coeff trace plot
lasso_coeff_p = plot_glmnet(lasso_fit, grad_train, features_to_plot = 6)

ggsave(filename = "results/lasso-coef-plot.png", 
       plot = lasso_coeff_p, 
       device = "png", 
       width = 5, 
       height = 3)

#extract coeffs
coef(lasso_fit, s = "lambda.1se")
lasso_coeffs = extract_std_coefs(lasso_fit, grad_train)

lasso_coeffs = lasso_coeffs %>%
  arrange(desc(abs(coefficient))) %>%
  filter(abs(coefficient) > 0)


#Make kable
lasso_regression_table = lasso_coeffs %>%
  kbl(caption = "Lasso Regression Coefficients", col.names = c("Feature", "Coefficient")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "HOLD_position")


#save the lasso fit object
save(lasso_fit, file = "results/lasso_fit.Rda")




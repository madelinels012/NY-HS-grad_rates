# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots                      # for dealing with dates# for creating maps
library(tidyverse)

# read in the cleaned data
grad_train = read_csv("data/clean/grad_train.csv")
grad_test = read_csv("data/clean/grad_test.csv")

# calculate median graduation rate
grad_train$grad_rate_combined = as.numeric(grad_train$grad_rate_combined)
median_grad_rate = median(grad_train$grad_rate_combined)
mean(grad_train$grad_rate_combined)

#calculate covariance between features
c_1 = cov(grad_train$teach_oc, grad_train$teach_inexp)
c_2 = cov(grad_train$expenditures, grad_train$cccr_level) #as one goes up the other goes down
c_3 = cov(grad_train$cccr_level, grad_train$combined_test) #yes, some
c_4 = cov(grad_train$grad_rate_combined, grad_train$combined_test) #lots
c_5 = cov(grad_train$grad_rate_combined, grad_train$cccr_level) #hardly any

#compile covariance numbers into a tibble
covariance_n = tibble(feature_1 = c("No. of Teachers out of Certification", "Expenditures per student", "CCCR Level", "Graduation Rate", "Graduation Rate"),
                      feature_2 = c("No. of Teachers with less than 4 yrs in role", "CCCR Level", "Test Scores", "Test Scores", "CCCR Level" ),
                      covariance = c(c_1, c_2, c_3, c_4, c_5))

covariance_n %>%
  kbl(caption = "Covariance between a few key features", col.names = c("Feature 1", "Feature 2", "Covariance")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "HOLD_position")

#variation within features
sd(grad_train$combined_test)
min(grad_train$combined_test)
max(grad_train$combined_test)
mean(grad_train$combined_test)


# create histogram of graduation rates
histogram = grad_train %>%
  ggplot(aes(x = grad_rate_combined)) + 
  geom_histogram(color = "gray", fill = "gray", binwidth = 1) +
  geom_vline(xintercept = median_grad_rate,
             linetype = "dashed") +
  labs(x = "Graduation Rate out of 100", 
       y = "Number of High Schools") +
  theme_bw()

# save the histogram
ggsave(filename = "results/response-histogram.png", 
       plot = histogram, 
       device = "png", 
       width = 5, 
       height = 3)


#Find top 10 High schools and bottom 10 high schools; make tables

top_ten_p = grad_train %>%
  select(entity_name, grad_rate_combined) %>%
  arrange(desc(grad_rate_combined)) %>%
  head(10) %>%
  kbl(caption = "Top High Schools in New York by Graduation Rate", col.names = c("School Name", "Graduation Rate")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "HOLD_position")

bottom_ten_p = grad_train %>%
  select(entity_name, grad_rate_combined) %>%
  arrange((grad_rate_combined)) %>%
  head(10) %>%
  kbl(caption = "High Schools with lowest Graduation Rate", col.names = c("School Name", "Graduation Rate")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "HOLD_position")

#average graduation rate of each county & make table
county_performance = grad_train %>%
  select(county_cd, county_name, entity_name, grad_rate_combined) %>%
  group_by(county_cd) %>%
  summarise(mean_gr = mean(grad_rate_combined), county = county_name) %>%
  ungroup() %>%
  arrange(desc(mean_gr))

county_performace_t = county_performance %>%
  select(-county_cd) %>%
  relocate(county) %>%
  head(10) %>%
  kbl(caption = "Counties with Highest Average Graduation Rate", col.names = c("School Name", "Graduation Rate")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "HOLD_position")


ggsave(filename = "results/county-performance.png", 
       plot = county_performace_t, 
       device = "png", 
       width = 5, 
       height = 3)


#Make box plot to study grad rate in terms of Need Index
need_boxplot = grad_train %>%
  ggplot(aes(x = needs_index, y = grad_rate_combined, group = needs_index, color = needs_index)) +
  geom_boxplot() +
  labs(x = "Need Index", y = "Graduation Rate", color = "Need (N/RC) Index") +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6")) +
  theme_bw()

ggsave(filename = "results/need-boxplot.png", 
       plot = need_boxplot, 
       device = "png", 
       width = 5, 
       height = 3)

#box plot for expenditure vs. need
cccr_p = grad_train %>%
  ggplot(aes(x = needs_index, y = expenditures, group = needs_index, color = needs_index)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Need (N/RC) Index", y = "Expenditures per student")

ggsave(filename = "results/cccr-boxplot.png", 
       plot = cccr_p, 
       device = "png", 
       width = 5, 
       height = 3)

average_money = mean(grad_train$expenditures)

#Scatter plots: grad rate vs. expenditure
expend_p = grad_train %>%
  ggplot(aes(x = expenditures, y = grad_rate_combined)) +
  geom_point() +
  theme_bw() +
  geom_vline(xintercept = average_money, linetype = "dashed") +
  labs(x = "Expenditures per Student", y = "Graduation Rate")

ggsave(filename = "results/expend-scatterplot.png", 
       plot = expend_p, 
       device = "png", 
       width = 5, 
       height = 3)

#Scatter plots: grad rate vs. test score
test_p = grad_train %>%
  ggplot(aes(x = combined_test, y = grad_rate_combined)) +
  geom_point() +
  theme_bw() +
  labs(x = "Test Score", y = "Graduation Rate")

ggsave(filename = "results/test-score-scatterplot.png", 
       plot = test_p, 
       device = "png", 
       width = 5, 
       height = 3)



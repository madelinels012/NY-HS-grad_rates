library(tidyverse)
library(readxl)

# read in the cleaned data
grad_data = read_csv("data/clean/data_final.csv")

# split into train and test (set seed here if applicable)
set.seed(27)
train_samples = sample(1:nrow(grad_data), 0.8*nrow(grad_data))
grad_train = grad_data %>% filter(row_number() %in% train_samples)
grad_test = grad_data %>% filter(!(row_number() %in% train_samples))

# save the train and test data
write_csv(grad_train, "data\\clean\\grad_train.csv")
write_csv(grad_test, "data\\clean\\grad_test.csv")

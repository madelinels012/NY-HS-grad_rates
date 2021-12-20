library(rpart)             
library(rpart.plot)
library(tidyverse)
library(kableExtra)

# read in the training data
grad_train = read_csv("data/clean/grad_train.csv")

#run decision tree model
tree_fit = rpart(grad_rate_combined ~ + `num_teach` + `teach_oc` + `teach_inexp` + `needs_index` + `pupil_count` + `expenditures` + `cccr_level` + `ela` + `social_studies` + `science` + `math` + `combined_test`,
                 data = grad_train)

#print the tree
tree = rpart.plot(tree_fit)

#get summary of info
cp_summary = printcp(tree_fit)

cp_summary = as_tibble(cp_summary)

cp_summary %>% 
  kbl(caption = "Complexity Parameter Summary", col.names = c("CP", "Splits", 
                                                              "1-R^2", "CV error", 
                                                              "CV standard error")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "HOLD_position")


#Cross validated plot over complexity parameter
cp_table = printcp(tree_fit) %>% as_tibble()
cp_table %>% 
  ggplot(aes(x = nsplit+1, y = xerror, 
             ymin = xerror - xstd, ymax = xerror + xstd)) + 
  geom_point() + geom_line() +
  geom_errorbar(width = 0.2) +
  xlab("Number of terminal nodes") + ylab("CV error") + 
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") + 
  theme_bw()


#find best cp value
optimal_tree_info = cp_table %>% 
  filter(xerror - xstd < min(xerror)) %>% 
  arrange(nsplit) %>% 
  head(1)
optimal_tree_info

#says best cp value happens when there are 4 splits

#get optimal tree -- aka prune the tree
optimal_tree = prune(tree = tree_fit, cp = optimal_tree_info$CP)

#plot optimal tree
rpart.plot(optimal_tree)

#figure out how to save these file types

#save data for optimal tree fit
save(optimal_tree, file = "results/tree_fit.Rda")




library(readxl)
library(readr)
library(dplyr)
library(stargazer)
library(car)
library(knitr)
library(kableExtra)
library(plm)
library(lmtest)
library(sandwich)
library(ggplot2)

library(tidymodels)
library(tidyverse)

###############################################
## subquestion 3:  predictive model making   ##
###############################################
# Load the dataset of additional websites there are five datasets, de, eu, fr, global,us. Add the rows of all these sets and order in popularity and remove duplicates
countries <- c("de", "eu", "fr", "global", "us")
additional_websites <- data.frame()
for (i in countries) {
    add <- read.csv(paste("C:/Users/megar/OneDrive/Documenten/Business Analytics Management/Scriptie/data/Whotracksme/", i, "/sites.csv", sep = ""))
    additional_websites <- rbind(additional_websites, add)
}
add <- additional_websites %>% arrange(desc(popularity)) %>% distinct(site, .keep_all = TRUE)


trim <- function(x){
  x[(x > mean(x)-1.5*IQR(x)) & (x < mean(x)+1.5*IQR(x))]
}

hist(trim(x))
hist(trim(add$popularity))

# in sample and out sample
in_sample <- add[sample(nrow(add), 500),]
# out sample is the rest
out_sample <- add[!add$site %in% in_sample$site,]
# remove all pay or okay entries from the out_sample
out_sample <- out_sample[!out_sample$site %in% original$site,]


original_q3 <- original
original_q3[original_q3$site == "finanzmarktwelt.de", "Price"] <- 49
original_q3 <- original_q3[!is.na(original_q3$Price),]
additional_websites_data <- in_sample %>% select(!X)

original_q3$pay_or_okay <- 1
additional_websites_data$pay_or_okay <- 0
additional_websites_data$pass <- 0
additional_websites_data$Price <- 0
colnames_q3 <- c(colnames(additional_websites_data), "pass", "Price")
original_q3 <- original_q3 %>% select(colnames_q3)
# combine the datasets
combined_data_q3 <- rbind(original_q3, additional_websites_data) %>%
    distinct(site, .keep_all = TRUE)

combined_data_q3$pay_or_okay <- as.factor(combined_data_q3$pay_or_okay)
combined_data_q3$pass <- as.factor(combined_data_q3$pass)

# summary statistics of the dataset
original_q3 %>% select(Price, category, popularity, country, hosts, trackers, companies, pass) %>%  summary()


# get the standaard deviation of the price, popularity hosts trackers adn companies, and pass
original_q3 %>% select(Price, popularity, hosts, trackers, companies, pass) %>% summarise_all(sd)
##########Part A: Prediction of pay-or-okay wall ###############
# for this part I need to add some extra data from the who tracks me dataset
pay_or_okay <- combined_data_q3 %>% select(-c(pass,Price))
# take a random sample of the additional websites
set.seed(421)
split_q3 <- initial_validation_split(pay_or_okay, prop = c(0.5,.2))

train_q3 <- training(split_q3)
validation_q3 <- validation(split_q3)
test_q3 <- testing(split_q3)

analysis_q3 <- validation_set(split_q3)

boost_model <- 
    boost_tree(trees = tune(), 
      tree_depth = tune(),
      learn_rate = tune(),
      stop_iter = 500) %>%
    set_mode("classification") %>%
    set_engine("xgboost")

boost_recipe <- 
    recipe(pay_or_okay ~ ., data = train_q3) %>%
    step_rm(site, month, country) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())

boost_wf <- 
  workflow() %>%
  add_recipe(boost_recipe) %>%
  add_model(boost_model)

num_cores <- parallel::detectCores()
doParallel::registerDoParallel(cores = num_cores - 1L)

# [a] parameter grid
boost_tune_grid <- crossing(
    trees = 500 * 1:20,
    learn_rate = c(0.1, 0.01, 0.001),
    tree_depth = c(1, 3, 7))
boost_tune_grid
# Choose the evaluation metrics
regression_metrics <- metric_set(rmse, mae, rsq)

# Tune the model
boost_res <- 
  boost_wf %>%
  tune_grid(
    resamples = analysis_q3,
    grid = boost_tune_grid,
    metrics = metric_set(roc_auc, accuracy, specificity, sensitivity, yardstick::precision, yardstick::recall, yardstick::f_meas)
  ) %>%
  suppressWarnings()


# Collect the metrics
boost_tune_metrics <-
  boost_res |>
  collect_metrics()

# Show the metrics of the best model
boost_tune_metrics |>
  filter(.metric == "specificity") |>
  arrange(desc(mean))

# Select the best hyperparameters
best_params <- boost_res %>%
    select_best(metric = "specificity")

# Finalize the workflow with the best hyperparameters
final_boost_workflow <- boost_wf %>%
   finalize_workflow(best_params)

# Train the final model on the full training data and get the metrics
final_boost_fit <- final_boost_workflow %>%
    fit(data = train_q3)

# Evaluate the final model on the test set
final_boost_predictions <- final_boost_fit %>%
    predict(new_data = test_q3, type = "prob") %>%
    bind_cols(test_q3)


# save final model 
# final_boost_fit %>% saveRDS("final_boost_fit.rds")

# make a graph where for each treshold the specificity sensitity and f_meas is given.
table_model_treshold <- data.frame()
# Set the threshold
for (i in seq(0, 1, 0.01)) {
    threshold <- i
    final_boost_predictions <- final_boost_predictions %>%
        mutate(predicted_class = if_else(.pred_1 >= threshold, 1, 0))
    
    if(final_boost_predictions %>% filter(predicted_class == 0) %>% nrow() == 0 | final_boost_predictions %>% filter(predicted_class == 1) %>% nrow() == 0){
        next
    }	
    final_boost_predictions <- final_boost_predictions %>%
        mutate(predicted_class = as.factor(predicted_class))
    # Calculate the confusion matrix
    confusion_matrix_Q3_1_boost <- final_boost_predictions %>%
    conf_mat(truth = pay_or_okay, estimate = predicted_class)

    sum <- confusion_matrix_Q3_1_boost %>% summary()
    # add the metrics to the table
    table_model_treshold <- rbind(table_model_treshold, c(name = "boosting",treshold = i, spec = sum %>% filter(.metric == "spec") %>% select(.estimate), sens = sum %>% filter(.metric == "sens") %>% select(.estimate), f_meas = sum %>% filter(.metric == "f_meas") %>% select(.estimate)))
}

plot(table_model_treshold$treshold, table_model_treshold$spec..estimate)
table_model_treshold
threshold <- 0.5
# how many are above the threshold
table(final_boost_predictions$.pred_1 >= threshold)

final_boost_predictions <- final_boost_predictions %>%
  mutate(predicted_class = if_else(.pred_1 >= threshold, 1, 0))

final_boost_predictions <- final_boost_predictions %>%
  mutate(predicted_class = as.factor(predicted_class))

# Calculate the confusion matrix
confusion_matrix_Q3_1_boost <- final_boost_predictions %>%
  conf_mat(truth = pay_or_okay, estimate = predicted_class)

# metric of the confusion matrix
confusion_matrix_Q3_1_boost %>%
    summary()

# Print the final metrics
print(confusion_matrix_Q3_1_boost)

###################### lasso model #########################
lasso_model <-
    logistic_reg(mode = "classification",penalty = tune(), mixture = 1) |>
    set_engine("glmnet")

lasso_recipe <-
    recipe(pay_or_okay ~ ., data = train_q3) %>%
    step_rm(site, month, country) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())

lasso_wf <-
    workflow() %>%
    add_recipe(lasso_recipe) %>%
    add_model(lasso_model)

lasso_tune_grid <- tibble(penalty = 10^seq(-2, 2, length = 100))

lasso_res <-
    lasso_wf %>%
    tune_grid(
        resamples = analysis_q3,
        grid = lasso_tune_grid,
        metrics = metric_set(f_meas,roc_auc, accuracy, specificity, sensitivity)) %>%
    suppressWarnings()


# [c] collect metrics
lasso_tune_metrics <- 
    lasso_res |> 
    collect_metrics()

# [d] visualize
lasso_tune_metrics |>
    filter(.metric %in% c("sensitivity", "specificity","f_meas")) |>
    ggplot(aes(x = penalty, y = mean,
                ymin = mean - std_err, ymax = mean + std_err, color = .metric)) +
    geom_errorbar() +
    geom_pointrange(alpha = 0.5, size = .125) + 
    scale_x_log10() + 
    geom_line() +
    geom_point() +
    scale_colour_manual(values = c("#D55E00", "#0072B2", "#D8E1FF")) +
    facet_wrap(~.metric, ncol = 1, scales = "free_y") +
    guides(colour = "none") +
    theme_bw()

lasso_res |> 
    show_best("specificity", n = 5) |>
    arrange(desc(mean))

# select the best model
lasso_1se_model <- 
    lasso_res |> 
    select_best("specificity")

lasso_final_wf <- 
    lasso_wf |> 
    finalize_workflow(lasso_1se_model)
lasso_final_wf

# fit 
lasso_final_fit <- 
    lasso_final_wf |> 
    fit(data = train_q3)

lasso_predictions <- 
    lasso_final_fit |> 
    predict(new_data = test_q3, type = "prob") |> 
    bind_cols(test_q3)

# Set the threshold
threshold <- 0.5
# how many are above the threshold
table(lasso_predictions$.pred_1 >= threshold)

lasso_predictions <- lasso_predictions %>% 
    mutate(predicted_class = if_else(.pred_1 >= threshold, 1, 0))

lasso_predictions <- lasso_predictions %>%
    mutate(predicted_class = as.factor(predicted_class))

# Calculate the confusion matrix
confusion_matrix_Q3_1_lasso <- lasso_predictions %>%
    conf_mat(truth = pay_or_okay, estimate = predicted_class)

# metric of the confusion matrix
confusion_matrix_Q3_1_lasso %>%
    summary()

# Print the final metrics
print(confusion_matrix_Q3_1_lasso)


for (i in seq(0, 1, 0.01)) {
    threshold <- i
    lasso_predictions <- lasso_predictions %>%
        mutate(predicted_class = if_else(.pred_1 >= threshold, 1, 0))
    
    if(lasso_predictions %>% filter(predicted_class == 0) %>% nrow() == 0 | lasso_predictions %>% filter(predicted_class == 1) %>% nrow() == 0){
        next
    }	
    lasso_predictions <- lasso_predictions %>%
        mutate(predicted_class = as.factor(predicted_class))
    # Calculate the confusion matrix
    confusion_matrix_Q3_1_boost <- lasso_predictions %>%
    conf_mat(truth = pay_or_okay, estimate = predicted_class)

    sum <- confusion_matrix_Q3_1_boost %>% summary()
    # add the metrics to the table
    table_model_treshold <- rbind(table_model_treshold, c(name = "lasso",treshold = i, spec = sum %>% filter(.metric == "spec") %>% select(.estimate), sens = sum %>% filter(.metric == "sens") %>% select(.estimate), f_meas = sum %>% filter(.metric == "f_meas") %>% select(.estimate)))
}


###################### k-NN model #########################
knn_model <- 
    nearest_neighbor(neighbors = tune()) %>% 
    set_engine("kknn") %>% 
    set_mode("classification")

knn_recipe <-
    recipe(pay_or_okay ~ ., data = train_q3) %>%
    step_rm(site, month, country, category) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())

knn_wf <-
    workflow() %>%
    add_recipe(knn_recipe) %>%
    add_model(knn_model)

knn_tune_grid <- tibble(neighbors = seq(1, 30, by = 1))

knn_res <- 
    knn_wf %>% 
    tune_grid(
        resamples = analysis_q3,
        grid = knn_tune_grid,
        metrics = metric_set(f_meas,roc_auc, accuracy, specificity, sensitivity)
    ) %>% 
    suppressWarnings()

# collect the metrics
knn_tune_metrics <- 
    knn_res |> 
    collect_metrics()

# show the metrics of the best model
knn_tune_metrics |>
    filter(.metric == "specificity") |>
    arrange(desc(mean))

# choose the best model based on accuracy
best_knn_model <- 
    knn_res |> 
    select_best("specificity")

# finalize the workflow
final_knn_workflow <- 
    knn_wf |> 
    finalize_workflow(best_knn_model)

# fit the model
final_knn_fit <- 
    final_knn_workflow |> 
    fit(data = train_q3)

# predict the model
final_knn_predictions <- 
    final_knn_fit |> 
    predict(new_data = test_q3, type = "prob") |> 
    bind_cols(test_q3)

# Set the threshold
threshold <- 0.5

# Convert probabilities to classes based on the threshold
final_knn_predictions <- final_knn_predictions %>% 
    mutate(predicted_class = if_else(.pred_1 >= threshold, 1, 0))

final_knn_predictions <- final_knn_predictions %>%
    mutate(predicted_class = as.factor(predicted_class))

# Calculate the confusion matrix
confusion_matrix_Q3_3_knn <- final_knn_predictions %>%
    conf_mat(truth = pay_or_okay, estimate = predicted_class)

# Summarize the confusion matrix
confusion_matrix_Q3_3_knn %>%
    summary()

print(confusion_matrix_Q3_3_knn)

# save KNN model
final_knn_fit %>% saveRDS("final_knn_fit.rds")

for (i in seq(0, 1, 0.01)) {
    threshold <- i
    final_knn_predictions <- final_knn_predictions %>%
        mutate(predicted_class = if_else(.pred_1 >= threshold, 1, 0))
    
    if(final_knn_predictions %>% filter(predicted_class == 0) %>% nrow() == 0 | final_knn_predictions %>% filter(predicted_class == 1) %>% nrow() == 0){
        next
    }	
    final_knn_predictions <- final_knn_predictions %>%
        mutate(predicted_class = as.factor(predicted_class))
    # Calculate the confusion matrix
    confusion_matrix_Q3_3_knn <- final_knn_predictions %>%
    conf_mat(truth = pay_or_okay, estimate = predicted_class)

    sum <- confusion_matrix_Q3_3_knn %>% summary()
    # add the metrics to the table
    table_model_treshold <- rbind(table_model_treshold, c(name = "k-NN",treshold = i, spec = sum %>% filter(.metric == "spec") %>% select(.estimate), sens = sum %>% filter(.metric == "sens") %>% select(.estimate), f_meas = sum %>% filter(.metric == "f_meas") %>% select(.estimate)))
}

# get a table with all the metrics specificity, sensitivity, precision for each model with the metrics as columns


# plot table_model_treshold where every line is a model and tresold is the x-axis and the y-axis is the metric specificity
# Plotting specificity and sensitivity on the same graph
# Plotting specificity, sensitivity, and F-measure on the same graph
ggplot(table_model_treshold, aes(x = treshold)) +
  geom_line(aes(y = spec..estimate, color = "Specificity")) +
  geom_point(aes(y = spec..estimate, color = "Specificity")) +
  geom_line(aes(y = sens..estimate, color = "Sensitivity")) +
  geom_point(aes(y = sens..estimate, color = "Sensitivity")) +
  geom_line(aes(y = f_meas..estimate, color = "F-measure")) +
  geom_point(aes(y = f_meas..estimate, color = "F-measure")) +
  facet_wrap(~ name, ncol = 1) +  # Facet by model name
  labs(title = "Model Metrics for Different Thresholds",
       x = "Threshold",
       y = "Value",
       color = "Metric") +
  scale_color_manual(values = c("Specificity" = "blue", "Sensitivity" = "red", "F-measure" = "green")) +
  theme_minimal()

#save plot
ggsave("C:/Users/megar/OneDrive/Documenten/Business Analytics Management/Scriptie/Output/Model_metrics_step1.png")

# save results
table_model_treshold %>% write.csv("C:/Users/megar/OneDrive/Documenten/Business Analytics Management/Scriptie/Output/output_metrics_step1.csv")


###############################################
###########Part B: Prediction of pass ###############
###############################################
# for this part no additional data is needed only the original dataset that is already loaded
# make train and test set with CV folds
set.seed(4342)
split_q3_pass <- initial_validation_split(combined_data_q3 %>% filter(pay_or_okay == 1) %>% select(-c(pay_or_okay,Price)), prop = c(0.5,.2))

train_q3_pass <- training(split_q3_pass)
validation_q3_pass <- validation(split_q3_pass)
test_q3_pass <- testing(split_q3_pass)

analysis_q3_pass <- validation_set(split_q3_pass)
#################### Lasso model ####################
lasso_model_pass <- 
    logistic_reg(penalty = tune(), mixture = 1) |>
    set_engine("glmnet")

lasso_recipe_pass <-
    recipe(pass ~ ., data = train_q3_pass) %>%
    step_rm(site, month, country) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())

lasso_wf_pass <-
    workflow() %>%
    add_recipe(lasso_recipe_pass) %>%
    add_model(lasso_model_pass)

lasso_tune_grid_pass <- tibble(penalty = 10^seq(-2, 2, length = 100))

metrics_pass <- metric_set(roc_auc, accuracy, specificity, sensitivity, yardstick::precision, yardstick::recall, yardstick::f_meas)

lasso_res_pass <- 
    lasso_wf_pass %>%
    tune_grid(
        resamples = analysis_q3_pass,
        grid = lasso_tune_grid_pass,
        metrics = metrics_pass
    ) %>%
    suppressWarnings()

# collect the metrics
lasso_tune_metrics_pass <- 
    lasso_res_pass |> 
    collect_metrics()

# show the metrics of the best model
lasso_tune_metrics_pass |>
    filter(.metric == "accuracy") |>
    arrange(desc(mean))

# choose the best model based on specificity
best_lasso_model_pass <- 
    lasso_res_pass |> 
    select_best("accuracy")

# finalize the workflow
final_lasso_workflow_pass <- 
    lasso_wf_pass |> 
    finalize_workflow(best_lasso_model_pass)

# fit the model
final_lasso_fit_pass <- 
    final_lasso_workflow_pass |> 
    fit(data = train_q3_pass)

# predict the model
final_lasso_predictions_pass <- 
    final_lasso_fit_pass |> 
    predict(new_data = test_q3_pass, type = "prob") |> 
    bind_cols(test_q3_pass)

# evaluate the model
# Set the threshold
threshold <- 0.5
# how many are above the threshold
table(final_lasso_predictions_pass$.pred_1 >= threshold)

final_lasso_predictions_pass <- final_lasso_predictions_pass %>% 
    mutate(predicted_class = if_else(.pred_1 >= threshold, 1, 0))

final_lasso_predictions_pass <- final_lasso_predictions_pass %>%
    mutate(predicted_class = as.factor(predicted_class))

# Calculate the confusion matrix
confusion_matrix <- final_lasso_predictions_pass %>%
    conf_mat(truth = pass, estimate = predicted_class)

# metric of the confusion matrix
confusion_matrix %>%
    summary()

# Print the final metrics
print(confusion_matrix)

# save lasso model 
final_lasso_fit_pass %>% saveRDS("final_lasso_fit_pass.rds")

table_model_treshold_step2 <- data.frame()
for (i in seq(0, 1, 0.01)) {
    threshold <- i
    final_lasso_predictions_pass <- final_lasso_predictions_pass %>%
        mutate(predicted_class = if_else(.pred_1 >= threshold, 1, 0))
    
    if(final_lasso_predictions_pass %>% filter(predicted_class == 0) %>% nrow() == 0 | final_lasso_predictions_pass %>% filter(predicted_class == 1) %>% nrow() == 0){
        next
    }	
    final_lasso_predictions_pass <- final_lasso_predictions_pass %>%
        mutate(predicted_class = as.factor(predicted_class))
    # Calculate the confusion matrix
    confusion_matrix_Q3_3_lasso <- final_lasso_predictions_pass %>%
    conf_mat(truth = pass, estimate = predicted_class)

    sum <- confusion_matrix_Q3_3_lasso %>% summary()
    # add the metrics to the table
    table_model_treshold_step2 <- rbind(table_model_treshold_step2, c(name = "Lasso",treshold = i, spec = sum %>% filter(.metric == "spec") %>% select(.estimate), sens = sum %>% filter(.metric == "sens") %>% select(.estimate), f_meas = sum %>% filter(.metric == "f_meas") %>% select(.estimate)))
}

#################### k-NN model ###################
# Define the k-NN model
knn_model_pass <- 
    nearest_neighbor(neighbors = tune()) %>%
    set_engine("kknn") %>%
    set_mode("classification")

# Create the recipe for pre-processing
knn_recipe_pass <- 
    recipe(pass ~ ., data = train_q3_pass) %>%
    step_rm(site, month, country) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())

# Define the workflow
knn_wf_pass <- 
    workflow() %>%
    add_recipe(knn_recipe_pass) %>%
    add_model(knn_model_pass)

# Define the tuning grid
knn_tune_grid_pass <- tibble(neighbors = seq(1, 30, by = 1))

# Define the metrics
metrics_pass <- metric_set(roc_auc, accuracy, specificity, sensitivity, yardstick::precision, yardstick::recall, yardstick::f_meas)

# Tune the k-NN model
knn_res_pass <- 
    knn_wf_pass %>%
    tune_grid(
        resamples = analysis_q3_pass,
        grid = knn_tune_grid_pass,
        metrics = metrics_pass
    ) %>%
    suppressWarnings()

# Collect the metrics
knn_tune_metrics_pass <- 
    knn_res_pass |> 
    collect_metrics()

# Show the metrics of the best model based on specificity
knn_tune_metrics_pass |>
    filter(.metric == "accuracy") |>
    arrange(desc(mean))

# Choose the best model based on specificity
best_knn_model_pass <- 
    knn_res_pass |> 
    select_best("accuracy")

# Finalize the workflow with the best model
final_knn_workflow_pass <- 
    knn_wf_pass |> 
    finalize_workflow(best_knn_model_pass)

# Fit the final model
final_knn_fit_pass <- 
    final_knn_workflow_pass |> 
    fit(data = train_q3_pass)

# Predict with the final model
final_knn_predictions_pass <- 
    final_knn_fit_pass |> 
    predict(new_data = test_q3_pass, type = "prob") |> 
    bind_cols(test_q3_pass)

# Set the threshold
threshold <- 0.5

# Convert probabilities to classes based on the threshold
final_knn_predictions_pass <- final_knn_predictions_pass %>% 
    mutate(predicted_class = if_else(.pred_1 >= threshold, 1, 0))

final_knn_predictions_pass <- final_knn_predictions_pass %>%
    mutate(predicted_class = as.factor(predicted_class))

# Calculate the confusion matrix
confusion_matrix <- final_knn_predictions_pass %>%
    conf_mat(truth = pass, estimate = predicted_class)

# Summarize the confusion matrix
confusion_matrix_summary <- confusion_matrix %>%
    summary()

# Print the final metrics
print(confusion_matrix)

for (i in seq(0, 1, 0.01)) {
    threshold <- i
    final_knn_predictions_pass <- final_knn_predictions_pass %>%
        mutate(predicted_class = if_else(.pred_1 >= threshold, 1, 0))
    
    if(final_knn_predictions_pass %>% filter(predicted_class == 0) %>% nrow() == 0 | final_knn_predictions_pass %>% filter(predicted_class == 1) %>% nrow() == 0){
        next
    }	
    final_knn_predictions_pass <- final_knn_predictions_pass %>%
        mutate(predicted_class = as.factor(predicted_class))
    # Calculate the confusion matrix
    confusion_matrix_Q3_3_knn <- final_knn_predictions_pass %>%
    conf_mat(truth = pass, estimate = predicted_class)

    sum <- confusion_matrix_Q3_3_knn %>% summary()
    # add the metrics to the table
    table_model_treshold_step2 <- rbind(table_model_treshold_step2, c(name = "k-NN",treshold = i, spec = sum %>% filter(.metric == "spec") %>% select(.estimate), sens = sum %>% filter(.metric == "sens") %>% select(.estimate), f_meas = sum %>% filter(.metric == "f_meas") %>% select(.estimate)))
}

print(table_model_treshold_step2 %>% filter(treshold == 0.5)

#################### Boosting model ###################
# Define the XGBoost model
xgboost_model_pass <- 
    boost_tree(trees = tune(), 
               tree_depth = tune(), 
               learn_rate = tune()) %>%
    set_engine("xgboost") %>%
    set_mode("classification")

# Create the recipe for pre-processing
xgboost_recipe_pass <- 
    recipe(pass ~ ., data = train_q3_pass) %>%
    step_rm(site, month, country) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())

# Define the workflow
xgboost_wf_pass <- 
    workflow() %>%
    add_recipe(xgboost_recipe_pass) %>%
    add_model(xgboost_model_pass)

# Define a smaller and faster tuning grid for XGBoost
# Define a smaller and faster tuning grid for XGBoost
xgboost_tune_grid_pass <- crossing(
    trees = 500 * 1:20,
    learn_rate = c(0.1, 0.01, 0.001),
    tree_depth = c(1, 3, 7))



# Define the metrics
metrics_pass <- metric_set(roc_auc, accuracy, specificity, sensitivity, yardstick::precision, yardstick::recall, yardstick::f_meas)

# Tune the XGBoost model
xgboost_res_pass <- 
    xgboost_wf_pass %>%
    tune_grid(
        resamples = analysis_q3_pass,
        grid = xgboost_tune_grid_pass,
        metrics = metrics_pass
    ) %>%
    suppressWarnings()

# Collect the metrics
xgboost_tune_metrics_pass <- 
    xgboost_res_pass |> 
    collect_metrics()

# Show the metrics of the best model based on specificity
xgboost_tune_metrics_pass |>
    filter(.metric == "accuracy") |>
    arrange(desc(mean))

# Choose the best model based on specificity
best_xgboost_model_pass <- 
    xgboost_res_pass |> 
    select_best("accuracy")

# Finalize the workflow with the best model
final_xgboost_workflow_pass <- 
    xgboost_wf_pass |> 
    finalize_workflow(best_xgboost_model_pass)

# Fit the final model
final_xgboost_fit_pass <- 
    final_xgboost_workflow_pass |> 
    fit(data = train_q3_pass)

# Predict with the final model
final_xgboost_predictions_pass <- 
    final_xgboost_fit_pass |> 
    predict(new_data = test_q3_pass, type = "prob") |> 
    bind_cols(test_q3_pass)

# Set the threshold
threshold <- 0.5

# Convert probabilities to classes based on the threshold
final_xgboost_predictions_pass <- final_xgboost_predictions_pass %>% 
    mutate(predicted_class = if_else(.pred_1 >= threshold, 1, 0))

final_xgboost_predictions_pass <- final_xgboost_predictions_pass %>%
    mutate(predicted_class = as.factor(predicted_class))

# Calculate the confusion matrix
confusion_matrix <- final_xgboost_predictions_pass %>%
    conf_mat(truth = pass, estimate = predicted_class)

# Summarize the confusion matrix
confusion_matrix %>%
    summary()

# Print the final metrics
print(confusion_matrix)

for (i in seq(0, 1, 0.01)) {
    threshold <- i
    final_xgboost_predictions_pass <- final_xgboost_predictions_pass %>%
        mutate(predicted_class = if_else(.pred_1 >= threshold, 1, 0))
    
    if(final_xgboost_predictions_pass %>% filter(predicted_class == 0) %>% nrow() == 0 | final_xgboost_predictions_pass %>% filter(predicted_class == 1) %>% nrow() == 0){
        next
    }	
    final_xgboost_predictions_pass <- final_xgboost_predictions_pass %>%
        mutate(predicted_class = as.factor(predicted_class))
    # Calculate the confusion matrix
    confusion_matrix_Q3_3_xgboost <- final_xgboost_predictions_pass %>%
    conf_mat(truth = pass, estimate = predicted_class)

    sum <- confusion_matrix_Q3_3_xgboost %>% summary()
    # add the metrics to the table
    table_model_treshold_step2 <- rbind(table_model_treshold_step2, c(name = "XGBoost",treshold = i, spec = sum %>% filter(.metric == "spec") %>% select(.estimate), sens = sum %>% filter(.metric == "sens") %>% select(.estimate), f_meas = sum %>% filter(.metric == "f_meas") %>% select(.estimate)))
}

ggplot(table_model_treshold_step2, aes(x = treshold)) +
  geom_line(aes(y = spec..estimate, color = "Specificity")) +
  geom_point(aes(y = spec..estimate, color = "Specificity")) +
  geom_line(aes(y = sens..estimate, color = "Sensitivity")) +
  geom_point(aes(y = sens..estimate, color = "Sensitivity")) +
  geom_line(aes(y = f_meas..estimate, color = "F-measure")) +
  geom_point(aes(y = f_meas..estimate, color = "F-measure")) +
  facet_wrap(~ name, ncol = 1) +  # Facet by model name
  labs(title = "Model Metrics for Different Thresholds",
       x = "Threshold",
       y = "Value",
       color = "Metric") +
  scale_color_manual(values = c("Specificity" = "blue", "Sensitivity" = "red", "F-measure" = "green")) +
  theme_minimal()

#save plot
ggsave("C:/Users/megar/OneDrive/Documenten/Business Analytics Management/Scriptie/Output/Model_metrics_step2.png")

# save results
table_model_treshold_step2 %>% write.csv("C:/Users/megar/OneDrive/Documenten/Business Analytics Management/Scriptie/Output/output_metrics_step2.csv")


###############################################
###########Part C: Prediction of price ###############
###############################################
# make train and test set with CV folds

set.seed(123)
before_split <- combined_data_q3 %>% filter(pass == 0 & pay_or_okay == 1) %>% select(-c(pay_or_okay,pass))
before_split[before_split$site == "finanzmarktwelt.de", "Price"] <- 49
before_split <- before_split[!is.na(before_split$Price),]
before_split$log_Price <- log(before_split$Price)
before_split <- before_split %>% select(-Price)
split <- initial_validation_split(before_split, prop = c(0.5,.2))
# add one price to the dataset df[df$site == "finanzmarktwelt.de", "Price"] <- 49



train <- training(split)
validation <- validation(split)
test <- testing(split)

analysis <- validation_set(split)

view(train)
# I already have a model, so I can use that one model_original6
# I need to change the model to a regression model
# I need to change the target variable to log(Price)
# I need to change the metrics to rmse, mae, rsq_trad

# [1] Define the model
model_price <- lm(formula6, data = train)

# use the test data to get the RMSE
res_model <- test %>%
    mutate(.pred = predict(model_price, newdata = test)) %>%
    metrics(truth = log_Price, estimate = .pred)
print(res_model)

formula_lasso <- "log_Price ~ country + category + popularity + bad_qs + tracked + https + requests_tracking + content_length + requests_failed + has_blocking + script + iframe + beacon + image + stylesheet + font + xhr + plugin + media + referer_leaked + referer_leaked_header + referer_leaked_url + cookie_samesite_none + t_active + hosts + companies + popularity*hosts"


lasso_model <-
    linear_reg(penalty = tune()) |>
    set_engine("glmnet")

recipe <- 
    recipe(log_modelPrice ~ ., data = train) %>%
    step_rm(site, country, category, month) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())
    


# [3] Define workflow
lasso_workflow <- 
    workflow() |> 
    add_model(lasso_model) |> 
    add_recipe(recipe)

# [4] set metrics
# already done above

# [5] Define & tune parameters

# [a] parameter grid    
    lasso_tune_grid <- grid_regular(penalty(range = c(-4.5, 0), 
                                            trans = log10_trans()), levels = 10)
    lasso_tune_grid

# [b] tune the model
lasso_res <- 
    lasso_workflow |> 
    tune_grid(
        resamples = analysis,
        grid = lasso_tune_grid,
        metrics = regression_metrics
    ) |> 
    suppressWarnings()

# [c] collect metrics
lasso_tune_metrics <- 
    lasso_res |> 
    collect_metrics()

# [d] visualize

# [e] select the best model
best_lasso_model <- 
    lasso_res |> 
    select_best("rmse")

# [f] finalize the workflow
final_lasso_workflow <- 
    lasso_workflow |> 
    finalize_workflow(best_lasso_model)

# [g] fit the model
final_lasso_fit <- 
    final_lasso_workflow |> 
    fit(data = train)

# [h] predict the model
final_lasso_predictions <- 
    final_lasso_fit |> 
    predict(new_data = test) |> 
    bind_cols(test)

# [i] evaluate the model
final_lasso_predictions |> 
    metrics(truth = Price, estimate = `.pred`) 



# now do a boosting model
boost_model <- 
    boost_tree(trees = tune(), 
      tree_depth = tune(),
      learn_rate = tune(),
      stop_iter = 500) %>%
    set_mode("regression") %>%
    set_engine("xgboost")

# do one-hot encoding
boost_recipe <-
    recipe(log_Price ~ ., data = train) %>%
    step_rm(site, country, category, month) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())

boost_wf <-
    workflow() %>%
    add_recipe(boost_recipe) %>%
    add_model(boost_model)

boost_tune_grid_price <- tibble(trees = c(500,2500,5000),
                          learn_rate = c(0.1, 0.01, 0.001),
                          tree_depth = c(1, 3, 7))

# tune the model
boost_res <- 
    boost_wf %>%
    tune_grid(
        resamples = analysis,
        grid = boost_tune_grid_price,
        metrics = regression_metrics
    ) %>%
    suppressWarnings()

# collect the metrics
boost_tune_metrics <- 
    boost_res |> 
    collect_metrics()

# show the metrics of the best model
boost_tune_metrics |>
    filter(.metric == "rmse") |>
    arrange(mean) |>
    head(20)

# select the best model
best_boost_model <- 
    boost_res |> 
    select_best("rmse")

# finalize the workflow
final_boost_workflow <- 
    boost_wf |> 
    finalize_workflow(best_boost_model)

# fit the model
final_boost_fit <- 
    final_boost_workflow |> 
    fit(data = train)

# predict the model
final_boost_predictions <- 
    final_boost_fit |> 
    predict(new_data = test) |> 
    bind_cols(test)

# evaluate the model
final_boost_predictions |> 
    metrics(truth = log_Price, estimate = .pred)


# save model
final_boost_fit %>% saveRDS("final_boost_fit_price.rds")

# add the results into one table
table_model_step3 <- data.frame()


# Compute metrics for the model with formula6
test_predictions_formula6 <- 
  test %>%
  mutate(.pred = predict(model_price, newdata = test), log_Price = log(Price))

metrics_formula6 <- 
  test_predictions_formula6 %>%
  metrics(truth = log_Price, estimate = .pred)

# Combine all metrics into one table
all_metrics <- bind_rows(
  final_lasso_predictions %>%
    metrics(truth = log_Price, estimate = .pred...37) %>%
    mutate(model = "Lasso"),
  final_boost_predictions %>%
    metrics(truth = log_Price, estimate = .pred...1) %>%
    mutate(model = "Boosting"),
  metrics_formula6 %>%
    metrics(truth = log_Price, estimate = .pred) %>%
    mutate(model = "Formula6")
)

# Display the table
print(all_metrics)



# knn_reg_model <- 
#     nearest_neighbor(neighbors = tune()) %>%
#     set_mode("regression") %>%
#     set_engine("kknn")

# knn_reg_recipe <- 
#     recipe(as.formula(formula_train), data = train) %>%
#     step_rm(country, category) %>%
#     step_log(Price) %>%
#     step_normalize(all_predictors())

# knn_reg_wf <- 
#     workflow() %>%
#     add_recipe(knn_reg_recipe) %>%
#     add_model(knn_reg_model)

# knn_reg_tune_grid <- tibble(neighbors = 1:20 * 2 - 1)
# knn_reg_tune_grid

# # choose a metric
# regression_metrics <- metric_set(rmse, mae, rsq_trad)

# knn_reg_res <- 
#         knn_reg_wf %>%
#         tune_grid(
#                 resamples = analysis,
#                 grid = knn_reg_tune_grid,
#                 metrics = regression_metrics
#         ) %>%
#         suppressWarnings()

# knn_reg_tune_metrics <-
#     knn_reg_res |>
#     collect_metrics()
# knn_reg_tune_metrics

# # give me the metrics of the best model
# knn_reg_res |>
#     collect_metrics()|>
#     arrange(mean) |>
#     head(5)

# # select the best model
# best_knn_reg_model <- 
#     knn_reg_res |>
#     select_best("rmse")
# best_knn_reg_model

# # I want to have the metrics of the best model
# best_knn_reg_res <- 
#     knn_reg_res |>
#     collect_metrics() |>
#     filter(.metric == "", .estimator == "mean") |>
#     filter(neighbors == best_knn_reg_model$neighbors)
# best_knn_reg_res

# knn_reg_workflow_final <- 
#     knn_reg_wf |>
#     finalize_workflow(best_knn_reg_model)





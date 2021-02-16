library(tidyverse)

# import data ---- 

# data does not contain column names, so we need to identify them

column_names <- c('age','work_class','final_weight','education','education_number','marital_status','occupation','relationship','race','sex','capital_gain','capital_loss','hours_per_week','native_country','salary_class')

na_vals = c('', ' ', 'NA','N/A','?', 'unknown', 'Unknown')

raw_train <- read_csv(file = 'data/adult.data', col_names = column_names, na = na_vals)
raw_test <- read_csv(file = 'data/adult.test', col_names = column_names, na = na_vals, skip = 1) # we skip the first row since it contains non-relevant info

# Data Preview ----

glimpse(raw_train)

raw_train %>%
  map_df(function(x) sum(is.na(x))) %>%
  pivot_longer(cols = everything(), names_to = 'variable', values_to = 'number_missing') %>%
  arrange(desc(number_missing)) %>%
  filter(number_missing > 0) %>%
  mutate(
    pct_missing = number_missing / nrow(raw_train)
  )

# Data Cleaning ----

## drop rows that have NULL values in any column

cln_train <- raw_train %>%
  drop_na()

## remove column native-country
cln_train <- cln_train %>%
  select(-native_country)

glimpse(cln_train)

# cln_train %>%
#   select(where(is.numeric)) %>%
#   summarize_all(list(stat_meanval = mean, stat_medianval = median, stat_minval = min, stat_maxval = max, stat_var = var, stat_sd = sd)) %>%
#   pivot_longer(
#     cols = everything(),
#     names_sep = "_stat_",
#     names_to = c('variable','.value')
#   )

raw_train %>%
  map_df(class) %>% 
  pivot_longer(cols = everything())

as.data.frame(glimpse(raw_train))

# numerical summary
raw_train %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarize(
    min = min(value),
    avg = mean(value),
    median = median(value),
    max = max(value),
    sd = sd(value),
    var = var(value),
    p01 = quantile(value, p = 0.01),
    p05 = quantile(value, p = 0.05),
    p10 = quantile(value, p = 0.10),
    p25 = quantile(value, p = 0.25),
    p75 = quantile(value, p = 0.75),
    p90 = quantile(value, p = 0.90),
    p95 = quantile(value, p = 0.95),
    p99 = quantile(value, p = 0.99),
    skew = moments::skewness(value),
    kurtosis = moments::kurtosis(value)
  )

# character summary
cln_train %>%
  select(where(is.character)) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarize(
    num_unique = length(unique(value))
  )

raw_train %>%
  drop_na() %>%
  select(where(is.character), -native_country) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarize(
    num_unique = length(unique(value))
  )

cln_train %>%
  group_by(occupation) %>%
  count()

cln_train %>%
  group_by(education) %>%
  count()

cln_train %>%
  select(education, education_number) %>%
  distinct() %>%
  arrange(education_number)

cln_train %>%
  group_by(education, education_number) %>%
  count() %>%
  arrange(education_number)

cln_train %>%
  group_by(sex) %>%
  count()

cln_train %>%
  group_by(work_class) %>%
  count()


cln_train %>%
  select(education, education_number) %>%
  distinct() %>%
  arrange(education_number)

cln_train %>%
  group_by(education, education_number) %>%
  count() %>%
  arrange(education_number) %>%
  ungroup() %>%
  mutate(pct = n / nrow(cln_train), cum_pct = cumsum(pct))

cln_train %>%
  select(marital_status, relationship) %>%
  distinct() %>%
  arrange(marital_status) %>%
  View()

cln_train %>%
  group_by(marital_status) %>%
  count()

cln_train %>%
  group_by(relationship) %>%
  count() %>%
  ungroup() %>%
  mutate(pct = n / nrow(cln_train))

cln_train %>%
  group_by(sex) %>%
  count() %>%
  ungroup() %>%
  mutate(pct = n / nrow(cln_train))

cln_train %>%
  group_by(race) %>%
  count() %>%
  ungroup() %>%
  mutate(pct = n / nrow(cln_train))

cln_train %>%
  group_by(work_class) %>%
  count() %>%
  ungroup() %>%
  mutate(pct = n / nrow(cln_train))

# correlation

cln_train %>%
  select(where(is.numeric)) %>%
  select(-education_number) %>%
  corrr::correlate()

raw_train %>%
  select(where(is.numeric), -education_number) %>%
  drop_na() %>%
  corrr::correlate()

# remove capital gain/loss and replace with categorical
# create new feature: income_class (1 if income is >50k or 0 if <= 50k)
# remove variable final_weight as it is a complex & unexplainable idea. Also it has MASSIVE variance. Almost no correlation. According to meta data (adult.names), this value is only applicable in its explanation to data points from within the same state and would be difficult to compare to across the states


cln_train <- cln_train %>%
  mutate(
    capital = case_when(
      capital_gain > 0 ~ 'gain',
      capital_loss > 0 ~ 'loss',
      capital_gain == 0 & capital_loss == 0 ~ 'neither',
      TRUE ~ 'error'
    ),
    income_class = ifelse(salary_class == '<=50K',0,1)
  ) %>%
   select(-capital_gain, -capital_loss, -salary_class, -final_weight, -education)


### EDA ----

cln_train %>%
  ggplot(aes(x = sex, y = hours_per_week, col = race)) +
  geom_boxplot() +
  facet_wrap(.~income_class)

cln_train %>%
  ggplot(aes(x = hours_per_week, fill = sex)) +
  geom_histogram() +
  facet_wrap(income_class ~.)

cln_train %>%
  group_by(race, sex) %>%
  summarize(
    min_hpw = min(hours_per_week),
    max_hpw = max(hours_per_week),
    median_hpw = median(hours_per_week),
    average_hpw = mean(hours_per_week)
  )

raw_train %>%
  ggplot(aes(x = age)) +
  geom_histogram(bins = 25, color = 'white', fill = 'steelblue') +
  labs(x = 'Age', y = 'Frequency', title = 'Age Distribution')

raw_train %>%
  select(age) %>%
  ggplot() +
  geom_histogram(aes(x = age), bins = 25, color = 'white', fill = 'steelblue', alpha = 0.8)

raw_train %>%
  select(age) %>%
  mutate(log_age = log(age)) %>%
  ggplot() +
  geom_histogram(aes(x = log_age), bins = 25, color = 'white', fill = 'steelblue', alpha = 0.8)

cln_train %>%
  filter(age > 60) %>%
  ggplot(aes(x = age)) +
  geom_histogram(bins = 25, color = 'white', fill = 'steelblue') +
  labs(x = 'Age', y = 'Frequency', title = 'Age Distribution')

cln_train %>%
  ggplot(aes(y = age, color = sex)) +
  geom_boxplot()

cln_train %>%
  ggplot(aes(y = hours_per_week, color = sex)) +
  geom_boxplot()

cln_train %>%
  ggplot(aes(y = hours_per_week, color = sex)) +
  geom_boxplot() +
  facet_wrap(income_class ~ .)

cln_train %>%
  ggplot(aes(y = hours_per_week, color = race)) +
  geom_boxplot()


cln_train %>%
  mutate(log_age = log(age)) %>%
  summarize(
    skew_age = moments::skewness(age),
    kurt_age = moments::kurtosis(age),
    skew_logage = moments::skewness(log_age),
    kurt_logage = moments::kurtosis(log_age)
  )

raw_train %>%
  drop_na() %>%
  ggplot(aes(x = salary_class, y = age)) +
  geom_boxplot()

raw_train %>%
  group_by(salary_class, race, sex) %>%
  summarize(
    median_age = median(age),
    median_hpw = median(hours_per_week),
    .groups = 'drop'
  )

raw_train %>%
  drop_na() %>%
  ggplot(aes(x = race, y = hours_per_week)) +
  geom_boxplot(aes(fill = sex), color = 'steelblue') +
  labs(x = "Race", y = "Hours Worked Per Week", title = "Comparison of Hours Worked\nby Race & Gender", fill = "Gender") +
  facet_grid(.~salary_class) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  theme(
    plot.title = element_text(color = 'grey100'),
    plot.background = element_rect(fill = 'grey10'),
    axis.text.x = element_text(color = 'grey70'),
    axis.text.y = element_text(color = 'grey70'),
    axis.title = element_text(color = 'grey100'),
    strip.background = element_rect(fill = 'grey30'),
    strip.text = element_text(color = 'grey80'),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = 'grey30', size = 0.2),
    panel.grid.minor = element_line(color = 'grey30', size = 0.2),
    legend.background = element_rect(fill = 'grey20'),
    legend.key = element_blank(),
    legend.title = element_text(color = 'grey80'),
    legend.text = element_text(color = 'grey90')
  )

raw_train %>%
  drop_na() %>%
  group_by(salary_class) %>%
  summarize(median_hpw = median(hours_per_week))

raw_train %>%
  drop_na() %>%
  ggplot(aes(x = hours_per_week)) +
  geom_histogram()


library(solitude)

tmp <- cln_train %>% select(age)

iforest <- solitude::isolationForest$new()

iforest$fit(tmp)

tmp$pred <- iforest$predict(tmp)

quantile(tmp$pred$anomaly_score, probs = seq(0.5, 1, length.out = 11))

tmp$outlier <- ifelse(tmp$pred$anomaly_score >=0.67, "outlier", "normal")

tmp %>%
  filter(outlier == 'outlier') %>%
  ggplot(aes(x = age)) +
  geom_histogram()

tmp %>%
  filter(outlier == 'outlier') %>%
  arrange(age) %>%
  View

outlier_age <- tmp %>%
  filter(outlier == 'outlier') %>%
  select(age) %>%
  distinct() %>%
  pull()

cln_train %>%
  filter(!age %in% c(outlier_age)) %>%
  summarize(
    min_age = min(age),
    max_age = max(age),
    median_age = median(age),
    avg_age = mean(age),
    sd_age = sd(age),
    var_age = var(age)
  )





tmp <- cln_train %>% select(hours_per_week)

iforest <- solitude::isolationForest$new()

iforest$fit(tmp)

tmp$pred <- iforest$predict(tmp)

quantile(tmp$pred$anomaly_score, probs = seq(0.5, 1, length.out = 11))

tmp$outlier <- ifelse(tmp$pred$anomaly_score >=0.681, "outlier", "normal")

outlier_hours <- tmp %>%
  filter(outlier == 'outlier') %>%
  select(hours_per_week) %>%
  distinct() %>%
  pull()

cln_train %>%
  filter(!hours_per_week %in% c(outlier_hours)) %>%
  summarize(
    min_hpw = min(hours_per_week),
    max_hpw = max(hours_per_week),
    median_hpw = median(hours_per_week),
    avg_hpw = mean(hours_per_week),
    sd_hpw = sd(hours_per_week),
    var_hpw = var(hours_per_week),
    skew_hpw = moments::skewness(hours_per_week),
    kurt_hpw = moments::kurtosis(hours_per_week)
  )


# model development ----

raw_train <- raw_train %>%
  mutate(income_class = ifelse(salary_class == ">50K", 1, 0)) %>%
  drop_na()

df_test <- raw_test %>%
  mutate(income_class = ifelse(salary_class == ">50K.", 1, 0)) %>%
  drop_na()

## data split ----
library(tidymodels)

set.seed(3372)
df_split <- initial_split(data = raw_train, prop = 0.70, strata = salary_class)
df_train <- training(df_split)
df_valid <- testing(df_split)

## recipes ----

### logistic regression recipe ----

mdl_recipe <- recipes::recipe(income_class ~ . , df_train) %>%
  # step_mutate(income_class = salary_class) %>%
  step_bin2factor(income_class, levels = c('1','0')) %>%
  # remove the variable `native_country`, capital_gain, capital_loss
  step_rm(native_country, capital_gain, capital_loss, salary_class, education_number, final_weight) %>%
  # create outlier_age (true if age is < 20 or >62)
  step_mutate(outlier_age = ifelse(age > 62 | age < 20, 1, 0), role = 'predictor') %>%
  step_mutate(age = case_when(age > 62 ~ 62, age < 20 ~ 20, TRUE ~ age), role = 'predictor') %>%
  # create outlier_hoursworked (true if hpw < 18 or > 60)
  step_mutate(outlier_hoursworked = ifelse(hours_per_week < 18 | hours_per_week > 60, 1, 0), role = 'predictor') %>%
  step_mutate(hours_per_week = case_when(hours_per_week < 18 ~ 18, hours_per_week > 60 ~ 60, TRUE ~ hours_per_week), role = 'predictor') %>%
  # dummy encode work_class, martial_status, occupation, relationship, race
  step_string2factor(work_class, marital_status, occupation, relationship, race, education, sex) %>%
  step_other(marital_status, occupation, work_class, education) %>%
  step_dummy(all_nominal(), -income_class) %>%
  # normalize (center & scale) age and hours_per_week
  step_normalize(age, hours_per_week) %>%
  # SMOTE to balance data
  themis::step_smote(income_class, skip = TRUE) %>%
  # prepare recipe
  # prep(training = df_train, strings_as_factors = FALSE) %>%
  identity()

mdl_recipe
summary(mdl_recipe)

# new_train <- juice(lr_recipe)
# new_test <- bake(lr_recipe, new_data = df_valid)
# glimpse(new_train)
# glimpse(df_train)



# new_train %>%
#   group_by(income_class) %>%
#   count()

# df_train %>%
#   drop_na() %>%
#   group_by(salary_class) %>%
#   count()

model_metrics <- metric_set(roc_auc, accuracy, f_meas)

set.seed(1234)
cv_splits <- rsample::vfold_cv(df_train, strata = income_class, v = 5, repeats = 5)

## logistic regression ----

lr_mdl <- logistic_reg(penalty = tune(), mixture = 1, mode = 'classification') %>%
  set_engine('glmnet')

lr_wkflow <- workflow() %>%
  add_model(lr_mdl) %>%
  add_recipe(lr_recipe)

lr_grid <- tibble(penalty = 10^seq(-4,1, length.out = 25))

lr_tune_results <- lr_wkflow %>%
  tune::tune_grid(
    resamples = lr_cv_splits,
    grid = lr_grid,
    metrics = yardstick::metric_set(accuracy, roc_auc, f_meas),
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  )

lr_tune_results %>%
  collect_metrics()

lr_best <- lr_tune_results %>%
  select_best(metric = 'f_meas')

final_lr_wkflow <- lr_wkflow %>%
  finalize_workflow(lr_best)

valid_lrfit <- final_lr_wkflow %>%
  last_fit(df_split)

collect_metrics(valid_lrfit)

collect_predictions(valid_lrfit) %>%
  conf_mat(income_class, .pred_class)

valid_lrfit %>%
  pull(.workflow) %>%
  pluck(1) %>%
  tidy(exponentiate = TRUE) %>%
  arrange(estimate) %>%
  # kable(digits = 3)
  identity()

## random forest ----

set.seed(1337)
doParallel::registerDoParallel()

# juice & bake

# train_baked <- bake(prep(mdl_recipe, df_train, strings_as_factors = F), new_data = df_train)
# valid_baked <- bake(prep(mdl_recipe, df_train, strings_as_factors = F), new_data = df_valid)
# test_baked <- bake(prep(mdl_recipe, df_train, strings_as_factors = F), new_data = df_test)

rf_mdl <- rand_forest(mtry = tune(), trees = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_grid <- expand.grid(mtry = c(5), trees = c(250))

# define workflow
rf_workflow <- workflow() %>%
  add_recipe(mdl_recipe) %>%
  add_model(rf_mdl)

rf_tune_results <- rf_workflow %>%
  tune::tune_grid(
    resamples = cv_splits,
    grid = rf_grid,
    metrics = model_metrics,
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  )

rf_tune_results %>%
  collect_metrics()

rf_best <- rf_tune_results %>%
  select_best(metric = 'f_meas')

final_rf_wkflow <- rf_workflow %>%
  finalize_workflow(rf_best)

valid_rf_fit <- final_rf_wkflow %>%
  last_fit(df_split)

collect_metrics(valid_rf_fit)

collect_predictions(valid_rf_fit) %>%
  conf_mat(income_class, .pred_class)

collect_predictions(valid_rf_fit) %>%
  accuracy(truth = income_class, estimate = .pred_class)

collect_predictions(valid_rf_fit) %>%
  f_meas(truth = income_class, estimate = .pred_class)

collect_predictions(valid_rf_fit) %>%
  roc_auc(truth = income_class, .pred_1)

final_rfmdl <- rf_workflow %>%
  finalize_workflow(rf_best) %>%
  parsnip::fit(raw_train)

baked_test <- bake(prep(mdl_recipe, df_train, strings_as_factors = F), df_test)

rf_test_pred_class <- predict.model_fit(final_rfmdl$fit$fit, new_data = bake(prep(mdl_recipe, df_train, strings_as_factors = F), df_test), type = 'class')
rf_test_pred_prob <- predict.model_fit(final_rfmdl$fit$fit, new_data = bake(prep(mdl_recipe, df_train, strings_as_factors = F), df_test), type = 'prob')

test_preds <- tibble(predicted_class = rf_test_pred_class$.pred_class,
       pred_class1 = rf_test_pred_prob$.pred_1,
       pred_class0 = rf_test_pred_prob$.pred_0,
       actual_class = baked_test$income_class)

yardstick::accuracy(test_preds, truth = actual_class, estimate = predicted_class)
yardstick::f_meas(test_preds, truth = actual_class, estimate = predicted_class)
yardstick::roc_auc(test_preds, truth = actual_class, pred_class1)

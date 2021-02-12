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

cln_train %>%
  select(where(is.numeric)) %>%
  summarize_all(list(stat_meanval = mean, stat_medianval = median, stat_minval = min, stat_maxval = max, stat_var = var, stat_sd = sd)) %>%
  pivot_longer(
    cols = everything(),
    names_sep = "_stat_",
    names_to = c('variable','.value')
  )

# numerical summary
cln_train %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarize(
    min = min(value),
    avg = mean(value),
    median = median(value),
    max = max(value),
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

cln_train %>%
  group_by(occupation) %>%
  count()

cln_train %>%
  group_by(education) %>%
  count()

cln_train %>%
  group_by(sex) %>%
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


# correlation

cln_train %>%
  select(where(is.numeric)) %>%
  select(-education_number) %>%
  corrr::correlate()

# remove capital gain/loss and replace with categorical
# create new feature: income_class (1 if income is >50k or 0 if <= 50k)
# remove variable final_weight as it is a complex & unexplainable idea. Also it has MASSIVE variance. Almost no correlation. According to meta data (adult.names), this value is only applicable in its explanation to data points from within the same state and would be difficult to compare to across the states


cln_train %>%
  mutate(
    capital = case_when(
      capital_gain > 0 ~ 'gain',
      capital_loss > 0 ~ 'loss',
      capital_gain == 0 & capital_loss == 0 ~ 'neither',
      TRUE ~ 'error'
    ),
    income_class = ifelse(salary_class == '<=50k',0,1)
  ) %>%
   select(-capital_gain, -capital_loss, -salary_class, -final_weight)

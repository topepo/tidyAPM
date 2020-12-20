library(caret)
library(tidymodels)

theme_set(theme_bw())

## -----------------------------------------------------------------------------

# These lines replicate what was used to create the book (along with the
# near-zero variance filter used below)

data(GermanCredit)

german_credit <- 
  GermanCredit %>% 
  select(-CheckingAccountStatus.lt.0, -SavingsAccountBonds.lt.100,
         -EmploymentDuration.lt.1, -EmploymentDuration.Unemployed,
         -Personal.Male.Married.Widowed, -Property.Unknown,
         -Housing.ForFree)

## -----------------------------------------------------------------------------

set.seed(1)
split <- initial_split(german_credit, strata = Class, prop = 0.8)
german_credit_train <- training(split)
german_credit_test  <- testing(split)

## -----------------------------------------------------------------------------

svm_radial <- 
  svm_rbf(cost = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

german_credit_rec <- recipe(Class ~ ., data = german_credit_train) %>% 
  step_nzv(all_predictors())

german_credit_wflow <- 
  workflow() %>% 
  add_recipe(german_credit_rec) %>% 
  add_model(svm_radial)

## -----------------------------------------------------------------------------

set.seed(1)
cv_10_fold <- vfold_cv(german_credit_train, v = 10, strata = Class)

set.seed(1)
cv_10_fold_reps <- vfold_cv(german_credit_train, v = 10, repeats = 5, strata = Class)

set.seed(1)
boots <- bootstraps(german_credit_train, times = 50)

set.seed(1)
cv_mc <- mc_cv(german_credit_train, times = 50, prop = 0.8)


## -----------------------------------------------------------------------------

process_resamples <- function(wflow, splits) {
  svm_grid <- tibble(cost = 2^ seq(-2, 7, length = 10))
  
  set.seed(3)
  res <- 
    wflow %>% 
    tune_grid(splits, grid = svm_grid, metrics = metric_set(accuracy))
  
  collect_metrics(res) %>% 
    dplyr::select(cost, mean, n, std_err)
}

## -----------------------------------------------------------------------------

library(doMC)
registerDoMC(cores = parallel::detectCores())

cv_10_fold_res <- process_resamples(german_credit_wflow, cv_10_fold) 
cv_10_fold_reps_res <- process_resamples(german_credit_wflow, cv_10_fold_reps) 
cv_mc_res <- process_resamples(german_credit_wflow, cv_mc) 
boots_res <- process_resamples(german_credit_wflow, boots) 

resampling_stats <- 
  cv_10_fold_res %>% 
  mutate(method = "10-fold Cross−Validation") %>% 
  bind_rows(
    cv_10_fold_reps_res %>% 
      mutate(method = "Repeated 10-fold Cross−Validation")
  ) %>% 
  bind_rows(
    cv_mc_res %>% 
      mutate(method = "Repeated Training/Test Splits")
  ) %>% 
  bind_rows(
    boots_res %>% 
      mutate(method = "Bootstrap")
  )

resampling_stats %>% 
  ggplot(aes(x = cost, y = mean)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = mean - 2 * std_err, ymax = mean + 2 * std_err),
                width = .1) +
  scale_x_continuous(trans = scales::log2_trans()) + 
  facet_wrap(~ method)

## -----------------------------------------------------------------------------

loo_calcs <- function(split, wflow) {
  # For parallel processing with purrr, it might be helpful to re-load
  # the libraries within the remote workers
  suppressPackageStartupMessages(library(tidymodels, quietly = TRUE))
  
  set.seed(4)
  tibble(cost = 2^ seq(-2, 7, length = 10)) %>% 
    mutate(
      # Update the workflow to put in a number in place of 'tune()'
      wflow = map(cost, ~ finalize_workflow(wflow, tibble(cost = .x))),
      # Fit the workflow
      fits = map(wflow, ~ fit(.x, data = analysis(split))),
      # Predict the holdout sample
      preds = map(fits, ~ predict(.x, assessment(split), type = "class")),
      # Keep row index of original data for later merging
      .row = as.integer(split, data = "assessment")
    ) %>% 
    # Keep the required columns and reformat
    dplyr::select(-wflow, -fits) %>% 
    tidyr::unnest(cols = "preds")
}

cv_loo <- loo_cv(german_credit_train)

# These values will be used to merge with the out-of-sample predictions
outcome_data <- german_credit_train %>% 
  mutate(.row = row_number())

# Setup parallel processing with using a version of `purrr::map()`:

library(furrr)
options(future.rng.onMisuse = "ignore")
plan(multisession)

# Compute the predictions for each cost value and holdout sample
cv_loo_res <- 
  future_map_dfr(cv_loo$splits, loo_calcs, wflow = german_credit_wflow) %>% 
  full_join(outcome_data, by = ".row") %>% 
  # Compute metrics for each SVM cost parameter value
  group_by(cost) %>% 
  # Compute performance
  do(rmse = accuracy(., Class, .pred_class)) %>% 
  unnest(cols = "rmse") %>% 
  # Reformat to bind to existing values. 
  dplyr::rename(mean = .estimate) %>% 
  mutate(
    n = NA_real_,
    std_err = NA_real_,
    method = "Leave One Out Cross Validation"
  )

resampling_stats <- 
  resampling_stats %>% 
  bind_rows(cv_loo_res) 

resampling_stats %>% 
  ggplot(aes(x = cost, y = mean)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = mean - 2 * std_err, ymax = mean + 2 * std_err),
                width = .1) +
  scale_x_continuous(trans = scales::log2_trans()) + 
  facet_wrap(~ method)


## -----------------------------------------------------------------------------

app_samp <- bootstraps(german_credit_train, times = 0, apparent = TRUE)
app_samp_res <- 
  process_resamples(german_credit_wflow, app_samp) %>% 
  dplyr::select(cost, apparaent = mean) %>% 
  full_join(boots_res, by = "cost") %>% 
  mutate(mean = (1 - exp(-1)) * mean + exp(-1) * apparaent) %>% 
  mutate(
    std_err = NA_real_,
    method = "Bootstrap 632"
  ) %>% 
  dplyr::select(cost, mean, n, std_err, method)


resampling_stats <- 
  resampling_stats %>% 
  bind_rows(app_samp_res) %>% 
  mutate(
    method = factor(method, 
                    levels = c("Bootstrap", "Bootstrap 632", 
                               "10-fold Cross−Validation", 
                               "Repeated 10-fold Cross−Validation", 
                               "Leave One Out Cross Validation", 
                               "Repeated Training/Test Splits")
                    )
  )

resampling_stats %>% 
  ggplot(aes(x = cost, y = mean)) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = mean - 2 * std_err, ymax = mean + 2 * std_err),
                width = .2, alpha = .6) +
  scale_x_continuous(trans = scales::log2_trans()) + 
  facet_wrap(~ method, ncol = 2)


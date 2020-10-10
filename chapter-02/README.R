## ----startup--------------------------------------------------------------------------------------
library(tidymodels)
library(AppliedPredictiveModeling)
library(patchwork)
library(sessioninfo)
library(conflicted)

## -----------------------------------------------------------------------------

conflict_prefer("filter", winner = "dplyr", quiet = TRUE)
conflict_prefer("select", winner = "dplyr", quiet = TRUE)

## -----------------------------------------------------------------------------

theme_set(theme_bw())

## -----------------------------------------------------------------------------

options(width = 100, digits = 3)
options(dplyr.print_min = 6, dplyr.print_max = 6)
options(cli.width = 85)
options(crayon.enabled = FALSE)

opts_chunk$set(
  digits = 3,
  fig.align = 'center',
  fig.width = 6,
  fig.height = 4.25
)


## ----chapter-02-data------------------------------------------------------------------------------
data(FuelEconomy)
ls()


## ----chapter-02-fig-01----------------------------------------------------------------------------
cars2010 %>% 
 mutate(Year = "2010 Model Year") %>% 
 bind_rows(
  cars2011 %>% 
   mutate(Year = "2011 Model Year")
 ) %>% 
 ggplot(aes(x = EngDispl, y = FE)) + 
 geom_point(alpha = .3) + 
 facet_wrap(~ Year) + 
 xlab("Engine Displacement") + 
 ylab("Fuel Efficiency (MPG)")


## ----chapter-02-folds-----------------------------------------------------------------------------
set.seed(121)
folds_2010 <- vfold_cv(cars2010)


## ----chapter-02-linear-reg------------------------------------------------------------------------
# Setup the model type and use `lm()` to fit the model: 
lm_spec <- linear_reg() %>% set_engine("lm")

# Create a control object that will save the out-of-sample predictions. 
ctrl <- control_resamples(save_pred = TRUE)

# Perform 10-fold cross-validation on the linear regression
lm_lin_resamples <- 
 lm_spec %>% 
 fit_resamples(FE ~ EngDispl, resamples = folds_2010, control = ctrl)

# Fit the linear model on the 2010 data
lm_lin_fit <- lm_spec %>% fit(FE ~ EngDispl, data = cars2010)

tidy(lm_lin_fit)


## ----chapter-02-diagnostic-function---------------------------------------------------------------
fuel_plots <- function(resamples, fit) {
 pred_line <- 
  predict(fit, cars2010) %>% 
  bind_cols(cars2010) %>% 
  arrange(EngDispl)
 
 profile_plot <- 
  cars2010 %>% 
  ggplot(aes(x = EngDispl, y = FE)) + 
  geom_point(alpha = .3) + 
  geom_line(data = pred_line, aes(y = .pred), col = "red") +
  xlab("Engine Displacement") + 
  ylab("Fuel Efficiency (MPG)")
 
 obs_vs_pred <- 
  collect_predictions(resamples) %>% 
  ggplot(aes(x = FE, y = .pred)) + 
  geom_abline(lty = 2) + 
  geom_point(alpha = .3) + 
  coord_obs_pred() + 
  xlab("Observed") + 
  ylab("Predicted")
 
 profile_plot + obs_vs_pred
}


## ----chapter-02-fig-02----------------------------------------------------------------------------
fuel_plots(lm_lin_resamples, lm_lin_fit)


## ----chapter-02-linear-reg-rmse-------------------------------------------------------------------
collect_metrics(lm_lin_resamples)


## ----chapter-02-linear-reg-quad-------------------------------------------------------------------
lm_quad_resamples <- 
 lm_spec %>% 
 fit_resamples(FE ~ EngDispl + I(EngDispl^2), resamples = folds_2010, control = ctrl)

lm_quad_fit <- lm_spec %>% fit(FE ~ EngDispl + I(EngDispl^2), data = cars2010)
tidy(lm_quad_fit)


## ----chapter-02-fig-03----------------------------------------------------------------------------
fuel_plots(lm_quad_resamples, lm_quad_fit)


## ----chapter-02-linear-reg-quad-rmse--------------------------------------------------------------
collect_metrics(lm_quad_resamples)


## ----chapter-02-mars, message = FALSE-------------------------------------------------------------
mars_spec <- 
 mars(num_terms = tune(), prune_method = "none") %>% 
 set_engine("earth") %>% 
 set_mode("regression")

ctrl <- control_grid(save_pred = TRUE)

mars_tune <-
 mars_spec %>%
 tune_grid(
  FE ~ EngDispl,
  resamples = folds_2010,
  control = ctrl,
  grid = tibble(num_terms = 2:5)
 )


## ----chapter-02-fig-04----------------------------------------------------------------------------
autoplot(mars_tune, metric = "rmse")


## ----chapter-02-mars-paramters--------------------------------------------------------------------
# Show the top results:
show_best(mars_tune, metric = "rmse")

# Pick the numerically best:
mars_param <- select_best(mars_tune, metric = "rmse")
mars_param

# Update the model to substitute the value of `tune()` with
# the chosen value, then fit the model. 
mars_fit <- 
 mars_spec %>% 
 finalize_model(mars_param) %>% 
 fit(FE ~ EngDispl, data = cars2010)

# The fitted MARS terms:
cat(format(mars_fit$fit))


## ----chapter-02-mars-cuts, include = FALSE--------------------------------------------------------
mars_cuts <- unique(mars_fit$fit$cuts[, "EngDispl"])
mars_cuts <- mars_cuts[mars_cuts > 0]
mars_cut_text <- knitr::combine_words(sort(mars_cuts))


## ----chapter-02-fig-05----------------------------------------------------------------------------
fuel_plots(mars_tune, mars_fit)


## ----chapter-02-fig-06----------------------------------------------------------------------------
# Predict the test set using the two best models

lm_quad_test_res <- 
 lm_quad_fit %>% 
 predict(cars2011) %>% 
 bind_cols(cars2011) %>% 
 mutate(model = 'Quadratic Regression')

mars_test_res <- 
 mars_fit %>% 
 predict(cars2011) %>% 
 bind_cols(cars2011) %>% 
 mutate(model = 'MARS') 

lm_quad_test_res  %>% 
 bind_rows(mars_test_res) %>% 
 ggplot(aes(x = EngDispl, y = FE)) + 
 geom_point(alpha = .5) + 
 geom_line(aes(y = .pred), col = "red") +
 facet_wrap(~ model) +
 xlab("Engine Displacement") + 
 ylab("Fuel Efficiency (MPG)")


## ----chapter-02-test-stats------------------------------------------------------------------------
lm_quad_test_res %>% rmse(FE, .pred)
mars_test_res %>% rmse(FE, .pred)


## ----chapter-02-session---------------------------------------------------------------------------
session_info()


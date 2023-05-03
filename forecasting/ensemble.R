library(xgboost)
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)
library(tidyverse)
library(lubridate)
library(timetk)
library(here)

load(file = here("data", "country.Rdata"))
# This toggles plots from plotly (interactive) to ggplot (static)
interactive <- FALSE

# Data
#m750 <- m4_monthly %>% filter(id == "M750")

#m750 %>%  plot_time_series(date, value, .interactive = interactive)

country %>%
  plot_time_series(date, Value, .interactive = interactive)

FREQ<- 52
WEEKS <- length(country$Value)

future <- as.integer(WEEKS*0.2)
dlimit <- head(tail(country,future),1)$date

nTrain <- length(country$date) - future
train <- country %>% select(Value, date) %>% filter(date < dlimit)
valid <- country %>% select(Value, date) %>% filter(date >= dlimit)

# Split Data 80/20
splits <- initial_time_split(train, prop = 0.8)

# Model 1: auto_arima ----
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(Value ~ date, data = training(splits))


# Model 2: arima_boost ----
model_fit_arima_boosted <- arima_boost(
  min_n = 1,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
      data = training(splits))

# Model 3: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Value ~ date, data = training(splits))

# Model 4: prophet ----
model_fit_prophet <- prophet_reg(seasonality_weekly = TRUE) %>%
  set_engine(engine = "prophet") %>%
  fit(Value ~ date, data = training(splits))

# Model 5: lm ----
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
      data = training(splits))

# Model 6: earth ----
#model_spec_mars <- mars(mode = "regression") %>%  set_engine("earth") 
# bad result

recipe_spec <- recipe(Value ~ date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num) %>%
  step_rm(date)

wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm#,
#  wflw_fit_mars
)

models_tbl

# calibrate
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = train)

calibration_tbl


# test set forecast and accuracy
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = country,
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive,
    .conf_interval_show = FALSE
  )

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = train)

refit_tbl %>%
  modeltime_forecast(h = "85 weeks", actual_data = country) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive,
    .conf_interval_show = FALSE
  )
#class(m750_models)
#class(refit_tbl)
#m750_models
ensemble_fit <- refit_tbl %>%
  ensemble_average(type = "mean")

ensemble_fit

# Calibration
calibration_tbl <- modeltime_table(
  ensemble_fit
) %>%
  modeltime_calibrate(train, quiet = FALSE)




country_code <- "USA"
# first in data is week 1, 2015
# 2020 is leap year - so 53 weeks

# set the parameters for the time series
startDate <- min(country$date)
startYear <- year(startDate)
startMonth <- month(startDate)
startWeek <- week(startDate)
startDay <- day(startDate)

endDate <- max(country$date)
endYear <- year(endDate)
endMonth <- month(endDate)
endWeek <- week(endDate)

# default graph labels
mtitle <- paste0(country_code," all causes deaths")
stitle <- paste0(startMonth, "/", startYear, " - ", endMonth, "/",endYear)
y_lab <- "Deaths"
x_lab <- "weeks"
y_lim <- c(48000, 90000)     
x_lim <- c(startDate, endDate)

splits <- initial_time_split(country, prop = 0.8)
# Forecast vs Test Set
png("docs/ensemble.png")
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = country
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE,
                          .title = paste0(mtitle,"\n ",stitle), 
                          .x_lab = x_lab,
                          .y_lab = y_lab)

dev.off()


calibration_tbl %>%
  modeltime_accuracy() %>% 
  table_modeltime_accuracy(
    .interactive = interactive
  )

calibration_tbl %>%
  modeltime_accuracy() #%>% table()

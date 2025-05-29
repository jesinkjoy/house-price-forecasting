getwd()

## House Price Analysis for Connecticut

library(fpp3)

# The dataset consists of the House Price Index for Connecticut from 1975 onwards.
# The time series data is contained in CHousing.csv.

CHousing.df <- read.csv("3. All-Transactions_House_Price_Index_for_Connecticut.csv")

# Convert the 'date' column to Date format and then to a quarterly period. Select only the relevant columns.
CHousing.df <- CHousing.df %>% 
  dplyr::mutate(Quarter = as.Date(date, "%m/%d/%y")) %>% 
  dplyr::mutate(Quarter = yearquarter(date)) %>%
  dplyr::select(Quarter, HousePrice)

# Convert the data frame to a tsibble for time series analysis
Housing.tb <-  CHousing.df %>%
  as_tsibble(index = Quarter)

# Partition the data: Use data from 1975 to 2020 as the training set, keeping 2021 onward for validation.
Housing.tb.train <- Housing.tb %>%
  filter(year(Quarter) < 2020)

## Visualize the training data
# Create a time series plot of the House Price Index
Housing.tb.train %>% autoplot(HousePrice)

# Create a seasonal plot of the House Price Index
Housing.tb.train %>% gg_season(HousePrice)

# Create a sub-series plot of the House Price Index to show trends by sub-period
Housing.tb.train %>% gg_subseries(HousePrice)

## Describe the form of the trend and seasonality observed in the training data
# The dataset appears to show a moderate linear trend with no significant seasonality.

## Fit a regression model to the training data with appropriate trend terms
# Fit a linear regression model with a trend term
fit_Housing <- Housing.tb.train %>%
  model(TSLM(HousePrice ~ trend()))

# Display the summary report for the fitted model
fit_Housing %>% report()

# Create a plot to compare the actual data to the fitted values (training data only)
fit_Housing %>%
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = HousePrice, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the forecast for 18 quarters with prediction intervals
fc_Housing1 <- fit_Housing %>%
  forecast(h = 18)

fc_Housing1 %>%
  autoplot(Housing.tb.train)

## Fit an ARIMA model to the training data
# Use an iterative procedure to find the best ARIMA model for this time series.
# Determine the level of differencing required to make the data stationary
Housing.tb.train %>% features(HousePrice, unitroot_nsdiffs) # D = 0 (no seasonal differencing needed)
Housing.tb.train %>% features(HousePrice, unitroot_ndiffs) # d = 1 (one regular differencing needed)

# Fit an ARIMA model with d=1 and D=0, and explore various parameter combinations
ARIMA0.fit <- Housing.tb.train %>% model(ARIMA(HousePrice ~ pdq(0,1,0)))
report(ARIMA0.fit) # AIC=1022.37
augment(ARIMA0.fit) %>% gg_tsdisplay(.resid, plot_type='partial') 

ARIMA1.fit <- Housing.tb.train %>% model(ARIMA(HousePrice ~ pdq(0,1,1)))
report(ARIMA1.fit) # AIC=986.24 
augment(ARIMA1.fit) %>% gg_tsdisplay(.resid, plot_type='partial') 

ARIMA2.fit <- Housing.tb.train %>% model(ARIMA(HousePrice ~ pdq(1,1,0)))
report(ARIMA2.fit) # AIC=980.8
augment(ARIMA2.fit) %>% gg_tsdisplay(.resid, plot_type='partial') 

ARIMA3.fit <- Housing.tb.train %>% model(ARIMA(HousePrice ~ pdq(2,1,0)))
report(ARIMA3.fit) # AIC=979.19
augment(ARIMA3.fit) %>% gg_tsdisplay(.resid, plot_type='partial') 

ARIMA4.fit <- Housing.tb.train %>% model(ARIMA(HousePrice ~ pdq(1,1,1)))
report(ARIMA4.fit) # AIC=970.65
augment(ARIMA4.fit) %>% gg_tsdisplay(.resid, plot_type='partial') 

# The ARIMA(1,1,1) model has the lowest AIC among tested models
# Plot the residuals to check for any patterns left
augment(ARIMA4.fit) %>% gg_tsdisplay(.resid, plot_type='partial') 

# Create a plot to compare the fitted ARIMA model to the training data
ARIMA4.fit %>%
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = HousePrice, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the ARIMA model forecast and prediction intervals
fc_Housing2 <- ARIMA4.fit %>%
  forecast(h = 18)

fc_Housing2 %>%
  autoplot(Housing.tb.train)

## Fit an automatic ARIMA model to the training data
ARIMAauto.fit <- Housing.tb.train %>% model(ARIMA(HousePrice))
report(ARIMAauto.fit) # AIC=956.55
# The fitted model is ARIMA(1,1,3)(0,0,1)

# Create a plot to compare the auto-fitted ARIMA model to the training data
ARIMAauto.fit %>%
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = HousePrice, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the forecast from the automatic ARIMA model
fc_Housing3 <- ARIMAauto.fit %>%
  forecast(h = 18)

fc_Housing3 %>%
  autoplot(Housing.tb.train)

## Fit an exponential smoothing (ETS) model to the training data
# Fit your recommended ETS model(s).
ets_manual1 <- Housing.tb.train %>% model(ETS(HousePrice ~ error("M") + trend("M") + season("N")))
report(ets_manual1) # AIC = 1476.761

ets_manual2 <- Housing.tb.train %>% model(ETS(HousePrice ~ error("M") + trend("A") + season("N")))
report(ets_manual2) # AIC = 1473.866

ets_manual6 <- Housing.tb.train %>% model(ETS(HousePrice ~ error("M") + trend("Md") + season("N")))
report(ets_manual6) # AIC = 1473.979

ets_manual3 <- Housing.tb.train %>% model(ETS(HousePrice ~ error("A") + trend("M") + season("N")))
report(ets_manual3) # AIC = 1420.145

ets_manual4 <- Housing.tb.train %>% model(ETS(HousePrice ~ error("A") + trend("A") + season("N")))
report(ets_manual4) # AIC = 1415.744

ets_manual5 <- Housing.tb.train %>% model(ETS(HousePrice ~ error("A") + trend("Md") + season("N")))
report(ets_manual5) # AIC = 1410.683 

# Based on visual inspection, we recommend using a model with additive error and additive trend.
ets_manual <- Housing.tb.train %>% model(ETS(HousePrice ~ error("A") + trend("Md") + season("N")))
report(ets_manual) # AIC=1410.683

# Create a plot to compare the fitted ETS model to the training data
ets_manual %>%
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = HousePrice, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the forecast from the fitted ETS model
fc_Housing4 <- ets_manual %>%
  forecast(h = 18)

fc_Housing4 %>%
  autoplot(Housing.tb.train)

## Fit an ETS model using automatic selection
ets_auto <- Housing.tb.train %>% model(ETS(HousePrice))
report(ets_auto) # AIC=1412.576
# This model is ETS(A,Ad,N)

# Create a plot to compare the auto-fitted ETS model to the training data
ets_auto %>%
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = HousePrice, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the forecast from the auto-fitted ETS model
fc_Housing5 <- ets_auto %>%
  forecast(h = 18)

fc_Housing5 %>%
  autoplot(Housing.tb.train)

## Fit multiple models to the training data for comparison
all.models.fit <- Housing.tb.train %>%
  model(
    arima_manual = ARIMA(HousePrice ~ pdq(1,1,1)),
    arima_auto = ARIMA(HousePrice),
    ts_reg = TSLM(HousePrice ~ trend()),
    ets_manual = ETS(HousePrice ~ error("A") + trend("A") + season("N")),
    ets_auto = ETS(HousePrice),
    naive = NAIVE(HousePrice),
    snaive = SNAIVE(HousePrice)
  ) %>%
  mutate(
    combination = ((ets_manual) + (arima_manual)) / 2 # Combines forecasts from two models with equal weights
  )

augment(all.models.fit) %>%
  filter(.model == "combination") %>%
  ggplot(aes(x = HousePrice)) +
  geom_line(aes(y = HousePrice, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Actual vs. Fitted Turnover Values for the Ensemble Model"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

# Let's generate forecasts
combination_fc <- all.models.fit %>%
  forecast(h = 18)

combination_fc %>%
  filter(.model == "combination") %>%
  autoplot(Housing.tb.train) +
  labs(y = "$",
       title = "House Price Analysis for Connecticut - Ensemble Model")

# Generate forecasts for all models
all.models.pred <- all.models.fit %>% forecast(h = 18)

# Evaluate model accuracy on the full dataset
all.models.pred %>% accuracy(Housing.tb) %>% arrange(MAPE, decreasing = TRUE)

# Recommend a model based on lowest MAPE for future forecasting
best_model <- Housing.tb %>%
  model(TSLM(HousePrice ~ trend()))

# Create a plot to show a 6-year forecast from the chosen model
fc_Housing <- best_model %>%
  forecast(h = 26)

fc_Housing %>%
  autoplot(Housing.tb)

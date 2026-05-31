# Appliance Energy Prediction

## Overview
This project models household appliance energy consumption using IoT sensor data collected from a low-energy building in Belgium. The goal is to identify which environmental and temporal factors best predict energy use, and to compare the predictive accuracy of several time series and machine learning models.

## Research Question
Which environmental conditions and time-of-day factors best predict appliance energy consumption, and can a time series regression or random forest model outperform a naive baseline?

## Dataset
The dataset contains 19,735 observations recorded every 10 minutes over ~4.5 months, with features including:
- **Temperature and humidity** readings from 9 rooms in the house
- **Weather station data** (outdoor temperature, humidity, pressure, wind speed, visibility, dew point)
- **Temporal features** (hour of day, day of week, weekend indicator)
- **Target variable**: Appliances energy use (Wh)

Source: [UCI Machine Learning Repository – Appliances Energy Prediction](https://archive.ics.uci.edu/dataset/374/appliances+energy+prediction)

## Methods
**Exploratory Data Analysis**
- Univariate distributions, log transformation of the skewed target variable
- Correlation matrix and hierarchical clustering of sensor variables
- Welch's ANOVA to test energy differences across light usage categories and days of week
- Time series visualization and outlier analysis via IQR

**Modeling** (evaluated using rolling-origin cross-validation to prevent data leakage)
| Model | Description |
|---|---|
| Naive | Lag-1 benchmark (ŷₜ = yₜ₋₁) |
| AR(1) | Autoregressive model, order selected via PACF |
| ARX (Saturated) | AR(1) + environmental predictors |
| ARX (AIC-reduced) | Stepwise-reduced ARX model |
| ARMAX(1,1/2,1/1,2) | ARMA error structure with external regressors |
| TSRF | Time Series Random Forest with tuned hyperparameters |

**Key predictors identified:** hour of day (cyclic encoded), outdoor humidity, living room temperature, outdoor temperature, and light usage category.

## Key Findings
- The ARX AIC-reduced model outperformed the naive baseline and matched the TSRF in predictive accuracy, suggesting the relationship between predictors and energy use is largely linear after accounting for autocorrelation.
- Hour of day was the strongest temporal predictor; outdoor humidity was the strongest environmental predictor.
- Approximately 14% of observations were flagged as outliers by IQR; these clustered in evening hours and colder months.

## Tools & Libraries
R · ggplot2 · dplyr · lubridate · caret · randomForest · forecast · tseries · rsample · patchwork · corrplot

## How to Run
1. Clone the repository
2. Open `R script-final project.R` in RStudio
3. Uncomment and run the `install.packages()` lines at the top if any libraries are not installed
4. Set your working directory to the repo root so `energydata_complete.csv` loads correctly
5. Run the script sequentially — note that ARMAX cross-validation sections are computationally intensive and may take several hours

# 🏠 House Price Forecasting in Connecticut

This project analyzes and forecasts the quarterly House Price Index (HPI) in Connecticut using multiple time series models. It was completed as part of the **SCMA669 – Forecasting Methods** course at VCU.

---

## 📁 Project Structure

- **TSLM Modeling**: Trend-based linear regression with log-transformation
- **ARIMA & ETS Models**: Manual and automated forecasting
- **Ensemble Forecasting**: Average and inverse-variance model combinations
- **Model Evaluation**: CRPS, MAPE, AIC used to determine best forecast model

---

## 🎯 Objectives

- Forecast housing price trends in Connecticut over the next 6 years.
- Compare traditional time series models to select the best-fitting forecast.
- Explore ensemble modeling to combine strengths of individual models.
- Recommend strategies for buyers, investors, and policymakers.

---

## 🔧 Tools & Technologies

- **Language**: R
- **Libraries**: `fpp3`, `tsibble`, `ggplot2`, `dplyr`
- **Models Used**:
  - TSLM (with and without log transformation)
  - Manual & Auto ARIMA
  - Manual & Auto ETS
  - Ensemble Models

---

## 📈 Key Results

- **Best Model**: Log-transformed TSLM
- **Best Hybrid**: Ensemble (inverse variance)
- **Trend Insight**: Long-term growth with notable dips around the 2008 crisis

---

## 💡 Insights & Recommendations

- **Homebuyers**: Consider buying early as prices show long-term upward trends.
- **Investors**: Connecticut housing offers steady growth potential.
- **Policymakers**: Monitor housing affordability; support first-time buyers.

---

## 📂 Files

- `ensemble_forecast_model.R` – Manual model comparison and ensemble modeling
- `log_tslm_arima_model.R` – Log-transformed TSLM and diagnostics with ARIMA(2,2,1)
- `3. All-Transactions_House_Price_Index_for_Connecticut-1.csv` – Quarterly HPI dataset (1980–2023)
- `forecasting_methods_presentation.pptx` – Final project presentation

---

## 📌 Disclaimer

This project was created for academic purposes. The data used was publicly sourced and should not be considered financial advice.

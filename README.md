### README: ABC Case Study Analysis

This R script performs a comprehensive analysis of the ABC case study using financial data. Below is a summary of the workflow and key functionalities implemented:

---

#### **1. Setup and Data Loading**
- **Packages:** Required packages (e.g., `readxl`, `tidyverse`) are installed and loaded.
- **Working Directory:** Sets the working directory to the specified path.
- **Data Import:** Loads `ABC.xlsx` containing data on stock returns, market indicators, sentiment, etc.

---

#### **2. Data Preparation**
- **Data Cleaning:** Converts `Date` column to `Date` format and removes `NA` values.
- **Visualization:** Plots:
  - Returns of ABC stock and Nifty.
  - Cumulative returns of ABC and Nifty.

---

#### **3. Data Analysis**
- **Descriptive Statistics:** Summarizes key metrics (mean, median, min, max, etc.).
- **Normality Tests:** Examines skewness, kurtosis, and performs statistical tests (Agostino, Jarque-Bera) to check deviations from normality.
- **Stationarity Tests:** Uses ADF, PP, and KPSS tests to confirm data stationarity, a key requirement for regression modeling.

---

#### **4. Model Training and Testing**
- **Train-Test Split:** Segregates data into training (2007-2016) and testing (2017 onwards) datasets.
- **Simple Linear Regression (SLR):**
  - Analyzes the relationship between ABC returns and Nifty returns.
  - Diagnostics: Examines residuals for normality, heteroscedasticity (Breusch-Pagan), and autocorrelation (Durbin-Watson).
  - Predictions: Compares actual vs. predicted returns.
- **Multiple Linear Regression (MLR):**
  - Models ABC returns using multiple predictors (Sensex, Sentiment, Nifty, etc.).
  - Addresses multicollinearity using VIF and correlation matrices.
  - Refines the model by dropping highly collinear variables.

---

#### **5. Model Comparison**
- **Alternative Models:**
  - Naive Model: Baseline model assuming no predictors.
  - Enhanced SLR Model: Adds higher-order terms (e.g., Nifty²).
- **Error Metrics:** Evaluates models using various metrics:
  - MSE, RMSE, MAE, SMAPE, MSLE, RMSLE, etc.
- **Best Model Selection:** Aggregates error metrics to identify the model with the best out-of-sample predictions.

---

#### **6. Visualizations**
- Plots comparing:
  - Predicted vs. actual returns for SLR and MLR models.
  - Density distributions of ABC and Nifty returns.
- Includes legends and annotations for clarity.

---

#### **7. Outputs**
- **Error Table:** Stores error metrics for all models.
- **Rank Table:** Ranks models based on performance metrics.
- **Final Recommendation:** Selects the MLR model as the best-fit solution for predicting ABC returns.

---

#### **Instructions to Run**
1. Place `ABC.xlsx` in the specified working directory (`D:/MBA737`).
2. Run the script sequentially to ensure proper execution of data preparation, analysis, and modeling steps.
3. Review visual outputs and error metrics to verify results.

---

#### **Dependencies**
Ensure the following R packages are installed:
- `readxl`, `tidyverse`, `lubridate`, `moments`, `Metrics`, `urca`, `car`, `lmtest`, `sandwich`.

---

# 📊 MCA Statistical Lab: Student Spending Analysis

A responsive R Shiny web application designed to analyze student consumer spending behavior.  
This project serves as a Syllabus Compliance Map for MCA Statistical Labs, providing an interactive dashboard for Exploratory Data Analysis (EDA), probability theory, distribution fitting, and hypothesis testing.

---

## 🔗 Project Resources

- 🌐 **Live Web App**: [Student Spending Lab](https://adarshsnarayanan.shinyapps.io/student-spending-lab/)

- 📝 **Data Collection Form**: [Google Form](https://docs.google.com/forms/d/e/1FAIpQLSeP6LlzWEARp-O2WxIO25dlJd8q1g_hjp_TpZ33tJBlwCT-7g/viewform)

- 📊 **Raw Data Tracking**: [Google Sheets View](https://docs.google.com/spreadsheets/d/1LLdDgr6ZOiqwg_Cv-eDCz2XAvEV4UfKo9CNv02-X6yQ/edit?gid=1207902354#gid=1207902354)

---

## 📌 Key Features

### 📊 Executive Summary
Provides an overview of student spending with:
- Avg Monthly Spend
- Modal Impulse Trigger
- Budget Overrun %

### 🧾 Check Reality (Live Sampling)
Users can submit their own spending data which updates analysis in real-time.

---

## 📈 Visual Analytics

- Radar Charts – Multi-dimensional spending comparison  
- Violin & Box Plots – Detect density and outliers  
- Pareto Charts – Category spending distribution  
- Correlation Heatmaps – Karl Pearson correlation analysis  
- Regression Scatterplots – Budget vs Actual spending analysis  

---

## 🧪 Statistical Testing

Includes dynamic models for:
- T-Test
- ANOVA (F-Test)
- Chi-Square Test

Each test includes plain-English interpretations.

---

## 🧬 Simulated Fallback Data

A mock data generator ensures the dashboard works even when the main CSV dataset is unavailable.

---

## 🎓 MCA Syllabus Compliance Map

### Module 1: R Basics & EDA
- Pareto Charts
- Histograms
- Pie Charts
- Sorted Line Graphs

### Module 2: Central Tendency & Dispersion
- Skewness & Kurtosis formulas
- Bayes Theorem interactive module
- Quartile calculations (Q1, Q3, Median)

### Module 3: Random Variables & Distributions
- Normal PDF fitting on spending histograms

### Module 4: Correlation & Regression
- Correlation matrix
- Regression model: Actual ~ Budget

### Module 5: Sampling & Hypothesis Testing
- T-Test
- ANOVA
- Chi-Square Test
- 95% Confidence Interval calculation

---

## 🛠 Tech Stack

- R Shiny
- ggplot2
- dplyr
- tidyr
- DT
- bslib
- plotly
- bsicons
- stats

---

## 📦 Required Libraries

```r
install.packages(c(
"shiny",
"ggplot2",
"dplyr",
"tidyr",
"DT",
"bslib",
"plotly",
"bsicons",
"stats"
))
```

---

## 🚀 How to Run Locally

1. Save the app as `app.R`
2. Place the dataset in the same directory

```
Student_Spending_Relevant_Data_*.csv
```

3. Run the app

```r
shiny::runApp("app.R")
```

---

## 📚 Academic Purpose

This project demonstrates MCA statistical concepts including:
- Exploratory Data Analysis
- Probability Theory
- Distribution Modeling
- Regression Analysis
- Hypothesis Testing

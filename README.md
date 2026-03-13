# 📊 Student Spending Lab

A responsive R Shiny web application designed to analyze student consumer spending behavior. It features an interactive dashboard providing statistical analysis, hypothesis testing, and dynamic visualizations of various expenditure demographics.

## 🔗 Important Links
- **Live Web App**: [Student Spending Lab](https://adarshsnarayanan.shinyapps.io/student-spending-lab/)
- **Data Collection Form**: [Google Form](https://docs.google.com/forms/d/e/1FAIpQLSeP6LlzWEARp-O2WxIO25dlJd8q1g_hjp_TpZ33tJBlwCT-7g/viewform)
- **Raw Data Tracking**: [Google Sheets View](https://docs.google.com/spreadsheets/d/1LLdDgr6ZOiqwg_Cv-eDCz2XAvEV4UfKo9CNv02-X6yQ/edit?gid=1207902354#gid=1207902354)

---

## 📌 Key Features

*   **Executive Summary**: Get an immediate overview with metric value boxes (Avg Monthly Spend, Highest Expense, Budget Overrun) and distribution charts comparing student segments.
*   **Descriptive Statistics**: Explore summary statistics and data tables breaking down Mean & SD comparisons between subcategories like Hostelers and Day Scholars.
*   **Visual Analytics**: Dive into in-depth UI cards featuring:
    *   Radar Charts (spending profiles)
    *   Stacked Bar Plots (absolute and proportional spending makeup)
    *   Violin and Box plots (detecting extreme variations and outliers in funding sources)
    *   Correlation Heatmaps (to discover variables that trend together)
*   **Hypothesis Testing**: Built-in dynamic computational models showing results of ANOVA, Chi-Square, and Two-Sample T-tests along with plain-English insights interpreting the statistical outcomes.
*   **Regression Analysis**: Interactive actual spending vs. available funds (Budget) scatterplots and regression lines measuring behavioral financial scaling.
*   **Simulated Fallback Data**: Robust mock data generator included—the app operates smoothly even if the main CSV dataset stream (`Student_Spending_Relevant_Data_*.csv`) is absent.

---

## 🛠 Tech Stack & Requirements

This app was built utilizing the powerful `shiny` framework overlaid with `bslib` for Bootstrap styling and `plotly` for interactive graphs. 

To run this application locally, you must have an R environment installed along with the following required libraries:

*   `shiny`
*   `ggplot2`
*   `dplyr`
*   `tidyr`
*   `DT`
*   `bslib`
*   `plotly`
*   `bsicons`

---

## 🚀 How to Run Locally

1. **Install Prerequisites**: If this is your first time, install the required packages using your RConsole or RStudio:
   ```R
   install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "DT", "bslib", "plotly", "bsicons"))
   ```
2. **Clone / Download**: Save the application code in a file named `app.R` in a new local directory.
3. **Data Retrieval (Optional)**: If you possess the raw dataset (`Student_Spending_Relevant_Data_*.csv`), place it alongside your `app.R` file. If not, the application will automatically populate utilizing a dynamically generated deterministic simulation so you can still preview its design and function.
4. **Launch Application**: 
   Open your R terminal or RStudio console, set your working directory to the folder containing your application, and execute:
   ```R
   shiny::runApp("app.R")
   ```
   *The application will launch on localhost in your default web browser.*

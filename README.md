📊 Student Spending: Advanced Statistical Lab

An interactive Shiny Web Application designed for deep-dive statistical analysis of student spending patterns. This tool bridges the gap between raw data and actionable financial insights, comparing "Hostelers" vs. "Day Scholars" across various expense categories.

🚀 Live Demo

Experience the interactive dashboard here:

Interactive Lab Demo

🧐 Features

Executive Summary: High-level KPIs including Average Monthly Spend, Top Expense Categories, and Budget Overrun percentages.

Visual Analytics:

Radar Plots: Visualizing spending "fingerprints."

Violin Plots: Showing density and distribution of expenses.

Correlation Heatmaps: Identifying relationships between different spending habits.

Statistical Testing:

ANOVA: Testing for significance between student types.

Chi-Square: Identifying dependencies between funding sources and impulse buys.

T-Tests: Rigorous mean comparison between groups.

Predictive Regression: A linear regression model exploring the relationship between planned budgets and actual expenditures.

🛠️ Installation & Local Setup

To run this app locally, ensure you have R and RStudio installed.

Clone the repository:

git clone [https://github.com/YOUR_USERNAME/student-spending-lab.git](https://github.com/YOUR_USERNAME/student-spending-lab.git)
cd student-spending-lab


Install required packages:
Open R or RStudio and run:

install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "DT", "bslib", "plotly", "bsicons"))


Run the App:

library(shiny)
runApp()


📊 Data Structure

The application expects a CSV file (Student_Spending_Relevant_Data_...csv) with the following key columns:

Student.Type: (Categorical) e.g., Hosteler, Day Scholar.

Funding.Source: (Categorical) e.g., Parents, Scholarship, Job.

Budget: (Numeric) Planned monthly budget.

Food, Travel, Academic, Lifestyle, Mobile: (Numeric) Actual monthly spend per category.

Impulse.Category: (Categorical) Primary area of unplanned spending.

Note: If the CSV is missing, the app includes a calibrated mock data generator for demonstration purposes.

🧪 Methodologies Used

Descriptive Statistics: Mean, Median, and Standard Deviation calculations.

Inferential Statistics: One-Way ANOVA and Independent Samples T-Tests.

Association Testing: Pearson’s Chi-squared test for categorical independence.

Linear Modeling: Simple Linear Regression ($Y = \beta_0 + \beta_1X + \epsilon$).

📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

Developed by Adarsh S Narayanan

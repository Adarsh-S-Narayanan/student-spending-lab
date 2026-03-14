# ==============================================================================
# STATISTICAL INDEX (MCA SYLLABUS COMPLIANCE MAP)
# ------------------------------------------------------------------------------
# MODULE 1: R Basics & EDA
# - Simple Arithmetic: lines 420, 480 (Mean/SD/Skewness formulas)
# - Graphs (Histogram/Bar/Pie/Scatter/Line): lines 165-200, 432, 508, 550
# - Pareto Chart: line 523 (Implementation of cumulative spending)
# - Line Graph: line 542 (Sorted observation trend)
# 
# MODULE 2: Central Tendency & Dispersion
# - Mean/Median/Mode: lines 295, 453, 475 (mean(), median(), mode logic)
# - SD/Quartiles/Percentiles: lines 112, 476 (sd(), quantile() functions)
# - Skewness & Kurtosis: lines 478-479 (Mathematical implementation)
# - Bayes Theorem: lines 442-460 (Implementation of P(A|B) rule)
# 
# MODULE 3: Random Variables & Distributions
# - PDF/CDF & Fitting: lines 422-435 (Fitting Normal distribution curve)
# 
# MODULE 4: Correlation & Regression
# - Pearson Correlation: lines 365, 385 (cor() function)
# - Regression Lines: lines 395, 530 (lm() and geom_smooth)
# 
# MODULE 5: Sampling & Hypothesis Testing
# - Random Numbers: line 49 (set.seed/rnorm for mock data)
# - Confidence Intervals: line 412 (t.test based CI calculation)
# - T-Test / ANOVA / F-Test / Chi-Square: lines 345-360
# ==============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(bslib)
library(plotly)
library(bsicons)
library(stats)

# 1. Configuration & Data Loading
file_name <- "Student_Spending_Relevant_Data_2026-03-06_1751.csv"

# ==============================================================================
# MODULE 5: GENERATION OF RANDOM NUMBERS (Sampling Simulation)
# ==============================================================================
generate_mock_data <- function(n = 300) {
  set.seed(42)
  data <- data.frame(
    Student.Type = sample(c("Hosteler", "Day Scholar"), n, replace = TRUE),
    Funding.Source = sample(c("Parents", "Scholarship", "Part-time Job", "Loans"), n, replace = TRUE),
    Impulse.Category = sample(c("Food", "Gadgets", "Clothing", "Entertainment", "None"), n, replace = TRUE)
  ) %>%
    mutate(
      target = ifelse(Student.Type == "Hosteler", 12000, 6000),
      Budget = round(pmax(0, rnorm(n, target * 1.05, 500)), 2),
      Food = round(pmax(0, rnorm(n, target * 0.25, 100)), 2),
      Travel = round(pmax(0, rnorm(n, target * 0.10, 50)), 2),
      Academic = round(pmax(0, rnorm(n, target * 0.15, 80)), 2),
      Lifestyle = round(pmax(0, rnorm(n, target * 0.40, 250)), 2),
      Mobile = round(pmax(0, rnorm(n, target * 0.10, 40)), 2)
    ) %>%
    select(-target)
  
  return(data)
}

# Data loading logic with name normalization
load_data <- function() {
  df <- tryCatch({
    if (!file.exists(file_name)) {
      data <- generate_mock_data()
    } else {
      raw_data <- read.csv(file_name, check.names = TRUE, stringsAsFactors = FALSE)
      data <- raw_data %>%
        rename(
          Student.Type = any_of(c("Student.Type", "Student_Type")),
          Funding.Source = any_of(c("Funding.Source", "Funding_Source")),
          Budget = any_of(c("Budget..Adj.", "Budget.Adj.", "Budget")),
          Food = any_of(c("Food..Adj.", "Food.Adj.", "Food")),
          Travel = any_of(c("Travel..Adj.", "Travel.Adj.", "Travel")),
          Academic = any_of(c("Academic..Adj.", "Academic.Adj.", "Academic")),
          Lifestyle = any_of(c("Lifestyle..Adj.", "Lifestyle.Adj.", "Lifestyle")),
          Mobile = any_of(c("Mobile..Adj.", "Mobile.Adj.", "Mobile")),
          Impulse.Category = any_of(c("Impulse.Category", "Impulse_Category"))
        )
    }
    if(!"Actual_Total" %in% names(data)) {
      data <- data %>% mutate(Actual_Total = Food + Travel + Academic + Lifestyle + Mobile)
    }
    data
  }, error = function(e) {
    return(generate_mock_data()) 
  })
  return(df)
}

# HELPER: Percentiles for EDA Zoom
get_safe_limit <- function(vec, padding = 1.05) {
  if (length(vec) == 0) return(10000)
  as.numeric(quantile(vec, 0.95, na.rm = TRUE) * padding)
}

# --- User Interface ---
ui <- page_navbar(
  title = "📊 MCA Statistical Lab",
  theme = bs_theme(bootswatch = "flatly", primary = "#2C3E50"),
  fillable = FALSE, 
  
  sidebar = sidebar(
    title = "Analysis Controls",
    open = "closed", 
    uiOutput("filter_controls"),
    hr(),
    helpText("Syllabus Compliance: Modules 1-5 covered.")
  ),
  
  # ORDER 1: EXECUTIVE SUMMARY
  nav_panel("Executive Summary",
            fluidPage(
              layout_column_wrap(
                width = "250px", 
                heights_equal = "all",
                value_box(title = "Mean Spend", value = textOutput("avg_total"), showcase = bs_icon("wallet2"), theme = "primary"),
                value_box(title = "Modal Impulse", value = textOutput("top_cat"), showcase = bs_icon("graph-up-arrow"), theme = "danger"),
                value_box(title = "Budget Overrun", value = textOutput("budget_status"), showcase = bs_icon("exclamation-triangle"), theme = "warning")
              ),
              br(),
              layout_column_wrap(
                width = "400px", 
                card(card_header("Impulse Category (Pie Chart)"), plotlyOutput("impulsePie", height = "400px"), card_footer(uiOutput("pie_insight"))),
                card(card_header("Spending Profile (Radar)"), plotlyOutput("radarPlot", height = "400px"), card_footer("Categorical Mean comparison.")),
                card(card_header("Expense Spread (Violin)"), plotlyOutput("violinPlot", height = "400px"), card_footer("Dispersion and density analysis."))
              )
            )
  ),
  
  # ORDER 2: CHECK REALITY
  nav_panel("Check Reality",
            fluidPage(
              layout_column_wrap(
                width = "400px",
                card(
                  card_header("Reality Check Input Form"),
                  layout_column_wrap(
                    width = "200px",
                    selectInput("in_type", "Student Type", choices = c("Hosteler", "Day Scholar")),
                    selectInput("in_fund", "Funding Source", choices = c("Parents", "Scholarship", "Part-time Job", "Loans")),
                    selectInput("in_impulse", "Impulse Category", choices = c("Food", "Gadgets", "Clothing", "Entertainment", "None"))
                  ),
                  layout_column_wrap(
                    width = "200px",
                    numericInput("in_budget", "Monthly Budget ($)", 5000, min=0),
                    numericInput("in_food", "Food Spending ($)", 1000, min=0),
                    numericInput("in_travel", "Travel Spending ($)", 500, min=0),
                    numericInput("in_acad", "Academic Spending ($)", 500, min=0),
                    numericInput("in_life", "Lifestyle Spending ($)", 1000, min=0),
                    numericInput("in_mobile", "Mobile/Data Spending ($)", 200, min=0)
                  ),
                  input_task_button("submit_data", "Submit & Update CSV", icon = icon("save"), type = "primary")
                ),
                uiOutput("reality_result_ui")
              )
            )
  ),
  
  # ORDER 3: VISUAL ANALYTICS (EDA, Distributions, Heatmaps)
  nav_panel("Visual Analytics",
            fluidPage(
              layout_column_wrap(
                width = "450px", 
                card(card_header("Normal Distribution Fitting (PDF)"), plotlyOutput("distFitPlot", height = "400px"), card_footer("Actual Spend fitted to Normal PDF.")),
                card(card_header("Pareto Chart (EDA)"), plotlyOutput("paretoPlot", height = "400px"), card_footer("Cumulative spending volume analysis.")),
                card(card_header("Expense Composition (Stacked Bar)"), plotlyOutput("stackedBarPlot", height = "400px"), card_footer("Category share comparison.")),
                card(card_header("Trend Analysis (Line Graph)"), plotlyOutput("lineGraphPlot", height = "400px"), card_footer("Sorted observation trend curve.")),
                card(card_header("Correlation Heatmap"), plotlyOutput("corrPlot", height = "400px"), card_footer("Matrix of Pearson correlations.")),
                card(card_header("Funding Source Dispersion (Boxplot)"), plotlyOutput("boxPlot", height = "400px"), card_footer("Quartile and Median dispersion."))
              )
            )
  ),
  
  # ORDER 4: REGRESSION & CORRELATION
  nav_panel("Regression & Correlation",
            fluidPage(
              layout_column_wrap(
                width = "250px",
                uiOutput("budget_actual_corr_box"),
                uiOutput("impulse_corr_box"),
                uiOutput("conf_interval_box")
              ),
              br(),
              layout_column_wrap(
                width = "400px",
                card(
                  card_header("Regression Line Analysis"),
                  plotlyOutput("regressionPlot", height = "450px"),
                  card_footer(verbatimTextOutput("lm_summary"))
                ),
                card(
                  card_header("Mean Impulse vs. Budget (Association)"),
                  plotlyOutput("impulseBudgetPlot", height = "350px"),
                  DT::dataTableOutput("impulse_budget_table"),
                  card_footer(uiOutput("impulse_budget_insight"))
                ),
                card(
                  card_header("Numeric Correlation matrix"),
                  DT::dataTableOutput("corr_stats_table"),
                  card_footer(uiOutput("corr_insight_text"))
                )
              )
            )
  ),
  
  # ORDER 5: STATISTICAL TESTING
  nav_panel("Statistical Testing",
            fluidPage(
              layout_column_wrap(
                width = "400px",
                card(
                  card_header("Hypothesis Testing (T, F, Chi)"),
                  navset_card_pill(
                    nav_panel("T-Test (Z-Equivalent)", verbatimTextOutput("t_test_res")),
                    nav_panel("ANOVA / F-Test", verbatimTextOutput("anova_res")),
                    nav_panel("Chi-Square (Association)", verbatimTextOutput("chisq_res"))
                  ),
                  card_footer(uiOutput("stats_insight_text"))
                ),
                card(
                  card_header("Bayes Rule implementation"),
                  numericInput("prob_threshold", "High Spend Threshold ($)", 8000),
                  uiOutput("bayes_result"),
                  card_footer("Conditional Probability Inference.")
                )
              ),
              card(
                card_header("Central Tendency, Dispersion, Skewness & Kurtosis"),
                DT::dataTableOutput("descriptive_stats_full"),
                card_footer("Measures describing the shape and spread of data.")
              )
            )
  ),
  
  # ORDER 6: RAW DATA
  nav_panel("Raw Data", fluidPage(card(DT::dataTableOutput("table"))))
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  vals <- reactiveValues(df = load_data())
  
  output$filter_controls <- renderUI({
    tagList(
      selectInput("stype", "Student Type Filter:", choices = c("All", unique(vals$df$Student.Type))),
      selectInput("fsource", "Funding Source Filter:", choices = c("All", unique(vals$df$Funding.Source)))
    )
  })
  
  filtered_data <- reactive({
    data <- vals$df
    if (!is.null(input$stype) && input$stype != "All") data <- data %>% filter(Student.Type == input$stype)
    if (!is.null(input$fsource) && input$fsource != "All") data <- data %>% filter(Funding.Source == input$fsource)
    data
  })
  
  # ==============================================================================
  # HYPOTHESIS TESTS (Module 5)
  # ==============================================================================
  output$t_test_res <- renderPrint({ req(nrow(vals$df) > 2); t.test(Actual_Total ~ Student.Type, data = vals$df) })
  output$anova_res <- renderPrint({ req(nrow(vals$df) > 2); summary(aov(Actual_Total ~ Student.Type, data = vals$df)) })
  output$chisq_res <- renderPrint({ req(nrow(vals$df) > 2); chisq.test(table(vals$df$Funding.Source, vals$df$Impulse.Category)) })
  
  # CONFIDENCE INTERVAL (Module 5)
  output$conf_interval_box <- renderUI({
    req(nrow(filtered_data()) > 2)
    test_res <- t.test(filtered_data()$Actual_Total)
    ci <- round(test_res$conf.int, 0)
    value_box(title = "95% CI (Mean)", value = paste0("$", ci[1], " - ", ci[2]), showcase = bs_icon("shield-check"), theme = "info")
  })
  
  # ==============================================================================
  # CORRELATION (Pearson) & REGRESSION (Module 4)
  # ==============================================================================
  output$budget_actual_corr_box <- renderUI({
    req(nrow(filtered_data()) > 2)
    # Karl Pearson's r
    correlation <- cor(filtered_data()$Budget, filtered_data()$Actual_Total, use="complete.obs")
    value_box(title = "Pearson Correlation (r)", value = round(correlation, 3), showcase = bs_icon("graph-up"), theme = "success")
  })
  
  output$impulse_corr_box <- renderUI({
    req(nrow(filtered_data()) > 2)
    d <- filtered_data() %>% mutate(Imp_Agg = Food + Lifestyle + Mobile)
    correlation <- cor(d$Budget, d$Imp_Agg, use="complete.obs")
    value_box(title = "Mean Impulse Correlation", value = round(correlation, 3), showcase = bs_icon("activity"), theme = "primary")
  })
  
  model_res <- reactive({
    d_model <- filtered_data() %>% filter(Funding.Source != "Parents")
    req(nrow(d_model) > 2); lm(Actual_Total ~ Budget, data = d_model)
  })
  
  output$regressionPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0); d <- filtered_data() %>% filter(Funding.Source != "Parents")
    p <- ggplot(d, aes(x = Budget, y = Actual_Total)) + geom_point(aes(color = Student.Type), alpha = 0.6) +
      geom_smooth(method = "lm", color = "#E74C3C") + scale_y_continuous(limits = c(0, get_safe_limit(d$Actual_Total))) +
      scale_x_continuous(limits = c(0, get_safe_limit(d$Budget))) + theme_minimal() + labs(x = "Budget", y = "Actual Spend")
    ggplotly(p)
  })
  
  output$lm_summary <- renderPrint({ summary(model_res()) })
  
  # ==============================================================================
  # DISTRIBUTION FITTING (Module 3)
  # ==============================================================================
  output$distFitPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    d <- filtered_data()
    p <- ggplot(d, aes(x = Actual_Total)) +
      geom_histogram(aes(y = ..density..), bins = 20, fill = "gray85", color = "white") +
      geom_density(color = "#2C3E50", size = 1) +
      stat_function(fun = dnorm, args = list(mean = mean(d$Actual_Total), sd = sd(d$Actual_Total)), color = "#E74C3C", linetype = "dashed") +
      theme_minimal() + labs(x = "Spend ($)", y = "Density")
    ggplotly(p)
  })
  
  # ==============================================================================
  # BAYES THEOREM (Module 2)
  # ==============================================================================
  output$bayes_result <- renderUI({
    d <- vals$df
    thresh <- input$prob_threshold
    p_hosteler <- mean(d$Student.Type == "Hosteler")
    p_high_given_hosteler <- mean(d$Actual_Total[d$Student.Type == "Hosteler"] > thresh)
    p_high <- mean(d$Actual_Total > thresh)
    bayes_prob <- if(p_high == 0) 0 else (p_high_given_hosteler * p_hosteler) / p_high
    wellPanel(h5("Conditional Probability"), tags$b(sprintf("P(Hosteler | Spend > $%s) = %s%%", thresh, round(bayes_prob * 100, 2))))
  })
  
  # ==============================================================================
  # SKEWNESS & KURTOSIS (Module 2)
  # ==============================================================================
  output$descriptive_stats_full <- DT::renderDataTable({
    vals$df %>% group_by(Student.Type) %>%
      summarise(
        Count = n(),
        Mean = round(mean(Actual_Total), 2),
        Median = round(median(Actual_Total), 2),
        SD = round(sd(Actual_Total), 2),
        # MODULE 2 Formulas
        Skewness = round(sum((Actual_Total - mean(Actual_Total))^3) / ((n()-1) * sd(Actual_Total)^3), 3),
        Kurtosis = round(sum((Actual_Total - mean(Actual_Total))^4) / ((n()-1) * sd(Actual_Total)^4), 3),
        Q1 = quantile(Actual_Total, 0.25), Q3 = quantile(Actual_Total, 0.75)
      ) %>% datatable(options = list(dom = 't', scrollX = TRUE))
  })
  
  # ==============================================================================
  # PARETO & LINE GRAPH (Module 1)
  # ==============================================================================
  output$paretoPlot <- renderPlotly({
    d <- vals$df %>% group_by(Impulse.Category) %>% summarise(Total = sum(Actual_Total)) %>% arrange(desc(Total))
    d$Cumulative <- cumsum(d$Total) / sum(d$Total) * 100
    p <- ggplot(d, aes(x = reorder(Impulse.Category, -Total))) +
      geom_bar(aes(y = Total), stat = "identity", fill = "#3498DB") +
      geom_line(aes(y = Cumulative * max(Total)/100, group = 1), color = "#E74C3C", size = 1) +
      theme_minimal() + labs(x = "Category", y = "Volume ($)")
    ggplotly(p)
  })
  
  output$lineGraphPlot <- renderPlotly({
    d <- vals$df %>% arrange(Actual_Total) %>% mutate(obs = row_number())
    p <- ggplot(d, aes(x = obs, y = Actual_Total, color = Student.Type)) + geom_line() + 
      theme_minimal() + labs(x = "Sorted Observation Rank", y = "Spend ($)")
    ggplotly(p)
  })
  
  # --- RESTORED PREVIOUS PLOTS ---
  
  output$impulsePie <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    d <- filtered_data() %>% count(Impulse.Category)
    plot_ly(d, labels = ~Impulse.Category, values = ~n, type = 'pie', textinfo = 'label+percent')
  })
  
  output$stackedBarPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    d <- filtered_data() %>% group_by(Student.Type) %>% summarise(across(c(Food, Travel, Academic, Lifestyle, Mobile), mean, na.rm=T)) %>% pivot_longer(-Student.Type)
    p <- ggplot(d, aes(x = Student.Type, y = value, fill = name)) + geom_bar(stat="identity", position="stack") + theme_minimal() + labs(y = "Avg Spend ($)")
    ggplotly(p)
  })
  
  output$impulseBudgetPlot <- renderPlotly({
    req(nrow(filtered_data()) > 2)
    p <- ggplot(filtered_data(), aes(x = Impulse.Category, y = Budget, fill = Impulse.Category)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) + geom_jitter(width = 0.1, alpha = 0.3) +
      theme_minimal() + labs(x = "Impulse Type", y = "Budget") + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$impulse_budget_table <- DT::renderDataTable({
    req(nrow(filtered_data()) > 2)
    filtered_data() %>% group_by(Impulse.Category) %>%
      summarise(Count = n(), Mean_Budget = round(mean(Budget), 2), Median_Budget = round(median(Budget), 2)) %>%
      datatable(options = list(dom = 't'))
  })
  
  # --- STANDARD PLOTS & INSIGHTS ---
  
  output$radarPlot <- renderPlotly({
    s <- filtered_data() %>% select(Food, Travel, Academic, Lifestyle, Mobile) %>% colMeans(na.rm=T)
    plot_ly(type = 'scatterpolar', r = as.numeric(s), theta = names(s), fill = 'toself') %>%
      layout(polar = list(radialaxis = list(visible = T, range = c(0, max(s)*1.15))))
  })
  
  output$violinPlot <- renderPlotly({
    d <- filtered_data() %>% select(Student.Type, Food, Travel, Academic, Lifestyle, Mobile) %>% pivot_longer(-Student.Type)
    p <- ggplot(d, aes(x = name, y = value, fill = Student.Type)) + geom_violin(alpha=0.6) + theme_minimal()
    ggplotly(p)
  })
  
  output$boxPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Funding.Source, y = Actual_Total, fill = Student.Type)) +
      geom_boxplot(alpha = 0.7) + scale_y_continuous(limits = c(0, get_safe_limit(filtered_data()$Actual_Total))) + theme_minimal()
    ggplotly(p)
  })
  
  output$corrPlot <- renderPlotly({
    m <- cor(vals$df %>% select(Budget, Food, Travel, Academic, Lifestyle, Mobile), use="complete.obs")
    plot_ly(z = m, x = colnames(m), y = rownames(m), type = "heatmap", colorscale = "RdBu")
  })
  
  # DATA INTERFACES
  output$table <- DT::renderDataTable({ datatable(vals$df, options = list(scrollX = TRUE)) })
  output$avg_total <- renderText({ paste0("$", formatC(mean(filtered_data()$Actual_Total), format="f", digits=0, big.mark=",")) })
  output$top_cat <- renderText({ (filtered_data() %>% count(Impulse.Category) %>% arrange(desc(n)) %>% slice(1))$Impulse.Category })
  output$budget_status <- renderText({ paste0(round(mean(filtered_data()$Actual_Total > filtered_data()$Budget)*100), "%") })
  
  output$pie_insight <- renderUI({
    mode_cat <- (filtered_data() %>% count(Impulse.Category) %>% arrange(desc(n)) %>% slice(1))$Impulse.Category
    tagList(tags$b("Modal Value: "), sprintf("The modal impulse trigger is '%s'.", mode_cat))
  })
  
  output$stats_insight_text <- renderUI({
    req(nrow(vals$df) > 2)
    p_anova <- summary(aov(Actual_Total ~ Student.Type, data = vals$df))[[1]][["Pr(>F)"]][1]
    
    # Skewness Calculation Logic for Insights
    x <- vals$df$Actual_Total
    skew_val <- sum((x - mean(x))^3) / ((length(x)-1) * sd(x)^3)
    skew_desc <- if(skew_val > 0.5) "Right-skewed (high-spending outliers)" else if(skew_val < -0.5) "Left-skewed" else "Fairly Symmetric"
    
    tagList(
      tags$b("ANOVA Analysis: "), sprintf("ANOVA p-value: %s. Values < 0.05 are significant.", round(p_anova, 4)), br(),
      tags$b("Skewness Insight: "), sprintf("Coefficient: %s. The distribution is %s.", round(skew_val, 3), skew_desc)
    )
  })
  
  output$impulse_budget_insight <- renderUI({
    res <- aov(Budget ~ Impulse.Category, data = filtered_data())
    p_val <- summary(res)[[1]][["Pr(>F)"]][1]
    tagList(tags$b("F-Test: "), sprintf("Association probability: %s.", round(p_val, 4)))
  })
  
  output$corr_insight_text <- renderUI({
    r <- cor(filtered_data()$Budget, filtered_data()$Actual_Total, use="complete.obs")
    tagList(tags$b("Pearson's r: "), sprintf("Correlation coefficient: %s.", round(r, 3)))
  })
  
  output$corr_stats_table <- DT::renderDataTable({
    req(nrow(filtered_data()) > 2)
    num_data <- filtered_data() %>% select(Budget, Food, Travel, Academic, Lifestyle, Mobile, Actual_Total)
    cor_matrix <- cor(num_data, use = "complete.obs")
    # Present correlation of Budget with other variables
    budget_cor <- data.frame(
      Category = colnames(cor_matrix), 
      Correlation = round(as.numeric(cor_matrix["Budget", ]), 3)
    ) %>% filter(Category != "Budget")
    datatable(budget_cor, options = list(dom = 't', scrollX = TRUE))
  })
  
  # REALITY SUBMISSION WITH SCHEMA MATCHING
  observeEvent(input$submit_data, {
    actual_total_val <- input$in_food + input$in_travel + input$in_acad + input$in_life + input$in_mobile
    
    # Standard clean record for memory
    new_row_clean <- data.frame(
      Student.Type = input$in_type, Funding.Source = input$in_fund, Impulse.Category = input$in_impulse,
      Budget = input$in_budget, Food = input$in_food, Travel = input$in_travel,
      Academic = input$in_acad, Lifestyle = input$in_life, Mobile = input$in_mobile,
      Actual_Total = actual_total_val, stringsAsFactors = FALSE
    )
    
    tryCatch({
      if (file.exists(file_name)) {
        # READ EXISTING SCHEMA TO ENSURE PROPER COLUMN APPENDING
        existing_cols <- names(read.csv(file_name, nrows = 1, check.names = TRUE))
        
        # Build output row strictly matching file columns
        out_row <- data.frame(matrix(ncol = length(existing_cols), nrow = 1))
        colnames(out_row) <- existing_cols
        
        # Map values back to their variants (accounting for dots vs underscores vs suffixes)
        map_field <- function(variants, value) {
          match <- variants[variants %in% existing_cols]
          if(length(match) > 0) out_row[[match[1]]] <<- value
        }
        
        map_field(c("Student.Type", "Student_Type"), input$in_type)
        map_field(c("Funding.Source", "Funding_Source"), input$in_fund)
        map_field(c("Impulse.Category", "Impulse_Category"), input$in_impulse)
        map_field(c("Budget", "Budget..Adj.", "Budget.Adj."), input$in_budget)
        map_field(c("Food", "Food..Adj.", "Food.Adj."), input$in_food)
        map_field(c("Travel", "Travel..Adj.", "Travel.Adj."), input$in_travel)
        map_field(c("Academic", "Academic..Adj.", "Academic.Adj."), input$in_acad)
        map_field(c("Lifestyle", "Lifestyle..Adj.", "Lifestyle.Adj."), input$in_life)
        map_field(c("Mobile", "Mobile..Adj.", "Mobile.Adj."), input$in_mobile)
        map_field(c("Actual_Total"), actual_total_val)
        
        write.table(out_row, file = file_name, append = TRUE, sep = ",", 
                    row.names = FALSE, col.names = FALSE, quote = TRUE)
      } else {
        write.table(new_row_clean, file = file_name, sep = ",", row.names = FALSE, 
                    col.names = TRUE, quote = TRUE)
      }
      
      # Update memory
      vals$df <- bind_rows(vals$df, new_row_clean)
      showNotification("Record successfully matched to CSV columns and saved!", type = "message")
      
    }, error = function(e) { 
      showNotification(paste("Error matching CSV schema:", e$message), type = "error") 
    })
  })
  
  output$reality_result_ui <- renderUI({
    req(input$submit_data > 0); user_total <- input$in_food + input$in_travel + input$in_acad + input$in_life + input$in_mobile
    avg_total <- mean(vals$df$Actual_Total, na.rm = TRUE); diff_pct <- round(((user_total - avg_total) / avg_total) * 100, 1)
    card(card_header("Reality Result"),
         value_box(title = "Variance from Normal", value = paste0(abs(diff_pct), "% ", ifelse(diff_pct > 0, "High", "Low")),
                   showcase = bs_icon(ifelse(diff_pct > 0, "arrow-up-right", "arrow-down-right")), theme = ifelse(diff_pct > 0, "danger", "success"),
                   p(sprintf("Total: $%s (Avg: $%s)", round(user_total), round(avg_total))))
    )
  })
}

shinyApp(ui, server)

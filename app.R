# Load required libraries
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
# CALCULATION: MOCK DATA GENERATOR
# Purpose: Creates a fallback dataset with specific targets for Hosteler  
# and Day Scholar  if the CSV is missing.
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

# ==============================================================================
# CALCULATION: INITIAL DATA LOAD & CLEANING
# ==============================================================================
df <- tryCatch({
  if (!file.exists(file_name)) {
    message("Warning: CSV file not found. Using calibrated simulated data.")
    data <- generate_mock_data()
  } else {
    raw_data <- read.csv(file_name, check.names = TRUE)
    data <- raw_data %>%
      rename(
        Student.Type = any_of(c("Student.Type", "Student_Type", "Student.Type.", "Student.Type")),
        Funding.Source = any_of(c("Funding.Source", "Funding_Source", "Funding.Source.", "Funding.Source")),
        Budget = any_of(c("Budget..Adj.", "Budget.Adj.", "Budget")),
        Food = any_of(c("Food..Adj.", "Food.Adj.", "Food")),
        Travel = any_of(c("Travel..Adj.", "Travel.Adj.", "Travel")),
        Academic = any_of(c("Academic..Adj.", "Academic.Adj.", "Academic")),
        Lifestyle = any_of(c("Lifestyle..Adj.", "Lifestyle.Adj.", "Lifestyle")),
        Mobile = any_of(c("Mobile..Adj.", "Mobile.Adj.", "Mobile")),
        Impulse.Category = any_of(c("Impulse.Category", "Impulse_Category", "Impulse.Category."))
      )
  }
  data %>% mutate(Actual_Total = Food + Travel + Academic + Lifestyle + Mobile)
}, error = function(e) {
  message("Error processing data: ", e$message)
  return(generate_mock_data()) 
})

# --- User Interface ---
ui <- page_navbar(
  title = "📊 Student Spending: Advanced Statistical Lab",
  theme = bs_theme(bootswatch = "flatly", primary = "#2C3E50"),
  
  sidebar = sidebar(
    title = "Analysis Controls",
    uiOutput("filter_controls"),
    hr(),
    helpText("Each result below includes an automated 'Analysis Insight' to help interpret the data.")
  ),
  
  nav_panel("Executive Summary",
            fluidPage(
              uiOutput("data_check_ui"),
              layout_column_wrap(
                width = 1/3,
                heights_equal = "all",
                value_box(
                  title = "Avg Monthly Spend", 
                  value = textOutput("avg_total"), 
                  showcase = bs_icon("wallet2"), 
                  theme = "primary",
                  p("Mean total individual spend.")
                ),
                value_box(
                  title = "Highest Expense", 
                  value = textOutput("top_cat"), 
                  showcase = bs_icon("graph-up-arrow"), 
                  theme = "danger",
                  p("Top category by average.")
                ),
                value_box(
                  title = "Budget Overrun", 
                  value = textOutput("budget_status"), 
                  showcase = bs_icon("exclamation-triangle"), 
                  theme = "warning",
                  p("% of students over budget.")
                )
              ),
              br(),
              fluidRow(
                column(6, 
                       card(
                         card_header("Spending Distribution"),
                         plotlyOutput("distPlot", height = "400px"),
                         card_footer(uiOutput("dist_analysis"))
                       )
                ),
                column(6,
                       card(
                         card_header("Impulse Category Breakdown"),
                         plotlyOutput("impulsePie", height = "400px"),
                         card_footer(uiOutput("pie_analysis"))
                       )
                )
              )
            )
  ),
  
  nav_panel("Descriptive Statistics",
            fluidPage(
              card(
                card_header("Summary Statistics by Student Type"),
                DT::dataTableOutput("total_spending_stats"),
                card_footer(uiOutput("table_analysis_1"))
              ),
              card(
                card_header("Category Detail (Mean & SD)"),
                DT::dataTableOutput("summary_stats_table"),
                card_footer(uiOutput("table_analysis_2"))
              )
            )
  ),
  
  nav_panel("Visual Analytics",
            fluidPage(
              fluidRow(
                column(6, 
                       card(
                         card_header("Spending Profile (Radar)"), 
                         plotlyOutput("radarPlot", height = "450px"),
                         card_footer("Analysis: Radar shows the relative weight of each spending category. Sharp peaks indicate heavy specialization in that area.")
                       )
                ),
                column(6, 
                       card(
                         card_header("Expense Composition (Stacked)"), 
                         plotlyOutput("stackedBarPlot", height = "450px"),
                         card_footer("Analysis: Stacked bars compare the absolute dollar volume and proportional makeup of spending across groups.")
                       )
                )
              ),
              br(),
              fluidRow(
                column(6, 
                       card(
                         card_header("Expense Spread (Violin Plots)"), 
                         plotlyOutput("violinPlot", height = "450px"),
                         card_footer("Analysis: Violin width shows where spending is most concentrated. Longer tails indicate more extreme variations in behavior.")
                       )
                ),
                column(6, 
                       card(
                         card_header("Category Correlation Heatmap"), 
                         plotlyOutput("corrPlot", height = "450px"),
                         card_footer("Analysis: Stronger colors (red/blue) indicate categories that tend to rise or fall together.")
                       )
                )
              )
            )
  ),
  
  nav_panel("Hypothesis Testing",
            fluidPage(
              layout_column_wrap(
                width = 1/2,
                card(
                  card_header("ANOVA Result (Spending by Type)"), 
                  verbatimTextOutput("anova_res"),
                  card_footer("Insight: ANOVA tests if the average spending of different student types is statistically different (Look for Pr(<F) < 0.05).")
                ),
                card(
                  card_header("Chi-Square Result (Funding vs Impulse)"), 
                  verbatimTextOutput("chisq_res"),
                  card_footer("Insight: Chi-Square checks if funding sources are linked to specific impulse buy categories.")
                )
              ),
              card(
                card_header("Two-Sample T-Test (Group Mean Comparison)"), 
                verbatimTextOutput("z_test_res"),
                card_footer("Insight: T-test strictly compares the mean of two groups. If the p-value is low, the difference is likely not due to chance.")
              )
            )
  ),
  
  nav_panel("Regression Analysis",
            fluidPage(
              card(
                card_header("Regression Model: Budget vs Actual Spending"),
                plotlyOutput("regressionPlot", height = "550px"),
                verbatimTextOutput("lm_summary"),
                card_footer(uiOutput("regression_analysis"))
              )
            )
  ),
  
  nav_panel("Raw Data",
            fluidPage(
              card(DT::dataTableOutput("table"))
            )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  output$filter_controls <- renderUI({
    if (nrow(df) == 0) return(p("No data loaded."))
    tagList(
      selectInput("stype", "Student Type Filter:", choices = c("All", unique(df$Student.Type))),
      selectInput("fsource", "Funding Source Filter:", choices = c("All", unique(df$Funding.Source)))
    )
  })
  
  filtered_data <- reactive({
    req(nrow(df) > 0)
    data <- df
    if (!is.null(input$stype) && input$stype != "All") {
      data <- data %>% filter(Student.Type == input$stype)
    }
    if (!is.null(input$fsource) && input$fsource != "All") {
      data <- data %>% filter(Funding.Source == input$fsource)
    }
    data
  })
  
  # CALCULATION: DYNAMIC SCALING HELPER
  get_safe_limit <- function(vec) {
    if(length(vec) == 0) return(5000)
    limit <- quantile(vec, 0.95, na.rm = TRUE)
    return(as.numeric(limit) * 1.05)
  }
  
  # CALCULATION: EXECUTIVE METRICS
  output$avg_total <- renderText({
    req(nrow(filtered_data()) > 0)
    val <- mean(filtered_data()$Actual_Total, na.rm = TRUE)
    paste0("$", formatC(val, format="f", digits=2, big.mark=","))
  })
  
  output$top_cat <- renderText({
    req(nrow(filtered_data()) > 0)
    s <- filtered_data() %>% select(Food, Travel, Academic, Lifestyle, Mobile) %>% colMeans(na.rm=T)
    names(which.max(s))
  })
  
  output$budget_status <- renderText({
    req(nrow(filtered_data()) > 0)
    paste0(round(mean(filtered_data()$Actual_Total > filtered_data()$Budget, na.rm=T)*100), "%")
  })
  
  # ==============================================================================
  # ANALYSIS LOGIC: DYNAMIC INSIGHTS
  # ==============================================================================
  
  output$dist_analysis <- renderUI({
    d <- filtered_data()
    avg <- mean(d$Actual_Total)
    strongest_group <- d %>% group_by(Student.Type) %>% summarise(m = mean(Actual_Total)) %>% arrange(desc(m)) %>% slice(1)
    p(tags$b("Analysis: "), sprintf("The current view shows an average spend of $%s. The %s group exhibits the highest spending density in this segment.", 
                                    formatC(avg, format="d", big.mark=","), strongest_group$Student.Type))
  })
  
  output$pie_analysis <- renderUI({
    d <- filtered_data() %>% count(Impulse.Category) %>% arrange(desc(n)) %>% slice(1)
    p(tags$b("Analysis: "), sprintf("'%s' is the primary driver of impulse spending in this demographic, accounting for %s percent of impulse transactions.", 
                                    d$Impulse.Category, round((d$n/nrow(filtered_data()))*100, 1)))
  })
  
  output$table_analysis_1 <- renderUI({
    d <- filtered_data()
    sd_val <- sd(d$Actual_Total)
    p(tags$b("Analysis: "), sprintf("The standard deviation of $%s suggests a %s level of spending variability among individual students.", 
                                    formatC(sd_val, format="d", big.mark=","), ifelse(sd_val > 3000, "high", "moderate")))
  })
  
  output$regression_analysis <- renderUI({
    m <- lm(Actual_Total ~ Budget, data = filtered_data())
    r2 <- summary(m)$r.squared
    p(tags$b("Analysis: "), sprintf("The model explains %s%% of spending variance based on budget alone. A steeper line suggests students scale their lifestyle directly with their available funds.", 
                                    round(r2*100, 1)))
  })
  
  # --- PLOTS ---
  
  output$total_spending_stats <- DT::renderDataTable({
    req(nrow(df) > 0)
    df %>%
      group_by(Student.Type) %>%
      summarise(Count = n(), Mean_Total = mean(Actual_Total), Median_Total = median(Actual_Total), SD_Total = sd(Actual_Total)) %>%
      mutate(across(where(is.numeric), ~round(., 2))) %>%
      datatable(options = list(dom = 't', scrollX = TRUE))
  })
  
  output$summary_stats_table <- DT::renderDataTable({
    req(nrow(df) > 0)
    df %>%
      group_by(Student.Type) %>%
      summarise(across(c(Budget, Food, Travel, Academic, Lifestyle, Mobile), list(Mean = ~mean(., na.rm=T), SD = ~sd(., na.rm=T)))) %>%
      pivot_longer(-Student.Type) %>%
      mutate(value = round(value, 2)) %>%
      datatable(options = list(pageLength = 12, scrollX = TRUE))
  })
  
  output$anova_res <- renderPrint({ req(nrow(df) > 0); summary(aov(Actual_Total ~ Student.Type, data = df)) })
  output$chisq_res <- renderPrint({ req(nrow(df) > 0); chisq.test(table(df$Funding.Source, df$Impulse.Category)) })
  output$z_test_res <- renderPrint({ req(nrow(df) > 0); t.test(Actual_Total ~ Student.Type, data = df) })
  
  output$distPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    p <- ggplot(filtered_data(), aes(x = Actual_Total, fill = Student.Type)) +
      geom_density(alpha = 0.5) + scale_x_continuous(limits = c(0, get_safe_limit(filtered_data()$Actual_Total))) +
      theme_minimal() + labs(x = "Total Monthly Spend ($)", y = "Frequency Density")
    ggplotly(p) %>% layout(margin = list(t = 40, b = 40))
  })
  
  output$impulsePie <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    d <- filtered_data() %>% count(Impulse.Category)
    plot_ly(d, labels = ~Impulse.Category, values = ~n, type = 'pie', textinfo = 'label+percent') %>% layout(margin = list(t = 40, b = 40))
  })
  
  output$radarPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    s <- filtered_data() %>% select(Food, Travel, Academic, Lifestyle, Mobile) %>% colMeans(na.rm=T)
    plot_ly(type = 'scatterpolar', r = as.numeric(s), theta = names(s), fill = 'toself') %>%
      layout(polar = list(radialaxis = list(visible = T, range = c(0, max(s)*1.15))))
  })
  
  output$stackedBarPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    d <- filtered_data() %>% group_by(Student.Type) %>% summarise(across(c(Food, Travel, Academic, Lifestyle, Mobile), mean, na.rm=T)) %>% pivot_longer(-Student.Type)
    p <- ggplot(d, aes(x = Student.Type, y = value, fill = name)) + geom_bar(stat="identity", position="stack") + theme_minimal() + labs(y = "Average Spend ($)")
    ggplotly(p)
  })
  
  output$violinPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    d <- filtered_data() %>% select(Student.Type, Food, Travel, Academic, Lifestyle, Mobile) %>% pivot_longer(-Student.Type)
    p <- ggplot(d, aes(x = name, y = value, fill = Student.Type)) + geom_violin(alpha=0.6, position = position_dodge(width = 0.8)) + 
      geom_boxplot(width = 0.1, position = position_dodge(width = 0.8), outlier.shape = NA) +
      scale_y_continuous(limits = c(0, get_safe_limit(d$value))) + theme_minimal() + labs(x = "Expense Category", y = "Spend ($)")
    ggplotly(p)
  })
  
  output$corrPlot <- renderPlotly({
    req(nrow(df) > 0)
    m <- cor(df %>% select(Budget, Food, Travel, Academic, Lifestyle, Mobile), use="complete.obs")
    plot_ly(z = m, x = colnames(m), y = rownames(m), type = "heatmap", colorscale = "RdBu")
  })
  
  output$regressionPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    d <- filtered_data()
    max_val <- max(get_safe_limit(d$Budget), get_safe_limit(d$Actual_Total))
    p <- ggplot(d, aes(x = Budget, y = Actual_Total)) + geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") + 
      geom_point(aes(color = Student.Type), alpha = 0.5) + geom_smooth(method = "lm", color = "#E74C3C") + 
      scale_x_continuous(limits = c(0, max_val)) + scale_y_continuous(limits = c(0, max_val)) +
      theme_minimal() + labs(x = "Monthly Budget ($)", y = "Actual Spend ($)")
    ggplotly(p) %>% layout(xaxis = list(range = c(0, max_val)), yaxis = list(range = c(0, max_val)))
  })
  
  output$lm_summary <- renderPrint({ req(nrow(filtered_data()) > 0); summary(lm(Actual_Total ~ Budget, data = filtered_data())) })
  output$table <- DT::renderDataTable({ datatable(filtered_data(), options = list(scrollX = TRUE)) })
  
  output$data_check_ui <- renderUI({
    if (!file.exists(file_name)) { card(class = "bg-info text-white", card_body(h4("ℹ️ Simulation Calibrated"), p("Analysis insights below are based on simulated data targets."))) }
  })
}

shinyApp(ui, server)
# Load packages used by the app
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)

source("setup.R")

# Set the default theme for ggplot2 plots
# UI
ui <- fluidPage(
  titlePanel("Behavior Genetics Research Participation Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectInput("plot_var", "Select a Variable to Plot", choices = NULL),
      checkboxGroupInput("samples", "Filter by Sample",
                         choices = c("prolific", "SONA"),
                         selected = c("prolific", "SONA")),
      hr(),
      selectInput("outcome_vars", "Select Outcomes for Logistic Regression",
                  choices = NULL, multiple = TRUE),
      selectInput("predictor_vars", "Select Predictors for Logistic Regression",
                  choices = NULL, multiple = TRUE),
      checkboxInput("filter_sig", "Show Only p < .05 in Heatmap", TRUE),
      checkboxInput("interactive", "Make Heatmap Interactive", FALSE),
      actionButton("run_model", "Run Logistic Models")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Distribution Plot", plotOutput("descriptives_plot")),
        tabPanel("Odds Ratios Heatmap",
                 conditionalPanel(
                   condition = "input.interactive == true",
                   plotlyOutput("heatmap_plot_plotly")
                 ),
                 conditionalPanel(
                   condition = "input.interactive == false",
                   plotOutput("heatmap_plot")
                 )
        ),
        tabPanel("Model Results Table", dataTableOutput("table_out"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  merged_data <- load_clean_data()

  observe({
    all_vars <- names(merged_data)
    updateSelectInput(session, "plot_var", choices = all_vars, selected = "age")
    outcome_choices <- all_vars[str_detect(all_vars, "^research")]
    predictor_choices <- all_vars
    updateSelectInput(session, "outcome_vars", choices = outcome_choices)
    updateSelectInput(session, "predictor_vars", choices = predictor_choices)
  })


  filtered_data <- reactive({
    req(input$samples)
    merged_data %>% filter(sample %in% input$samples)
  })

 # model_results <- eventReactive(input$run_model, {
 #   req(input$outcome_vars)
 #   req(input$predictor_vars)
 #   compute_odds_ratios(filtered_data(), input$outcome_vars, input$predictor_vars)
#  })


  model_results <- eventReactive(input$run_model, {
    req(input$outcome_vars, input$outcome_vars)

    dataset_name <- if (identical(input$samples, c("SONA"))) {
      "sona"
    } else if (identical(input$samples, c("prolific"))) {
      "prolific"
    } else {
      "merged"
    }

    model_results_list <- list()

      for (outcome in input$outcome_vars) {
        filename <- glue("precomputed/{dataset_name}__{outcome}.rds")
        model_results_list[[outcome]] <- readRDS(filename) %>%
          filter(predictor %in% input$predictor_vars)
      }
return(bind_rows(model_results_list))#, .id = "outcome_id"))
  })


  output$descriptives_plot <- renderPlot({
    req(input$plot_var)
    df <- filtered_data()

    var <- input$plot_var
# add title of dataset
    if (is.numeric(df[[var]])) {
      ggplot(df, aes(x =  .data[[var]], fill = "sample")) +
        geom_density(alpha = 0.5) +
        labs(title = glue::glue("Density Plot of {var} by Sample"),
             x = var, y = "Density") +
        theme_minimal()

    } else if (is.factor(df[[var]]) || is.character(df[[var]])) {
      df[[var]] <- as.factor(df[[var]])

      ggplot(df, aes(x =  .data[[var]], fill = "sample")) +
        geom_bar(position = "dodge") +
        labs(title = glue::glue("Bar Plot of {var} by Sample"),
             x = var, y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot() + theme_void() + ggtitle("Unsupported variable type.")
    }
  })

  output$heatmap_plot_plotly <- renderPlotly({
    df <- model_results()
    if (input$filter_sig) df <- df %>% filter(p.value < 0.05)
    df <- df %>% mutate(log_odds = log(estimate)) #%>%
    #  filter(!term %in% c("(Intercept)","Intercept","intercept"))
    df <- tidyr::complete(df, term, outcome, fill = list(log_odds = NA))

    df <- df %>%
      mutate(
        term = factor(term, levels = unique(term)),
        outcome = factor(outcome, levels = unique(outcome))
      )

static_plot <-    ggplot(df, aes(x = predictor, y = outcome, fill = log_odds)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Odds Ratios (log scale)", x = "Predictors", y = "Outcome")

p <- plotly::ggplotly(static_plot)

p

  })

  output$heatmap_plot <- renderPlot({
    df <- model_results()
    if (input$filter_sig) df <- df %>% filter(p.value < 0.05)
    df <- df %>% mutate(log_odds = log(estimate)) #%>%
     # filter(!term %in% c("(Intercept)","Intercept","intercept"))
    ggplot(df, aes(x = predictor, y = outcome, fill = log_odds)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Odds Ratios (log scale)", x = "Predictors", y = "Outcome")
  })

  output$table_out <- DT::renderDT({
    df <-  model_results()
    # term,estimate,std.error,statistic,p.value,conf.low,conf.high,predictor,outcome,null.deviance,df.null,logLik,AIC,BIC,deviance,df.residual,nobs
    if (input$filter_sig) df <- df %>% filter(p.value < 0.05)
    df <- df %>%
      select(outcome, predictor, term, estimate, p.value, conf.low, conf.high,nobs) %>%
      mutate(across(c(estimate, conf.low, conf.high), ~ round(.x, 3)),
             p.value = format.pval(p.value, digits = 2, scientific = TRUE)) %>%
      mutate(log_odds = log(estimate)) %>%
      arrange(outcome, predictor)
  })
}
# Create the Shiny app
shinyApp(ui = ui, server = server)

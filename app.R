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
      if (interactive()) {
        selectInput("plot_var", "Select a Variable to Plot", choices = NULL)
      },
      checkboxGroupInput("samples", "Filter by Sample",
        choices = c("prolific", "SONA"),
        selected = c("prolific", "SONA")
      ),
      hr(),
      selectInput("outcome_vars", "Select Outcomes for Logistic Regression",
        choices = NULL, multiple = TRUE
      ),
      selectInput("predictor_vars", "Select Predictors for Logistic Regression",
        choices = NULL, multiple = TRUE
      ),
      checkboxInput("filter_sig", "Show Only p < .05 in Heatmap", FALSE),
      checkboxInput("interactive", "Make Heatmap Interactive", TRUE),
      actionButton("run_model", "Run Logistic Models")
    ),
    mainPanel(
      tabsetPanel(
        if (interactive()) {
          tabPanel("Distribution Plot", plotOutput("descriptives_plot"))
        },
        tabPanel(
          "Odds Ratios Heatmap",
          conditionalPanel(
            condition = "input.interactive == true",
            plotlyOutput("heatmap_plot_plotly")
          ),
          conditionalPanel(
            condition = "input.interactive == false",
            plotOutput("heatmap_plot")
          )
        ),
        tabPanel(
          "Model Results",
          conditionalPanel(
            condition = "input.table_format == 'Summary Table'",
            DTOutput("table_out")
          ),
          conditionalPanel(
            condition = "input.table_format == 'Regression Table'",
            DTOutput("regression_style_table")
          ),
          radioButtons("table_format", "Display Format",
            choices = c("Summary Table", "Regression Table"),
            selected = "Summary Table", inline = TRUE
          )
        )
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
    updateSelectInput(session, "outcome_vars", choices = outcome_choices, selected = "research_type_12")
    updateSelectInput(session, "predictor_vars",
      choices = predictor_choices,
      selected = "race_combined"
    )
  })


  filtered_data <- reactive({
    req(input$samples)
    merged_data %>% filter(sample %in% input$samples)
  })


  model_results <- eventReactive(input$run_model, {
    req(input$outcome_vars, input$predictor_vars)

    dataset_name <- if (identical(input$samples, c("SONA"))) {
      "sona"
    } else if (identical(input$samples, c("prolific"))) {
      "prolific"
    } else {
      "merged"
    }

    model_results_list <- list()

    for (outcome in input$outcome_vars) {
      filename <- glue("data/{dataset_name}__{outcome}.rds")
      model_results_list[[outcome]] <- readRDS(filename) %>%
        filter(predictor %in% input$predictor_vars)
    }
    return(bind_rows(model_results_list)) # , .id = "outcome_id"))
  })


  output$descriptives_plot <- renderPlot({
    req(input$plot_var)
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(NULL)
    }
    var <- input$plot_var
    # add title of dataset
    if (is.numeric(df[[var]])) {
      ggplot(df, aes(x = .data[[var]], fill = sample)) +
        geom_density(alpha = 0.5) +
        labs(
          title = glue::glue("Density Plot of {var} by Sample"),
          x = var, y = "Density"
        ) +
        theme_minimal()
    } else if (is.factor(df[[var]]) || is.character(df[[var]])) {
      df[[var]] <- as.factor(df[[var]])

      ggplot(df, aes(x = .data[[var]], fill = sample)) +
        geom_bar(position = "dodge") +
        labs(
          title = glue::glue("Bar Plot of {var} by Sample"),
          x = var, y = "Count"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot() +
        theme_void() +
        ggtitle("Unsupported variable type.")
    }
  })

  output$heatmap_plot_plotly <- renderPlotly({
    df <- model_results()
    if (nrow(df) == 0) {
      return(NULL)
    }
    if (input$filter_sig) df <- df %>% filter(p.value < 0.05)
    df <- df %>% mutate(log_odds = log(estimate)) # %>%
    #  filter(!term %in% c("(Intercept)","Intercept","intercept"))
    df <- tidyr::complete(df, term, outcome, fill = list(log_odds = NA))

    df <- df %>%
      mutate(
        term = factor(term, levels = unique(term)),
        outcome = factor(outcome, levels = unique(outcome))
      )

    static_plot <- ggplot(df, aes(x = predictor, y = outcome, fill = log_odds)) +
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
    if (nrow(df) == 0) {
      return(NULL)
    }
    if (input$filter_sig) df <- df %>% filter(p.value < 0.05)
    df <- df %>% mutate(log_odds = round(log(estimate), digits = 3))

    # filter(!term %in% c("(Intercept)","Intercept","intercept"))
    ggplot(df, aes(x = predictor, y = outcome, fill = log_odds)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Odds Ratios (log scale)", x = "Predictors", y = "Outcome")
  })


  output$table_out <- DT::renderDT({
    df <- model_results()
    if (nrow(df) == 0) {
      return(NULL)
    }
    if (input$filter_sig) df <- df %>% filter(p.value < 0.05)
    # term,estimate,std.error,statistic,p.value,conf.low,conf.high,predictor,outcome,null.deviance,df.null,logLik,AIC,BIC,deviance,df.residual,nobs
    df <- df %>%
      select(outcome, predictor, term, estimate, p.value, conf.low, conf.high, nobs) %>%
      mutate(log_odds = log(estimate)) %>%
      arrange(outcome, predictor)

    DT::datatable(
      df,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv"),
        scrollX = TRUE,
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      colnames = c(
        "Outcome", "Predictor", "Term", "Estimate", "p-Value",
        "CI Lower", "CI Upper", "N Obs", "Log(Odds)"
      )
    ) %>%
      DT::formatRound(columns = c("estimate", "conf.low", "conf.high", "log_odds"), digits = 3) %>%
      DT::formatSignif(columns = "p.value", digits = 2)
  })


  output$regression_style_table <- DT::renderDT({
    df <- model_results()
    if (nrow(df) == 0) {
      return(NULL)
    }
    if (input$filter_sig) {
      df <- df %>% filter(p.value < 0.05)
    }

    df <- df %>%
      mutate(
        model_id = paste0(outcome, "___", predictor),
        stars = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**",
          p.value < 0.05 ~ "*",
          TRUE ~ ""
        ),
        formatted = paste0(round(estimate, 3), stars, "<br>(", round(std.error, 2), ")")
      )

    # Extract coefficients
    coef_rows <- df %>%
      select(model_id, term, formatted)

    # Extract fit statistics
    stats_rows <- df %>%
      select(model_id, AIC, logLik, nobs) %>%
      distinct() %>%
      pivot_longer(cols = c(AIC, logLik, nobs), names_to = "term", values_to = "formatted") %>%
      mutate(formatted = as.character(round(formatted, 3)))
    # Combine everything
    all_rows <- bind_rows(coef_rows, stats_rows)

    # Reshape: term = rows, outcome = columns
    regression_table <- all_rows %>%
      pivot_wider(names_from = model_id, values_from = formatted)

    # Order rows: intercept, predictors, then fit stats
    all_terms <- regression_table$term
    # how about just terms that contain "intercept" and then the rest

    user_intercept <- unique(c("(Intercept)", "intercept", "Intercept", grep("intercept", all_terms, value = TRUE)))
    user_predictors <- unique(c(input$predictor_vars, all_terms))
    user_predictors <- user_predictors[!user_predictors %in% user_intercept]


    ordered_rows <- unique(c(user_intercept, user_predictors, "AIC", "logLik", "nobs"))

    regression_table <- regression_table %>%
      arrange(factor(term, levels = ordered_rows))

    colnames(regression_table)[-1] <- gsub("___.*", "", colnames(regression_table)[-1])

    DT::datatable(
      regression_table,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        pageLength = nrow(regression_table),
        dom = "t",
        ordering = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    )
  })
}
# Create the Shiny app
shinyApp(ui = ui, server = server)

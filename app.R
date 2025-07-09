# Load packages used by the app
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)
library(qrcode)
source("setup.R")

# Set the default theme for ggplot2 plots
# UI
ui <- fluidPage(
  titlePanel("Who is Willing to Participate in Behavior Genetic Research? Exploring Barriers and Predictors."),
  sidebarLayout(
    sidebarPanel(
      if (interactive()) {
        selectInput("plot_var", "Select a Variable to Plot", choices = NULL)
      },
      checkboxGroupInput("samples", "Filter by Sample",
        choices = c("prolific", "SONA"),
        selected = c("prolific")
      ), helpText(
        "Select samples to filter the data. If no samples are selected, all data will be used."
      ), 
      checkboxInput("standardized_predictors", "Standardize Continuous varibles", TRUE),
      hr(),
      selectInput("outcome_vars", "Select Outcomes for Logistic Regression",
        choices = NULL, multiple = TRUE
      ),
      selectInput("predictor_vars", "Select Predictors for Logistic Regression",
        choices = NULL, multiple = TRUE
      ),
      actionButton("run_model", "Run Logistic Models"),
      hr(),
      checkboxInput("heatmap_interactive", "Make Heatmap Interactive", TRUE),
      hr(),
      h4("Access App on Mobile"),
      # click goes to
      uiOutput("qrcode_link"),
      helpText("Scan the QR code or click it to open the app in a new tab."),
    ),
    mainPanel(
      tabsetPanel(
        if (interactive()) {
          tabPanel("Distribution Plot", plotOutput("descriptives_plot"))
        },
        tabPanel(
          "About",
          h3("About This Study"),
          p(
            "The present study aimed to explore", tags$ul(
              tags$li(
                "the willingness of individuals within the general public to participate in genetic research,"
              ),
              tags$li("whether willingness to participate in genetic research differed by age, gender, race, ethnicity, and education level,"),
              tags$li("whether an individual’s trust in research establishment, knowledge of genetics, level of worry, health anxiety, altruism, health status (for themselves or a loved one) predicted their willingness to participate in genetics research.,")
            ),
            "For individuals unwilling to provide a saliva sample/blood sample for genetic research, we explored",
            tags$ul(
              tags$li("the primary reason for their unwillingness and "),
              tags$li("factors that impact their decision to participate in genetic research (e.g., compensation, confidentiality, analytic approach, topic studied, feedback options).")
            )
          ),
          h3("About This App"),
          p(
            "This interactive Shiny application allows users to explore predictors of willingness to participate in behavior genetic research.
     Users can visualize variable distributions across participant samples (Prolific and SONA), view outcome definitions, and run logistic regression models
     to examine how predictors relate to willingness to participate in different research types."
          ),
          p(
            strong("Important: "),
            "To view model results (tables and heatmaps), you must first select outcomes and predictors, then press the ",
            strong("Run Logistic Models"),
            " button in the sidebar. Model results will not appear or update until this is done."
          ),
          h3("Authors"),
          tags$ul(
            tags$li("Shannon M. O’Connor, University of Toledo"),
            tags$li("S. Mason Garrison, Wake Forest University")
          ), h4("Contact"),
          p("For questions or feedback, please contact: ", tags$a(href = "mailto:garrissm@wfu.edu", "garrissm@wfu.edu"), "or ", tags$a(href = "mailto:Shannon.OConnor@utoledo.edu", "Shannon.OConnor@utoledo.edu")),
          h4("Source Code"),
          p(
            "The full source code is available on ",
            tags$a(
              href = "https://github.com/R-Computing-Lab/public-science",
              "GitHub",
              target = "_blank"
            ),
            "."
          )
        ),
        tabPanel(
          "Outcome Variable Reference",
          helpText("This table provides descriptions of the outcome variables used in the logistic regression models."),
          h2("Outcome Prompt"),
          p("Please take your time and read through the following list. Assume that you would be compensated for your time. This is hypothetical, so also assume that you have free time that would allow you to participate."),
          DTOutput("outcome_reference_table")
        ),
        tabPanel(
          "Odds Ratios Heatmap",
          helpText("This heatmap displays the odds ratios for each predictor variable across different outcome variables."),
          checkboxInput("filter_sig_heat", "Show Only p < .05 in Heatmap", FALSE),
          conditionalPanel(
            condition = "input.heatmap_interactive == true",
            plotlyOutput("heatmap_plot_plotly")
          ),
          conditionalPanel(
            condition = "input.heatmap_interactive == false",
            plotOutput("heatmap_plot")
          )
        ),
        tabPanel(
          "Model Results",
          radioButtons("table_format", "Display Format",
            choices = c("Summary Table", "Regression Table"),
            selected = "Regression Table", inline = TRUE
          ),
          conditionalPanel(
            helpText("This table displays the results of the logistic regression models, including estimates, p-values, and confidence intervals."),
            condition = "input.table_format == 'Summary Table'",
            DTOutput("table_out")
          ),
          conditionalPanel(
            helpText("This table displays the results of the logistic regression models in a format similar to regression tables, with coefficients, standard errors, and significance stars. Each column represents a different model."),
            condition = "input.table_format == 'Regression Table'",
            DTOutput("regression_style_table")
          ),
          checkboxInput("filter_sig_table", "Show Only p < .05 in Tables", FALSE)
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
    updateSelectInput(session, "outcome_vars",
      choices = outcome_choices,
      selected = c("research_type_9updated", "research_type_10updated")
    )
    updateSelectInput(session, "predictor_vars",
      choices = predictor_choices,
      selected = "age"
    )
  })


  filtered_data <- reactive({
    req(input$samples)
    merged_data %>% filter(sample %in% input$samples)
  })


  output$qrcode_img <- renderImage(
    {
      tmpfile <- tempfile(fileext = ".png")
      url <- "https://smasongarrison-publicscience.share.connect.posit.cloud/"
      png(tmpfile, width = 300, height = 300)
      plot(qrcode::qr_code(url))
      dev.off()
      list(
        src = tmpfile,
        contentType = "image/png",
        alt = "QR Code",
        width = 150,
        height = 150
      )
    },
    deleteFile = TRUE
  )

  output$qrcode_link <- renderUI({
    tags$a(
      href = "https://smasongarrison-publicscience.share.connect.posit.cloud/",
      target = "_blank",
      imageOutput("qrcode_img", width = "50%", height = "50%")
    )
  })
  #

  model_results <- eventReactive(input$run_model, {
    req(input$outcome_vars, input$predictor_vars)

    dataset_name <- if (identical(input$samples, c("SONA"))) {
      "sona"
    } else if (identical(input$samples, c("prolific"))) {
      "prolific"
    } else {
      "merged"
    }

    standardized_predictors <- if (input$standardized_predictors == TRUE) {
      "standardized"
    } else {
      "raw"
    }
    model_results_list <- list()

    for (outcome in input$outcome_vars) {
      filename <- glue("data/{standardized_predictors}_{dataset_name}__{outcome}.rds")
      model_results_list[[outcome]] <- readRDS(filename) %>%
        filter(predictor %in% input$predictor_vars)
    }
    return(bind_rows(model_results_list)) # , .id = "outcome_id"))
  })
  outcome_reference <- tibble::tibble(
    Variable_name = c(
      "research_type_1",
      "research_type_2",
      "research_type_3",
      "research_type_4",
      "research_type_5",
      "research_type_6",
      "research_type_7",
      "research_type_8",
      "research_type_9updated",
      "research_type_10updated",
      "research_type_11",
      "research_type_12",
      "research_type_13"
    ),
    Description = c(
      "Research Type 1: Questionnaire/Survey",
      "Research Type 2: Focus group in-person",
      "Research Type 3: Focus group online",
      "Research Type 4: One-on-one interview in-person",
      "Research Type 5: One-on-one interview on phone",
      "Research Type 6: One-on-one interview via Zoom",
      "Research Type 7: Saliva for hormone levels",
      "Research Type 8: Blood for hormone levels",
      "Research Type 9: Saliva for genetic research",
      "Research Type 10: Blood for genetic research",
      "Research Type 11: Phone access",
      "Research Type 12: Eye-tracking",
      "Research Type 13: EEG, MRI, PET scan"
    )
  )


  # Render the outcome reference table
  output$outcome_reference_table <- DT::renderDT({
    DT::datatable(
      outcome_reference,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = nrow(outcome_reference),
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))
      )
    )
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
    if (input$filter_sig_heat == TRUE) df <- df %>% filter(p.value < 0.05)
    if (nrow(df) == 0) {
      return(NULL)
    }
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
    if (input$filter_sig_heat == TRUE) df <- df %>% filter(p.value < 0.05)
    if (nrow(df) == 0) {
      return(NULL)
    }

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
    if (input$filter_sig_table == TRUE) df <- df %>% filter(p.value < 0.05)
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
    if (input$filter_sig_table == TRUE) {
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

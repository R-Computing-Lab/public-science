library(shiny)
library(tidyverse)
library(ggplot2)
library(janitor)
library(haven)
library(gt)
library(broom)
library(stringr)
library(plotly)
library(DT)
library(glue)

## Installs library if missing
install_packages_if_missing <- function(packages) {
  install_if_missing <- function(packages) {
    if (!require(package, quietly = TRUE)) {
      install.packages(packages)
      library(packages, character.only = TRUE)
    }
  }

  install_github_if_missing <- function(repo) {
    if (!require(repo, quietly = TRUE)) {
      remotes::install_github(repo)
      library(repo, character.only = TRUE)
    }
  }

  lapply(packages, function(packages) {
    if (startsWith(package, "github")) {
      install_github_if_missing(packages)
    } else {
      install_if_missing(packages)
    }
  })
}


## good workaround for stupid default setting; running mean function but we are overriding default
new_mean <- function(x) {
  mean(x, na.rm = TRUE)
}

# Load and merge data once on startup
load_clean_data <- function(return_all = FALSE) {
  if (!file.exists("data/Prolific - Willingness to Participate in Research_Cleaning_v2.sav") ||
    !file.exists("data/SONA - Willingness to Participate in Research_Cleaning_v2.sav")) {
    #  stop("Data files not found. Please ensure the data files are in the 'data' directory.")
    names_df <- read.csv("data/predictor_vars.csv") %>% rename("name" = x)
    merged_data <- setNames(as.data.frame(matrix(nrow = 0, ncol = nrow(names_df))), names_df$name) %>% select(-c(
    
      attention_check3,
      attention_check1r, attention_check2r,
      attention_check3r, 
      research_type_9,
      research_type_10,
      miss_items_hai,
      miss_items_pswq,
      submissionid,
      statusv_p,
      customstudytncsacceptedatv_p,
      startedatv_p,
      completedatv_p,
      reviewedatv_p,
      archivedatv_p,
      timetakenv_p,
      totalapprovalsv_p,
      duration_minutes,
      attention_check2_sona,
      attention_check3_sona
    ))
    return(merged_data)
  } else {
    prolific <- read_sav("data/Prolific - Willingness to Participate in Research_Cleaning_v2.sav") %>%
      janitor::clean_names() %>%
      mutate(
        sample = "prolific",
        age = as.numeric(age),
        gender = fct_recode(as_factor(gender, levels = "labels"),
          "Prefer not to say" = "I would not like to answer"
        ),
        race_combined = fct_recode(as_factor(race_combined, levels = "labels"),
          "Black/African American" = "Black/ African American"
        )
      ) %>%
      select(-c(
        start_date, end_date, recorded_date,
        response_id, distribution_channel,
        user_language, define_genetic_researc,
        define_change, why_change,
        race_other, organized_religion_8_text,
        major_23_text, industry_57_text,
        info_needed, why_concern, why_options,
        no_hormone_explain, no_blood_sample_5_text, highesteducation_8_text,
        concern_mental_6_text, concern_medical_6_text, comment, prolific_id, prolific_pid # , sample
      ))

    sona <- read_sav("data/SONA - Willingness to Participate in Research_Cleaning_v2.sav") %>%
      janitor::clean_names() %>%
      rename(age_factor = age, age = age_num) %>%
      mutate(
        sample = "SONA",
        age = as.numeric(age),
        gender = as_factor(gender, levels = "labels"),
        race_combined = fct_recode(as_factor(race_combined, levels = "labels"),
          "Black/African American" = "Black/African American"
        )
      ) %>%
      rename(
        attention_check2_sona = attention_check2,
        attention_check3_sona = attention_check3,
        household_income_sona = household_income,
        more_info_updated_sona = more_info_updated
      ) %>%
      select(-c(
        start_date, end_date, recorded_date,
        response_id, distribution_channel,
        user_language, define_genetic_researc,
        define_change, why_change,
        race_other, organized_religion_8_text,
        major_23_text, # industry_57_text,
        info_needed, why_concern, why_options,
        no_hormone_explain, no_blood_sample_5_text, # highesteducation_8_text,
        concern_mental_6_text, concern_medical_6_text, comment # , #prolific_id, prolific_pid#, sample
      ))

    merged_data <- full_join(prolific, sona) %>%
      mutate(sample = factor(sample, levels = c("prolific", "SONA")))

    categorical_vars <- c("sample", "gender", "race_combined", "ethnicity")
    merged_data <- merged_data %>%
      mutate(across(all_of(intersect(categorical_vars, names(.))), as_factor))

    date_columns <- c("start_date", "end_date", "recorded_date")
    merged_data <- merged_data %>%
      mutate(across(all_of(intersect(date_columns, names(.))), \(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")))
  }
  if (return_all == TRUE) {
    list(merged = merged_data, sona = sona, prolific = prolific)
  } else {
    merge <- merged_data
    return(merge)
  }
}

# Logistic regression + OR computation (your defined function)
compute_odds_ratios <- function(df, outcomes, predictors = NULL) {
  df <- df %>%
    select_if(~ is.numeric(.) || (is.factor(.) && nlevels(.) > 1)) %>%
    mutate(across(where(is.factor), droplevels))

  if (is.null(predictors)) {
    predictors <- names(df)
  } else {
    predictors <- predictors[predictors %in% names(df)]
  }

  results <- list()
  for (outcome in outcomes) {
    df[[outcome]] <- as.factor(df[[outcome]])

    if (!outcome %in% names(df)) {
      warning(glue("Outcome variable '{outcome}' not found in the dataset. Skipping."))
      next
    }

    odds_ratios <- lapply(setdiff(predictors, outcome), function(var) {
      model <- tryCatch(
        {
          glm(as.formula(paste(outcome, "~", var)),
            data = df, family = binomial
          )
        },
        error = function(e) {
          warning(glue("Error fitting model for outcome '{outcome}' with predictor '{var}': {e$message}"))
          return(NULL)
        }
      )
      if (is.null(model)) {
        return(NULL)
      }
      ci <- tryCatch(
        {
          confint(model)
        },
        error = function(e) {
          warning(glue("Error computing confidence intervals for outcome '{outcome}' with predictor '{var}': {e$message}"))
          return(NULL)
        }
      )
      if (is.null(ci)) {
        rownames_ci <- c("(Intercept)", var)
      } else {
        rownames_ci <- rownames(ci)
        ci <- as.data.frame(ci) %>%
          rownames_to_column(var = "term") %>%
          rename(lower_ci = `2.5 %`, upper_ci = `97.5 %`) %>%
          mutate(term = rownames_ci)
      }
      tidy_model <- tryCatch(
        {
          broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
            #  cbind(ci) %>%
            mutate(
              term = str_replace(term, "\\(Intercept\\)", ifelse(length(rownames_ci) > 2,
                paste0(var, "_intercept"),
                "intercept"
              )),
              term = str_replace(term, "get\\(var\\)", ifelse(length(rownames_ci) > 2,
                paste0(var, "_"),
                var
              )),
              predictor = var,
              outcome = outcome
            )
        },
        error = function(e) {
          warning(glue("Error tidying model for outcome '{outcome}' with predictor '{var}': {e$message}"))
          return(NULL)
        }
      )
      if (is.null(tidy_model)) {
        return(NULL)
      }

      glance <- broom::glance(model) %>%
        mutate(outcome = outcome, predictor = var)
      tidy_model <- tidy_model %>%
        left_join(glance, by = c("outcome", "predictor"))

      return(tidy_model)
    })
    results[[outcome]] <- bind_rows(odds_ratios)
  }
  return(bind_rows(results))
}

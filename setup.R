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

load_variable_names <- function() {
  if (!file.exists("data/available_vars.csv")) {
    warning("Variable names file not found. Please ensure 'available_vars.csv' is in the 'data' directory.")
    # use full list
    if (!file.exists("data/predictor_vars.csv")) {
      prod_predictor_vars <- read.csv("data/names.csv") %>% rename("name" = x)
      
    } else {
    prod_predictor_vars <- read.csv("data/predictor_vars.csv") %>% rename("name" = x)
}  
  } else {
    # use available vars
    prod_predictor_vars <- read.csv("data/available_vars.csv") %>% rename("name" = x)
  }
  return(prod_predictor_vars)
}


# Load and merge data once on startup
load_clean_data <- function(return_all = FALSE) {
  prolific_file <- "data/Prolific - Willingness to Participate in Research_Cleaning_v3.sav"
  sona_file <- "data/SONA - Willingness to Participate in Research_Cleaning_v2.sav"

  if (!file.exists(prolific_file) ||
    !file.exists(sona_file)) {
    #  stop("Data files not found. Please ensure the data files are in the 'data' directory.")
  #  names_df <- read.csv("data/predictor_vars.csv") %>% rename("name" = x)
    names_df <- read.csv("data/names.csv") %>% rename("name" = x)
    merged_data <- setNames(as.data.frame(matrix(nrow = 0, ncol = nrow(names_df))), names_df$name) # %>% select(-c(
    #  research_type_9,
  #    research_type_10
   # ))
    return(merged_data)
  } else {
  
  prolific <- read_sav(prolific_file) %>%
    janitor::clean_names() %>%
    mutate(
      sample = "prolific",
      age = as.numeric(age),
      household_income = case_when(
        household_income %in% c(8, 9) ~ NA,
        TRUE ~ household_income
      ),
      gender = fct_recode(as_factor(gender, levels = "labels"),
        "Prefer not to say" = "I would not like to answer"
      ),
      race_combined = fct_recode(as_factor(race_combined, levels = "labels"),
        "Black/African American" = "Black/ African American"
      ),
        ethnicityv_p = fct_recode(as_factor(ethnicityv_p),
        "White/Caucasian" = "White"
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



  sona <- read_sav(sona_file) %>%
    janitor::clean_names() %>%
    rename(age_factor = age, age = age_num) %>%
    mutate(
      sample = "SONA",
      age = as.numeric(age),
      gender = as_factor(gender, levels = "labels"),
      household_income = case_when(
        household_income %in% c(8, 9) ~ NA,
        TRUE ~ household_income
      ),
      race_combined = fct_recode(as_factor(race_combined, levels = "labels"),
        "Black/African American" = "Black/African American"
      )
    ) %>%
    rename(
      attention_check2_sona = attention_check2,
      attention_check3_sona = attention_check3#,
  #    more_info_updated_sona = more_info_updated
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
    mutate(
      sample = factor(sample, levels = c("prolific", "SONA")),
      organized_religion_collapse = case_when(
        organized_religion %in% c(1) ~ "Non-religious",
        organized_religion %in% c(2) ~ "Christianity",
        organized_religion %in% c(3:8) ~ "Other religion"
      ),
      race_combined = case_when(
        race_combined %in% c(
          "Amercian Indian/Alaska Native",
          "Amercian Indian/Alaskan Native"
        ) ~ "Amercian Indian/Alaska Native",
        race_combined == "Would not like to indicate" ~ NA,
        TRUE ~ race_combined
      ),
      race_NHtoAsian = as_factor(case_when(
        race_combined %in% c("Native Hawaiian or Other Pacific Islander") ~ "Asian",
        TRUE ~ race_combined
      )),
      health_self_total = rowSums(cbind(
        medical_condition_1_1, medical_condition_2_1,
        medical_condition_3_1, medical_condition_4_1,
        medical_condition_5_1, medical_condition_6_1,
        medical_condition_7_1, medical_condition_8_1,
        medical_condition_9_1, medical_condition_10_1
      ), na.rm = TRUE),
      health_self_total = ifelse(is.na(health_self_total), 0, health_self_total),
      health_self_any = ifelse(health_self_total > 0, TRUE, FALSE),
      health_firstdegree_total = rowSums(cbind(
        medical_condition_1_2, # Cancer
        medical_condition_2_2, # Heart Disease
        medical_condition_3_2, # Diabetes
        medical_condition_4_2, # Alzheimer
        medical_condition_5_2, # Depression
        medical_condition_6_2, # Bipolar
        medical_condition_7_2, # Anxiety
        medical_condition_8_2, # Schizophrenia
        medical_condition_9_2, # Substance Abuse
        medical_condition_10_2 # Eating Disorder
      ), na.rm = TRUE),
      health_firstdegree_total = ifelse(is.na(health_firstdegree_total), 0, health_firstdegree_total),
      health_firstdegree_any = ifelse(health_firstdegree_total > 0, TRUE, FALSE),
      health_2nddegree_total = rowSums(cbind(
        medical_condition_1_3, medical_condition_2_3,
        medical_condition_3_3, medical_condition_4_3,
        medical_condition_5_3, medical_condition_6_3,
        medical_condition_7_3, medical_condition_8_3,
        medical_condition_9_3, medical_condition_10_3
      ), na.rm = TRUE),
      health_2nddegree_total = ifelse(is.na(health_2nddegree_total), 0, health_2nddegree_total),
      health_2nddegree_any = ifelse(health_2nddegree_total > 0, TRUE, FALSE),
      health_relative_total = rowSums(cbind(health_2nddegree_total, health_firstdegree_total), na.rm = TRUE),
      health_relative_any = ifelse(health_relative_total > 0, TRUE, FALSE),
      mhealth_self_total = rowSums(cbind(
        # medical_condition_1_1, medical_condition_2_1,
        # medical_condition_3_1, medical_condition_4_1,
        medical_condition_5_1, medical_condition_6_1,
        medical_condition_7_1, medical_condition_8_1,
        medical_condition_9_1, medical_condition_10_1
      ), na.rm = TRUE),
      mhealth_self_total = ifelse(is.na(mhealth_self_total), 0, mhealth_self_total),
      mhealth_self_any = ifelse(mhealth_self_total > 0, TRUE, FALSE),
      mhealth_firstdegree_total = rowSums(cbind(
        #  medical_condition_1_2, # Cancer
        #  medical_condition_2_2, # Heart Disease
        #   medical_condition_3_2, # Diabetes
        #  medical_condition_4_2, # Alzheimer
        medical_condition_5_2, # Depression
        medical_condition_6_2, # Bipolar
        medical_condition_7_2, # Anxiety
        medical_condition_8_2, # Schizophrenia
        medical_condition_9_2, # Substance Abuse
        medical_condition_10_2 # Eating Disorder
      ), na.rm = TRUE),
      mhealth_firstdegree_total = ifelse(is.na(mhealth_firstdegree_total), 0, mhealth_firstdegree_total),
      mhealth_firstdegree_any = ifelse(mhealth_firstdegree_total > 0, TRUE, FALSE),
      mhealth_2nddegree_total = rowSums(cbind(
        #  medical_condition_1_3, medical_condition_2_3,
        #  medical_condition_3_3, medical_condition_4_3,
        medical_condition_5_3, medical_condition_6_3,
        medical_condition_7_3, medical_condition_8_3,
        medical_condition_9_3, medical_condition_10_3
      ), na.rm = TRUE),
      mhealth_2nddegree_total = ifelse(is.na(mhealth_2nddegree_total), 0, mhealth_2nddegree_total),
      mhealth_2nddegree_any = ifelse(mhealth_2nddegree_total > 0, TRUE, FALSE),
      mhealth_relative_total = rowSums(cbind(mhealth_2nddegree_total, mhealth_firstdegree_total), na.rm = TRUE),
      mhealth_relative_any = ifelse(mhealth_relative_total > 0, TRUE, FALSE),
      phealth_self_total = rowSums(cbind(
        medical_condition_1_1, medical_condition_2_1,
        medical_condition_3_1, medical_condition_4_1 # ,
        # medical_condition_5_1, medical_condition_6_1,
        #  medical_condition_7_1, medical_condition_8_1,
        #  medical_condition_9_1, medical_condition_10_1
      ), na.rm = TRUE),
      phealth_self_total = ifelse(is.na(phealth_self_total), 0, phealth_self_total),
      phealth_self_any = ifelse(phealth_self_total > 0, TRUE, FALSE),
      phealth_firstdegree_total = rowSums(cbind(
        medical_condition_1_2, # Cancer
        medical_condition_2_2, # Heart Disease
        medical_condition_3_2, # Diabetes
        medical_condition_4_2 # , # Alzheimer
        #   medical_condition_5_2, # Depression
        #   medical_condition_6_2, # Bipolar
        #   medical_condition_7_2, #Anxiety
        #   medical_condition_8_2, # Schizophrenia
        #   medical_condition_9_2,  #Substance Abuse
        #   medical_condition_10_2 # Eating Disorder
      ), na.rm = TRUE),
      phealth_firstdegree_total = ifelse(is.na(phealth_firstdegree_total), 0, phealth_firstdegree_total),
      phealth_firstdegree_any = ifelse(phealth_firstdegree_total > 0, TRUE, FALSE),
      phealth_2nddegree_total = rowSums(cbind(
        medical_condition_1_3, medical_condition_2_3,
        medical_condition_3_3, medical_condition_4_3 # ,
        # medical_condition_5_3, medical_condition_6_3,
        # medical_condition_7_3, medical_condition_8_3,
        # medical_condition_9_3, medical_condition_10_3
      ), na.rm = TRUE),
      phealth_2nddegree_total = ifelse(is.na(phealth_2nddegree_total), 0, phealth_2nddegree_total),
      phealth_2nddegree_any = ifelse(phealth_2nddegree_total > 0, TRUE, FALSE),
      phealth_relative_total = rowSums(cbind(
        phealth_2nddegree_total,
        phealth_firstdegree_total
      ), na.rm = TRUE),
      phealth_relative_any = ifelse(phealth_relative_total > 0, TRUE, FALSE),
      education_levels = factor(case_when(
        highesteducation %in% c(1, 2) ~ "Not completed high school",
        highesteducation %in% c(3, 4, 5) ~ "Completed high school",
        highesteducation %in% c(6) ~ "Completed 4 year college",
        highesteducation == 7 ~ "Graduate degree",
        highesteducation == 8 ~ "Other",
        highesteducation == 9 ~ NA
      )),
      education_levels_noanswer = factor(case_when(
        highesteducation %in% c(1, 2) ~ "Not completed high school",
        highesteducation %in% c(3, 4, 5) ~ "Completed high school",
        highesteducation %in% c(6) ~ "Completed 4 year college",
        highesteducation == 7 ~ "Graduate degree",
        highesteducation == 8 ~ "Other",
        highesteducation == 9 ~ "I would not like to answer"
      )),
      highesteducation_mths = case_when(
        highesteducation %in% c(1, 2, 3, 8) ~ 0,
        highesteducation %in% c(4, 5, 6, 7) ~ 1,
        highesteducation == 9 ~ NA
      ),
      gender_collapsed = case_when(
        gender %in% c(
          "Non-binary",
          "Transgender Female",
          "Prefer not to say",
          "Transgender Male"
        ) ~ "Non-cisgender",
        TRUE ~ gender
      ),
      organized_religion_collapseChristianity = fct_relevel(organized_religion_collapse,
                                                            "Christianity", after = Inf)

      )
  categorical_vars <- c(
    "education_levels",
    "education_levels_noanswer",
    "ethnicity",
    "ethnicitysimplifiedv_p",
    "ethnicityv_p",
    "ethnicityv_p_cat",
    "ethnicitysimplifiedv_p_cat",
    "gender",
    "gender_collapsed",
    "health_2nddegree_any",
    "health_firstdegree_any",
    "health_relative_any",
    "health_self_any",
    "highesteducation",
    "highesteducation_mths",
    "mhealth_2nddegree_any",
    "mhealth_firstdegree_any",
    "mhealth_relative_any",
    "mhealth_self_any",
    "organized_religion_collapse",
    "organized_religion_collapseChristianity",
    "phealth_2nddegree_any",
    "phealth_firstdegree_any",
    "phealth_relative_any",
    "phealth_self_any",
    "race_NHtoAsian",
    "race_combined",
    "religion",
    "religion_other",
    "sample"
  )
      if(("highest_ed_recoded" %in% names(merged_data))) {
        merged_data <- merged_data %>%
          mutate(
            highest_ed_recoded_factor = factor(case_when(
              highest_ed_recoded == 1 ~ "Less than 4 years of college",
              highest_ed_recoded == 2 ~ "4 years of college",
              highest_ed_recoded == 3 ~ "Graduate school",
              highest_ed_recoded == 4 ~ "Current student",
            ),
            levels = c("Less than 4 years of college",
                       "4 years of college",
                       "Graduate school",
                       "Current student")),
            highest_ed_recoded_factor_ref4y = factor(case_when(
              highest_ed_recoded == 1 ~ "Less than 4 years of college",
              highest_ed_recoded == 2 ~ "4 years of college",
              highest_ed_recoded == 3 ~ "Graduate school",
              highest_ed_recoded == 4 ~ "Current student",
            ),
            levels = c("4 years of college",
                       "Less than 4 years of college",
                       "Graduate school",
                       "Current student"))

            )
        categorical_vars   <- c(categorical_vars,
          "highest_ed_recoded",
          "highest_ed_recoded_ref4y",
          "highest_ed_recoded_factor")

      }
    if(("highest_ed_recoded_currentcoded" %in% names(merged_data))) {
    merged_data <- merged_data %>%
      mutate(
      highest_ed_recoded_currentcoded_factor = factor(case_when(
        highest_ed_recoded_currentcoded == 1 ~ "Less than 4 years of college",
        highest_ed_recoded_currentcoded == 2 ~ "4 years of college",
        highest_ed_recoded_currentcoded == 3 ~ "Graduate school"
      ),
      levels = c("Less than 4 years of college",
                 "4 years of college",
                 "Graduate school")),
      highest_ed_recoded_currentcoded_factor_ref4y = factor(case_when(
        highest_ed_recoded_currentcoded == 1 ~ "Less than 4 years of college",
        highest_ed_recoded_currentcoded == 2 ~ "4 years of college",
        highest_ed_recoded_currentcoded == 3 ~ "Graduate school"
      ),
      levels = c("4 years of college",
                 "Less than 4 years of college",
                 "Graduate school"))

      )
    
        categorical_vars   <- c(categorical_vars,
                            "highest_ed_recoded_currentcoded",
                            "highest_ed_recoded_currentcoded_factor_ref4y",
                            "highest_ed_recoded_currentcoded_factor")
    }
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

# Logistic regression + OR computation
# Logistic regression + OR computation (your defined function)
compute_odds_ratios <- function(df, outcomes, predictors = NULL) {
  df <- df %>%
    select_if(~ is.numeric(.) || is.integer(.) || is.logical(.) || (is.factor(.) && nlevels(.) > 1)) %>%
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
      reference_level <- if (is.factor(df[[var]])) {
        levels(df[[var]])[1]
      } else {
        NA
      }

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
              outcome = outcome,
              reference_level = reference_level
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

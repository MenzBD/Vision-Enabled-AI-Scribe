# =============================================================================
# HELPER FUNCTIONS FOR MENZ ET AL MULTIMODAL AI SCRIBE STUDY
# FINAL VERSION:
# =============================================================================

# =============================================================================
# DATA PREPARATION FUNCTIONS
# =============================================================================

#' Prepare and clean the dataset
#' @param file_path Path to the CSV file
#' @return Cleaned dataset with calculated indicators
prepare_data <- function(file_path = "ds_final.csv") {
  ds <- read.csv(file_path, stringsAsFactors = FALSE)[,1:12]
  
  ds <- ds %>%
    filter(!is.na(Category) & Category != "") %>%
    mutate(
      # Clean category names
      Category_clean = case_when(
        str_detect(Category, "Patient name|date of birth|ADR|Allergy") ~ "Patient Details",
        str_detect(Category, "Medication") & !str_detect(Category, "Strength") ~ "Medication Name",
        str_detect(Category, "Strength") ~ "Strength & Form",
        str_detect(Category, "Dose|dose|Dosage|dosage") ~ "Dosing directions",
        str_detect(Category, "Indication") ~ "Indication",
        str_detect(Category, "Clinical") ~ "Clinical Notes",
        TRUE ~ "Dosing directions"  # Default to catch any remaining dose-related entries
      ),
      
      # Recode pharmacist names to anonymized codes
      Pharmacist = case_when(
        Pharmacist == "Vidya" ~ "p.1",
        Pharmacist == "Nicholas" ~ "p.2",
        Pharmacist == "Eugene" ~ "p.3",
        Pharmacist == "Dib" ~ "p.4",
        Pharmacist == "Natansh" ~ "p.5",
        Pharmacist == "Jimit" ~ "p.6",
        Pharmacist == "Lee" ~ "p.7",
        Pharmacist == "Dorsa" ~ "p.8",
        Pharmacist == "Erik" ~ "p.9",
        Pharmacist == "Kezia" ~ "p.10",
        TRUE ~ Pharmacist  # Keep original if not found (safety fallback)
      ),
      
      # Create correct indicators
      Multimodal_correct = ifelse(trimws(Multimodal_outcome) %in% c("Correct", "Correct - additional"), 1, 0),
      Audio_correct = ifelse(trimws(Audio_outcome) %in% c("Correct", "Correct - additional"), 1, 0),
      
      # Create error indicators
      Multimodal_incorrect = ifelse(trimws(Multimodal_outcome) == "Incorrect", 1, 0),
      Audio_incorrect = ifelse(trimws(Audio_outcome) == "Incorrect", 1, 0),
      
      # Create specific error type indicators
      Multimodal_commission = ifelse(Multimodal_incorrect == 1 & 
                                       tolower(trimws(Multimodal_outcome_reason)) == "commission", 1, 0),
      Multimodal_omission = ifelse(Multimodal_incorrect == 1 & 
                                     tolower(trimws(Multimodal_outcome_reason)) == "omission", 1, 0),
      Audio_commission = ifelse(Audio_incorrect == 1 & 
                                  tolower(trimws(Audio_outcome_reason)) == "commission", 1, 0),
      Audio_omission = ifelse(Audio_incorrect == 1 & 
                                tolower(trimws(Audio_outcome_reason)) == "omission", 1, 0)
    )
  
  return(ds)
}

# =============================================================================
# STATISTICAL HELPER FUNCTIONS
# =============================================================================

#' Calculate p-value using McNemar's test for paired data
#' @param data Dataframe with paired observations
#' @param method1_correct Column name for method 1 correct indicator
#' @param method2_correct Column name for method 2 correct indicator
#' @return p-value from McNemar's test
calculate_mcnemar_p_value <- function(data, method1_correct, method2_correct) {
  # Create McNemar's test contingency table
  # Rows = Method 1 (Video), Columns = Method 2 (Audio)
  both_correct <- sum(data[[method1_correct]] == 1 & data[[method2_correct]] == 1)
  method1_only <- sum(data[[method1_correct]] == 1 & data[[method2_correct]] == 0)
  method2_only <- sum(data[[method1_correct]] == 0 & data[[method2_correct]] == 1)
  both_incorrect <- sum(data[[method1_correct]] == 0 & data[[method2_correct]] == 0)
  
  # Create 2x2 table for McNemar's test
  mcnemar_table <- matrix(c(both_correct, method1_only, 
                            method2_only, both_incorrect), 
                          nrow = 2, byrow = TRUE,
                          dimnames = list(c("Video_Correct", "Video_Incorrect"),
                                          c("Audio_Correct", "Audio_Incorrect")))
  
  # Perform McNemar's test
  # Handle cases where there are no discordant pairs
  if (method1_only + method2_only == 0) {
    return(1.0)  # Perfect agreement, no difference
  }
  
  # Use exact binomial test for small samples, otherwise continuity correction
  if (method1_only + method2_only < 25) {
    # Exact McNemar's test using binomial distribution
    p_value <- binom.test(method1_only, method1_only + method2_only, p = 0.5)$p.value
  } else {
    # Standard McNemar's test with continuity correction
    tryCatch({
      mcnemar_result <- mcnemar.test(mcnemar_table, correct = TRUE)
      p_value <- mcnemar_result$p.value
    }, error = function(e) {
      # Fallback to exact test if standard fails
      p_value <- binom.test(method1_only, method1_only + method2_only, p = 0.5)$p.value
    })
  }
  
  return(p_value)
}

#' Calculate p-value using McNemar's test for specific error types
#' @param data Dataframe with paired observations
#' @param method1_error Column name for method 1 error indicator
#' @param method2_error Column name for method 2 error indicator
#' @return p-value from McNemar's test
calculate_mcnemar_error_p_value <- function(data, method1_error, method2_error) {
  # Create McNemar's test contingency table for errors
  both_error <- sum(data[[method1_error]] == 1 & data[[method2_error]] == 1)
  method1_only_error <- sum(data[[method1_error]] == 1 & data[[method2_error]] == 0)
  method2_only_error <- sum(data[[method1_error]] == 0 & data[[method2_error]] == 1)
  neither_error <- sum(data[[method1_error]] == 0 & data[[method2_error]] == 0)
  
  # Handle cases where there are no discordant pairs
  if (method1_only_error + method2_only_error == 0) {
    return(1.0)  # Perfect agreement, no difference
  }
  
  # Use exact binomial test for small samples
  if (method1_only_error + method2_only_error < 25) {
    p_value <- binom.test(method1_only_error, method1_only_error + method2_only_error, p = 0.5)$p.value
  } else {
    # Standard McNemar's test
    mcnemar_table <- matrix(c(both_error, method1_only_error,
                              method2_only_error, neither_error),
                            nrow = 2, byrow = TRUE)
    tryCatch({
      mcnemar_result <- mcnemar.test(mcnemar_table, correct = TRUE)
      p_value <- mcnemar_result$p.value
    }, error = function(e) {
      p_value <- binom.test(method1_only_error, method1_only_error + method2_only_error, p = 0.5)$p.value
    })
  }
  
  return(p_value)
}

#' Format p-values for display
#' @param p_value Numeric p-value
#' @return Formatted p-value string
format_p_value <- function(p_value) {
  case_when(
    p_value < 0.001 ~ "<0.001",
    p_value >= 0.999 ~ "1",
    TRUE ~ sprintf("%.3f", p_value)
  )
}

#' Convert p-values to asterisks for significance
#' @param p_value Numeric p-value
#' @return Asterisk notation string
p_to_asterisk <- function(p_value) {
  case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**", 
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  )
}

# =============================================================================
# TABLE CREATION FUNCTIONS
# =============================================================================

#' Create Table 1: Accuracy Comparison by Category using McNemar's test
#' @param ds Prepared dataset
#' @return GT table object
create_table1 <- function(ds) {
  category_levels <- c("Patient Details", "Medication Name", "Strength & Form", 
                       "Dosing directions", "Indication", "Clinical Notes")
  
  # Create category comparison table using McNemar's test
  category_comparison <- ds %>%
    group_by(Category_clean) %>%
    group_modify(~ {
      category_data <- .x
      total <- nrow(category_data)
      multimodal_correct <- sum(category_data$Multimodal_correct)
      audio_correct <- sum(category_data$Audio_correct)
      
      # Calculate percentages
      multimodal_pct <- round(multimodal_correct / total * 100, 0)
      audio_pct <- round(audio_correct / total * 100, 0)
      
      # Format n/N, (%) columns
      multimodal_formatted <- sprintf("%d/%d, (%d%%)", multimodal_correct, total, multimodal_pct)
      audio_formatted <- sprintf("%d/%d, (%d%%)", audio_correct, total, audio_pct)
      
      # Calculate McNemar's p-value
      p_value_raw <- calculate_mcnemar_p_value(category_data, "Multimodal_correct", "Audio_correct")
      p_value_formatted <- format_p_value(p_value_raw)
      
      tibble(
        Multimodal_formatted = multimodal_formatted,
        Audio_formatted = audio_formatted,
        p_value_formatted = p_value_formatted
      )
    }) %>%
    ungroup()
  
  # Calculate pooled totals using McNemar's test
  pooled_multimodal_correct <- sum(ds$Multimodal_correct)
  pooled_audio_correct <- sum(ds$Audio_correct)
  pooled_total <- nrow(ds)
  
  # Overall McNemar's p-value
  overall_p <- calculate_mcnemar_p_value(ds, "Multimodal_correct", "Audio_correct")
  
  # Format pooled row
  pooled_row <- tibble(
    Category_clean = "Pooled totals",
    Multimodal_formatted = sprintf("%d/%d, (%d%%)", 
                                   pooled_multimodal_correct, pooled_total,
                                   round(pooled_multimodal_correct / pooled_total * 100, 0)),
    Audio_formatted = sprintf("%d/%d, (%d%%)", 
                              pooled_audio_correct, pooled_total,
                              round(pooled_audio_correct / pooled_total * 100, 0)),
    p_value_formatted = format_p_value(overall_p)
  )
  
  # Combine results
  final_table <- bind_rows(
    category_comparison %>%
      arrange(factor(Category_clean, levels = category_levels)),
    pooled_row
  )
  
  # Create GT table
  table1_gt <- final_table %>%
    gt() %>%
    tab_header(title = "Table 1. Accuracy Comparison by Category (McNemar's Test)") %>%
    cols_label(
      Category_clean = "Category",
      Multimodal_formatted = "n/N, (%) correct",
      Audio_formatted = "n/N, (%) correct",
      p_value_formatted = "P-value"
    ) %>%
    tab_spanner("Video-recordings", columns = c(Multimodal_formatted)) %>%
    tab_spanner("Audio-recordings", columns = c(Audio_formatted)) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = nrow(final_table))
    ) %>%
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = nrow(final_table))
    ) %>%
    tab_footnote(
      footnote = "P-values calculated using McNemar's test for paired data comparison",
      locations = cells_column_labels(columns = p_value_formatted)
    )
  
  return(list(table = table1_gt, data = final_table))
}

#' Create Table 2: Error Breakdown using McNemar's test
#' @param ds Prepared dataset
#' @return GT table object
create_table2 <- function(ds) {
  category_levels <- c("Patient Details", "Medication Name", "Strength & Form", 
                       "Dosing directions", "Indication", "Clinical Notes")
  
  # Calculate category-level error breakdown with McNemar's p-values
  category_error_breakdown <- ds %>%
    group_by(Category_clean) %>%
    group_modify(~ {
      category_data <- .x
      count <- nrow(category_data)
      multimodal_omission <- sum(category_data$Multimodal_omission, na.rm = TRUE)
      multimodal_commission <- sum(category_data$Multimodal_commission, na.rm = TRUE)
      audio_omission <- sum(category_data$Audio_omission, na.rm = TRUE)
      audio_commission <- sum(category_data$Audio_commission, na.rm = TRUE)
      
      # McNemar's p-value for omissions comparison
      p_value_omissions <- calculate_mcnemar_error_p_value(category_data, "Multimodal_omission", "Audio_omission")
      
      # McNemar's p-value for commissions comparison  
      p_value_commissions <- calculate_mcnemar_error_p_value(category_data, "Multimodal_commission", "Audio_commission")
      
      tibble(
        Count = count,
        Multimodal_Omission = multimodal_omission,
        Multimodal_Commission = multimodal_commission,
        Audio_Omission = audio_omission,
        Audio_Commission = audio_commission,
        p_omissions_formatted = format_p_value(p_value_omissions),
        p_commissions_formatted = format_p_value(p_value_commissions)
      )
    }) %>%
    ungroup() %>%
    arrange(factor(Category_clean, levels = category_levels))
  
  # Calculate totals
  total_count <- nrow(ds)
  total_multimodal_omissions <- sum(ds$Multimodal_omission, na.rm = TRUE)
  total_multimodal_commissions <- sum(ds$Multimodal_commission, na.rm = TRUE)
  total_audio_omissions <- sum(ds$Audio_omission, na.rm = TRUE)
  total_audio_commissions <- sum(ds$Audio_commission, na.rm = TRUE)
  
  # Overall McNemar's p-values
  overall_p_omissions <- calculate_mcnemar_error_p_value(ds, "Multimodal_omission", "Audio_omission")
  overall_p_commissions <- calculate_mcnemar_error_p_value(ds, "Multimodal_commission", "Audio_commission")
  
  # Create total counts row
  total_counts_row <- tibble(
    Category_clean = "Total Counts",
    Count = total_count,
    Multimodal_Omission = total_multimodal_omissions,
    Audio_Omission = total_audio_omissions,
    p_omissions_formatted = format_p_value(overall_p_omissions),
    Multimodal_Commission = total_multimodal_commissions,
    Audio_Commission = total_audio_commissions,
    p_commissions_formatted = format_p_value(overall_p_commissions)
  )
  
  # Combine rows
  error_breakdown_table <- bind_rows(
    category_error_breakdown %>% 
      dplyr::select(Category_clean, Count, Multimodal_Omission, Audio_Omission, p_omissions_formatted, 
                    Multimodal_Commission, Audio_Commission, p_commissions_formatted),
    total_counts_row
  )
  
  # Create GT table
  table2_gt <- error_breakdown_table %>%
    gt() %>%
    tab_header(title = "Table 2. Error breakdown showing omissions and commissions by medication history category (McNemar's Test)") %>%
    cols_label(
      Category_clean = "Category",
      Count = "Count", 
      Multimodal_Omission = "Audio-video",
      Audio_Omission = "Audio",
      p_omissions_formatted = "P",
      Multimodal_Commission = "Audio-video", 
      Audio_Commission = "Audio",
      p_commissions_formatted = "P"
    ) %>%
    tab_spanner("Omissions", columns = c(Multimodal_Omission, Audio_Omission, p_omissions_formatted)) %>%
    tab_spanner("Commissions", columns = c(Multimodal_Commission, Audio_Commission, p_commissions_formatted)) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = nrow(error_breakdown_table))
    ) %>%
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = nrow(error_breakdown_table))
    ) %>%
    tab_footnote(
      footnote = "P-values calculated using McNemar's test for paired error type comparisons",
      locations = cells_column_labels(columns = c(p_omissions_formatted, p_commissions_formatted))
    )
  
  return(list(table = table2_gt, data = error_breakdown_table))
}

#' Create error analysis table for Tables 3 & 4
#' @param data Dataset
#' @param scribe_type "Multimodal" or "Audio"
#' @return List containing detailed errors and summary
create_error_analysis_table <- function(data, scribe_type) {
  category_levels <- c("Patient Details", "Medication Name", "Strength & Form", 
                       "Dosing directions", "Indication", "Clinical Notes")
  
  # Set column names based on scribe type
  outcome_col <- paste0(scribe_type, "_outcome")
  reason_col <- paste0(scribe_type, "_outcome_reason") 
  scribed_col <- paste0(scribe_type, "_scribed")
  detail_col <- paste0(scribe_type, "_outcome_detail")
  
  # Extract commission and omission errors
  commissions <- data %>%
    filter(.data[[outcome_col]] == "Incorrect", 
           tolower(trimws(.data[[reason_col]])) == "commission") %>%
    dplyr::select(Category_clean, Pharmacist_scribed, all_of(scribed_col), all_of(detail_col)) %>%
    mutate(Type = "Commission")
  
  omissions <- data %>%
    filter(.data[[outcome_col]] == "Incorrect", 
           tolower(trimws(.data[[reason_col]])) == "omission") %>%
    dplyr::select(Category_clean, Pharmacist_scribed, all_of(scribed_col), all_of(detail_col)) %>%
    mutate(Type = "Omission")
  
  # Combine errors
  combined_errors <- bind_rows(commissions, omissions) %>%
    rename(Scribed = all_of(scribed_col), Outcome_detail = all_of(detail_col)) %>%
    mutate(Type = factor(Type, levels = c("Commission", "Omission"))) %>%
    arrange(Type, Category_clean)
  
  # Create summary
  error_summary <- combined_errors %>%
    count(Category_clean, Type) %>%
    tidyr::pivot_wider(names_from = Type, values_from = n, values_fill = 0)
  
  # Handle missing columns
  if(!"Commission" %in% names(error_summary)) error_summary$Commission <- 0
  if(!"Omission" %in% names(error_summary)) error_summary$Omission <- 0
  
  error_summary <- error_summary %>%
    mutate(Total = Commission + Omission) %>%
    arrange(factor(Category_clean, levels = category_levels))
  
  # Add total row
  total_row <- tibble(
    Category_clean = "Total",
    Commission = sum(error_summary$Commission, na.rm = TRUE),
    Omission = sum(error_summary$Omission, na.rm = TRUE),
    Total = sum(error_summary$Total, na.rm = TRUE)
  )
  
  final_summary <- bind_rows(error_summary, total_row)
  
  return(list(detailed = combined_errors, summary = final_summary))
}

#' Create Tables 3 & 4 GT objects
#' @param summary_data Summary data from create_error_analysis_table
#' @param title Table title
#' @return GT table object
create_error_gt_table <- function(summary_data, title) {
  summary_data %>%
    gt() %>%
    tab_header(title = title) %>%
    cols_label(
      Category_clean = "Category",
      Commission = "Commission",
      Omission = "Omission", 
      Total = "Total Errors"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = nrow(summary_data))
    ) %>%
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = nrow(summary_data))
    )
}

#' Create pharmacist performance table using chi-square tests for between-pharmacist variation
#' @param ds Prepared dataset
#' @return GT table object
create_pharmacist_performance_table <- function(ds) {
  # Calculate pharmacist performance by category (showing only video performance)
  pharmacist_category_data <- ds %>%
    group_by(Pharmacist, Category_clean) %>%
    summarise(
      Total = n(),
      Correct = sum(Multimodal_correct),
      Percentage = round(Correct / Total * 100, 1),
      Formatted = sprintf("%d/%d, (%.1f%%)", Correct, Total, Percentage),
      .groups = 'drop'
    ) %>%
    complete(Pharmacist, Category_clean, fill = list(Total = 0, Correct = 0, Percentage = 0, Formatted = "0/0, (0.0%)"))
  
  # Calculate chi-square p-values for each category (testing between-pharmacist variation)
  category_p_values <- ds %>%
    split(.$Category_clean) %>%
    map_dfr(~ {
      category_name <- .x$Category_clean[1]
      
      pharmacist_performance <- .x %>%
        group_by(Pharmacist) %>%
        summarise(
          Correct = sum(Multimodal_correct),
          Total = n(),
          Incorrect = Total - Correct,
          .groups = 'drop'
        )
      
      if (nrow(pharmacist_performance) > 1 && sum(pharmacist_performance$Total) > 0) {
        contingency_matrix <- as.matrix(pharmacist_performance[, c("Correct", "Incorrect")])
        rownames(contingency_matrix) <- pharmacist_performance$Pharmacist
        
        # Check if all pharmacists have same success rate (no variation)
        if (length(unique(pharmacist_performance$Correct/pharmacist_performance$Total)) == 1) {
          p_value <- 1.0
        } else {
          # Use appropriate statistical testing
          tryCatch({
            # Calculate expected frequencies to check assumptions
            row_totals <- rowSums(contingency_matrix)
            col_totals <- colSums(contingency_matrix)
            grand_total <- sum(contingency_matrix)
            expected_freq <- outer(row_totals, col_totals) / grand_total
            min_expected <- min(expected_freq)
            
            # Use chi-square if assumptions met, otherwise Fisher's exact test
            if (min_expected >= 5 && all(contingency_matrix >= 0)) {
              chi_result <- suppressWarnings(chisq.test(contingency_matrix))
              p_value <- chi_result$p.value
            } else {
              fisher_result <- fisher.test(contingency_matrix, simulate.p.value = TRUE)
              p_value <- fisher_result$p.value
            }
          }, error = function(e) {
            p_value <- 1.0  # Fallback if both tests fail
          })
        }
      } else {
        p_value <- 1.0
      }
      
      tibble(Category_clean = category_name, p_value = p_value)
    }) %>%
    mutate(
      p_formatted = case_when(
        p_value < 0.001 ~ "P<0.001***",
        p_value < 0.01 ~ sprintf("P=%.3f**", p_value),
        p_value < 0.05 ~ sprintf("P=%.3f*", p_value),
        p_value >= 0.999 ~ "P=1.000",
        TRUE ~ sprintf("P=%.3f", p_value)
      )
    )
  
  # Reshape data for table format
  table_data <- pharmacist_category_data %>%
    dplyr::select(Pharmacist, Category_clean, Formatted) %>%
    pivot_wider(
      names_from = Category_clean, 
      values_from = Formatted,
      values_fill = "0/0, (0.0%)"
    ) %>%
    dplyr::select(Pharmacist, `Patient Details`, `Medication Name`, `Strength & Form`, 
                  `Dosing directions`, `Indication`, `Clinical Notes`)
  
  # Add p-value row
  p_value_row <- tibble(
    Pharmacist = "P-value",
    `Patient Details` = category_p_values$p_formatted[category_p_values$Category_clean == "Patient Details"],
    `Medication Name` = category_p_values$p_formatted[category_p_values$Category_clean == "Medication Name"],
    `Strength & Form` = category_p_values$p_formatted[category_p_values$Category_clean == "Strength & Form"],
    `Dosing directions` = category_p_values$p_formatted[category_p_values$Category_clean == "Dosing directions"],
    `Indication` = category_p_values$p_formatted[category_p_values$Category_clean == "Indication"],
    `Clinical Notes` = category_p_values$p_formatted[category_p_values$Category_clean == "Clinical Notes"]
  )
  
  final_table_data <- bind_rows(table_data, p_value_row)
  
  # Create GT table
  pharmacist_table_gt <- final_table_data %>%
    gt() %>%
    tab_header(title = "Video Recording Performance by Pharmacist and Category") %>%
    cols_label(
      Pharmacist = "Pharmacist",
      `Patient Details` = "Patient Details",
      `Medication Name` = "Medication Name", 
      `Strength & Form` = "Strength & Form",
      `Dosing directions` = "Dosing directions",
      `Indication` = "Indication",
      `Clinical Notes` = "Clinical Notes"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = nrow(final_table_data))
    ) %>%
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = nrow(final_table_data))
    ) %>%
    tab_footnote(
      footnote = "Values represent correctly scribed out of total possible (percentage correct). P-values calculated using chi-square tests for comparison between pharmacists.",
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_footnote(
      footnote = "* p<0.05, ** p<0.01, *** p<0.001",
      locations = cells_body(columns = `Patient Details`, rows = nrow(final_table_data))
    )
  
  return(list(table = pharmacist_table_gt, data = final_table_data, p_values = category_p_values))
}

# =============================================================================
# VISUALIZATION FUNCTIONS
# =============================================================================

#' Custom theme for publication-ready plots
#' @return ggplot2 theme object
theme_custom <- function() {
  theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major.y = element_line(color = "grey90", size = 0.5),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_rect(color = "grey80", fill = NA, size = 0.8),
      text = element_text(family = "sans", color = "grey20"),
      plot.title = element_text(color = "grey10"),
      axis.title = element_text(color = "grey20"),
      axis.text = element_text(color = "grey30"),
      strip.background = element_rect(fill = "grey95", color = "grey80"),
      strip.text = element_text(color = "grey10", face = "bold"),
      legend.background = element_rect(fill = "white", color = "grey80"),
      legend.key = element_rect(fill = "white", color = NA),
      legend.margin = margin(5, 5, 5, 5),
      plot.margin = margin(10, 10, 10, 10)
    )
}

#' Create Figure 1: Accuracy Comparison Plot (updated with McNemar's test)
#' @param ds Prepared dataset
#' @return ggplot object
create_accuracy_plot <- function(ds) {
  category_levels <- c("Patient Details", "Medication Name", "Strength & Form", 
                       "Dosing directions", "Indication", "Clinical Notes")
  
  # Color palette (colorblind-friendly)
  colors <- c("Video" = "darkcyan", "Audio" = "darkslateblue")
  
  # Prepare all accuracy data at once using McNemar's test
  accuracy_data <- ds %>%
    mutate(Category_clean = factor(Category_clean, levels = category_levels)) %>%
    group_by(Category_clean) %>%
    group_modify(~ {
      category_data <- .x
      total <- nrow(category_data)
      video_correct <- sum(category_data$Multimodal_correct)
      audio_correct <- sum(category_data$Audio_correct)
      video_acc <- round(video_correct / total * 100, 0)
      audio_acc <- round(audio_correct / total * 100, 0)
      p_value <- calculate_mcnemar_p_value(category_data, "Multimodal_correct", "Audio_correct")
      
      tibble(
        Total = total,
        Video_correct = video_correct,
        Audio_correct = audio_correct,
        Video_acc = video_acc,
        Audio_acc = audio_acc,
        p_value = p_value
      )
    }) %>%
    ungroup() %>%
    mutate(asterisk = p_to_asterisk(p_value))
  
  # Prepare data for individual category plots
  viz_data_categories <- accuracy_data %>%
    dplyr::select(Category_clean, Video_acc, Audio_acc, asterisk) %>%
    pivot_longer(cols = c(Video_acc, Audio_acc), 
                 names_to = "Model", values_to = "Accuracy") %>%
    mutate(Model = factor(str_remove(Model, "_acc"), levels = c("Video", "Audio")))
  
  # Prepare overall accuracy data using McNemar's test
  total_accuracy <- ds %>%
    summarise(
      Total_cases = n(),
      Video_total = sum(Multimodal_correct),
      Audio_total = sum(Audio_correct)
    ) %>%
    mutate(
      Video_acc = round(Video_total / Total_cases * 100, 0),
      Audio_acc = round(Audio_total / Total_cases * 100, 0),
      p_value = calculate_mcnemar_p_value(ds, "Multimodal_correct", "Audio_correct")
    ) %>%
    dplyr::select(Video_acc, Audio_acc, p_value) %>%
    pivot_longer(cols = c(Video_acc, Audio_acc), 
                 names_to = "Model", values_to = "Overall_Accuracy") %>%
    mutate(
      Model = factor(str_remove(Model, "_acc"), levels = c("Video", "Audio")),
      asterisk = p_to_asterisk(p_value[1])
    )
  
  # Main plot - Overall accuracy
  p_main <- ggplot(total_accuracy, aes(x = Model, y = Overall_Accuracy, fill = Model)) +
    geom_col(alpha = 0.8, width = 0.95) +
    geom_text(aes(label = paste0(Overall_Accuracy, "%")), 
              vjust = 2.5, size = 5, fontface = "bold", color = "white") +
    {if(unique(total_accuracy$asterisk) != "") 
      annotate("text", x = 1.5, y = 102, label = unique(total_accuracy$asterisk), 
               size = 5, fontface = "bold", hjust = 0.5)} +
    scale_fill_manual(values = colors) +
    scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
    scale_x_discrete(labels = c("", "")) +
    coord_cartesian(xlim = c(0.7, 2.3)) +
    labs(
      title = "Overall Accuracy",
      x = NULL,
      y = "Accuracy (%)",
      fill = "Recording"
    ) +
    theme_custom() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 18, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12, face = "bold")
    )
  
  # Function to create individual category plots
  create_category_plot <- function(category_name) {
    data_subset <- viz_data_categories %>% filter(Category_clean == category_name)
    asterisk <- unique(accuracy_data$asterisk[accuracy_data$Category_clean == category_name])
    
    ggplot(data_subset, aes(x = Model, y = Accuracy, fill = Model)) +
      geom_col(alpha = 0.8, width = 0.9) +
      geom_text(aes(label = paste0(Accuracy, "%")), 
                vjust = 3.5, size = 3.5, fontface = "bold", color = "white") +
      {if(asterisk != "") annotate("text", x = 1.5, y = 102, label = asterisk, 
                                   size = 5, fontface = "bold", hjust = 0.5)} +
      scale_fill_manual(values = colors) +
      scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +
      scale_x_discrete(labels = c("", "")) +
      labs(
        title = category_name,
        x = NULL,
        y = "Accuracy (%)"
      ) +
      theme_custom() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        legend.position = "none",
        plot.margin = margin(5, 5, 5, 5)
      )
  }
  
  # Create individual plots
  category_plots <- map(category_levels, create_category_plot)
  
  # Combine plots
  combined_plot <- p_main + wrap_plots(category_plots, nrow = 2, ncol = 3) + 
    plot_layout(widths = c(1, 2))
  
  # Final plot with annotations
  final_plot <- combined_plot + 
    plot_annotation(
      title = "Accuracy Comparison: Video vs Audio Recording (McNemar's Test)",
      caption = "Left: Overall pooled accuracy | Right: Individual category breakdowns\n* p<0.05, ** p<0.01, *** p<0.001 (McNemar's test for paired data)",
      theme = theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 11, hjust = 0.5, margin = margin(t = 10))
      )
    )
  
  return(final_plot)
}

#' Create Figure 2: Performance Heatmap
#' @param ds Prepared dataset
#' @param show_numbers Whether to show difference numbers on heatmap
#' @return ggplot object
create_pharmacist_heatmap <- function(ds, show_numbers = TRUE) {
  # Data preparation
  heatmap_data <- ds %>%
    group_by(Pharmacist, Category_clean) %>%
    summarise(
      Multimodal_acc = sum(Multimodal_correct) / n() * 100,
      Audio_acc = sum(Audio_correct) / n() * 100,
      Accuracy_Diff = round(Multimodal_acc - Audio_acc, 0),
      .groups = 'drop'
    ) %>%
    mutate(
      Category_clean = factor(Category_clean, 
                              levels = c("Clinical Notes", "Indication", "Dosing directions", 
                                         "Strength & Form", "Medication Name", "Patient Details")),
      # Create display text
      Display_Text = if (show_numbers) {
        case_when(
          Accuracy_Diff == 0 ~ "0",
          Accuracy_Diff > 0 ~ paste0("+", Accuracy_Diff),
          TRUE ~ as.character(Accuracy_Diff)
        )
      } else {
        ""
      },
      # Color categories
      Color_Category = case_when(
        Accuracy_Diff < -10 ~ "Video >10% worse",
        Accuracy_Diff >= -10 & Accuracy_Diff <= -1 ~ "Video 1-10% worse",
        Accuracy_Diff >= 0 & Accuracy_Diff <= 10 ~ "Video 0-10% better",
        Accuracy_Diff > 10 ~ "Video >10% better"
      ),
      Color_Category = factor(Color_Category, levels = c("Video >10% worse", "Video 1-10% worse", 
                                                         "Video 0-10% better", "Video >10% better")),
      Text_Color = ifelse(Color_Category %in% c("Video 0-10% better", "Video 1-10% worse"), 
                          "black", "white")
    )
  
  # Color palette
  color_palette <- c(
    "Video >10% worse" = "#e34a33",      # Red
    "Video 1-10% worse" = "#fc8d59",     # Orange  
    "Video 0-10% better" = "#74a9cf",    # Light blue
    "Video >10% better" = "#045a8d"      # Dark blue
  )
  
  # Ensure all factor levels are present
  all_levels <- c("Video >10% worse", "Video 1-10% worse", "Video 0-10% better", "Video >10% better")
  heatmap_data$Color_Category <- factor(heatmap_data$Color_Category, levels = all_levels)
  
  # Add dummy data to force legend levels to appear
  dummy_data <- data.frame(
    Pharmacist = "ZZZ_Dummy",
    Category_clean = factor("Patient Details", levels = levels(heatmap_data$Category_clean)),
    Multimodal_acc = 0,
    Audio_acc = 15,
    Accuracy_Diff = -15,
    Display_Text = "",
    Color_Category = factor("Video >10% worse", levels = all_levels),
    Text_Color = "white"
  )
  
  plot_data <- rbind(heatmap_data, dummy_data)
  
  # Define pharmacist order
  pharmacist_order <- c("p.1", "p.2", "p.3", "p.4", "p.5", "p.6", "p.7", "p.8", "p.9", "p.10")
  pharmacist_order <- pharmacist_order[pharmacist_order %in% unique(heatmap_data$Pharmacist)]
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Pharmacist, y = Category_clean, fill = Color_Category)) +
    geom_tile(color = "white", size = 0.5) +
    {if(show_numbers) geom_text(aes(label = Display_Text, color = Text_Color),
                                fontface = "bold", size = 3.8)} +
    scale_fill_manual(values = color_palette,
                      name = "Video vs Audio\nAccuracy",
                      breaks = c("Video >10% worse", "Video 1-10% worse", "Video 0-10% better", "Video >10% better"),
                      labels = c("Video >10% worse", "Video 1-10% worse", "Video 0-10% better", "Video >10% better"),
                      na.value = "grey50",
                      drop = FALSE) +
    {if(show_numbers) scale_color_identity()} +
    scale_x_discrete(limits = pharmacist_order) +
    labs(
      title = "",
      caption = "Blue = Video recording better • Orange/Red = Audio recording better",
      x = "",
      y = ""
    ) +
    theme_custom() +
    theme(
      axis.text.x = element_text(hjust = 0.5, face = 'bold', size = 8),
      axis.text.y = element_text(face = "bold", size = 9),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      plot.caption = element_text(size = 9, hjust = 0.5, color = "grey40"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "grey30")
    )
  
  return(p)
}
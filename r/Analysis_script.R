# =============================================================================
# ANALYSIS SCRIPT FOR MENZ ET AL MULTIMODAL AI SCRIBE STUDY
# =============================================================================

# Source helper functions
source("helper_functions.R")

# =============================================================================
# DATA PREPARATION
# =============================================================================

cat("Loading and preparing data...\n")
ds <- prepare_data("ds_final.csv")
cat("Data loaded successfully. Rows:", nrow(ds), "Columns:", ncol(ds), "\n\n")

# ==============================================================================
# TABLES
# ==============================================================================

# =============================================================================
# ACCURACY COMPARISON BY CATEGORY (McNemar's Test)
# =============================================================================

# table 1.

cat("Creating Table 1: Accuracy Comparison by Category using McNemar's test...\n")
table1_results <- create_table1(ds)

# Display and save Table 1
table1_results$table
gtsave(table1_results$table, "Table1_Accuracy_Comparison_McNemar.html")
write.csv(table1_results$data, "Table1_Results_McNemar.csv", row.names = FALSE)
cat("Table 1 saved successfully with McNemar's test results.\n\n")

# =============================================================================
# PHARMACIST PERFORMANCE TABLE (Chi-square test for between-pharmacist comparisons)
# =============================================================================

# supplementary table 1.

cat("Creating Pharmacist Performance Table (using chi-square for between-pharmacist comparisons)...\n")
pharmacist_results <- create_pharmacist_performance_table(ds)

# Display and save pharmacist table
pharmacist_results$table
gtsave(pharmacist_results$table, "Pharmacist_Performance_Table.html")
write.csv(pharmacist_results$data, "Pharmacist_Performance_Results.csv", row.names = FALSE)

# Display summary statistics
cat("\nSummary of pharmacist performance analysis:\n")
cat("Number of pharmacists:", length(unique(ds$Pharmacist)), "\n")
cat("Categories analyzed:", paste(names(pharmacist_results$data)[-1], collapse = ", "), "\n")
print(pharmacist_results$p_values)
cat("Pharmacist Performance Table saved successfully.\n\n")


# =============================================================================
# Supplementary table 2 & 3: multimodal errors
# =============================================================================

# Table 3 - Multimodal errors
cat("Creating Table 3: Multimodal Errors...\n")
multimodal_results <- create_error_analysis_table(ds, "Multimodal")
table3_gt <- create_error_gt_table(multimodal_results$summary, 
                                   "Table 3. Detailed Error Analysis - Multimodal Scribe")

# Display and save Table 3
table3_gt
gtsave(table3_gt, "Table3_Multimodal_Detailed_Errors.html")
write.csv(multimodal_results$detailed, "Table3_Combined_Multimodal_Errors.csv", row.names = FALSE)
write.csv(multimodal_results$summary, "Table3_Multimodal_Summary.csv", row.names = FALSE)
cat("Table 3 saved successfully.\n")


# =============================================================================
# ERROR BREAKDOWN (McNemar's Test), Supplementary table 4
# =============================================================================

cat("Creating Table 2: Error Breakdown using McNemar's test...\n")
table2_results <- create_table2(ds)

# Display and save Table 2
table2_results$table
gtsave(table2_results$table, "Table2_Error_Breakdown_McNemar.html")
write.csv(table2_results$data, "Table2_Results_McNemar.csv", row.names = FALSE)
cat("Table 2 saved successfully with McNemar's test results.\n\n")

# =============================================================================
# Supplementary table 5 & 6: audio-only errors
# =============================================================================

# Table 4 - Audio errors
cat("Creating Table 4: Audio Errors...\n")
audio_results <- create_error_analysis_table(ds, "Audio")
table4_gt <- create_error_gt_table(audio_results$summary, 
                                   "Table 4. Detailed Error Analysis - Audio Only Scribe")

# Display and save Table 4
table4_gt
gtsave(table4_gt, "Table4_Audio_Detailed_Errors.html")
write.csv(audio_results$detailed, "Table4_Combined_Audio_Errors.csv", row.names = FALSE)
write.csv(audio_results$summary, "Table4_Audio_Summary.csv", row.names = FALSE)
cat("Table 4 saved successfully.\n\n")


# ==============================================================================
# VISUALIZATIONS
# ==============================================================================

# =============================================================================
# ACCURACY COMPARISON PLOT (Updated with McNemar's Test)
# =============================================================================

# Figure 1.

cat("Creating Figure 1: Accuracy Comparison Plot using McNemar's test...\n")
figure1_plot <- create_accuracy_plot(ds)

# Display and save Figure 1
print(figure1_plot)
ggsave("Figure1_Accuracy_Comparison_McNemar.png", figure1_plot, 
       width = 16, height = 10, dpi = 600, bg = "white")
ggsave("Figure1_Accuracy_Comparison_McNemar.pdf", figure1_plot, 
       width = 16, height = 10, device = "pdf")
ggsave("Figure1_Accuracy_Comparison_McNemar.tiff", figure1_plot, 
       width = 16, height = 10, dpi = 600, compression = "lzw")
cat("Figure 1 saved successfully with McNemar's test annotations.\n\n")

# =============================================================================
# PERFORMANCE HEATMAP (Updated with McNemar's Test Information)
# =============================================================================

# figure 4.

cat("Creating Figure 2: Performance Heatmap with McNemar's test context...\n")

# Create both versions (with and without numbers)
heatmap_with_numbers <- create_pharmacist_heatmap(ds, show_numbers = TRUE)
heatmap_without_numbers <- create_pharmacist_heatmap(ds, show_numbers = FALSE)

# Display plots
print(heatmap_with_numbers)
print(heatmap_without_numbers)

# Save both versions
ggsave("Figure2_Pharmacist_Heatmap_WithNumbers_McNemar.png", heatmap_with_numbers, 
       width = 12, height = 8, dpi = 600, bg = "white")
ggsave("Figure2_Pharmacist_Heatmap_Clean_McNemar.png", heatmap_without_numbers, 
       width = 12, height = 8, dpi = 600, bg = "white")
ggsave("Figure2_Pharmacist_Heatmap_WithNumbers_McNemar.pdf", heatmap_with_numbers, 
       width = 12, height = 8, device = "pdf")
ggsave("Figure2_Pharmacist_Heatmap_Clean_McNemar.pdf", heatmap_without_numbers, 
       width = 12, height = 8, device = "pdf")
cat("Figure 2 saved successfully with McNemar's test context.\n\n")



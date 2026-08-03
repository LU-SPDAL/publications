library(openxlsx)

header_style <- createStyle(
  fontColour = "white",
  fgFill = "#365F91",
  textDecoration = "bold",
  halign = "center",
  border = "Bottom"
)

write_workbook <- function(file, sheets) {
  workbook <- createWorkbook()

  for (sheet_name in names(sheets)) {
    addWorksheet(workbook, sheet_name)
    writeDataTable(
      workbook,
      sheet = sheet_name,
      x = as.data.frame(sheets[[sheet_name]]),
      tableStyle = "TableStyleMedium2"
    )
    addStyle(
      workbook,
      sheet = sheet_name,
      style = header_style,
      rows = 1,
      cols = seq_len(ncol(sheets[[sheet_name]])),
      gridExpand = TRUE
    )
    freezePane(workbook, sheet_name, firstRow = TRUE)
    setColWidths(workbook, sheet_name, cols = 1:ncol(sheets[[sheet_name]]), widths = "auto")
  }

  saveWorkbook(workbook, file, overwrite = TRUE)
}

article_results <- list(
  Table1_reported = table1_coverage,
  Table1_MCSE = table1_mcse,
  Table1_raw = variance_ratio_table1,
  Figure1_data = correction_roles,
  Figure2_data = dependence_sensitivity,
  Table2_reported = table2_coverage,
  Table2_MCSE = table2_mcse,
  Table2_raw = innovation_robustness_table2
)

additional_results <- list(
  Variance_ratio = variance_ratio_estimators,
  Ratio_tuning = variance_ratio_tuning,
  Block_constant = block_constant_grid,
  Block_selector = block_selector_study,
  Innovation_robustness = innovation_robustness
)

write_workbook("results/article_results.xlsx", article_results)
write_workbook("results/additional_results.xlsx", additional_results)

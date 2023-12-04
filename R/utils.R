as_data_table <- function(df) {
  if (is.data.table(df)) {
    result <- data.table::copy(df)
  } else {
    result <- as.data.table(df)
  }
  result
}

pp <- function(x) {
  result <- formattable::accounting(x, big.mark = ".", decimal.mark = ",")
  result
}

join <- function(x, y, by) {
  x <- as_data_table(x) 
  x <-  x[UO_COD != 4701]
  y <- as_data_table(y)
  y <- y[UO_COD != 4691]
  
  dt <- merge(x, y, by = by, all = TRUE)
  numeric_cols <- setdiff(names(dt), by)
  setnafill(dt, fill = 0, cols = numeric_cols)
}

replace_na <- function(x) {
  dplyr::coalesce(x, 0)
}

format_check_result <- function(df, report, status = "ok", stop_on_failure, output) {
  summary <- validate::summary(report)
  valid <- isTRUE(all.equal(summary$items, summary$passes))
  pass <- validate::satisfying(df, report)
  fail <- validate::violating(df, report, include_missing = TRUE)
  info <- validate::satisfying(df, report)
  if(valid) {
    info <- summary
  } else {
    if (stop_on_failure) { stop(status) }
    info <- fail
  }
  
  if (output) {
    result <- list("valid" = valid, 
                   "summary" = summary,
                   "status" = status,
                   "info" = info,
                   "fail" = fail,
                   "pass" = pass)  
  } else {
    result <- valid
  }
  
  result
}

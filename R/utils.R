as_data_table <- function(df) {
  if (data.table::is.data.table(df)) {
    result <- data.table::copy(df)
  } else {
    result <- data.table::as.data.table(df)
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

as_accounting <- function(df, pattern = "^vlr_|^vl_|^vr", replace_missing = FALSE) {
  result <- as_data_table(df)
  cols <- names(df)[grepl(pattern, names(df))]
  
  if(replace_missing) {
    format <- function(x) pp(replace_na(x))
  } else {
    format <- function(x) pp(x)
  }
  
  # format those columns
  result[, (cols) := lapply(.SD, format), .SDcols = cols]
  result[]
}

format_check_result <- function(df, report, status = "ok", stop_on_failure, output) {
  summary <- validate::summary(report)
  if (summary$error) {stop("Erro durante a validação da expressão ", summary$expression)}
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

read_datapackage <- function(path) {
  package <- suppressMessages(frictionless::read_package(here::here(path)))
  resource_names <- frictionless::resources(package)
  result <- lapply(resource_names, function(resource_name) {
    data.table::as.data.table(frictionless::read_resource(package, resource_name))
  })
  names(result) <- resource_names
  result
}

summarize <- function(data, cols, by = NULL, rename = NULL, filter = NULL) {
  data <- as_data_table(data)
  
  if(!is.null(rename)) {
    data.table::setnames(data, names(rename), as.character(rename))
  }
  
  columns <- names(data)[grepl(cols, names(data))]
  if (is.null(by)) {
    by <- setdiff(names(data), columns)
  }
  
  if(deparse1(substitute(filter)) != "NULL") {
    data <- data[eval(substitute(filter)), ]
  }
  
  data[, lapply(.SD, sum), by = by, .SDcols = columns]
}

`%notin%` <- Negate(`%in%`)
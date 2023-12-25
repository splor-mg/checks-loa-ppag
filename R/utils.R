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

check_result <- function(df, report, status = "ok", stop_on_failure, output, summary = NULL) {
  check_summary <- validate::summary(report)
  # check_summary is too big for reporting, need only items, passes, fails, expression
  summary <- summary %||% check_summary
  if (any(check_summary$error)) {stop("Erro durante a validação da expressão ", check_summary$expression)}
  valid <- isTRUE(all.equal(check_summary$items, check_summary$passes))
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

summarize <- function(data, cols, by = NULL, rename = NULL, filter = NULL) {
  data <- as_data_table(data)
  
  if(!is.null(rename)) {
    data.table::setnames(data, names(rename), as.character(rename))
  }
  
  columns <- names(data)[grepl(cols, names(data))]
  
  if(deparse1(substitute(filter)) != "NULL") {
    data <- data[eval(substitute(filter)), ]
  }
  
  data[, lapply(.SD, sum), by = by, .SDcols = columns]
}

`%notin%` <- Negate(`%in%`)

`%||%` <- function(x, y) if (is.null(x)) y else x


to_data_table <- function(path) {
  your_list <- yaml::yaml.load_file(path)
  
  # Function to process each item
  process_item <- function(item) {
    sapply(item, function(x) {
      if (length(x) > 1) {
        paste(x, collapse = "; ")
      } else {
        x
      }
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # Process the list and convert to data.table
  processed_list <- lapply(your_list, process_item)
  result <- data.table::rbindlist(processed_list, fill = TRUE)
  result
}

#' @export
render_table <- function(data) {
  DT::datatable(data,
                extensions = "Buttons",
                options = list(
                  dom = "Blfrtip",
                  buttons = c("copy"),
                  searching = FALSE,
                  lengthChange = FALSE
                )
  )
}

#' @export
my_custom_hook <- function(x, options) {
  return(options)
}

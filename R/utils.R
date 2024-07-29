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

check_result <- function(df, report, status = "ok", stop_on_failure, output, summary = NULL,
                         json_outfile = NULL, log_level = "ERROR", 
                         msg_template = "Foram encontrados erros no teste.") {
  
  check_summary <- validate::summary(report)
  # check_summary is too big for reporting, need only items, passes, fails, expression
  summary <- summary %||% check_summary
  if (any(check_summary$error)) {stop("Erro durante a validação da expressão ", check_summary$expression)}
  valid <- isTRUE(all.equal(check_summary$items, check_summary$passes))
  pass <- tryCatch(validate::satisfying(df, report), error = function(e) NULL)
  fail <- tryCatch(validate::violating(df, report, include_missing = TRUE), error = function(e) NULL)
  info <- tryCatch(validate::satisfying(df, report), error = function(e) NULL)
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
  
  output_json_env <- Sys.getenv("LOG_FILE")
  if (output_json_env == "") {
    output_json_env <- NULL
  }
  
  # environment variable overlaps the param to write the json
  json_outfile <- json_outfile %||% output_json_env
  
  # Write failures as a JSON Lines file 
  if (!is.null(json_outfile) && !valid) {
    
    con <- file(json_outfile, open = "a", encoding = "UTF-8")  
    
    for (i in seq_len(nrow(fail))) {
      
      log_entry <- list(
        type = as.character(sys.calls()[[sys.parent()]][[1]]),
        log_level = log_level,
        timestamp = Sys.time(),
        message = glue_data(fail[i, ], msg_template),
        valid = valid,
        row = as.list(fail[i, ])
        
      )
      writeLines(jsonlite::toJSON(log_entry, auto_unbox = TRUE), con)
    }
    close(con)
  }
  
  result
}

aggregate <- function(data, cols, by = NULL, rename = NULL, filter = NULL) {
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
check_hook <- function(before, options, envir) {
  if (before == FALSE) {
    if (isTRUE(envir$check$valid)) {
      CHECK_COUNT$pass <<- CHECK_COUNT$pass + 1
      return('<span class="label label-info">Sucesso</span>')
    } else {
      CHECK_COUNT$fail <<- CHECK_COUNT$fail + 1
      return('<p class="label label-danger">Erro</p>')
    }
  }
}

#' @export
document_hook <- function(x) {
  content <- old_hook(x)
  content <- paste(content, collapse = "\n")
  
  matches <- regexec("^(.*)\r?\n---\r?\n(.*)$", content)
  matches <- regmatches(content, matches)
  
  header <- matches[[1]][2]
  body <- matches[[1]][3] 
  
  if(CHECK_COUNT$fail == 0) {
    alert_type <- "alert-success"
  } else {
    alert_type <- "alert-danger"
  }
  pass <- glue::glue('<p>Sucesso <span class="badge">{CHECK_COUNT$pass}</span></p>')
  fail <- glue::glue('<p>Erro <span class="badge">{CHECK_COUNT$fail}</span></p>')
  info <- glue::glue('<div class="alert {alert_type}"> {pass} {fail} </div>')
  return(old_hook(c(header, "---", info, body)))
  
}

#' @export
render_table <- function(data) {
  DT::datatable(data,
                extensions = "Buttons",
                rownames = FALSE,
                options = list(
                  dom = "Blfrtip",
                  buttons = c("copy"),
                  searching = FALSE,
                  lengthChange = FALSE
                )
  )
}

is_url <- function(x) {
  grepl("http:|https:", x)
}

#' Verificar quantidade de programas incluídos entre as bases
#'
#' @export
check_count_programas_is_new <- function(programas_planejamento,
                                         acoes_planejamento,
                                         output = FALSE,
                                         stop_on_failure = FALSE,
                                         json_outfile = NULL, log_level = "ERROR",
                                         msg_template = NULL) {
  x <- programas_planejamento |>
    filter(is_deleted_programa == FALSE & is_new_programa == TRUE) |>
    distinct(programa_cod, programa_desc) |>
    rename(programas = programa_desc)

  y <- acoes_planejamento |>
    filter(is_deleted_programa == FALSE & is_new_programa == TRUE) |>
    distinct(programa_cod, programa_desc) |>
    rename(acoes = programa_desc)

  df <- merge(x, y, by = "programa_cod", all = TRUE)

  report <- check_that(df, programas == acoes)
  
  default_message = "String interpolada {placeholder}."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message

  check_result(
    df, report,
    stop_on_failure = stop_on_failure, output = output, summary = count(df), 
    json_outfile = json_outfile, log_level = log_level, msg_template = msg_template
  )
}

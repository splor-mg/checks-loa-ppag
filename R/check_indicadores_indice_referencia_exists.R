#' Verifica se indicadores apurados possuem índice de referência
#'
#' Se o indicador não está em apuração, ele não pode estar vazio.
#'
#' @export
check_indicadores_indice_referencia_exists <- function(indicadores_planejamento, output = FALSE, stop_on_failure = FALSE,
                                                       json_outfile = NULL, log_level = "ERROR",
                                                       msg_template = NULL) {
  df <- indicadores_planejamento |>
    filter(is_deleted_programa == FALSE &
      is_deleted_indicador == FALSE)

  report <- check_that(df, if(is_em_apuracao_indice_de_referencia == FALSE) !is.na(indice_de_referencia))
  
  default_message = "String interpolada {placeholder}."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message

  check_result(df, report, stop_on_failure = stop_on_failure, output = output,
               json_outfile = json_outfile, log_level = log_level, msg_template = msg_template)
}

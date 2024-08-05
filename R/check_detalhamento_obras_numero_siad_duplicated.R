#' Verifica se número da obra siad possui valores duplicados por ação
#'
#' @export
check_detalhamento_obras_numero_siad_duplicated <- function(
    base_detalhamento_obras, 
    stop_on_failure = FALSE, 
    output = FALSE,
    json_outfile = NULL, log_level = "ERROR") {
  
  x <- base_detalhamento_obras |> 
    filter(!is.na(numero_da_obra_siad)) |> 
    distinct(uo_cod, acao_cod, numero_da_obra_sisor, numero_da_obra_siad)
  
  df <- x
  report <- df |> check_that(is_unique(uo_cod, acao_cod, numero_da_obra_siad))
  
  default_message = "String interpolada {placeholder}."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output, json_outfile = json_outfile, log_level = log_level)
}

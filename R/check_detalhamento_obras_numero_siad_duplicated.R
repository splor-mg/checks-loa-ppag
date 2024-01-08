#' Verifica se número da obra siad possui valores duplicados por ação
#'
#' @export
check_detalhamento_obras_numero_siad_duplicated <- function(
    base_detalhamento_obras, 
    stop_on_failure = FALSE, 
    output = FALSE) {
  
  x <- base_detalhamento_obras |> 
    filter(!is.na(numero_da_obra_siad)) |> 
    distinct(uo_cod, acao_cod, numero_da_obra_sisor, numero_da_obra_siad)
  
  df <- x
  report <- df |> check_that(is_unique(uo_cod, acao_cod, numero_da_obra_siad))
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

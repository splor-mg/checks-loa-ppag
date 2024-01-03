#' Verifica se indicadores não possuem índice de referência com data futura
#'
#' A data futura ocorre quando dt_apuracao >= updated_at
#'
#' @export
check_indicadores_indice_referencia_data_futura <- function(indicadores_planejamento, output = FALSE, stop_on_failure = FALSE) {
  df <- indicadores_planejamento |>
    filter(is_deleted_programa == FALSE &
      is_deleted_indicador == FALSE &
      is_em_apuracao_indice_de_referencia == FALSE)

  # operador menor ou igual pois o indice de referencia pode ter sido
  # calculado no dia da atualizacao (eg. Indicador "SUCESSO DO PLANEJAMENTO ORÇAMENTÁRIO")
  report <- check_that(df, dt_apuracao <= as.Date(updated_at))

  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' Verifica se indicadores não possuem índice de referência zerados
#'
#' @export
check_indicadores_indice_referencia_zerado <- function(indicadores_planejamento, output = FALSE, stop_on_failure = FALSE) {
  df <- indicadores_planejamento |>
    filter(is_deleted_programa == FALSE &
      is_deleted_indicador == FALSE &
      is_em_apuracao_indice_de_referencia == FALSE)

  report <- check_that(df, indice_de_referencia != 0)

  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

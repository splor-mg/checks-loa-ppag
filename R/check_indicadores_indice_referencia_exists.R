#' Verifica se indicadores apurados possuem índice de referência
#'
#' Se o indicador não está em apuração, ele não pode estar vazio.
#'
#' @export
check_indicadores_indice_referencia_exists <- function(indicadores_planejamento, output = FALSE, stop_on_failure = FALSE) {
  df <- indicadores_planejamento |>
    filter(is_deleted_programa == FALSE &
      is_deleted_indicador == FALSE)

  report <- check_that(df, if(is_em_apuracao_indice_de_referencia == FALSE) !is.na(indice_de_referencia))

  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

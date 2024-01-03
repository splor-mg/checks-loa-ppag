#' Verifica se indicadores com índice de referência “em apuração” possuem justificativa
#'
#' Somente são aceitas como válidas justificativas acima de 50 caracteres
#'
#' @export
check_indicadores_justificativa_indice_referencia_em_apuracao <- function(indicadores_planejamento, output = FALSE, stop_on_failure = FALSE) {
  df <- indicadores_planejamento

  report <- check_that(
    df,
    if (is_deleted_programa == FALSE &
      is_deleted_indicador == FALSE & 
      is_em_apuracao_indice_de_referencia == TRUE) 
      !is.na(justificativa_status_apuracao_indice_ref) & 
      (stringr::str_length(justificativa_status_apuracao_indice_ref) > 50)
  )

  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

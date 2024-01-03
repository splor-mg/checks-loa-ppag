#' Verifica se indicadores com previsões “em apuração” possuem justificativa
#'
#' Somente são aceitas como válidas justificativas acima de 50 caracteres
#'
#' @export
check_indicadores_justificativa_previsao_em_apuracao <- function(
    indicadores_planejamento,
    output = FALSE,
    stop_on_failure = FALSE) {
  df <- indicadores_planejamento |>
    filter(is_deleted_programa == FALSE &
      is_deleted_indicador == FALSE)

  report <- check_that(
    df,
    if (is.na(previsao_para_ano0) |
      is.na(previsao_para_ano1) |
      is.na(previsao_para_ano2) |
      is.na(previsao_para_ano3)) 
      !is.na(justificativa_status_apuracao_previsoes) &
        (stringr::str_length(justificativa_status_apuracao_previsoes) > 50)
    
  )

  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

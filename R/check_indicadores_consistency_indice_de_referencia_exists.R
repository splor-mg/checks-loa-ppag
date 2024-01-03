#' Verifica se existem indicadores com índice, mas sem metas previstas
#'
#' @export
check_indicadores_consistency_indice_de_referencia_exists <- function(
  indicadores_planejamento, 
  output = FALSE, 
  stop_on_failure = FALSE
) {
  df <- indicadores_planejamento |>
    filter(is_deleted_programa == FALSE &
      is_deleted_indicador == FALSE)

  report <- check_that(
    df,
    if (!is.na(indice_de_referencia))
      !is.na(previsao_para_ano0) &
        !is.na(previsao_para_ano1) &
        !is.na(previsao_para_ano2) &
        !is.na(previsao_para_ano3)
  )

  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

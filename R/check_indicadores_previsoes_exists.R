#' Verifica se indicadores com previsões apuradas possuem previsões
#'
#' Se a previsão do indicador não está em apuração, ele não pode estar vazio.
#'
#' @export
check_indicadores_previsoes_exists <- function(indicadores_planejamento, output = FALSE, stop_on_failure = FALSE) {
  df <- indicadores_planejamento |>
    filter(is_deleted_programa == FALSE &
                    is_deleted_indicador == FALSE)
  
  report <- check_that(df, 
                                 if(is_em_apuracao_ano0 == FALSE) !is.na(previsao_para_ano0),
                                 if(is_em_apuracao_ano1 == FALSE) !is.na(previsao_para_ano1),
                                 if(is_em_apuracao_ano2 == FALSE) !is.na(previsao_para_ano2),
                                 if(is_em_apuracao_ano3 == FALSE) !is.na(previsao_para_ano3))
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' Verifica se indicadores não possuem previsões zeradas
#'
#' @export
check_indicadores_previsoes_zeradas <- function(indicadores_planejamento, output = FALSE, stop_on_failure = FALSE) {
  df <- indicadores_planejamento |>
    filter(is_deleted_programa == FALSE &
                    is_deleted_indicador == FALSE)
  
  report <- check_that(df, 
                                 if(is_em_apuracao_ano0 == FALSE) previsao_para_ano0 != 0,
                                 if(is_em_apuracao_ano1 == FALSE) previsao_para_ano1 != 0,
                                 if(is_em_apuracao_ano2 == FALSE) previsao_para_ano2 != 0,
                                 if(is_em_apuracao_ano3 == FALSE) previsao_para_ano3 != 0)
  
  default_message = "String interpolada {placeholder}."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

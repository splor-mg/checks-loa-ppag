#' Verifica se indicadores com previsões apuradas possuem previsões
#'
#' Se a previsão do indicador não está em apuração, ele não pode estar vazio.
#'
#'
#'
#' @export
check_indicadores_previsoes_exists <- function(indicadores_planejamento,
                                               output = FALSE,
                                               stop_on_failure = FALSE,
                                               json_outfile = NULL,
                                               log_level = "ERROR",
                                               msg_template = NULL
                                               ) {
  df <- indicadores_planejamento |>
        filter(is_deleted_programa == FALSE &
               is_deleted_indicador == FALSE
               )
  
  report <- check_that(df, 
                       if (is_em_apuracao_ano0 == FALSE) !is.na(previsao_para_ano0),
                       if (is_em_apuracao_ano1 == FALSE) !is.na(previsao_para_ano1),
                       if (is_em_apuracao_ano2 == FALSE) !is.na(previsao_para_ano2),
                       if (is_em_apuracao_ano3 == FALSE) !is.na(previsao_para_ano3))
  
  default_message = "O programa {programa_cod} previsão previsão ({paste(unlist(strsplit(indicador, ' '))[1:3], collapse = ' ')}...) que, apesar de não estar em apuração, está sem valor para o ano {ifelse(is.na(previsao_para_ano0), 0, ifelse(is.na(previsao_para_ano1), 1, ifelse(is.na(previsao_para_ano2), 2, 3)))}."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(df, report,
               stop_on_failure = stop_on_failure,
               output = output,
               json_outfile = json_outfile,
               log_level = log_level,
               msg_template = msg_template
               )
}

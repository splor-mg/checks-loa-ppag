#' Verifica se indicadores com previsões “em apuração” possuem justificativa
#'
#' Somente são aceitas como válidas justificativas acima de 50 caracteres
#'
#'
#'
#' @export
check_indicadores_justificativa_previsao_em_apuracao <- function(indicadores_planejamento,
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
                       if (is.na(previsao_para_ano0) |
                           is.na(previsao_para_ano1) |
                           is.na(previsao_para_ano2) |
                           is.na(previsao_para_ano3)
                           ) 
                           !is.na(justificativa_status_apuracao_previsoes) &
                           (stringr::str_length(justificativa_status_apuracao_previsoes) > 50)
                       )
  
  default_message = "O programa {programa_cod} contém indicador ({paste(unlist(strsplit(indicador, ' '))[1:3], collapse = ' ')}...) que, apesar de estar sem previsão para o ano {ifelse(is.na(previsao_para_ano0), 0, ifelse(is.na(previsao_para_ano1), 1, ifelse(is.na(previsao_para_ano2), 2, 3)))}, apresenta justificativa de previsão inválida, com menos de 50 caracteres ({stringr::str_length(justificativa_status_apuracao_previsoes)} caract. - {paste(unlist(strsplit(justificativa_status_apuracao_previsoes, ' '))[1:3], collapse = ' ')}...)."
  
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

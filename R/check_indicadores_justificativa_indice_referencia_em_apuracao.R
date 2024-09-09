#' Verifica se indicadores com índice de referência “em apuração” possuem justificativa
#'
#' Somente são aceitas como válidas justificativas acima de 50 caracteres
#'
#'
#'
#' @export
check_indicadores_justificativa_indice_referencia_em_apuracao <- function(indicadores_planejamento,
                                                                          output = FALSE,
                                                                          stop_on_failure = FALSE,
                                                                          json_outfile = NULL,
                                                                          log_level = "ERROR",
                                                                          msg_template = NULL
                                                                          ) {
  df <- indicadores_planejamento

  report <- check_that(df,
                       if (is_deleted_programa == FALSE &
                           is_deleted_indicador == FALSE & 
                           is_em_apuracao_indice_de_referencia == TRUE
                           ) 
                           !is.na(justificativa_status_apuracao_indice_ref) & 
                           (stringr::str_length(justificativa_status_apuracao_indice_ref) > 50)
                       )
  
  default_message = "O programa {programa_cod} contém indicador ({paste(unlist(strsplit(indicador, ' '))[1:3], collapse = ' ')}...) que está em apuração, porém com justificativa de status inválida, menos de 50 caracteres {ifelse(is.na(justificativa_status_apuracao_indice_ref), '(0 caract.)', paste0('(', stringr::str_length(justificativa_status_apuracao_indice_ref), ' caract. - ', paste(unlist(strsplit(justificativa_status_apuracao_indice_ref, ' '))[1:3], collapse = ' '), '...)'))}."
  
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

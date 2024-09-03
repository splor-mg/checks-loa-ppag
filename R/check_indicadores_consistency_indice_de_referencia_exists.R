#' Verifica se existem indicadores com índice, mas sem metas previstas
#'
#'
#'
#'
#'
#' @export
check_indicadores_consistency_indice_de_referencia_exists <- function(indicadores_planejamento,
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
                       if (!is.na(indice_de_referencia))
                           !is.na(previsao_para_ano0) &
                           !is.na(previsao_para_ano1) &
                           !is.na(previsao_para_ano2) &
                           !is.na(previsao_para_ano3)
                       )
  
  default_message = "Foram encontrados erros no teste."
  
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

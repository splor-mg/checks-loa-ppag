#' Verificar se as ações estratégicas possuem algum projeto estratégico

#' @export
check_projetos_estrategicos <- function(acoes_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
  df <- acoes_planejamento
  
  report <- check_that(df, if(is_deleted_acao == FALSE & iag_cod == 1) !is.na(projeto_estrategico_cod) & !is.na(projeto_estrategico))
  
  default_message = "String interpolada {placeholder}."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(
    df, report, stop_on_failure = stop_on_failure, output = output
  )
}

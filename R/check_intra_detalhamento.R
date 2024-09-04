#' Receitas e Despesas Intra-Orcamentárias
#'
#' Verifica se as operações intra-orçamentárias registradas na
#' base intraorçamentária de repasse estão adequadamente detalhadas no orçamento.
#'
#'
#' @export
check_intra_detalhamento <- function(base_intra_orcamentaria_detalhamento,
                                     stop_on_failure = FALSE,
                                     output = FALSE,
                                     json_outfile = NULL,
                                     log_level = "ERROR",
                                     msg_template = NULL
                                     ) {
  df <- base_intra_orcamentaria_detalhamento
  
  report <- check_that(df, vlr_recebido == vlr_detalhado)
  
  default_message = "Foram encontrados erros no teste."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(df, report,
               stop_on_failure = stop_on_failure, 
               output = output,
               summary = aggregate(df, "vlr"),
               json_outfile = json_outfile,
               log_level = log_level,
               msg_template = msg_template
               )
}

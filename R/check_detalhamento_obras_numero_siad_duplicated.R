#' Verifica se o número da obra siad possui valores duplicados por ação
#'
#' Espera-se que haja um, e somente uma, obra (nº de obra) por ação em cada
#' unidade orçamentária.
#'
#'
#' @export
check_detalhamento_obras_numero_siad_duplicated <- function(base_detalhamento_obras, 
                                                            stop_on_failure = FALSE, 
                                                            output = FALSE,
                                                            json_outfile = NULL,
                                                            log_level = "ERROR",
                                                            msg_template = NULL
                                                            ) {
  
  x <- base_detalhamento_obras |> 
    filter(!is.na(numero_da_obra_siad)) |> 
    distinct(uo_cod,
             acao_cod,
             numero_da_obra_sisor,
             numero_da_obra_siad
             )
  
  df <- x
  report <- df |> check_that(is_unique(uo_cod, acao_cod, numero_da_obra_siad))
  
  default_message = paste0(
                    "Existe mais de um número de obra associado à ação {acao_cod}, UO {uo_cod} ", 
                    "na base de detalhamento de obras."
                    )
  
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

#' Verifica chaves das bases
#'
#' Existência de linhas duplicadas
#'
#'
#'
#' @export
check_composite_primary_key_localizadores <- function(localizadores_todos_planejamento,
                                                      stop_on_failure = FALSE,
                                                      output = FALSE,
                                                      json_outfile = NULL, 
                                                      log_level = "ERROR",
                                                      msg_template = NULL
                                                      ){
  
  df <- localizadores_todos_planejamento |>
        filter(is_deleted_localizador == FALSE)
  
  rule <- validate::validator(is_complete(uo_acao_cod,
                                          acao_cod,
                                          is_deleted_acao, localizador_cod
                                          ),
                              is_unique(uo_acao_cod,
                                        acao_cod,
                                        is_deleted_acao,
                                        localizador_cod
                                        )
                              )
  
  report <- validate::confront(df, rule)
  
  default_message = "Linha contém valores ausentes ou duplicados na chave uo_acao_cod = {uo_acao_cod}, acao_cod = {acao_cod}, is_deleted_acao = {is_deleted_acao},
  localizador_cod = {localizador_cod}"
  
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

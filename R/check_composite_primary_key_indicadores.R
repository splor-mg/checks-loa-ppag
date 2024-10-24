#' Verifica chaves das bases
#'
#' Existência de linhas duplicadas
#'
#'
#'
#' @export
check_composite_primary_key_indicadores <- function(indicadores_planejamento,
                                                    stop_on_failure = FALSE,
                                                    output = FALSE,
                                                    json_outfile = NULL, 
                                                    log_level = "ERROR",
                                                    msg_template = NULL
                                                    ){
  
  df <- indicadores_planejamento |>
        filter(is_deleted_indicador == FALSE)
  
  rule <- validate::validator(is_complete(programa_cod,
                                          is_deleted_programa,
                                          indicador,
                                          is_deleted_indicador
                                          ),
                              is_unique(programa_cod,
                                        is_deleted_programa,
                                        indicador,
                                        is_deleted_indicador
                                        )
                              )
  
  report <- validate::confront(df, rule)
  
  default_message = "Linha contém valores ausentes ou duplicados na chave programa_cod = {programa_cod}, programa_cod = {programa_cod}, 
  is_deleted_programa = {is_deleted_programa}, indicador = {indicador}, is_deleted_indicador = {is_deleted_indicador}"
  
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

#' Verifica chaves das bases
#'
#' Existência de linhas duplicadas
#'
#'
#'
#' @export
check_composite_primary_key_programas <- function(programas_planejamento,
                                                  stop_on_failure = FALSE,
                                                  output = FALSE,
                                                  json_outfile = NULL,
                                                  log_level = "ERROR",
                                                  msg_template = NULL
                                                  ){
  
  df <- programas_planejamento |>
        filter(is_deleted_programa == FALSE)
  
  rule <- validate::validator(is_complete(uo_programa_cod,
                                          programa_cod,
                                          is_deleted_programa,
                                          objetivo_estrategico_cod,
                                          diretriz_estrategica_cod,
                                          ods_titulo
                                          ),
                              is_unique(uo_programa_cod,
                                        programa_cod,
                                        is_deleted_programa,
                                        objetivo_estrategico_cod,
                                        diretriz_estrategica_cod,
                                        ods_titulo
                                        )
                              )
  
  report <- validate::confront(df, rule)
  
  default_message = paste0("Linha contém valores ausentes ou duplicados na chave uo_programa_cod = {uo_programa_cod}, programa_cod = {programa_cod}, ",
                              "is_deleted_programa = {is_deleted_programa}, objetivo_estrategico_cod = {objetivo_estrategico_cod}, ",
                           "diretriz_estrategica_cod = {diretriz_estrategica_cod}, ods_titulo = {ods_titulo}")
  
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

#' Verifica chaves das bases
#'
#' Existência de linhas duplicadas
#'
#' @export
check_composite_primary_key_programas <- function(
    programas_planejamento,
    stop_on_failure = FALSE,
    output = TRUE,
    json_outfile = NULL, 
    log_level = "ERROR",
    msg_template = NULL
){
  df <- programas_planejamento
  rule <- validate::validator(
    is_complete(uo_programa_cod, programa_cod, is_deleted_programa, objetivo_estrategico_cod, diretriz_estrategica_cod, ods_titulo),
    is_unique(uo_programa_cod, programa_cod, is_deleted_programa, objetivo_estrategico_cod, diretriz_estrategica_cod, ods_titulo)
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

#' @export
check_composite_primary_key_acoes <- function(
    acoes_planejamento,
    stop_on_failure = FALSE,
    output = TRUE,
    json_outfile = NULL, 
    log_level = "ERROR",
    msg_template = NULL
){
  df <- acoes_planejamento
  rule <- validate::validator(
    is_complete(uo_acao_cod, acao_cod, is_deleted_acao),
    is_unique(uo_acao_cod, acao_cod, is_deleted_acao)
  )
  report <- validate::confront(df, rule)
  
  default_message = "Linha contém valores ausentes ou duplicados na chave uo_acao_cod = {uo_acao_cod}, acao_cod = {acao_cod}, is_deleted_acao = {is_deleted_acao}"  
  
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

#' @export
check_composite_primary_key_localizadores <- function(
    localizadores_todos_planejamento,
    stop_on_failure = FALSE,
    output = TRUE,
    json_outfile = NULL, 
    log_level = "ERROR",
    msg_template = NULL
){
  df <- localizadores_todos_planejamento
  rule <- validate::validator(
    is_complete(uo_acao_cod, acao_cod, is_deleted_acao, localizador_cod),
    is_unique(uo_acao_cod, acao_cod, is_deleted_acao, localizador_cod)
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

#' @export
check_composite_primary_key_indicadores <- function(
    indicadores_planejamento,
    stop_on_failure = FALSE,
    output = TRUE,
    json_outfile = NULL, 
    log_level = "ERROR",
    msg_template = NULL
){
  df <- indicadores_planejamento
  rule <- validate::validator(
    is_complete(programa_cod, is_deleted_programa, indicador, is_deleted_indicador),
    is_unique(programa_cod, is_deleted_programa, indicador, is_deleted_indicador)
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

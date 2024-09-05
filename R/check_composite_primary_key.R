#' @export
check_composite_primary_key  <- function(siplan){
  
  pkey_list <- list(
    programas = c('uo_programa_cod', 'programa_cod', 'is_deleted_programa', 'objetivo_estrategico_cod', 'diretriz_estrategica_cod', 'ods_titulo'),
    acoes = c('uo_acao_cod', 'acao_cod', 'is_deleted_acao'),
    localizadores = c('uo_acao_cod', 'acao_cod', 'is_deleted_acao', 'localizador_cod'),
    indicadores = c('programa_cod', 'is_deleted_programa', 'indicador', 'is_deleted_indicador')
  )
  
  df_list <- list (
    programas = sigplan$programas_planejamento,
    acoes = sigplan$acoes_planejamento,
    localizadores = sigplan$localizadores_todos_planejamento,
    indicadores = sigplan$indicadores_planejamento
  )
  
  for (name in names(df_list)) {
    df_list[[name]][, pkey := do.call(paste, c(.SD, sep = "|")), .SDcols = pkey_list[[name]]]
  }

  rule_list <- list(
    rule_programas <- validate::validator(
      is_complete(uo_programa_cod, programa_cod, is_deleted_programa, objetivo_estrategico_cod, diretriz_estrategica_cod, ods_titulo), 
      is_unique(uo_programa_cod, programa_cod, is_deleted_programa, objetivo_estrategico_cod, diretriz_estrategica_cod, ods_titulo)
    ),
    rule_acoes <- validate::validator(
      is_complete(uo_acao_cod, acao_cod, is_deleted_acao),
      is_unique(uo_acao_cod, acao_cod, is_deleted_acao)
    ),
    rule_localizadores <- validate::validator(
      is_complete(uo_acao_cod, acao_cod, is_deleted_acao, localizador_cod),
      is_unique(uo_acao_cod, acao_cod, is_deleted_acao, localizador_cod)
    ),
    rule_indicadores <- validate::validator(
      is_complete(programa_cod, is_deleted_programa, indicador, is_deleted_indicador),
      is_unique(programa_cod, is_deleted_programa, indicador, is_deleted_indicador)
    )
  )
  
  report_list <- map2(df_list, rule_list, validate::confront)
  report_summary_list <- map(report_list, validate::summary)
 
  check_validity <- function(df) {
    isTRUE(all.equal(df$items, df$passes))
  }
  
  valid_list <- map(report_summary_list, check_validity)

  for (name in names(valid_list)) {
    if (valid_list[[name]] == FALSE) {
      invalid_rows <- validate::violating(df_list[[name]], report_list[[name]], include_missing = TRUE)
      invalid_rows <- invalid_rows[,.(pkey)]
      pkey_columns <- paste(pkey_list[[name]], collapse = ", ")
      msg <- glue::glue_data(invalid_rows,
        "A base {name} contém erro na primary-key {pkey_columns}: {pkey}"
      )
    }
  }
  return(msg)
}




#' @export
check_composite_primary_key  <- function(siplan){
  df_list <- list (
    
    programas = sigplan$programas_planejamento|> 
      mutate(pkey = paste(uo_programa_cod, programa_cod, is_deleted_programa, objetivo_estrategico_cod, diretriz_estrategica_cod, ods_titulo, sep = '|')),
    acoes = sigplan$acoes_planejamento |> 
      mutate(pkey = paste(uo_acao_cod, acao_cod, is_deleted_acao, sep = '|')),
    localizadores = sigplan$localizadores_todos_planejamento |>
      mutate(pkey = paste(uo_acao_cod, acao_cod, is_deleted_acao, localizador_cod, sep = '|')),
    indicadores = sigplan$indicadores_planejamento |>
      mutate(pkey = paste(programa_cod, is_deleted_programa, indicador, is_deleted_indicador, sep = '|'))
  )
  
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
  violations_list <- map2(df_list, report_list, function(df, report) {
    validate::violating(df, report) %>%
      select(pkey)  
  })
  
  return(violations_list)
}

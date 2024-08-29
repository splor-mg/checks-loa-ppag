#' @export
check_composite_primary_key <- function(
    programas_planejamento,
    stop_on_failure = FALSE,
    output = TRUE,
    pattern = c('uo_programa_cod|programa_cod|is_deleted_programa|objetivo_estrategico_cod|diretriz_estrategica_cod|ods_titulo') # Buscar essa informação do raw.schema primary-key? Parsear o nome da base e sua respectiva primary-key?
  ){
  df <- programas_planejamento
  cols = names(df)[grepl(pattern, names(df))]
  rule <- validate::validator(
  is_complete(df[cols]), #pq funcionou somente com df[cols] e não com cols?
  is_unique(df[cols])
  )
  report <- validate::confront(df, rule)
  checksplanejamento:::check_result(df, report, stop_on_failure, output)
  }

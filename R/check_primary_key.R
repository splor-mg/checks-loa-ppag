#' @export
check_primary_key <- function(
    acoes_planejamento,
    stop_on_failure = FALSE,
    output = TRUE
  ){
  df <- acoes_planejamento
  rule <- validate::validator(is_unique(uo_acao_cod, acao_cod, is_deleted_acao))
  report <- validate::confront(df, rule)
  checksplanejamento:::check_result(df, report, stop_on_failure, output)
}


check_is_na <- function(
    acoes_planejamento,
    stop_on_failure = FALSE,
    output = TRUE
){
  df <- acoes_planejamento
  rule <- validate::validator(
    !is.na(uo_acao_cod)
    , !is.na(acao_cod)
    , !is.na(is_deleted_acao)
  )
  report <- validate::confront(df, rule)
  checksplanejamento:::check_result(df, report, stop_on_failure, output)
}

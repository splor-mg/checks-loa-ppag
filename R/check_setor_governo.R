#' Verificar se os setores de governo estão corretos

#' @export
check_setor_governo <- function(acoes_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
  df <- acoes_planejamento |> 
        dplyr::distinct(uo_programa_cod, uo_acao_cod, setor_governo)
  
  report <- validate::check_that(df, !is.na(setor_governo))
  
  check_result(
    df, report, stop_on_failure = stop_on_failure, output = output
  )
}
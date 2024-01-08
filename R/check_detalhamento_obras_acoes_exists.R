#' Verifica exclusão do detalhamento de obras de ações excluídas
#'
#' @export
check_detalhamento_obras_acoes_exists <- function(
    base_detalhamento_obras, 
    acoes_planejamento, 
    stop_on_failure = FALSE, 
    output = FALSE) {
  
  x <- base_detalhamento_obras |> 
    distinct(uo_cod, acao_cod)
    
  y <- acoes_planejamento |>
    filter(is_deleted_acao == TRUE) |>
    distinct(uo_acao_cod, acao_cod, is_deleted_acao) |> 
    rename(uo_cod = uo_acao_cod)
  
  df <- merge(x, y, all.x = TRUE, by = c("uo_cod", "acao_cod"))
  report <- df |> check_that(is.na(is_deleted_acao))
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' Verificar quantidade de ações incluídas entre as bases
#'
#' @export
check_count_acoes_is_new <- function(acoes_planejamento,
                                     localizadores_todos_planejamento,
                                     output = FALSE,
                                     stop_on_failure = FALSE) {
  x <- acoes_planejamento |>
    filter(is_deleted_acao == FALSE & is_new_acao == TRUE) |>
    distinct(acao_cod, acao_desc)

  df <- merge(x, y, by = c("uo_acao_cod", "acao_cod"), all = TRUE)

  report <- check_that(df, acoes == localizadores)

  check_result(
    df, report,
    stop_on_failure = stop_on_failure, output = output, summary = count(df)
  )
}

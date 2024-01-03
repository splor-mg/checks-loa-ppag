#' Verificar quantidade de ações excluídos entre as bases
#'
#' Se o processo de exclusão de ações que foram incluídos e excluídos
#' durante o mesmo ciclo (momento R) não tiver sido realizado no SIGPLAN
#' esse teste vai apontar problemas.
#'
#' @export
check_count_acoes_is_deleted <- function(acoes_planejamento,
                                         localizadores_todos_planejamento,
                                         output = FALSE,
                                         stop_on_failure = FALSE) {
  x <- acoes_planejamento |>
    filter(is_deleted_acao == TRUE) |>
    distinct(uo_acao_cod, uo_acao_nome, acao_cod, acao_desc) |>
    rename(acoes = acao_desc)

  y <- localizadores_todos_planejamento |>
    filter(is_deleted_acao == TRUE) |>
    distinct(uo_acao_cod, uo_acao_nome, acao_cod, acao_desc) |>
    rename(localizadores = acao_desc)

  df <- merge(x, y, by = c("uo_acao_cod", "acao_cod"), all = TRUE)

  report <- check_that(df, acoes == localizadores)

  check_result(
    df, report,
    stop_on_failure = stop_on_failure, output = output, summary = count(df)
  )
}

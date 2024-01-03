#' Verificar quantidade de programas incluídos entre as bases
#'
#' @export
check_count_programas_is_new <- function(programas_planejamento,
                                         acoes_planejamento,
                                         output = FALSE,
                                         stop_on_failure = FALSE) {
  x <- programas_planejamento |>
    filter(is_deleted_programa == FALSE & is_new_programa == TRUE) |>
    distinct(programa_cod, programa_desc) |>
    rename(programas = programa_desc)

  y <- acoes_planejamento |>
    filter(is_deleted_programa == FALSE & is_new_programa == TRUE) |>
    distinct(programa_cod, programa_desc) |>
    rename(acoes = programa_desc)

  df <- merge(x, y, by = "programa_cod", all = TRUE)

  report <- check_that(df, programas == acoes)

  check_result(
    df, report,
    stop_on_failure = stop_on_failure, output = output, summary = count(df)
  )
}

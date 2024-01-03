#' Verificar quantidade de programas entre as bases
#'
#' @export
check_count_programas <- function(programas_planejamento,
                                  acoes_planejamento,
                                  localizadores_todos_planejamento,
                                  output = FALSE,
                                  stop_on_failure = FALSE) {
  x <- programas_planejamento |>
    filter(is_deleted_programa == FALSE) |>
    distinct(programa_cod, programa_desc) |>
    rename(programas = programa_desc)

  y <- acoes_planejamento |>
    filter(is_deleted_programa == FALSE) |>
    distinct(programa_cod, programa_desc) |>
    rename(acoes = programa_desc)

  z <- localizadores_todos_planejamento |>
    filter(is_deleted_programa == FALSE) |>
    distinct(programa_cod, programa_desc) |>
    rename(localizadores = programa_desc)

  df <- merge(x, y, by = "programa_cod", all = TRUE) |>
    merge(z, by = "programa_cod", all = TRUE)

  report <- check_that(df, programas == acoes, acoes == localizadores)

  check_result(
    df, report,
    stop_on_failure = stop_on_failure, output = output, summary = count(df)
  )
}

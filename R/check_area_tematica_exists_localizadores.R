#' Verifica se cada programa possui apenas uma área temática - localizadores_todos_planejamento
#'
#' É obrigatório a existência de uma área por programa (ie. _one and only one_).
#'
#' @export
check_area_tematica_exists_localizadores <- function(localizadores_todos_planejamento, stop_on_failure = FALSE, output = FALSE) {
  df <- localizadores_todos_planejamento |>
    filter(is_deleted_localizador == FALSE) |>
    distinct(programa_cod, programa_desc, area_tematica_cod, area_tematica_desc) |>
    group_by(programa_cod) |>
    summarize(area_tematica_cod_count = sum(!is.na(area_tematica_cod)))

  report <- check_that(df, area_tematica_cod_count == 1)

  check_result(df, report, status = "Base localizadores_todos_planejamento possui erros.", stop_on_failure = stop_on_failure, output = output)
}

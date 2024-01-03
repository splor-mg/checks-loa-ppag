#' Verifica se cada programa possui apenas uma área temática - programas_planejamento
#'
#' É obrigatório a existência de uma área por programa (ie. _one and only one_).
#'
#' @export
check_area_tematica_exists_programas <- function(programas_planejamento, stop_on_failure = FALSE, output = FALSE) {
  df <- programas_planejamento |>
    filter(is_deleted_programa == FALSE) |>
    distinct(programa_cod, programa_desc, area_tematica_cod, area_tematica_desc) |>
    group_by(programa_cod) |>
    summarize(area_tematica_cod_count = sum(!is.na(area_tematica_cod)))

  report <- check_that(df, area_tematica_cod_count == 1)

  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

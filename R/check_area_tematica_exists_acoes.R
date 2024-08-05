#' Verifica se cada programa possui uma única área temática - acoes_planejamento
#'
#' É obrigatório a existência de uma área por programa (ie. _one and only one_).
#'
#' @export
check_area_tematica_exists_acoes <- function(acoes_planejamento,
                                             stop_on_failure = FALSE,
                                             output = FALSE,
                                             json_outfile = NULL, log_level = "ERROR",
                                             msg_template = NULL
                                             ) {
  df <- acoes_planejamento |>
    filter(is_deleted_acao == FALSE) |>
    distinct(
      programa_cod,
      programa_desc,
      area_tematica_cod,
      area_tematica_desc
    ) |>
    group_by(programa_cod) |>
    summarize(area_tematica_cod_count = sum(!is.na(area_tematica_cod)))

  report <- check_that(df, area_tematica_cod_count == 1)

  default_message = "String interpolada {placeholder}."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output, 
               json_outfile = json_outfile, log_level = log_level, msg_template = msg_template)
}

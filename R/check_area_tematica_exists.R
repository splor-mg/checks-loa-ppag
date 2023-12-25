#' Área temática por programa
#'
#' @description
#' 
#' Verifica se cada programa possui apenas uma área temática. É obrigatório a existência de uma área por programa (ie. _one and only one_).
#' 
#' A verificação é realizada por base de dados:
#' 
#' - programas_planejamento ([check_area_tematica_exists_programas()])
#' - acoes_planejamento ([check_area_tematica_exists_acoes()])
#' - localizadores_todos_planejamento ([check_area_tematica_exists_localizadores()])
#' 
#' @name check_area_tematica_exists
NULL

#' @rdname check_area_tematica_exists
#' @export
check_area_tematica_exists_programas <- function(programas_planejamento, stop_on_failure = FALSE, output = FALSE) {
  
  df <- programas_planejamento |> 
          dplyr::filter(is_deleted_programa == FALSE) |> 
          dplyr::distinct(programa_cod, programa_desc, area_tematica_cod, area_tematica_desc) |> 
          dplyr::group_by(programa_cod) |> 
          dplyr::summarize(area_tematica_cod_count = sum(!is.na(area_tematica_cod)))
  
  report <- validate::check_that(df, area_tematica_cod_count == 1)
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @rdname check_area_tematica_exists
#' @export
check_area_tematica_exists_acoes <- function(acoes_planejamento, stop_on_failure = FALSE, output = FALSE) {
  
  df <- acoes_planejamento |> 
    dplyr::filter(is_deleted_acao == FALSE) |> 
    dplyr::distinct(programa_cod, programa_desc, area_tematica_cod, area_tematica_desc) |> 
    dplyr::group_by(programa_cod) |> 
    dplyr::summarize(area_tematica_cod_count = sum(!is.na(area_tematica_cod)))

  report <- validate::check_that(df, area_tematica_cod_count == 1)
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @rdname check_area_tematica_exists
#' @export
check_area_tematica_exists_localizadores <- function(localizadores_todos_planejamento, stop_on_failure = FALSE, output = FALSE) {
  
  df <- localizadores_todos_planejamento |> 
    dplyr::filter(is_deleted_localizador == FALSE) |> 
    dplyr::distinct(programa_cod, programa_desc, area_tematica_cod, area_tematica_desc) |> 
    dplyr::group_by(programa_cod) |> 
    dplyr::summarize(area_tematica_cod_count = sum(!is.na(area_tematica_cod)))
  
  report <- validate::check_that(df, area_tematica_cod_count == 1)
  
  check_result(df, report, status = "Base localizadores_todos_planejamento possui erros.", stop_on_failure = stop_on_failure, output = output)
}
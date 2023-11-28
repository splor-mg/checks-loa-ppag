#' Área temática por programa
#'
#' @description
#' 
#' Verifica se cada programa possui apenas uma área temática. É obrigatório a existência de uma área por programa (ie. one and only one).
#' 
#' A verificação é realizada por base de dados:
#' 
#' - programas_planejamento
#' - acoes_planejamento
#' - localizadores_todos_planejamento
#' 
#' @name check_area_tematica_exists
NULL

#' @rdname check_area_tematica_exists
#' @export
check_area_tematica_exists_programas <- function(programas_planejamento) {
  
  df <- programas_planejamento |> 
       filter(is_deleted_programa == FALSE) |> 
       distinct(programa_cod, programa_desc, area_tematica_cod, area_tematica_desc) |> 
       group_by(programa_cod) |> 
       summarize(area_tematica_cod_count = sum(!is.na(area_tematica_cod)))
  
  report <- validate::check_that(df, area_tematica_cod_count == 1)
  
  format_check_result(df, report)
}

#' @rdname check_area_tematica_exists
#' @export
check_area_tematica_exists_acoes <- function(acoes_planejamento) {
  
  df <- acoes_planejamento |> 
    filter(is_deleted_acao == FALSE) |> 
    distinct(programa_cod, programa_desc, area_tematica_cod, area_tematica_desc) |> 
    group_by(programa_cod) |> 
    summarize(area_tematica_cod_count = sum(!is.na(area_tematica_cod)))

  report <- validate::check_that(df, area_tematica_cod_count == 1)
  
  format_check_result(df, report)
}

#' @rdname check_area_tematica_exists
#' @export
check_area_tematica_exists_localizadores <- function(localizadores_todos_planejamento) {
  
  df <- localizadores_todos_planejamento |> 
    filter(is_deleted_localizador == FALSE) |> 
    distinct(programa_cod, programa_desc, area_tematica_cod, area_tematica_desc) |> 
    group_by(programa_cod) |> 
    summarize(area_tematica_cod_count = sum(!is.na(area_tematica_cod)))
  
  report <- validate::check_that(df, area_tematica_cod_count == 1)
  
  format_check_result(df, report, status = "Base localizadores_todos_planejamento possui erros.")
}
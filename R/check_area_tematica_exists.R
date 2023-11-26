#' Área temática por programa
#'
#' Verificar se cada programa possui apenas uma área temática. É obrigatório a existência de uma área por programa;
#' @export
check_area_tematica_exists <- function(programas_planejamento, acoes_planejamento, localizadores_todos_planejamento) {
  x <- programas_planejamento |> 
       filter(is_deleted_programa == FALSE) |> 
       distinct(programa_cod, programa_desc, area_tematica_cod, area_tematica_desc) |> 
       group_by(programa_cod, programa_desc) |> 
       count(name = "area_tematica_count")
  
  y <- acoes_planejamento |> 
    filter(is_deleted_programa == FALSE) |> 
    distinct(programa_cod, programa_desc, area_tematica_cod, area_tematica_desc) |> 
    group_by(programa_cod, programa_desc) |> 
    count(name = "area_tematica_count")
  
  z <- localizadores_todos_planejamento |> 
    filter(is_deleted_programa == FALSE) |> 
    distinct(programa_cod, programa_desc, area_tematica_cod, area_tematica_desc) |> 
    group_by(programa_cod, programa_desc) |> 
    count(name = "area_tematica_count")
  
}
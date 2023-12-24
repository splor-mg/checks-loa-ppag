#' Verificar vinculação dos programas aos Objetivos de Desenvolvimento Sustentável (ODS)
#'
#' @description
#' 
#' São realizadas duas verificações:
#' 
#' - cada programa possui pelo menos um ODS com exceção do programa padronizado 705 ([check_ods_exists()]);
#' - se existem programas com ODS 18 - Não Possui ODS, conjugado com outro(s) ODS(s) [check_ods_consistency()].
#' 
#' @name check_ods
NULL

#' @rdname check_ods
#' @export
check_ods_exists <- function(programas_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
  df <- programas_planejamento |> 
    dplyr::filter(is_deleted_programa == FALSE & programa_cod %notin% c(705)) |> 
    dplyr::distinct(programa_cod, programa_desc, ods_titulo, ods_subtitulo) |> 
    dplyr::summarize(
      ods_count = sum(!is.na(ods_titulo) & ods_titulo != "18 - Não Possui Objetivo de Desenvolvimento Sustentável"), 
      .by = c("programa_cod", "programa_desc")
    )
  
  report <- validate::check_that(df, ods_count > 0)
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @rdname check_ods
#' @export
check_ods_consistency <- function(programas_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
  PROGRAMAS <- programas_planejamento |> 
    dplyr::filter(ods_titulo == "18 - Não Possui Objetivo de Desenvolvimento Sustentável") |> 
    dplyr::pull(programa_cod)
  
  df <- programas_planejamento |> 
    dplyr::filter(is_deleted_programa == FALSE & programa_cod %in% PROGRAMAS) |> 
    dplyr::distinct(programa_cod, programa_desc, ods_titulo)
    
  report <- validate::check_that(df, is_unique(programa_cod))
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)  
}

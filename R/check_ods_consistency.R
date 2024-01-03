#' Verifica se existem programas com ODS 18 conjugado com outro ODS
#'
#' Se a opção "ODS 18 - Não Possui ODS" foi selecionada, não faz sentido 
#' que outro ODS também seja selecionado
#'
#' @export
check_ods_consistency <- function(programas_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
  PROGRAMAS <- programas_planejamento |> 
    filter(ods_titulo == "18 - Não Possui Objetivo de Desenvolvimento Sustentável") |> 
    pull(programa_cod)
  
  df <- programas_planejamento |> 
    filter(is_deleted_programa == FALSE & programa_cod %in% PROGRAMAS) |> 
    distinct(programa_cod, programa_desc, ods_titulo)
    
  report <- check_that(df, is_unique(programa_cod))
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)  
}

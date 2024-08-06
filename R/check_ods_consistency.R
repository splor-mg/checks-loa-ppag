#' Verifica se existem programas com ODS 18 conjugado com outro ODS
#'
#' Se a opção "ODS 18 - Não Possui ODS" foi selecionada, não faz sentido 
#' que outro ODS também seja selecionado
#'
#' @export
check_ods_consistency <- function(programas_planejamento, output = FALSE, stop_on_failure = FALSE,
                                  json_outfile = NULL, log_level = "ERROR",
                                  msg_template = NULL) {
  
  PROGRAMAS <- programas_planejamento |> 
    filter(ods_titulo == "18 - Não Possui Objetivo de Desenvolvimento Sustentável") |> 
    pull(programa_cod)
  
  df <- programas_planejamento |> 
    filter(is_deleted_programa == FALSE & programa_cod %in% PROGRAMAS) |> 
    distinct(programa_cod, programa_desc, ods_titulo)
    
  report <- check_that(df, is_unique(programa_cod))
  
  default_message = "String interpolada {placeholder}."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output,
               json_outfile = json_outfile, log_level = log_level, msg_template = msg_template)  
}

#' Verifica cada programa possui pelo menos um ODS
#'
#' @description
#' Com exceção do programa 705, todos os demais devem possuir
#' um ou mais Objetivos de Desenvolvimento Sustentável (ODS)
#' 
#' @export
check_ods_exists <- function(programas_planejamento,
                             output = FALSE,
                             stop_on_failure = FALSE,
                             json_outfile = NULL,
                             log_level = "ERROR",
                             msg_template = NULL
                             ) {
  
  df <- programas_planejamento |> 
        filter(is_deleted_programa == FALSE &
               programa_cod %notin% c(705, 999)
               ) |> 
        distinct(programa_cod,
                 programa_desc,
                 ods_titulo,
                 ods_subtitulo
                 ) |> 
        summarize(ods_count = sum(!is.na(ods_titulo) & 
                                  ods_titulo != "18 - Não Possui Objetivo de Desenvolvimento Sustentável"
                                  ), 
                  .by = c("programa_cod", "programa_desc")
                  )
      
  report <- check_that(df, ods_count > 0)
  
  default_message = "O programa {programa_cod} está sem objetivo de desenvolvimento estratégico definido."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(df, report,
               stop_on_failure = stop_on_failure,
               output = output,
               json_outfile = json_outfile,
               log_level = log_level,
               msg_template = msg_template
               )
}

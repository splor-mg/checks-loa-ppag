#' Verifica se cada programa possui pelo menos um objetivo estratégico
#'
#'  
#' 
#'
#'
#' @export
check_objetivos_estrategicos_exists <- function(programas_planejamento,
                                                output = FALSE,
                                                stop_on_failure = FALSE,
                                                json_outfile = NULL,
                                                log_level = "ERROR",
                                                msg_template = NULL
                                                ) {

  df <- programas_planejamento |> 
        filter(is_deleted_programa == FALSE) |> 
        distinct(programa_cod,
                 programa_desc,
                 objetivo_estrategico_cod,
                 objetivo_estrategico_desc
                 ) |> 
        group_by(programa_cod, 
                 programa_desc,
                 objetivo_estrategico_cod,
                 objetivo_estrategico_desc
                 ) |>  
        summarize(objetivo_estrategico_count = sum(!is.na(objetivo_estrategico_cod) & 
                                                         objetivo_estrategico_cod != 3
                                                         )
                  )
  
  report <- check_that(df, objetivo_estrategico_count > 0)
  
  default_message = paste0( 
                    "O programa {programa_cod} está sem objetivo estratégico válido na base programas-",
                    "planejamento ({objetivo_estrategico_cod}-{objetivo_estrategico_desc}). ",
                    "Cada programa precisa ter pelo menos um objetivo estratégico."
                    )
  
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


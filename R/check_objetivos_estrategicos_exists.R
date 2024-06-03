#' Verifica se cada programa possui pelo menos um objetivo estratégico
#'
#'  
#' 
#'
#' @export
check_objetivos_estrategicos_exists <- function(programas_planejamento, output = FALSE, stop_on_failure = FALSE) {

  df <- programas_planejamento |> 
    filter(is_deleted_programa == FALSE) |> 
    distinct(programa_cod, programa_desc, objetivo_estrategico_cod, objetivo_estrategico_desc) |> 
    summarize(
      objetivo_estrategico_count = sum(is.na(objetivo_estrategico_cod) | objetivo_estrategico_cod == 3), 
      .by = c("programa_cod", "programa_desc")
    )
  
  report <- check_that(df, objetivo_estrategico_count == 0)
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

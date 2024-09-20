#' Verificar quantidade de programas incluídos entre as bases
#'
#' Confere se o parâmetro is_new está idêntico entre as bases.
#'
#'
#'
#' @export
check_count_programas_is_new <- function(programas_planejamento,
                                         acoes_planejamento,
                                         output = FALSE,
                                         stop_on_failure = FALSE,
                                         json_outfile = NULL,
                                         log_level = "ERROR",
                                         msg_template = NULL) {
  x <- programas_planejamento |>
    filter(is_deleted_programa == FALSE &
           is_new_programa == TRUE
           ) |>
    distinct(programa_cod,
             programa_desc
             ) |>
    rename(programas = programa_desc)

  y <- acoes_planejamento |>
    filter(is_deleted_programa == FALSE &
           is_new_programa == TRUE
           ) |>
    distinct(programa_cod,
             programa_desc
             ) |>
    rename(acoes = programa_desc)

  df <- merge(x, y, by = "programa_cod",
                    all = TRUE)

  report <- check_that(df, programas == acoes)
  
  default_message =   "O programa {programa_cod} {ifelse(
                      is.na(acoes), 
                      'consta como novo na base programas, porém não na base ações-planejamento.', 
                      ifelse(
                        is.na(programas), 
                        'consta como novo na base ações-planejamento, porém não na base programas.', 
                        paste('apresenta descrição inconsistente entre as bases programas (',
                                      paste(unlist(strsplit(programas, ' '))[1:2], collapse = ' '),
                                      '...), e ações-planejamento (',
                                      paste(unlist(strsplit(acoes, ' '))[1:2], collapse = ' '),
                                      '...).'
                              )
                             )
                            )
                          }"
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message

  check_result(df, report,
               stop_on_failure = stop_on_failure,
               output = output,
               summary = count(df),
               json_outfile = json_outfile,
               log_level = log_level,
               msg_template = msg_template
  )
}

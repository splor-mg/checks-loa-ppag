#' Verificar quantidade de programas excluídos entre as bases
#'
#' Se, no processo de exclusão de programas que foram incluídos e excluídos
#' durante o mesmo ciclo (momento R), o mesmo não tiver sido realizado no 
#' SIGPLAN, esse teste vai apontar problemas.
#'
#' @export
check_count_programas_is_deleted <- function(programas_planejamento,
                                             acoes_planejamento,
                                             localizadores_todos_planejamento,
                                             output = FALSE,
                                             stop_on_failure = FALSE,
                                             json_outfile = NULL,
                                             log_level = "ERROR",
                                             msg_template = NULL) {
  x <- programas_planejamento |>
    filter(is_deleted_programa == TRUE) |>
    distinct(programa_cod,
             programa_desc
             ) |>
    rename(programas = programa_desc)

  y <- acoes_planejamento |>
    filter(is_deleted_programa == TRUE) |>
    distinct(programa_cod,
             programa_desc
             ) |>
    rename(acoes = programa_desc)

  z <- localizadores_todos_planejamento |>
    filter(is_deleted_programa == TRUE) |>
    distinct(programa_cod,
             programa_desc
             ) |>
    rename(localizadores = programa_desc)

  df <- merge(x, y, by = "programa_cod",
                    all = TRUE
                    ) |>
        merge(z, by = "programa_cod",
                 all = TRUE)

  report <- check_that(df, programas == acoes,
                           acoes == localizadores)
  
  default_message = "O programa_cod {programa_cod} {ifelse(is.na(acoes) & is.na(localizadores), 'consta como deletado na base programas, porém não nas bases ações e localizadores', ifelse(is.na(programas) & is.na(localizadores), 'consta como deletado na base ações, porém não nas bases programas e localizadores', ifelse(is.na(programas) & is.na(acoes), 'consta como deletado na base localizadores, porém não nas bases programas e ações', ifelse(is.na(programas), 'não consta como deletado na base programas, em detrimento das bases ações e localizadores', ifelse(is.na(acoes), 'não consta como deletado na base ações, em detrimento das bases programas e localizadores', ifelse(is.na(localizadores), 'não consta como deletado na base localizadores, em detrimento das bases programas e ações', 'apresenta descrição inconsistente entre as bases programas, ações e localizadores'))))))}"
  
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

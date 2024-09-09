#' Verificar quantidade de ações entre as bases
#'
#' É esperado que haja a mesma quantidade de ações nas bases de ações e de localizadores,
#' ambas com a mesma descrição para cada ação.
#'
#'
#' @export
check_count_acoes <- function(acoes_planejamento,
                              localizadores_todos_planejamento,
                              output = FALSE,
                              stop_on_failure = FALSE,
                              json_outfile = NULL,
                              log_level = "ERROR",
                              msg_template = NULL
                              ) {
  x <- acoes_planejamento |>
    filter(is_deleted_acao == FALSE) |>
    distinct(uo_acao_cod,
             uo_acao_nome,
             acao_cod,
             acao_desc
             ) |>
    rename(acoes = acao_desc)

  y <- localizadores_todos_planejamento |>
    filter(is_deleted_acao == FALSE) |>
    distinct(uo_acao_cod,
             uo_acao_nome,
             acao_cod,
             acao_desc
             ) |>
    rename(localizadores = acao_desc)

  df <- merge(x, y, by = c("uo_acao_cod", "acao_cod"),
                    all = TRUE)

  report <- check_that(df, acoes == localizadores)
  
  default_message = "A ação {acao_cod}, uo {uo_acao_cod}, consta {ifelse(
                    is.na(acoes), 
                    paste('na base localizadores (',
                          paste(unlist(strsplit(localizadores, ' '))[1:2], collapse = ' '),
                          '...) em inconsistência com a base ações-planejamento (NA).'), ifelse(
                    is.na(localizadores),
                    paste('na base ações-planejamento (',
                          paste(unlist(strsplit(acoes, ' '))[1:2], collapse = ' '),
                          '...) em inconsistência com a base localizadores (NA).'),
                    paste('na base ações-planejamento (',
                          paste(unlist(strsplit(acoes, ' '))[1:2], collapse = ' '),
                          '...) em inconsistência com a base localizadores (',
                          paste(unlist(strsplit(localizadores, ' '))[1:2], collapse = ' '),
                          '...).')
                          ))}"

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

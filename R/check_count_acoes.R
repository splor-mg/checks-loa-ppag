#' Verificar quantidade de ações entre as bases
#'
#' É esperado que haja a mesma quantidade de ações nas bases de ações e de localizadores.
#'
#' @export
check_count_acoes <- function(acoes_planejamento,
                              localizadores_todos_planejamento,
                              output = FALSE,
                              stop_on_failure = FALSE,
                              json_outfile = NULL,
                              log_level = "ERROR",
                              msg_template = NULL) {
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
  
  default_message <- ifelse(is.na(df$localizadores),
                            paste0(
                              "A ação {df$acao_cod}, da UO {df$uo_acao_cod} ",
                              "consta na base acoes_planejamento, porém não ",
                              "na base localizadores_todos_planejamento."),
                            ifelse(is.na(df$acoes),
                              paste0(
                               "A ação {df$acao_cod}, da UO {df$uo_acao_cod} ",
                               "consta na base localizadores_todos_planejamento, ",
                               "porém não na base acoes_planejamento."),
                              paste0(
                               "A ação {df$acao_cod}, da UO {df$uo_acao_cod} ",
                               "apresenta descrição diferente nas bases ",
                               "localizadores_todos_planejamento e acoes_planejamento.")
                            )
                     )
  
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

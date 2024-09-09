#' Verifica exclusão do detalhamento de obras de ações excluídas
#'
#' Espera-se que todas as ações registradas como deletadas não tenham
#' valores registrados na base de detalhamento de obras.
#'
#'
#' @export
check_detalhamento_obras_acoes_exists <- function(base_detalhamento_obras, 
                                                  acoes_planejamento, 
                                                  stop_on_failure = FALSE, 
                                                  output = FALSE,
                                                  json_outfile = NULL,
                                                  log_level = "ERROR",
                                                  msg_template = NULL
                                                  ) {
  x <- base_detalhamento_obras |> 
    distinct(uo_cod,
             acao_cod
             )
    
  y <- acoes_planejamento |>
    filter(is_deleted_acao == TRUE) |>
    distinct(uo_acao_cod,
             acao_cod,
             is_deleted_acao
             ) |> 
    rename(uo_cod = uo_acao_cod)
  
  df <- merge(x, y,
              all.x = TRUE,
              by = c("uo_cod", "acao_cod")
              )
  
  report <- df |> check_that(is.na(is_deleted_acao))
  
  default_message = paste0(
                    "A ação {acao_cod}, apesar de constar como deletada na base ações-planejamento ",
                    "possui valores registrados na uo {uo_cod}, base detalhamento de obras."
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

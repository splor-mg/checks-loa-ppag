#' Valor Limite anual = Valor Utilizado + Valor Transferido
#'
#'
#'
#'
#'
#' @export
check_limite_ano0 <- function(base_limite_cota,
                              stop_on_failure = FALSE,
                              output = FALSE,
                              json_outfile = NULL,
                              log_level = "ERROR",
                              msg_template = NULL
                              ) {
  df <- base_limite_cota |>
        as_accounting()
  
  report <- check_that(df, vlr_limite_ano0 == vlr_utilizado_ano0 + vlr_transferido)
  
  default_message = paste0(
                    "A uo {uo_cod} está com uma diferença de R$ ",
                    "{vlr_limite_ano0 - vlr_utilizado_ano0 - vlr_transferido} entre o valor de limite ",
                    "no ano 0 (R$ {vlr_limite_ano0}) e a soma do valor utilizado no ano ",
                    "(R$ {vlr_utilizado_ano0}) com o valor transferido no ano (R$ {vlr_transferido}), ",
                    "que totalizam R$ {vlr_utilizado_ano0 + vlr_transferido}."
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

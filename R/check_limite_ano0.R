#' Valor Limite anual = Valor Utilizado + Valor Transferido
#'
#' @export
check_limite_ano0 <- function(base_limite_cota, stop_on_failure = FALSE, output = FALSE) {
  df <- base_limite_cota |> as_accounting()
  report <- df |> check_that(
    vlr_limite_ano0 == vlr_utilizado_ano0 + vlr_transferido
  )
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

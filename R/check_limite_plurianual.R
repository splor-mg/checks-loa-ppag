#' Valor Limite anual = Valor Utilizado + Valor Transferido
#'
#' @export
check_limite_plurianual <- function(base_limite_cota, stop_on_failure = FALSE, output = FALSE) {
  df <- base_limite_cota |> as_accounting()
  report <- df |> check_that(
    vlr_limite_ano1 == vlr_utilizado_ano1,
    vlr_limite_ano2 == vlr_utilizado_ano2,
    vlr_limite_ano3 == vlr_utilizado_ano3
  )
  
  default_message = "String interpolada {placeholder}."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

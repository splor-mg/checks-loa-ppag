#' Consistência entre entre detalhamento de obras e QDD (Fiscal, Investimento, Plurianuais)
#'
#' @description
#' Detalhamento de Obras igual a QDD Fiscal - elemento de despesa 51 | QDD Invest - Cat. 4610
#' Detalhamento de Obras plurianual igual a Menor ou Igual ao GND 44 - QDD FISCAL*
#'
#' @export
check_detalhamento_obras_orcam_investimento <- function(base_qdd_investimento, base_detalhamento_obras, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "funcao_cod", "subfuncao_cod", "programa_cod", "acao_cod", "iag_cod")
  
  x <- base_qdd_investimento |> 
    aggregate("vlr_loa_desp_invest", by = key, filter = natureza_cod == 4613)
  
  y <- base_detalhamento_obras |> 
    aggregate("vlr_outros_ano0", by = key, filter = str_sub(uo_cod, 1, 1) == 5)
  
  df <- merge(x, y, by = key, all = TRUE) |> as_accounting(replace_missing = TRUE)
  report <- df |> check_that(vlr_loa_desp_invest == vlr_outros_ano0)
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

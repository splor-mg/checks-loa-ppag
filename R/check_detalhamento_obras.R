#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de
#' investimento das empresas controladas) coincide com a projeção do PPAG
#' para o ano seguinte
#' @name check_detalhamento_obras
NULL

#' @export
check_detalhamento_obras_fiscal_tesouro <- function(base_qdd_fiscal, base_detalhamento_obras, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "funcao_cod", "subfuncao_cod", "programa_cod", "acao_cod", "iag_cod")

  x <- base_qdd_fiscal |> 
       summarize("vlr_loa_desp$", by = key, filter = elemento_cod == 51 & fonte_cod %in% c(10, 15))

  y <- base_detalhamento_obras |> summarize("vlr_tesouro_ano0", by = key)

  df <- merge(x, y, by = key, all = TRUE) |> as_accounting(replace_missing = TRUE)
  report <- df |> check_that(vlr_loa_desp == vlr_tesouro_ano0)
  format_check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @export
check_detalhamento_obras_fiscal_outros <- function(base_qdd_fiscal, base_detalhamento_obras, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "funcao_cod", "subfuncao_cod", "programa_cod", "acao_cod", "iag_cod")
  
  x <- base_qdd_fiscal |> 
    summarize("vlr_loa_desp$", by = key, filter = elemento_cod == 51 & fonte_cod %notin% c(10, 15))
  
  y <- base_detalhamento_obras |> summarize("vlr_outros_ano0", by = key)
  
  df <- merge(x, y, by = key, all = TRUE) |> as_accounting(replace_missing = TRUE)
  report <- df |> check_that(vlr_loa_desp == vlr_outros_ano0)
  format_check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @export
check_detalhamento_obras_investimento <- function(base_qdd_investimento, base_detalhamento_obras, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "funcao_cod", "subfuncao_cod", "programa_cod", "acao_cod", "iag_cod")
  
  x <- base_qdd_investimento |> 
    summarize("vlr_loa_desp_invest", by = key, filter = categoria_cod == "4610 - IMOBILIZAÇÕES")
  
  y <- base_detalhamento_obras |> summarize("vlr_outros_ano0", by = key)
  
  df <- merge(x, y, by = key, all = TRUE) |> as_accounting(replace_missing = TRUE)
  report <- df |> check_that(vlr_loa_desp == vlr_outros_ano0)
  format_check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}
#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de
#' investimento das empresas controladas) coincide com a projeção do PPAG
#' para o ano seguinte
#' @name check_detalhamento_obras
NULL

#' @export
check_detalhamento_obras_orcam_fiscal_tesouro <- function(base_qdd_fiscal, base_detalhamento_obras, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "funcao_cod", "subfuncao_cod", "programa_cod", "acao_cod", "iag_cod")

  x <- base_qdd_fiscal |> 
        summarize("vlr_loa_desp$", by = key, filter = elemento_cod == 51 & fonte_cod %in% c(10, 15))

  y <- base_detalhamento_obras |> 
        summarize("vlr_tesouro_ano0", by = key, filter = str_sub(uo_cod, 1, 1) != 5)

  df <- merge(x, y, by = key, all = TRUE) |> as_accounting(replace_missing = TRUE)
  report <- df |> check_that(vlr_loa_desp == vlr_tesouro_ano0)
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @export
check_detalhamento_obras_orcam_fiscal_tesouro_plurianual <- function(base_qdd_plurianual, base_detalhamento_obras, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "funcao_cod", "subfuncao_cod", "programa_cod", "acao_cod", "iag_cod")
  
  x <- base_qdd_plurianual |> 
    summarize("vlr_loa_desp_ano", by = key, filter = grupo_cod == 4 & fonte_cod %in% c(10, 15))
  
  y <- base_detalhamento_obras |> 
    summarize("vlr_tesouro_ano", by = key, filter = str_sub(uo_cod, 1, 1) != 5)
  
  df <- merge(x, y, by = key, all = TRUE) |> as_accounting(replace_missing = TRUE)
  report <- df |> check_that(
    vlr_loa_desp_ano0 > vlr_tesouro_ano0,
    vlr_loa_desp_ano1 > vlr_tesouro_ano1,
    vlr_loa_desp_ano2 > vlr_tesouro_ano2,
    vlr_loa_desp_ano3 > vlr_tesouro_ano3
  )
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @export
check_detalhamento_obras_orcam_fiscal_outros <- function(base_qdd_fiscal, base_detalhamento_obras, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "funcao_cod", "subfuncao_cod", "programa_cod", "acao_cod", "iag_cod")
  
  x <- base_qdd_fiscal |> 
    summarize("vlr_loa_desp$", by = key, filter = elemento_cod == 51 & fonte_cod %notin% c(10, 15))
  
  y <- base_detalhamento_obras |> 
    summarize("vlr_outros_ano0", by = key, filter = str_sub(uo_cod, 1, 1) != 5)
  
  df <- merge(x, y, by = key, all = TRUE) |> as_accounting(replace_missing = TRUE)
  report <- df |> check_that(vlr_loa_desp == vlr_outros_ano0)
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @export
check_detalhamento_obras_orcam_fiscal_outros_plurianual <- function(base_qdd_plurianual, base_detalhamento_obras, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "funcao_cod", "subfuncao_cod", "programa_cod", "acao_cod", "iag_cod")
  
  x <- base_qdd_plurianual |> 
    summarize("vlr_loa_desp_ano", by = key, filter = grupo_cod == 4 & fonte_cod %notin% c(10, 15))
  
  y <- base_detalhamento_obras |> 
    summarize("vlr_tesouro_ano", by = key, filter = str_sub(uo_cod, 1, 1) != 5)
  
  df <- merge(x, y, by = key, all = TRUE) |> as_accounting(replace_missing = TRUE)
  report <- df |> check_that(
    vlr_loa_desp_ano0 > vlr_tesouro_ano0,
    vlr_loa_desp_ano1 > vlr_tesouro_ano1,
    vlr_loa_desp_ano2 > vlr_tesouro_ano2,
    vlr_loa_desp_ano3 > vlr_tesouro_ano3
  )
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @export
check_detalhamento_obras_orcam_investimento <- function(base_qdd_investimento, base_detalhamento_obras, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "funcao_cod", "subfuncao_cod", "programa_cod", "acao_cod", "iag_cod")
  
  x <- base_qdd_investimento |> 
    summarize("vlr_loa_desp_invest", by = key, filter = categoria_cod == "4610 - IMOBILIZAÇÕES")
  
  y <- base_detalhamento_obras |> 
    summarize("vlr_outros_ano0", by = key, filter = str_sub(uo_cod, 1, 1) == 5)
  
  df <- merge(x, y, by = key, all = TRUE) |> as_accounting(replace_missing = TRUE)
  report <- df |> check_that(vlr_loa_desp_invest == vlr_outros_ano0)
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @export
check_detalhamento_obras_orcam_investimento_plurianual <- function(base_qdd_plurianual_invest, base_detalhamento_obras, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "funcao_cod", "subfuncao_cod", "programa_cod", "acao_cod", "iag_cod")
  
  x <- base_qdd_plurianual_invest |> 
    summarize("vlr_loa_desp_invest", by = key, filter = categoria == "4610")
  
  y <- base_detalhamento_obras |> 
    summarize("vlr_outros_ano", by = key, filter = str_sub(uo_cod, 1, 1) == 5)
  
  df <- merge(x, y, by = key, all = TRUE) |> as_accounting(replace_missing = TRUE)
  report <- df |> check_that(
    vlr_loa_desp_invest_ano0 > vlr_outros_ano0,
    vlr_loa_desp_invest_ano1 > vlr_outros_ano1,
    vlr_loa_desp_invest_ano2 > vlr_outros_ano2,
    vlr_loa_desp_invest_ano3 > vlr_outros_ano3
  )
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}
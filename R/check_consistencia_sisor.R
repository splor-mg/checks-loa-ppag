#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de
#' investimento das empresas controladas) coincide com a projeção do PPAG
#' para o ano seguinte
#' @export
check_consistencia_sisor <- function(base_qdd_fiscal, base_orcam_despesa_item_fiscal, stop_on_failure = FALSE, output = FALSE) {
  key <- c("orgao_cod", "uo_cod", 
           "funcao_cod", "subfuncao_cod", "programa_cod", "acao_cod",
           "categoria_cod", "grupo_cod", "modalidade_cod", "elemento_cod", 
           "iag_cod", "fonte_cod", "ipu_cod")

  x <- base_qdd_fiscal |> summarize("vlr_qdd_fiscal", by = key, rename = list(vlr_loa_desp = "vlr_qdd_fiscal"))

  y <- base_orcam_despesa_item_fiscal |> summarize("vlr_item_fiscal", by = key, rename = list(vlr_loa_desp = "vlr_item_fiscal"))

  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()
  report <- df |> check_that(vlr_qdd_fiscal == vlr_item_fiscal)
  format_check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

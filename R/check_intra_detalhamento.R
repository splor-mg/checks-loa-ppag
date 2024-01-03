#' Receitas e Despesas Intra-Orcamentárias
#'
#' Verifica se as operações intra-orçamentárias registradas na
#' base intraorçamentária de repasse estão adequadamente detalhadas no orçamento.
#'
#' @export
check_intra_detalhamento <- function(base_intra_orcamentaria_detalhamento, stop_on_failure = FALSE, output = FALSE) {
  df <- base_intra_orcamentaria_detalhamento
  report <- df |> check_that(vlr_recebido == vlr_detalhado)
  check_result(base_intra_orcamentaria_detalhamento, report,
    stop_on_failure = stop_on_failure, 
    output = output,
    summary = aggregate(df, "vlr")
  )
}

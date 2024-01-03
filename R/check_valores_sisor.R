#' Consistência entre "QDD Fiscal" e "Orçamento da Despesa Fiscal / Itens de Despesa"
#'
#' @description
#'
#' Verificar se o valor total do orçamento é igual entre as bases
#' "QDD Fiscal" e "Orçamento da Despesa Fiscal / Itens de Despesa" com
#' agrupamento pelas classificações:
#'
#' - orgão
#' - unidade orçamentária
#' - função
#' - subfunção
#' - programa
#' - projeto, atividade ou operação especial
#' - categoria econômica
#' - grupo de despesa
#' - modalidade de aplicação
#' - elemento de despesa
#' - identificador de ação governamental (IAG)
#' - fonte de recurso
#' - identificador de procedência e uso (IPU)
#'
#' @export
check_valores_sisor <- function(base_qdd_fiscal,
                                     base_orcam_despesa_item_fiscal,
                                     stop_on_failure = FALSE,
                                     output = FALSE) {
  key <- c(
    "orgao_cod", "uo_cod",
    "funcao_cod", "subfuncao_cod", "programa_cod", "acao_cod",
    "categoria_cod", "grupo_cod", "modalidade_cod", "elemento_cod",
    "iag_cod", "fonte_cod", "ipu_cod"
  )

  x <- base_qdd_fiscal |>
    aggregate("vlr_qdd_fiscal",
      by = key,
      rename = list(vlr_loa_desp = "vlr_qdd_fiscal")
    )

  y <- base_orcam_despesa_item_fiscal |>
    aggregate("vlr_item_fiscal",
      by = key,
      rename = list(vlr_loa_desp = "vlr_item_fiscal")
    )

  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()
  report <- df |> check_that(vlr_qdd_fiscal == vlr_item_fiscal)
  check_result(df,
    report,
    stop_on_failure = stop_on_failure,
    output = output,
    summary = aggregate(df, "vlr")
  )
}

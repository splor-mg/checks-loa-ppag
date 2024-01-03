#' Receitas e Despesas Intra-Orcamentárias
#'
#' Verifica se as operações intra-orçamentárias registradas na
#' base intraorçamentária de repasse estão adequadamente detalhadas no orçamento.
#'
#' @export
check_intra_receita <- function(base_orcam_receita_fiscal, base_intra_orcamentaria_repasse, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod")

  x <- base_orcam_receita_fiscal |>
    aggregate("vlr_loa_rec$", by = key, filter = categoria %in% c(7, 8))

  y <- base_intra_orcamentaria_repasse |>
    aggregate("vlr_repassado",
      by = key,
      rename = list(uo_beneficiada_cod = "uo_cod")
    )

  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()
  report <- df |> check_that(vlr_loa_rec == vlr_repassado)
  check_result(df, report,
    stop_on_failure = stop_on_failure, 
    output = output, 
    summary = aggregate(df, "vlr")
  )
}

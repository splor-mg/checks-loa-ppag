#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de
#' investimento das empresas controladas) coincide com a projeção do PPAG
#' para o ano seguinte
#' @export
check_repasse_recursos <- function(base_qdd_fiscal, base_repasse_recursos, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "grupo_cod", "iag_cod", "fonte_cod", "ipu_cod")

  x <- base_qdd_fiscal |>
    aggregate("vlr_loa_desp$", by = key, filter = ipu_cod == 2 | (ipu_cod == 5 & fonte_cod %in% c(42, 43)))

  y <- base_repasse_recursos |>
        aggregate("vlr_repasse",
      by = key,
      rename = list(uo_beneficiada_cod = "uo_cod")
    )

  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()
  report <- df |> check_that(vlr_loa_desp == vlr_repasse)
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

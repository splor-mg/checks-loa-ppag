#' Fechamento receita e despesa por fonte do orçamento fiscal
#'
#' @export
check_fechamento_fonte_orcam_fiscal <- function(base_orcam_receita_fiscal, base_qdd_fiscal, stop_on_failure = FALSE, output = FALSE) {
  key <- c("fonte_cod")

  x <- base_orcam_receita_fiscal |>
    aggregate("vlr_loa_rec$", by = key)

  y <- base_qdd_fiscal |>
    aggregate("vlr_loa_desp$",
      by = key
    )

  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()
  report <- df |> check_that(vlr_loa_rec == vlr_loa_desp)
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

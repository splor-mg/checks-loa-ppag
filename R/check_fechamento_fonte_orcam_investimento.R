#' Fechamento receita e despesa por fonte do orçamento de investimento
#'
#' @export
check_fechamento_fonte_orcam_investimento <- function(base_orcam_receita_investimento, base_qdd_investimento, stop_on_failure = FALSE, output = FALSE) {
  key <- c("fonte_cod")

  x <- base_orcam_receita_investimento |>
    mutate(
      fonte_cod = as.numeric(paste0(
        categoria,
        subcategoria,
        alinea,
        str_pad(subalinea, 2, pad = "0")
      ))
    ) |>
    aggregate("vlr_loa_rec_invest$", by = key)

  y <- base_qdd_investimento |>
    aggregate("vlr_loa_desp_invest", by = "fonte_cod")

  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()
  report <- df |> check_that(vlr_loa_rec_invest  == vlr_loa_desp_invest)
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

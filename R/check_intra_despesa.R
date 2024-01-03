#' Receitas e Despesas Intra-Orcamentárias
#'
#' Verifica se as operações intra-orçamentárias registradas na
#' base intraorçamentária de repasse estão adequadamente detalhadas no orçamento.
#'
#' @export
check_intra_despesa <- function(base_orcam_despesa_item_fiscal, base_intra_orcamentaria_repasse, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "identificador_tipo_acao_cod", "projeto_atividade_cod", "grupo_cod", "modalidade_cod", "elemento_cod", "item_cod")

  x <- base_orcam_despesa_item_fiscal |>
    aggregate("vlr_loa_desp$", by = key, filter = modalidade_cod == 91)

  y <- base_intra_orcamentaria_repasse |>
    separate(programa_trabalho_fmt,
      sep = "\u00A0",
      convert = TRUE,
      into = c(
        NA, NA, NA,
        "funcao_cod",
        "subfuncao_cod",
        "programa_cod",
        "identificador_tipo_acao_cod",
        "projeto_atividade_cod",
        NA
      )
    ) |>
    separate(natureza_desp_fmt,
      sep = "\u00A0",
      convert = TRUE,
      into = c(
        "categoria_cod",
        "grupo_cod",
        "modalidade_cod",
        "elemento_cod",
        "item_cod"
      )
    ) |>
    aggregate("vlr_repassado",
      by = key,
      rename = list(uo_repassadora_cod = "uo_cod")
    )

  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()
  report <- df |> check_that(vlr_loa_desp == vlr_repassado)
  check_result(df, report,
    stop_on_failure = stop_on_failure,
    output = output,
    summary = aggregate(df, "vlr")
  )
}

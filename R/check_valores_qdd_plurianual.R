#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de
#' investimento das empresas controladas) coincide com a projeção do PPAG
#' para o ano seguinte
#'
#' @export
check_valores_qdd_plurianual <- function(base_qdd_plurianual, acoes_planejamento, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "programa_cod", "acao_cod", "funcao_cod", "subfuncao_cod", "iag_cod")

  x <- base_qdd_plurianual |>
    aggregate("vlr_loa_desp", by = key)

  y <- acoes_planejamento |>
    aggregate("vr_meta_orcamentaria_ano",
      by = key,
      rename = list(uo_acao_cod = "uo_cod"),
      filter = is_deleted_acao == FALSE & identificador_tipo_acao_cod %in% c(1, 2, 4, 7, 9)
    )

  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()

  report <- df |> check_that(
    vlr_loa_desp_ano0 == vr_meta_orcamentaria_ano0,
    vlr_loa_desp_ano1 == vr_meta_orcamentaria_ano1,
    vlr_loa_desp_ano2 == vr_meta_orcamentaria_ano2,
    vlr_loa_desp_ano3 == vr_meta_orcamentaria_ano3
  )

  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

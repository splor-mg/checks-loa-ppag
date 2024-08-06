#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de
#' investimento das empresas controladas) coincide com a projeção do PPAG
#' para o ano seguinte
#'
#' @export
check_valores_qdd_fiscal <- function(base_qdd_fiscal, acoes_planejamento, stop_on_failure = FALSE, output = FALSE, 
                                     json_outfile = NULL, log_level = "ERROR",
                                     msg_template = NULL) {
  
  key <- c("uo_cod", "programa_cod", "acao_cod", "funcao_cod", "subfuncao_cod", "iag_cod")

  x <- base_qdd_fiscal |>
    aggregate("vlr_loa_desp$", by = key)

  y <- acoes_planejamento |>
    aggregate("vr_meta_orcamentaria_ano0",
      by = key,
      filter = is_deleted_acao == FALSE & identificador_tipo_acao_cod %in% c(1, 2, 4, 7, 9),
      rename = list(uo_acao_cod = "uo_cod")
    )

  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()
  report <- df |> check_that(vlr_loa_desp == vr_meta_orcamentaria_ano0)
  
  default_message = "Funcional programática {programa_cod}.{acao_cod}.{funcao_cod}.{subfuncao_cod} da UO {uo_cod} com valor na base QDD Fiscal ({vlr_loa_desp}) diferente da base Açoes Planejamento do SIGPLAN ({vr_meta_orcamentaria_ano0}"
  
  msg_template = msg_template %||% default_message
  
  default_message = "Foram encontrados erros no teste."
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output, json_outfile = json_outfile, log_level = log_level, msg_template = msg_template)
}

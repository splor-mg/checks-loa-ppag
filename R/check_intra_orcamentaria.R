#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de
#' investimento das empresas controladas) coincide com a projeção do PPAG
#' para o ano seguinte
#' @name check_intra_orcamentaria
NULL

#' @export
check_intra_repasse <- function(base_orcam_despesa_item_fiscal, base_intra_orcamentaria_repasse, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "identificador_tipo_acao_cod", "projeto_atividade_cod", "grupo_cod", "modalidade_cod", "elemento_cod", "item_cod")

  x <- base_orcam_despesa_item_fiscal |>
    summarize("vlr_loa_desp$", by = key, filter = modalidade_cod == 91)

  y <- base_intra_orcamentaria_repasse |>
    tidyr::separate(programa_trabalho_fmt, 
                    sep = "\u00A0",
                    convert = TRUE,
                    into = c(NA, NA, NA, 
                             "funcao_cod", 
                             "subfuncao_cod", 
                             "programa_cod", 
                             "identificador_tipo_acao_cod", 
                             "projeto_atividade_cod", 
                             NA)) |>
    tidyr::separate(natureza_desp_fmt, 
                    sep = "\u00A0",
                    convert = TRUE,
                    into = c("categoria_cod", 
                             "grupo_cod", 
                             "modalidade_cod", 
                             "elemento_cod", 
                             "item_cod")) |>
    summarize("vlr_repassado",
      by = key,
      rename = list(uo_repassadora_cod = "uo_cod")
    )

  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()
  report <- df |> check_that(vlr_loa_desp == vlr_repassado)
  format_check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @export
check_intra_receita <- function(base_orcam_receita_fiscal, base_intra_orcamentaria_repasse, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod")
  
  x <- base_orcam_receita_fiscal |>
    summarize("vlr_loa_rec$", by = key, filter = categoria %in% c(7, 8))
  
  y <- base_intra_orcamentaria_repasse |>
    summarize("vlr_repassado",
              by = key,
              rename = list(uo_beneficiada_cod = "uo_cod")
    )
  
  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()
  report <- df |> check_that(vlr_loa_rec == vlr_repassado)
  format_check_result(df, report, stop_on_failure = stop_on_failure, output = output)  
}

#' @export
check_intra_detalhamento <- function(base_intra_orcamentaria_detalhamento, stop_on_failure = FALSE, output = FALSE) {
  report <- base_intra_orcamentaria_detalhamento |> check_that(vlr_recebido == vlr_detalhado)
  format_check_result(base_intra_orcamentaria_detalhamento, report, stop_on_failure = stop_on_failure, output = output)  
}
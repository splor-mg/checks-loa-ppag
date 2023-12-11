#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de 
#' investimento das empresas controladas) coincide com a projeção do PPAG 
#' para o ano seguinte
#' @name check_sisor
NULL

#' @export
check_repasse_recursos <- function(base_qdd_fiscal, base_repasse_recursos, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "grupo_cod", "fonte_cod", "ipu_cod")
  
  x <- base_qdd_fiscal |>
    summarize("vlr_loa_desp$", by = key, filter = ipu_cod == 2 | (ipu_cod == 5 & fonte_cod %in% c(42, 43)))
  
  y <- base_repasse_recursos |>
    summarize("vr_repasse",
              by = key,
              rename = list(cod_uo_beneficiada = "uo_cod", 
                            grupo_de_despesa = "grupo_cod", 
                            fonte = "fonte_cod", 
                            ipu = "ipu_cod", 
                            valor_transferido = "vr_repasse")
    )
  
  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()
  report <- df |> check_that(vlr_loa_desp == vr_repasse)
  format_check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}
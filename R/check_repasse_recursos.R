#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de
#' Investimento das empresas controladas) coincide com a projeção do PPAG
#' para o ano seguinte
#'
#' @export
check_repasse_recursos <- function(base_qdd_fiscal,
                                   base_repasse_recursos,
                                   stop_on_failure = FALSE,
                                   output = FALSE,
                                   json_outfile = NULL,
                                   log_level = "ERROR",
                                   msg_template = NULL
                                   ) {
  
  key <- c("uo_cod",
           "grupo_cod",
           "iag_cod",
           "fonte_cod",
           "ipu_cod"
           )

  x <- base_qdd_fiscal |>
       aggregate("vlr_loa_desp$",
                 by = key,
                 filter = ipu_cod == 2 | (ipu_cod == 5 & fonte_cod %in% c(42, 43))
                 )

  y <- base_repasse_recursos |>
        aggregate("vlr_repasse",
                  by = key,
                  rename = list(uo_beneficiada_cod = "uo_cod")
                  )

  df <- merge(x, y,
              by = key,
              all = TRUE
              ) |>
        as_accounting()
  
  report <- check_that(df, vlr_loa_desp == vlr_repasse)
  
  default_message = paste0(
                    "A uo {uo_cod}, fonte {fonte_cod}, grupo {grupo_cod}, ipu {ipu_cod} está ",
                    "com valor de despesa na base qdd-fiscal (R$ {vlr_loa_desp}) diferente do ",
                    "valor repassado para essa mesma uo  na base repasse-recursos ",
                    "(R$ {vlr_repasse}). Diferença de R$ {vlr_loa_desp - vlr_repasse}."
                    )
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(df, report,
               stop_on_failure = stop_on_failure,
               output = output,
               json_outfile = json_outfile,
               log_level = log_level,
               msg_template = msg_template
               )
}

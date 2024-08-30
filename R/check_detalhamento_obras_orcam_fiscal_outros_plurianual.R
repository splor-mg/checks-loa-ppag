#' Consistência entre entre detalhamento de obras e QDD (Fiscal, Investimento, Plurianuais)
#'
#' @description
#' Detalhamento de Obras igual a QDD Fiscal - elemento de despesa 51 | QDD Invest - Cat. 4610
#' Detalhamento de Obras plurianual igual a Menor ou Igual ao GND 44 - QDD FISCAL*
#'
#' @export
check_detalhamento_obras_orcam_fiscal_outros_plurianual <- function(base_qdd_plurianual,
                                                                    base_detalhamento_obras,
                                                                    stop_on_failure = FALSE,
                                                                    output = FALSE,
                                                                    json_outfile = NULL,
                                                                    log_level = "ERROR",
                                                                    msg_template = NULL
                                                                    ) {
  key <- c("uo_cod",
           "funcao_cod",
           "subfuncao_cod",
           "programa_cod",
           "acao_cod",
           "iag_cod"
           )
  
  x <- base_qdd_plurianual |> 
    aggregate("vlr_loa_desp_ano",
              by = key,
              filter = grupo_cod == 4 & fonte_cod %notin% c(10, 15)
              )
  
  y <- base_detalhamento_obras |> 
    aggregate("vlr_outros_ano",
              by = key,
              filter = str_sub(uo_cod, 1, 1) != 5
              )
  
  df <- merge(x, y,
              by = key,
              all = TRUE
              ) |> 
     as_accounting(replace_missing = TRUE)
  
  report <- df |> check_that(vlr_loa_desp_ano0 >= vlr_outros_ano0,
                             vlr_loa_desp_ano1 >= vlr_outros_ano1,
                             vlr_loa_desp_ano2 >= vlr_outros_ano2,
                             vlr_loa_desp_ano3 >= vlr_outros_ano3
                            )
  
  default_message = "A ação {acao_cod}, na UO {uo_cod}, função-subfunção {funcao_cod}-{subfuncao_cod}, programa {programa_cod} e iag {iag_cod} está com valor na base qdd plurianual R$ {ifelse(vlr_loa_desp_ano0 < vlr_outros_ano0, vlr_loa_desp_ano0, ifelse(vlr_loa_desp_ano1 < vlr_outros_ano1, vlr_loa_desp_ano1, ifelse(vlr_loa_desp_ano2 < vlr_outros_ano2, vlr_loa_desp_ano2, vlr_loa_desp_ano3)))} menor do que na base detalhamento de obras R$ {ifelse(vlr_loa_desp_ano0 < vlr_outros_ano0, vlr_outros_ano0, ifelse(vlr_loa_desp_ano1 < vlr_outros_ano1, vlr_outros_ano1, ifelse(vlr_loa_desp_ano2 < vlr_outros_ano2, vlr_outros_ano2, vlr_outros_ano3)))}."
  
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

#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de
#' investimento das empresas controladas) coincide com a projeção do PPAG
#' para o ano seguinte
#'
#' @export
check_valores_qdd_plurianual <- function(base_qdd_plurianual,
                                         acoes_planejamento,
                                         stop_on_failure = FALSE,
                                         output = FALSE,
                                         json_outfile = NULL,
                                         log_level = "ERROR",
                                         msg_template = NULL
                                         ) {

  key <- c("uo_cod",
           "programa_cod",
           "acao_cod",
           "funcao_cod",
           "subfuncao_cod",
           "iag_cod"
           )

  x <- base_qdd_plurianual |>
       aggregate("vlr_loa_desp",
                 by = key
                 )

  y <- acoes_planejamento |>
       aggregate("vr_meta_orcamentaria_ano",
                 by = key,
                 filter = is_deleted_acao == FALSE &
                          identificador_tipo_acao_cod %in% c(1, 2, 4, 7, 9),
                 rename = list(uo_acao_cod = "uo_cod")
                 )

  df <- merge(x, y,
              by = key,
              all = TRUE
              ) |>
        as_accounting()

  report <- check_that(df,
                       vlr_loa_desp_ano0 == vr_meta_orcamentaria_ano0,
                       vlr_loa_desp_ano1 == vr_meta_orcamentaria_ano1,
                       vlr_loa_desp_ano2 == vr_meta_orcamentaria_ano2,
                       vlr_loa_desp_ano3 == vr_meta_orcamentaria_ano3
                       )
  
  default_message = "A uo {uo_cod}, programa {programa_cod}, ação {acao_cod}, função {funcao_cod}, subfunção {subfuncao_cod} está com valor do ano {ifelse(vlr_loa_desp_ano0 != vr_meta_orcamentaria_ano0 | is.na(vlr_loa_desp_ano0) | is.na(vr_meta_orcamentaria_ano0), 
                    paste0('0, base qdd-plurianual (R$ ', vlr_loa_desp_ano0, ') diferente da base ações-planejamento (R$ ', vr_meta_orcamentaria_ano0, '). Diferença de R$ ', 
                    ifelse(is.na(vlr_loa_desp_ano0) & is.na(vr_meta_orcamentaria_ano0), 'NA', ifelse(is.na(vlr_loa_desp_ano0), vr_meta_orcamentaria_ano0, ifelse(is.na(vr_meta_orcamentaria_ano0), vlr_loa_desp_ano0, vlr_loa_desp_ano0 - vr_meta_orcamentaria_ano0))), '.'), 

                    ifelse(vlr_loa_desp_ano1 != vr_meta_orcamentaria_ano1 | is.na(vlr_loa_desp_ano1) | is.na(vr_meta_orcamentaria_ano1), 
                    paste0('1, base qdd-plurianual (R$ ', vlr_loa_desp_ano1, ') diferente da base ações-planejamento (R$ ', vr_meta_orcamentaria_ano1, '). Diferença de R$ ', 
                    ifelse(is.na(vlr_loa_desp_ano1) & is.na(vr_meta_orcamentaria_ano1), 'NA', ifelse(is.na(vlr_loa_desp_ano1), vr_meta_orcamentaria_ano1, ifelse(is.na(vr_meta_orcamentaria_ano1), vlr_loa_desp_ano1, vlr_loa_desp_ano1 - vr_meta_orcamentaria_ano1))), '.'),

                    ifelse(vlr_loa_desp_ano2 != vr_meta_orcamentaria_ano2 | is.na(vlr_loa_desp_ano2) | is.na(vr_meta_orcamentaria_ano2), 
                    paste0('2, base qdd-plurianual (R$ ', vlr_loa_desp_ano2, ') diferente da base ações-planejamento (R$ ', vr_meta_orcamentaria_ano2, '). Diferença de R$ ', 
                    ifelse(is.na(vlr_loa_desp_ano2) & is.na(vr_meta_orcamentaria_ano2), 'NA', ifelse(is.na(vlr_loa_desp_ano2), vr_meta_orcamentaria_ano2, ifelse(is.na(vr_meta_orcamentaria_ano2), vlr_loa_desp_ano2, vlr_loa_desp_ano2 - vr_meta_orcamentaria_ano2))), '.'),

                    paste0('3, base qdd-plurianual (R$ ', vlr_loa_desp_ano3, ') diferente da base ações-planejamento (R$ ', vr_meta_orcamentaria_ano3, '). Diferença de R$ ', 
                    ifelse(is.na(vlr_loa_desp_ano3) & is.na(vr_meta_orcamentaria_ano3), 'NA', ifelse(is.na(vlr_loa_desp_ano3), vr_meta_orcamentaria_ano3, ifelse(is.na(vr_meta_orcamentaria_ano3), vlr_loa_desp_ano3, vlr_loa_desp_ano3 - vr_meta_orcamentaria_ano3))), '.'))))}"
  
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

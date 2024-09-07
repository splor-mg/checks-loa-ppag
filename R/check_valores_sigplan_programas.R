#' Verificar se os totais dos valores orçamentários e físicos de cada ano do PPAG estão consistentes entre as bases
#'
#' @description
#'
#' Verificar se os totais dos valores orçamentários e físicos de cada ano do PPAG são iguais 
#' entre as bases "Programas", "Ações" e "Localizadores" com agrupamento pelas colunas:
#' 
#' - orgão
#' - unidade orçamentária
#' - função
#' - subfunção
#' - programa
#' - projeto, atividade ou operação especial
#' - categoria econômica
#' - grupo de despesa
#' - modalidade de aplicação
#' - elemento de despesa
#' - identificador de ação governamental (IAG)
#' - fonte de recurso
#' - identificador de procedência e uso (IPU)
#'  
#' @export
check_valores_sigplan_programas <- function(acoes_planejamento,
                                            programas_planejamento,
                                            stop_on_failure = FALSE,
                                            output = FALSE,
                                            json_outfile = NULL,
                                            log_level = "ERROR",
                                            msg_template = NULL
                                            ) {
  
  key <- c("programa_cod",
           "area_tematica_cod",
           "uo_programa_cod"
           )
  
  x <- acoes_planejamento |> 
       aggregate("vr_meta_orcamentaria_ano", 
                 by = key,
                 filter = is_deleted_programa == FALSE &
                          is_deleted_acao == FALSE,
                 rename = list(
                               vr_meta_orcamentaria_ano0 = "vr_meta_orcamentaria_ano0_acoes",
                               vr_meta_orcamentaria_ano1 = "vr_meta_orcamentaria_ano1_acoes",
                               vr_meta_orcamentaria_ano2 = "vr_meta_orcamentaria_ano2_acoes",
                               vr_meta_orcamentaria_ano3 = "vr_meta_orcamentaria_ano3_acoes"
                               )
                 )
    
  y <- programas_planejamento |> 
       dplyr::filter(is_deleted_programa == FALSE) |> 
       unique(by = c("uo_programa_cod", "programa_cod")) |> 
       aggregate("vr_meta_orcamentaria_ano", 
                 by = key,
                 rename = list(
                               vr_meta_orcamentaria_ano0 = "vr_meta_orcamentaria_ano0_programas",
                               vr_meta_orcamentaria_ano1 = "vr_meta_orcamentaria_ano1_programas",
                               vr_meta_orcamentaria_ano2 = "vr_meta_orcamentaria_ano2_programas",
                               vr_meta_orcamentaria_ano3 = "vr_meta_orcamentaria_ano3_programas"
                               )
                 )
  
  df <- merge(x, y,
              by = key,
              all = TRUE
              ) |>
        as_accounting()

  report <- check_that(df, 
                       vr_meta_orcamentaria_ano0_acoes == vr_meta_orcamentaria_ano0_programas,
                       vr_meta_orcamentaria_ano1_acoes == vr_meta_orcamentaria_ano1_programas,
                       vr_meta_orcamentaria_ano2_acoes == vr_meta_orcamentaria_ano2_programas,
                       vr_meta_orcamentaria_ano3_acoes == vr_meta_orcamentaria_ano3_programas
                       )
  
  default_message = "A uo {uo_programa_cod}, 
        programa {programa_cod}, 
        área temática {area_tematica_cod} está com valor 
        {ifelse(
            vr_meta_orcamentaria_ano0_acoes != vr_meta_orcamentaria_ano0_programas | 
            is.na(vr_meta_orcamentaria_ano0_acoes) | 
            is.na(vr_meta_orcamentaria_ano0_programas), 
            paste0('de meta orçamentária do ano 0, base programas (R$ ', 
                   vr_meta_orcamentaria_ano0_programas, 
                   ') diferente da base ações-planejamento (R$ ', 
                   vr_meta_orcamentaria_ano0_acoes, 
                   '). Diferença de R$ ', 
                   ifelse(is.na(vr_meta_orcamentaria_ano0_acoes) & 
                          is.na(vr_meta_orcamentaria_ano0_programas), 
                          'NA', 
                          ifelse(is.na(vr_meta_orcamentaria_ano0_acoes), 
                                 vr_meta_orcamentaria_ano0_programas, 
                                 ifelse(is.na(vr_meta_orcamentaria_ano0_programas), 
                                        vr_meta_orcamentaria_ano0_acoes, 
                                        vr_meta_orcamentaria_ano0_acoes - 
                                        vr_meta_orcamentaria_ano0_programas))), '.'), 
            ifelse(
                vr_meta_orcamentaria_ano1_acoes != vr_meta_orcamentaria_ano1_programas | 
                is.na(vr_meta_orcamentaria_ano1_acoes) | 
                is.na(vr_meta_orcamentaria_ano1_programas), 
                paste0('de meta orçamentária do ano 1, base programas (R$ ', 
                       vr_meta_orcamentaria_ano1_programas, 
                       ') diferente da base ações-planejamento (R$ ', 
                       vr_meta_orcamentaria_ano1_acoes, 
                       '). Diferença de R$ ', 
                       ifelse(is.na(vr_meta_orcamentaria_ano1_acoes) & 
                              is.na(vr_meta_orcamentaria_ano1_programas), 
                              'NA', 
                              ifelse(is.na(vr_meta_orcamentaria_ano1_acoes), 
                                     vr_meta_orcamentaria_ano1_programas, 
                                     ifelse(is.na(vr_meta_orcamentaria_ano1_programas), 
                                            vr_meta_orcamentaria_ano1_acoes, 
                                            vr_meta_orcamentaria_ano1_acoes - 
                                            vr_meta_orcamentaria_ano1_programas))), '.'), 
                ifelse(
                    vr_meta_orcamentaria_ano2_acoes != vr_meta_orcamentaria_ano2_programas | 
                    is.na(vr_meta_orcamentaria_ano2_acoes) | 
                    is.na(vr_meta_orcamentaria_ano2_programas), 
                    paste0('de meta orçamentária do ano 2, base programas (R$ ', 
                           vr_meta_orcamentaria_ano2_programas, 
                           ') diferente da base ações-planejamento (R$ ', 
                           vr_meta_orcamentaria_ano2_acoes, 
                           '). Diferença de R$ ', 
                           ifelse(is.na(vr_meta_orcamentaria_ano2_acoes) & 
                                  is.na(vr_meta_orcamentaria_ano2_programas), 
                                  'NA', 
                                  ifelse(is.na(vr_meta_orcamentaria_ano2_acoes), 
                                         vr_meta_orcamentaria_ano2_programas, 
                                         ifelse(is.na(vr_meta_orcamentaria_ano2_programas), 
                                                vr_meta_orcamentaria_ano2_acoes, 
                                                vr_meta_orcamentaria_ano2_acoes - 
                                                vr_meta_orcamentaria_ano2_programas))), '.'), 
                      paste0('de meta orçamentária do ano 3, base programas (R$ ', 
                             vr_meta_orcamentaria_ano3_programas, 
                             ') diferente da base ações-planejamento (R$ ', 
                             vr_meta_orcamentaria_ano3_acoes, 
                             '). Diferença de R$ ', 
                             ifelse(is.na(vr_meta_orcamentaria_ano3_acoes) & 
                                    is.na(vr_meta_orcamentaria_ano3_programas), 
                                    'NA', 
                                    ifelse(is.na(vr_meta_orcamentaria_ano3_acoes), 
                                           vr_meta_orcamentaria_ano3_programas, 
                                           ifelse(is.na(vr_meta_orcamentaria_ano3_programas), 
                                                  vr_meta_orcamentaria_ano3_acoes, 
                                                  vr_meta_orcamentaria_ano3_acoes - 
                                                  vr_meta_orcamentaria_ano3_programas))), '.')
                )
            )
        )}"
  
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

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
check_valores_sigplan_localizadores <- function(acoes_planejamento,
                                                localizadores_todos_planejamento,
                                                stop_on_failure = FALSE,
                                                output = FALSE,
                                                json_outfile = NULL,
                                                log_level = "ERROR",
                                                msg_template = NULL
                                                ) {

  key <- c("uo_acao_cod",
           "programa_cod",
           "area_tematica_cod",
           "acao_cod",
           "funcao_cod",
           "subfuncao_cod",
           "iag_cod"
           )
  
  x <- acoes_planejamento |> 
       mutate(across(starts_with("vr_"),
                     replace_na
                     )
              ) |> 
       aggregate("vr_meta_orcamentaria_ano|vr_meta_fisica_ano", 
                 by = key,
                 filter = is_deleted_programa == FALSE &
                          is_deleted_acao == FALSE,
                 rename = list(
                               vr_meta_orcamentaria_ano0 = "vr_meta_orcamentaria_ano0_acoes",
                               vr_meta_orcamentaria_ano1 = "vr_meta_orcamentaria_ano1_acoes",
                               vr_meta_orcamentaria_ano2 = "vr_meta_orcamentaria_ano2_acoes",
                               vr_meta_orcamentaria_ano3 = "vr_meta_orcamentaria_ano3_acoes",
                               vr_meta_fisica_ano0 = "vr_meta_fisica_ano0_acoes",
                               vr_meta_fisica_ano1 = "vr_meta_fisica_ano1_acoes",
                               vr_meta_fisica_ano2 = "vr_meta_fisica_ano2_acoes",
                               vr_meta_fisica_ano3 = "vr_meta_fisica_ano3_acoes"
                                )
                 )
  
  # uma acao pode ter sido deletada sem que os seus localizadores sejam marcados como deletados
  y <- localizadores_todos_planejamento |> 
       mutate(across(starts_with("vr_"),
                     replace_na
                     )
              ) |> 
       aggregate("vr_meta_orcamentaria_ano|vr_meta_fisica_ano", 
                 by = key,
                 filter = is_deleted_programa == FALSE & 
                          is_deleted_acao == FALSE &
                          is_deleted_localizador == FALSE,
                 rename = list(
                               vr_meta_orcamentaria_ano0 = "vr_meta_orcamentaria_ano0_localizadores",
                               vr_meta_orcamentaria_ano1 = "vr_meta_orcamentaria_ano1_localizadores",
                               vr_meta_orcamentaria_ano2 = "vr_meta_orcamentaria_ano2_localizadores",
                               vr_meta_orcamentaria_ano3 = "vr_meta_orcamentaria_ano3_localizadores",
                               vr_meta_fisica_ano0 = "vr_meta_fisica_ano0_localizadores",
                               vr_meta_fisica_ano1 = "vr_meta_fisica_ano1_localizadores",
                               vr_meta_fisica_ano2 = "vr_meta_fisica_ano2_localizadores",
                               vr_meta_fisica_ano3 = "vr_meta_fisica_ano3_localizadores"
                              )
                 )
        
  df <- merge(x, y,
              by = key,
              all = TRUE
              ) |>
        as_accounting()
  
  report <- check_that(df,
                       vr_meta_orcamentaria_ano0_acoes == vr_meta_orcamentaria_ano0_localizadores,
                       vr_meta_orcamentaria_ano1_acoes == vr_meta_orcamentaria_ano1_localizadores,
                       vr_meta_orcamentaria_ano2_acoes == vr_meta_orcamentaria_ano2_localizadores,
                       vr_meta_orcamentaria_ano3_acoes == vr_meta_orcamentaria_ano3_localizadores,
                       vr_meta_fisica_ano0_acoes == vr_meta_fisica_ano0_localizadores,
                       vr_meta_fisica_ano1_acoes == vr_meta_fisica_ano1_localizadores,
                       vr_meta_fisica_ano2_acoes == vr_meta_fisica_ano2_localizadores,
                       vr_meta_fisica_ano3_acoes == vr_meta_fisica_ano3_localizadores
                       )
  
  # radical da mensagem default_message
  default_message = "A uo {uo_acao_cod}, programa {programa_cod}, ação {acao_cod}, função {funcao_cod}, subfunção {subfuncao_cod}, área temática {area_tematica_cod} está com valor {ifelse(vr_meta_orcamentaria_ano0_acoes != vr_meta_orcamentaria_ano0_localizadores | is.na(vr_meta_orcamentaria_ano0_acoes) | is.na(vr_meta_orcamentaria_ano0_localizadores), paste0('de meta orçamentária do ano 0, base localizadores (R$ ', vr_meta_orcamentaria_ano0_localizadores, ') diferente da base ações-planejamento (R$ ', vr_meta_orcamentaria_ano0_acoes, '). Diferença de R$ ', ifelse(is.na(vr_meta_orcamentaria_ano0_acoes) & is.na(vr_meta_orcamentaria_ano0_localizadores), 'NA', ifelse(is.na(vr_meta_orcamentaria_ano0_acoes), vr_meta_orcamentaria_ano0_localizadores, ifelse(is.na(vr_meta_orcamentaria_ano0_localizadores), vr_meta_orcamentaria_ano0_acoes, vr_meta_orcamentaria_ano0_acoes - vr_meta_orcamentaria_ano0_localizadores))), '.'), ifelse(vr_meta_orcamentaria_ano1_acoes != vr_meta_orcamentaria_ano1_localizadores | is.na(vr_meta_orcamentaria_ano1_acoes) | is.na(vr_meta_orcamentaria_ano1_localizadores), paste0('de meta orçamentária do ano 1, base localizadores (R$ ', vr_meta_orcamentaria_ano1_localizadores, ') diferente da base ações-planejamento (R$ ', vr_meta_orcamentaria_ano1_acoes, '). Diferença de R$ ', ifelse(is.na(vr_meta_orcamentaria_ano1_acoes) & is.na(vr_meta_orcamentaria_ano1_localizadores), 'NA', ifelse(is.na(vr_meta_orcamentaria_ano1_acoes), vr_meta_orcamentaria_ano1_localizadores, ifelse(is.na(vr_meta_orcamentaria_ano1_localizadores), vr_meta_orcamentaria_ano1_acoes, vr_meta_orcamentaria_ano1_acoes - vr_meta_orcamentaria_ano1_localizadores))), '.'), ifelse(vr_meta_orcamentaria_ano2_acoes != vr_meta_orcamentaria_ano2_localizadores | is.na(vr_meta_orcamentaria_ano2_acoes) | is.na(vr_meta_orcamentaria_ano2_localizadores), paste0('de meta orçamentária do ano 2, base localizadores (R$ ', vr_meta_orcamentaria_ano2_localizadores, ') diferente da base ações-planejamento (R$ ', vr_meta_orcamentaria_ano2_acoes, '). Diferença de R$ ', ifelse(is.na(vr_meta_orcamentaria_ano2_acoes) & is.na(vr_meta_orcamentaria_ano2_localizadores), 'NA', ifelse(is.na(vr_meta_orcamentaria_ano2_acoes), vr_meta_orcamentaria_ano2_localizadores, ifelse(is.na(vr_meta_orcamentaria_ano2_localizadores), vr_meta_orcamentaria_ano2_acoes, vr_meta_orcamentaria_ano2_acoes - vr_meta_orcamentaria_ano2_localizadores))), '.'), ifelse(vr_meta_orcamentaria_ano3_acoes != vr_meta_orcamentaria_ano3_localizadores | is.na(vr_meta_orcamentaria_ano3_acoes) | is.na(vr_meta_orcamentaria_ano3_localizadores), paste0('de meta orçamentária do ano 3, base localizadores (R$ ', vr_meta_orcamentaria_ano3_localizadores, ') diferente da base ações-planejamento (R$ ', vr_meta_orcamentaria_ano3_acoes, '). Diferença de R$ ', ifelse(is.na(vr_meta_orcamentaria_ano3_acoes) & is.na(vr_meta_orcamentaria_ano3_localizadores), 'NA', ifelse(is.na(vr_meta_orcamentaria_ano3_acoes), vr_meta_orcamentaria_ano3_localizadores, ifelse(is.na(vr_meta_orcamentaria_ano3_localizadores), vr_meta_orcamentaria_ano3_acoes, vr_meta_orcamentaria_ano3_acoes - vr_meta_orcamentaria_ano3_localizadores))), '.'), ifelse(vr_meta_fisica_ano0_acoes != vr_meta_fisica_ano0_localizadores | is.na(vr_meta_fisica_ano0_acoes) | is.na(vr_meta_fisica_ano0_localizadores), paste0('de meta física do ano 0, base localizadores (R$ ', vr_meta_fisica_ano0_localizadores, ') diferente da base ações-planejamento (R$ ', vr_meta_fisica_ano0_acoes, '). Diferença de R$ ', ifelse(is.na(vr_meta_fisica_ano0_acoes) & is.na(vr_meta_fisica_ano0_localizadores), 'NA', ifelse(is.na(vr_meta_fisica_ano0_acoes), vr_meta_fisica_ano0_localizadores, ifelse(is.na(vr_meta_fisica_ano0_localizadores), vr_meta_fisica_ano0_acoes, vr_meta_fisica_ano0_acoes - vr_meta_fisica_ano0_localizadores))), '.'), ifelse(vr_meta_fisica_ano1_acoes != vr_meta_fisica_ano1_localizadores | is.na(vr_meta_fisica_ano1_acoes) | is.na(vr_meta_fisica_ano1_localizadores), paste0('de meta física do ano 1, base localizadores (R$ ', vr_meta_fisica_ano1_localizadores, ') diferente da base ações-planejamento (R$ ', vr_meta_fisica_ano1_acoes, '). Diferença de R$ ', ifelse(is.na(vr_meta_fisica_ano1_acoes) & is.na(vr_meta_fisica_ano1_localizadores), 'NA', ifelse(is.na(vr_meta_fisica_ano1_acoes), vr_meta_fisica_ano1_localizadores, ifelse(is.na(vr_meta_fisica_ano1_localizadores), vr_meta_fisica_ano1_acoes, vr_meta_fisica_ano1_acoes - vr_meta_fisica_ano1_localizadores))), '.'), ifelse(vr_meta_fisica_ano2_acoes != vr_meta_fisica_ano2_localizadores | is.na(vr_meta_fisica_ano2_acoes) | is.na(vr_meta_fisica_ano2_localizadores), paste0('de meta física do ano 2, base localizadores (R$ ', vr_meta_fisica_ano2_localizadores, ') diferente da base ações-planejamento (R$ ', vr_meta_fisica_ano2_acoes, '). Diferença de R$ ', ifelse(is.na(vr_meta_fisica_ano2_acoes) & is.na(vr_meta_fisica_ano2_localizadores), 'NA', ifelse(is.na(vr_meta_fisica_ano2_acoes), vr_meta_fisica_ano2_localizadores, ifelse(is.na(vr_meta_fisica_ano2_localizadores), vr_meta_fisica_ano2_acoes, vr_meta_fisica_ano2_acoes - vr_meta_fisica_ano2_localizadores))), '.'), paste0('de meta física do ano 3, base localizadores (R$ ', vr_meta_fisica_ano3_localizadores, ') diferente da base ações-planejamento (R$ ', vr_meta_fisica_ano3_acoes, '). Diferença de R$ ', ifelse(is.na(vr_meta_fisica_ano3_acoes) & is.na(vr_meta_fisica_ano3_localizadores), 'NA', ifelse(is.na(vr_meta_fisica_ano3_acoes), vr_meta_fisica_ano3_localizadores, ifelse(is.na(vr_meta_fisica_ano3_localizadores), vr_meta_fisica_ano3_acoes, vr_meta_fisica_ano3_acoes - vr_meta_fisica_ano3_localizadores))), '.'))))))))}"
  
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

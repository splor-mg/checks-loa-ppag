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
check_valores_sigplan_localizadores <- function(acoes_planejamento, localizadores_todos_planejamento, stop_on_failure = FALSE, output = FALSE) {
  key <- c("programa_cod", "area_tematica_cod", "acao_cod", "uo_acao_cod", "funcao_cod", "subfuncao_cod", "iag_cod")
  
  x <- acoes_planejamento |> 
        mutate(across(starts_with("vr_"), replace_na)) |> 
        aggregate("vr_meta_orcamentaria_ano|vr_meta_fisica_ano", 
                  by = key,
                  filter = is_deleted_programa == FALSE & is_deleted_acao == FALSE,
                  rename = list(
                    vr_meta_orcamentaria_ano0 = "vr_meta_orcamentaria_ano0_acoes",
                    vr_meta_orcamentaria_ano1 = "vr_meta_orcamentaria_ano1_acoes",
                    vr_meta_orcamentaria_ano2 = "vr_meta_orcamentaria_ano2_acoes",
                    vr_meta_orcamentaria_ano3 = "vr_meta_orcamentaria_ano3_acoes",
                    vr_meta_fisica_ano0 = "vr_meta_fisica_ano0_acoes",
                    vr_meta_fisica_ano1 = "vr_meta_fisica_ano1_acoes",
                    vr_meta_fisica_ano2 = "vr_meta_fisica_ano2_acoes",
                    vr_meta_fisica_ano3 = "vr_meta_fisica_ano3_acoes"
                  ))
        
  
  # uma acao pode ter sido deletada sem que os seus localizadores sejam marcados como deletados
  y <- localizadores_todos_planejamento |> 
        mutate(across(starts_with("vr_"), replace_na)) |> 
        aggregate("vr_meta_orcamentaria_ano|vr_meta_fisica_ano", 
                  filter = is_deleted_programa == FALSE & is_deleted_acao == FALSE & is_deleted_localizador == FALSE,
                  by = key,
                  rename = list(
                    vr_meta_orcamentaria_ano0 = "vr_meta_orcamentaria_ano0_localizadores",
                    vr_meta_orcamentaria_ano1 = "vr_meta_orcamentaria_ano1_localizadores",
                    vr_meta_orcamentaria_ano2 = "vr_meta_orcamentaria_ano2_localizadores",
                    vr_meta_orcamentaria_ano3 = "vr_meta_orcamentaria_ano3_localizadores",
                    vr_meta_fisica_ano0 = "vr_meta_fisica_ano0_localizadores",
                    vr_meta_fisica_ano1 = "vr_meta_fisica_ano1_localizadores",
                    vr_meta_fisica_ano2 = "vr_meta_fisica_ano2_localizadores",
                    vr_meta_fisica_ano3 = "vr_meta_fisica_ano3_localizadores"
                  ))
        
  
  df <- merge(x, y, by = key, all = TRUE) |> as_accounting()
  report <- df |> check_that(
    vr_meta_orcamentaria_ano0_acoes == vr_meta_orcamentaria_ano0_localizadores,
    vr_meta_orcamentaria_ano1_acoes == vr_meta_orcamentaria_ano1_localizadores,
    vr_meta_orcamentaria_ano2_acoes == vr_meta_orcamentaria_ano2_localizadores,
    vr_meta_orcamentaria_ano3_acoes == vr_meta_orcamentaria_ano3_localizadores,
    vr_meta_fisica_ano0_acoes == vr_meta_fisica_ano0_localizadores,
    vr_meta_fisica_ano1_acoes == vr_meta_fisica_ano1_localizadores,
    vr_meta_fisica_ano2_acoes == vr_meta_fisica_ano2_localizadores,
    vr_meta_fisica_ano3_acoes == vr_meta_fisica_ano3_localizadores)
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

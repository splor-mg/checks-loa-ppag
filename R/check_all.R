#' Verifica todos os testes de validação
#'
#' @export
check_all <- function(programas_planejamento,
                      acoes_planejamento,
                      localizadores_todos_planejamento,
                      indicadores_planejamento,
                      base_categoria_pessoal,
                      base_detalhamento_obras,
                      base_intra_orcamentaria_detalhamento,
                      base_intra_orcamentaria_repasse,
                      base_limite_cota,
                      base_orcam_despesa_item_fiscal,
                      base_orcam_receita_fiscal,
                      base_orcam_receita_investimento,
                      base_qdd_fiscal,
                      base_qdd_investimento,
                      base_qdd_plurianual,
                      base_qdd_plurianual_invest,
                      base_repasse_recursos,
                      desc_setor_governo,
                      uo_acao_inativo_civil,
                      output = FALSE,
                      stop_on_failure = FALSE) {

            checks <- list(
            check_area_tematica_exists_programas = check_area_tematica_exists_programas(programas_planejamento),
            check_area_tematica_exists_acoes = check_area_tematica_exists_acoes(acoes_planejamento),
            check_area_tematica_exists_localizadores = check_area_tematica_exists_localizadores(localizadores_todos_planejamento),
            check_count_programas = check_count_programas(programas_planejamento, acoes_planejamento, localizadores_todos_planejamento),
            check_count_programas_is_deleted = check_count_programas_is_deleted(programas_planejamento, acoes_planejamento, localizadores_todos_planejamento),
            check_count_programas_is_new = check_count_programas_is_new(programas_planejamento, acoes_planejamento),
            check_count_acoes = check_count_acoes(acoes_planejamento, localizadores_todos_planejamento),
            check_count_acoes_is_deleted = check_count_acoes_is_deleted(acoes_planejamento, localizadores_todos_planejamento),
            check_detalhamento_obras_acoes_exists = check_detalhamento_obras_acoes_exists(base_detalhamento_obras, acoes_planejamento),
            check_detalhamento_obras_numero_siad_duplicated = check_detalhamento_obras_numero_siad_duplicated(base_detalhamento_obras),
            check_detalhamento_obras_orcam_fiscal_tesouro = check_detalhamento_obras_orcam_fiscal_tesouro(base_qdd_fiscal, base_detalhamento_obras),
            check_detalhamento_obras_orcam_fiscal_tesouro_plurianual = check_detalhamento_obras_orcam_fiscal_tesouro_plurianual(base_qdd_plurianual, base_detalhamento_obras),
            check_detalhamento_obras_orcam_fiscal_outros = check_detalhamento_obras_orcam_fiscal_outros(base_qdd_fiscal, base_detalhamento_obras),
            check_detalhamento_obras_orcam_fiscal_outros_plurianual = check_detalhamento_obras_orcam_fiscal_outros_plurianual(base_qdd_plurianual, base_detalhamento_obras),
            check_detalhamento_obras_orcam_investimento = check_detalhamento_obras_orcam_investimento(base_qdd_investimento, base_detalhamento_obras),
            check_detalhamento_obras_orcam_investimento_plurianual = check_detalhamento_obras_orcam_investimento_plurianual(base_qdd_plurianual_invest, base_detalhamento_obras),
            check_detalhamento_pessoal_inativo_civil = check_detalhamento_pessoal_inativo_civil(base_categoria_pessoal, base_qdd_fiscal, uo_acao_inativo_civil),
            check_fechamento_fonte_orcam_fiscal = check_fechamento_fonte_orcam_fiscal(base_orcam_receita_fiscal, base_qdd_fiscal),
            check_fechamento_fonte_orcam_investimento = check_fechamento_fonte_orcam_investimento(base_orcam_receita_investimento, base_qdd_investimento),
            check_indicadores_consistency_indice_de_referencia_exists = check_indicadores_consistency_indice_de_referencia_exists(indicadores_planejamento),
            check_indicadores_consistency_indice_de_referencia_missing = check_indicadores_consistency_indice_de_referencia_missing(indicadores_planejamento),
            check_indicadores_justificativa_indice_referencia_em_apuracao = check_indicadores_justificativa_indice_referencia_em_apuracao(indicadores_planejamento),
            check_indicadores_indice_referencia_data_futura = check_indicadores_indice_referencia_data_futura(indicadores_planejamento),
            check_indicadores_indice_referencia_exists = check_indicadores_indice_referencia_exists(indicadores_planejamento),
            check_indicadores_indice_referencia_zerado = check_indicadores_indice_referencia_zerado(indicadores_planejamento),
            check_indicadores_justificativa_previsao_em_apuracao = check_indicadores_justificativa_previsao_em_apuracao(indicadores_planejamento),
            check_indicadores_previsoes_exists = check_indicadores_previsoes_exists(indicadores_planejamento),
            check_indicadores_previsoes_zeradas = check_indicadores_previsoes_zeradas(indicadores_planejamento),
            check_intra_despesa = check_intra_despesa(base_orcam_despesa_item_fiscal, base_intra_orcamentaria_repasse),
            check_intra_receita = check_intra_receita(base_orcam_receita_fiscal, base_intra_orcamentaria_repasse),
            check_intra_detalhamento = check_intra_detalhamento(base_intra_orcamentaria_detalhamento),
            check_limite_ano0 = check_limite_ano0(base_limite_cota),
            check_limite_plurianual = check_limite_plurianual(base_limite_cota),
            check_ods_exists = check_ods_exists(programas_planejamento),
            check_ods_consistency = check_ods_consistency(programas_planejamento),
            check_projetos_estrategicos = check_projetos_estrategicos(acoes_planejamento),
            check_repasse_recursos = check_repasse_recursos(base_qdd_fiscal, base_repasse_recursos),
            check_setor_governo = check_setor_governo(acoes_planejamento, desc_setor_governo),
            check_valores_sigplan_localizadores = check_valores_sigplan_localizadores(acoes_planejamento, localizadores_todos_planejamento),
            check_valores_sigplan_programas = check_valores_sigplan_programas(acoes_planejamento, programas_planejamento),
            check_valores_qdd_fiscal = check_valores_qdd_fiscal(base_qdd_fiscal, acoes_planejamento),
            check_valores_qdd_plurianual = check_valores_qdd_plurianual(base_qdd_plurianual, acoes_planejamento),
            check_valores_qdd_investimento = check_valores_qdd_investimento(base_qdd_investimento, acoes_planejamento),
            check_valores_qdd_plurianual_invest = check_valores_qdd_plurianual_invest(base_qdd_plurianual_invest, acoes_planejamento),
            check_valores_sisor = check_valores_sisor(base_qdd_fiscal, base_orcam_despesa_item_fiscal)
            )

            is_valid <- all(unlist(checks))
            
            if(is_valid == FALSE & stop_on_failure == TRUE) {
              stop("There are failing checks.")
            }
            
            if(output == TRUE) {
              result <- data.table::data.table(check = names(checks), valid = unlist(checks))
              data.table::setorder(result, valid)
              return(result)
            }
            
            is_valid
}

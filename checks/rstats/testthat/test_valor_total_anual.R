test_that("Total do orçamento fiscal e investimento SIGPLAN vs SISOR", {
  #' @details
  #' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de 
  #' investimento das empresas controladas) coincide com a projeção do PPAG 
  #' para o ano seguinte
  
  sisor <- sisor$base_qdd_fiscal[, sum(VL_LOA_DESP)]
  sigplan <- sigplan$acoes_planejamento[is_deleted_acao == FALSE & 
                                identificador_tipo_acao_cod %in% c(1, 2, 4, 7, 9), 
                                sum(vr_meta_orcamentaria_ano0)]
  expect_equal(sisor, sigplan)
})

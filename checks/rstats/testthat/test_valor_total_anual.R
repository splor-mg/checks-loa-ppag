test_that("Total do orçamento fiscal e investimento SIGPLAN vs SISOR", {
  #' @details
  #' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de 
  #' investimento das empresas controladas) coincide com a projeção do PPAG 
  #' para o ano seguinte
  
  base_qdd_fiscal <- fread(here("datapackages/loa/data/base_qdd_fiscal.csv"))
  
  acoes_planejamento <- read_resource(ppag, "acoes_planejamento") |> as.data.table()
  acoes_planejamento <- fread(here("datapackages", "ppag", "acoes_planejamento.csv"))
  
  sisor <- base_qdd_fiscal[, sum(VL_LOA_DESP)]
  sigplan <- acoes_planejamento[is_deleted_acao == FALSE & 
                                identificador_tipo_acao_cod %in% c(1, 2, 4, 7, 9), 
                                sum(vr_meta_orcamentaria_ano0)]
  expect_equal(sisor, sigplan)
})

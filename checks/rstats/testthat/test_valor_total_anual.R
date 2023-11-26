test_that("Total do orçamento fiscal e investimento SIGPLAN vs SISOR", {
  base_qdd_fiscal <- sisor$base_qdd_fiscal
  acoes_planejamento <- sigplan$acoes_planejamento
  expect_true(
    checkmate::check_total_orcamento_fiscal(base_qdd_fiscal, 
                                            acoes_planejamento)
  )
})

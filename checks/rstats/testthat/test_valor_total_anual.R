test_that("Total do orçamento fiscal e investimento SIGPLAN vs SISOR", {
  expect_true(checks::check_total_orcamento_fiscal(sisor$base_qdd_fiscal, sigplan$acoes_planejamento))
})

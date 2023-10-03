from checks import check_total_orcamento_fiscal

def test_check_total_orcamento_fiscal(base_qdd_fiscal, acoes_planejamento):
  result = check_total_orcamento_fiscal(base_qdd_fiscal, acoes_planejamento)
  assert result

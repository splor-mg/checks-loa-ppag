import pytest
from frictionless import Package

@pytest.fixture
def acoes_planejamento():
  sigplan = Package('datapackages/sigplan/datapackage.json')
  result = sigplan.get_resource('acoes_planejamento')
  return result

@pytest.fixture
def base_qdd_fiscal():
  sisor = Package('datapackages/sisor/datapackage.json')
  result = sisor.get_resource('base_qdd_fiscal')
  return result

library(checkmate)

sigplan <- read_datapackage("datapackages/sigplan/datapackage.json")
sisor <- read_datapackage("datapackages/sisor/datapackage.json")

base_qdd_fiscal <- sisor$base_qdd_fiscal
base_qdd_investimento <- sisor$base_qdd_investimento
base_qdd_plurianual_invest <- sisor$base_qdd_plurianual_invest

acoes_planejamento <- sigplan$acoes_planejamento


check_equivalencia_qdd_investimento(sisor$base_qdd_investimento, sigplan$acoes_planejamento)
check_equivalencia_qdd_plurianual_invest(sisor$base_qdd_plurianual_invest, sigplan$acoes_planejamento)

names(base)
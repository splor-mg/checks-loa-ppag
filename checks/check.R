library(checkmate)

# checks

out <- check_consistencia_sisor(base_qdd_fiscal, base_orcam_despesa_item_fiscal, output = TRUE)

check_area_tematica_exists_acoes(sigplan$acoes_planejamento)

# bases

sigplan <- read_datapackage("datapackages/sigplan/datapackage.json")
sisor <- read_datapackage("datapackages/sisor/datapackage.json")

base_qdd_fiscal <- sisor$base_qdd_fiscal
base_qdd_plurianual <- sisor$base_qdd_plurianual
base_qdd_investimento <- sisor$base_qdd_investimento
base_qdd_plurianual_invest <- sisor$base_qdd_plurianual_invest
base_repasse_recursos <- sisor$base_repasse_recursos
acoes_planejamento <- sigplan$acoes_planejamento
base_intra_orcamentaria_detalhamento <- sisor$base_intra_orcamentaria_detalhamento
base_intra_orcamentaria_repasse <- sisor$base_intra_orcamentaria_repasse
base_orcam_receita_fiscal <- sisor$base_orcam_receita_fiscal
base_orcam_despesa_item_fiscal <- sisor$base_orcam_despesa_item_fiscal
base_detalhamento_obras <- sisor$base_detalhamento_obras



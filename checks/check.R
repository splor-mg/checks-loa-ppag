library(checkmate)

# to_data_table("docs/checks-loa-ppag.yaml") |> writexl::write_xlsx("output.xlsx")

# checks

check_previsoes_zeradas(indicadores_planejamento)
out <- check_previsoes_zeradas(indicadores_planejamento, output = TRUE)

out$fail |> View()



# bases

sigplan <- read_datapackage("datapackages/sigplan/datapackage.json")
acoes_planejamento <- sigplan$acoes_planejamento
programas_planejamento <- sigplan$programas_planejamento
indicadores_planejamento <- sigplan$indicadores_planejamento
localizadores_todos_planejamento <- sigplan$localizadores_todos_planejamento

sisor <- read_datapackage("datapackages/sisor/datapackage.json")
base_qdd_fiscal <- sisor$base_qdd_fiscal
base_qdd_plurianual <- sisor$base_qdd_plurianual
base_qdd_investimento <- sisor$base_qdd_investimento
base_qdd_plurianual_invest <- sisor$base_qdd_plurianual_invest
base_repasse_recursos <- sisor$base_repasse_recursos
base_intra_orcamentaria_detalhamento <- sisor$base_intra_orcamentaria_detalhamento
base_intra_orcamentaria_repasse <- sisor$base_intra_orcamentaria_repasse
base_orcam_receita_fiscal <- sisor$base_orcam_receita_fiscal
base_orcam_despesa_item_fiscal <- sisor$base_orcam_despesa_item_fiscal
base_detalhamento_obras <- sisor$base_detalhamento_obras



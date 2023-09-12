library(data.table); library(frictionless)

sigplan_dp <- read_package(here::here("datapackages/sigplan/datapackage.json"))
sigplan <- list(acoes_planejamento = as.data.table(read_resource(sigplan_dp, "acoes_planejamento")))

resources(sigplan_dp)
indicadores_planejamento <- read_resource(sigplan_dp, "indicadores_planejamento") |> as.data.table()

indicadores_planejamento[, .N, indice_de_referencia][order(indice_de_referencia)] |> View()

# ok porque apesar de NA ele está em apuração
indicadores_planejamento[is.na(indice_de_referencia), .(programa_cod, programa_nome, indicador, indice_de_referencia, em_apuracao_indice_de_referencia)] |> View()

sisor_dp <- read_package(here::here("datapackages/sisor/datapackage.json"))
sisor <- list(base_qdd_fiscal = as.data.table(read_resource(sisor_dp, "base_qdd_fiscal")))

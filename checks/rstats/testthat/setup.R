library(data.table)
library(frictionless)
library(checkmate)

sigplan_dp <- suppressMessages(read_package(here::here("datapackages/sigplan/datapackage.json")))
sigplan <- list(acoes_planejamento = as.data.table(read_resource(sigplan_dp, "acoes_planejamento")))

sisor_dp <- suppressMessages(read_package(here::here("datapackages/sisor/datapackage.json")))
sisor <- list(base_qdd_fiscal = as.data.table(read_resource(sisor_dp, "base_qdd_fiscal")))

library(frictionless)
library(checkmate)

sigplan <- read_package("datapackages/sigplan/datapackage.json")
localizadores_todos_planejamento <- read_resource(sigplan, "localizadores_todos_planejamento")

?check_area_tematica_exists
check_area_tematica_exists_localizadores

check_area_tematica_exists_localizadores(localizadores_todos_planejamento)
check_area_tematica_exists_localizadores(localizadores_todos_planejamento, stop_on_failure = TRUE)
result <- check_area_tematica_exists_localizadores(localizadores_todos_planejamento, output = TRUE)


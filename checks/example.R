library(checkmate)

sigplan <- read_datapackage("datapackages/sigplan/datapackage.json")
sisor <- read_datapackage("datapackages/sisor/datapackage.json")

base_qdd_fiscal <- sisor$base_qdd_fiscal
base_qdd_plurianual <- sisor$base_qdd_plurianual
base_qdd_investimento <- sisor$base_qdd_investimento
base_qdd_plurianual_invest <- sisor$base_qdd_plurianual_invest
base_repasse_recursos <- sisor$base_repasse_recursos
acoes_planejamento <- sigplan$acoes_planejamento
sisor$base_intra_orcamentaria_detalhamento
sisor$base_intra_orcamentaria_repasse


check_repasse_recursos(base_qdd_fiscal, base_repasse_recursos)


data <- data.frame(a = c("a", "a", "a", "a","b"), b_1 = rnorm(5), b_2 = rnorm(5))

debugonce(summarize)
summarize(data, by = "dim", rename = list(b = "fact", a = "dim"))
summarize(data, "b", by = "a")
summarize(data, by = "a")
summarize(data, by = "a", columns = "_1")

summarize(data, by = "a", filter = b > 0)
summarize(data, by = "a", filter = a == "b" & b > 0)

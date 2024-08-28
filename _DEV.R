library(data.table)
library(testthat)
library(validate)
library(rlang)
library(dplyr)
library(glue)
library(checksplanejamento)

sigplan <- read_datapackage("../cc-checks-planejamento-oficina/datapackages/sigplan/datapackage.json")
sisor <- read_datapackage("../cc-checks-planejamento-oficina/datapackages/sisor/datapackage.json")

debugonce(check_count_acoes)

#===============================================================================
#1

BASE <- sigplan$acoes_planejamento

check_1 <- check_area_tematica_exists_acoes(BASE,
                                            stop_on_failure = FALSE,
                                            output = TRUE,
                                            json_outfile = "logfile.json", 
                                            log_level = "ERROR",
                                            msg_template = NULL
)

CODIGO_REF <- 1
cod_para_alterar <- "KKKKKKKKKKKKKKKKKKKKKKKK"
# colnames(sigplan$acoes_planejamento) |> print()
# unique(sigplan$acoes_planejamento[, .(programa_cod)]) |> print()
# num_linhas <- nrow(sigplan$acoes_planejamento[programa_cod == CODIGO_REF & is_deleted_acao == FALSE]) |> print()
index <- which(BASE$programa_cod == CODIGO_REF & BASE$is_deleted_acao == FALSE)[1] |> print()
BASE[index, area_tematica_cod := cod_para_alterar]
View(BASE)
CODIGO_REF_2 <- 2
index_2 <- which(BASE$programa_cod == CODIGO_REF_2 & BASE$is_deleted_acao == FALSE)[1] |> print()
BASE[index_2, area_tematica_cod := cod_para_alterar]

#===============================================================================
#2

BASE <- sigplan$localizadores_todos_planejamento

check_2 <- check_area_tematica_exists_localizadores(BASE,
                                                    stop_on_failure = FALSE,
                                                    output = TRUE,
                                                    json_outfile = "logfile.json", 
                                                    log_level = "ERROR",
                                                    msg_template = NULL
)

CODIGO_REF <- 2
cod_para_alterar <- "YYYYYYYYYYYYYYYYYYYYYYYY"
# colnames(sigplan$acoes_planejamento) |> print()
# unique(sigplan$acoes_planejamento[, .(programa_cod)]) |> print()
# num_linhas <- nrow(sigplan$acoes_planejamento[programa_cod == CODIGO_REF & is_deleted_acao == FALSE]) |> print()
index <- which(BASE$programa_cod == CODIGO_REF & BASE$is_deleted_localizador == FALSE)[1] |> print()
BASE[index, area_tematica_cod := cod_para_alterar]
View(BASE)


#===============================================================================
#3

BASE <- sigplan$programas_planejamento

check_3 <- check_area_tematica_exists_programas(BASE,
                                                stop_on_failure = FALSE,
                                                output = TRUE,
                                                json_outfile = "logfile.json", 
                                                log_level = "ERROR",
                                                msg_template = NULL
)

COL_NAME <- "programa_cod"
CODIGO_REF <- 3
COL_ALTERAR <- "area_tematica_cod"
cod_para_alterar <- "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"

# colnames(sigplan$acoes_planejamento) |> print()
# unique(sigplan$acoes_planejamento[, .(programa_cod)]) |> print()
# num_linhas <- nrow(sigplan$acoes_planejamento[programa_cod == CODIGO_REF & is_deleted_acao == FALSE]) |> print()
index <- which(BASE[[COL_NAME]] == CODIGO_REF & BASE$is_deleted_programa == FALSE)[1] |> print()
set(BASE, i = index, j = COL_ALTERAR, value = cod_para_alterar)
View(BASE)


#===============================================================================
#4
#sigplan <- read_datapackage("../cc-checks-planejamento-oficina/datapackages/sigplan/datapackage.json")

BASE_1 <- sigplan$acoes_planejamento
#colnames(BASE_1)

BASE_2 <- sigplan$localizadores_todos_planejamento
#colnames(localizadores_todos_planejamento)

acoes_planejamento <- sigplan$acoes_planejamento
localizadores_todos_planejamento <- sigplan$localizadores_todos_planejamento
output = TRUE
stop_on_failure = FALSE
json_outfile = "logfile.json"
log_level = "ERROR"
msg_template = NULL


check_4 <- check_count_acoes(BASE_1,
                             BASE_2,
                             output = TRUE,
                             stop_on_failure = FALSE,
                             json_outfile = "logfile.json",
                             log_level = "ERROR",
                             msg_template = NULL)
#alt1
COL_NAME <- "acao_cod"
CODIGO_REF <- 4304
COL_ALTERAR <- "acao_desc"
cod_para_alterar <- "TESTE LOCALIZADORES"
# colnames(BASE_1) |> print()
# unique(BASE_1[, ..COL_NAME]) |> print()
# num_linhas <- nrow(sigplan$acoes_planejamento[programa_cod == CODIGO_REF & is_deleted_acao == FALSE]) |> print()
index <- which(BASE_1[[COL_NAME]] == CODIGO_REF & BASE_1$is_deleted_programa == FALSE)[1] |> print()
BASE_1 <- set(BASE_1, i = index, j = COL_ALTERAR, value = cod_para_alterar)

#alt2
COL_NAME <- "acao_cod"
CODIGO_REF <- 4305
COL_ALTERAR <- "acao_desc"
cod_para_alterar <- "TESTE ACOES"
index <- which(BASE_2[[COL_NAME]] == CODIGO_REF & BASE_2$is_deleted_programa == FALSE)[1] |> print()
BASE_2 <- set(BASE_2, i = index, j = COL_ALTERAR, value = cod_para_alterar)

#alt3 
COL_NAME <- "acao_cod"
CODIGO_REF <- 2113
COL_ALTERAR <- "acao_desc"
cod_para_alterar <- NA
# colnames(BASE_1) |> print()
# unique(BASE_1[, ..COL_NAME]) |> print()
# num_linhas <- nrow(sigplan$acoes_planejamento[programa_cod == CODIGO_REF & is_deleted_acao == FALSE]) |> print()
index <- which(BASE_2[[COL_NAME]] == CODIGO_REF & BASE_2$is_deleted_programa == FALSE)[1] |> print()
BASE_2 <- set(BASE_2, i = index, j = COL_ALTERAR, value = cod_para_alterar)

#alt4
COL_NAME <- "acao_cod"
CODIGO_REF <- 2114
COL_ALTERAR <- "acao_desc"
cod_para_alterar <- NA
index <- which(BASE_1[[COL_NAME]] == CODIGO_REF & BASE_1$is_deleted_programa == FALSE)[1] |> print()
BASE_1 <- set(BASE_1, i = index, j = COL_ALTERAR, value = cod_para_alterar)

#alt5
new_row <- data.frame(
  uo_acao_cod = 9901,
  uo_acao_nome = "TESTE",
  acao_cod = 7777,
  acao_desc = "xxxxxxxxxxxxxxxxxxx"
)
outros_campos <- setdiff(colnames(BASE_1), colnames(new_row))
new_row[outros_campos] <- "-"
#a <- cat(colnames(new_row), sep = "\n")
#b <- cat(colnames(BASE_1), sep = "\n")
BASE_1 <- rbind(BASE_1, new_row)

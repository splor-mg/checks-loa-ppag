test_that("check_area_tematica_exists", {
  
  acoes_planejamento <- tribble(
    ~uo_acao_cod, ~acao_cod, ~is_deleted_acao, ~programa_cod, ~programa_desc,                     ~area_tematica_cod, ~area_tematica_desc,
    1521,         4304,      FALSE,            1,             "MINAS TRANSPARENTE",               "13AT13",           "Transparência",
    1521,         4305,      FALSE,            1,             "MINAS TRANSPARENTE",               "13AT13",           "Transparência",
    1521,         4306,      FALSE,            1,             "MINAS TRANSPARENTE",               "",                 "",
    2121,         4010,      FALSE,            8,             "SERVIÇOS PREVIDENCIÁRIOS",         "07AT07",           "Segurança Pública",
    2121,         4011,      FALSE,            8,             "SERVIÇOS PREVIDENCIÁRIOS",         "07AT08",           "Segurança Pública",
    2121,         4012,      TRUE,             9,             "APOIO HABITACIONAL AOS MILITARES", "",                 "",
    4541,         4349,      FALSE,            9,             "APOIO HABITACIONAL AOS MILITARES", "07AT07",           "Segurança Pública",
    1301,         4290,      FALSE,            117,           "MOBILIDADE E TRANSPORTES",         "05AT05",           "Infraestrutura e Mobilidade",
    1301,         4291,      FALSE,            117,           "MOBILIDADE E TRANSPORTES",         "05AT05",           "Infraestrutura e Mobilidade",
    1301,         4292,      FALSE,            117,           "MOBILIDADE E TRANSPORTES",         "05AT05",           "Infraestrutura"
  ) 
  
  result <- check_area_tematica_exists_acoes(acoes_planejamento, output = TRUE)
  
  expect_equal(result$info$programa_cod, c(1, 8, 117))
})

#' Verificar quantidade de programas e ações entre as bases (incluindo excluídos e incluídos)
#' @name check_count
NULL

#' @export
check_count_programas <- function(programas_planejamento, acoes_planejamento, localizadores_todos_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
  x <- programas_planejamento |> 
    dplyr::filter(is_deleted_programa == FALSE) |> 
    dplyr::distinct(programa_cod, programa_desc) |> 
    dplyr::rename(programas = programa_desc)
  
  y <- acoes_planejamento |> 
    dplyr::filter(is_deleted_programa == FALSE) |> 
    dplyr::distinct(programa_cod, programa_desc) |> 
    dplyr::rename(acoes = programa_desc)
  
  z <- localizadores_todos_planejamento |> 
    dplyr::filter(is_deleted_programa == FALSE) |> 
    dplyr::distinct(programa_cod, programa_desc) |> 
    dplyr::rename(localizadores = programa_desc)
  
  df <- merge(x, y, by = "programa_cod", all = TRUE) |> 
          merge(z, by = "programa_cod", all = TRUE)
  
  report <- validate::check_that(df, programas == acoes, acoes == localizadores)
  
  check_result(
    df, report, stop_on_failure = stop_on_failure, output = output, summary = dplyr::count(df)
  )
}

#' @export
check_count_programas_is_deleted <- function(programas_planejamento, acoes_planejamento, localizadores_todos_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
  x <- programas_planejamento |> 
    dplyr::filter(is_deleted_programa == TRUE) |> 
    dplyr::distinct(programa_cod, programa_desc) |> 
    dplyr::rename(programas = programa_desc)
  
  y <- acoes_planejamento |> 
    dplyr::filter(is_deleted_programa == TRUE) |> 
    dplyr::distinct(programa_cod, programa_desc) |> 
    dplyr::rename(acoes = programa_desc)
  
  z <- localizadores_todos_planejamento |> 
    dplyr::filter(is_deleted_programa == TRUE) |> 
    dplyr::distinct(programa_cod, programa_desc) |> 
    dplyr::rename(localizadores = programa_desc)
  
  df <- merge(x, y, by = "programa_cod", all = TRUE) |> 
    merge(z, by = "programa_cod", all = TRUE)
  
  report <- validate::check_that(df, programas == acoes, acoes == localizadores)
  
  check_result(
    df, report, stop_on_failure = stop_on_failure, output = output, summary = dplyr::count(df)
  )
}

#' @export
check_count_programas_is_new <- function(programas_planejamento, acoes_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
  x <- programas_planejamento |> 
    dplyr::filter(is_deleted_programa == FALSE & is_new_programa == TRUE) |> 
    dplyr::distinct(programa_cod, programa_desc) |> 
    dplyr::rename(programas = programa_desc)
  
  y <- acoes_planejamento |> 
    dplyr::filter(is_deleted_programa == FALSE & is_new_programa == TRUE) |> 
    dplyr::distinct(programa_cod, programa_desc) |> 
    dplyr::rename(acoes = programa_desc)
  
  df <- merge(x, y, by = "programa_cod", all = TRUE)
  
  report <- validate::check_that(df, programas == acoes)
  
  check_result(
    df, report, stop_on_failure = stop_on_failure, output = output, summary = dplyr::count(df)
  )
}

#' @export
check_count_acoes <- function(acoes_planejamento, localizadores_todos_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
  x <- acoes_planejamento |> 
    dplyr::filter(is_deleted_acao == FALSE) |> 
    dplyr::distinct(uo_acao_cod, uo_acao_nome, acao_cod, acao_desc) |> 
    dplyr::rename(acoes = acao_desc)
  
  y <- localizadores_todos_planejamento |> 
    dplyr::filter(is_deleted_acao == FALSE) |> 
    dplyr::distinct(uo_acao_cod, uo_acao_nome, acao_cod, acao_desc) |> 
    dplyr::rename(localizadores = acao_desc)
  
  df <- merge(x, y, by = c("uo_acao_cod", "acao_cod"), all = TRUE)
  
  report <- validate::check_that(df, acoes == localizadores)
  
  check_result(
    df, report, stop_on_failure = stop_on_failure, output = output, summary = dplyr::count(df)
  )
}


check_count_acoes_is_deleted <- function(acoes_planejamento, localizadores_todos_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
  x <- acoes_planejamento |> 
    dplyr::filter(is_deleted_acao == TRUE) |> 
    dplyr::distinct(uo_acao_cod, uo_acao_nome, acao_cod, acao_desc) |> 
    dplyr::rename(acoes = acao_desc)
  
  y <- localizadores_todos_planejamento |> 
    dplyr::filter(is_deleted_acao == TRUE) |> 
    dplyr::distinct(uo_acao_cod, uo_acao_nome, acao_cod, acao_desc) |> 
    dplyr::rename(localizadores = acao_desc)
  
  df <- merge(x, y, by = c("uo_acao_cod", "acao_cod"), all = TRUE)
  
  report <- validate::check_that(df, acoes == localizadores)
  
  check_result(
    df, report, stop_on_failure = stop_on_failure, output = output, summary = dplyr::count(df)
  )
}

check_count_acoes_is_new <- function(acoes_planejamento, localizadores_todos_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
  x <- acoes_planejamento |> 
    dplyr::filter(is_deleted_acao == FALSE & is_new_acao == TRUE) |> 
    dplyr::distinct(acao_cod, acao_desc)
  
  df <- merge(x, y, by = c("uo_acao_cod", "acao_cod"), all = TRUE)
  
  report <- validate::check_that(df, acoes == localizadores)
  
  check_result(
    df, report, stop_on_failure = stop_on_failure, output = output, summary = dplyr::count(df)
  )
}

#' @export
check_count_programas <- function(programas_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
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
check_count_programas_is_deleted <- function(programas_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
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
check_count_programas_is_new <- function(programas_planejamento, output = FALSE, stop_on_failure = FALSE) {
  
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
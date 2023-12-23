#' Verificações na base indicadores
#'
#' @description
#' São realizadas as seguintes verificações:
#'
#' - Indicadores com índice de referência “em apuração” possuem justificativa (\code{\link{check_indicadores_justificativa_indice_referencia_em_apuracao}});
#' - Indicadores não possuem índice de referência com data futura (\code{\link{check_indicadores_indice_referencia_data_futura}});
#' - Indicadores não possuem índice de referência zerados (\code{\link{check_indicadores_indice_referencia_zerado}});
#' - Indicadores com previsões “em apuração” possuem justificativa (\code{\link{check_indicadores_justificativa_previsao_em_apuracao}});
#' - Indicadores não possuem previsões zeradas (\code{\link{check_indicadores_previsoes_zeradas}});

#' @name check_indicadores
NULL

#' @export
check_indicadores_justificativa_indice_referencia_em_apuracao <- function(indicadores_planejamento, output = FALSE, stop_on_failure = FALSE) {
  df <- indicadores_planejamento

  report <- validate::check_that(
    df,
    if (is_deleted_programa == FALSE &
      is_deleted_indicador == FALSE & 
      is_em_apuracao_indice_de_referencia == TRUE) {
      !is.na(justificativa_status_apuracao_indice_ref)
    }
  )

  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @export
check_indicadores_indice_referencia_data_futura <- function(indicadores_planejamento, output = FALSE, stop_on_failure = FALSE) {
  df <- indicadores_planejamento |>
    dplyr::filter(is_deleted_programa == FALSE &
      is_deleted_indicador == FALSE &
      is_em_apuracao_indice_de_referencia == FALSE)

  # operador menor ou igual pois o indice de referencia pode ter sido
  # calculado no dia da atualizacao (eg. Indicador "SUCESSO DO PLANEJAMENTO ORÇAMENTÁRIO")
  report <- validate::check_that(df, dt_apuracao <= as.Date(updated_at))

  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}


#' @export
check_indicadores_indice_referencia_zerado <- function(indicadores_planejamento, output = FALSE, stop_on_failure = FALSE) {
  df <- indicadores_planejamento |>
    dplyr::filter(is_deleted_programa == FALSE &
      is_deleted_indicador == FALSE &
      is_em_apuracao_indice_de_referencia == FALSE)

  report <- validate::check_that(df, indice_de_referencia != 0)

  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @export
check_indicadores_justificativa_previsao_em_apuracao <- function(indicadores_planejamento, output = FALSE, stop_on_failure = FALSE) {
  df <- indicadores_planejamento |>
    dplyr::filter(is_deleted_programa == FALSE &
                    is_deleted_indicador == FALSE)
  
  report <- validate::check_that(
    df,
    if (is.na(previsao_para_ano0) |
        is.na(previsao_para_ano1) |
        is.na(previsao_para_ano2) |
        is.na(previsao_para_ano3)) {
      !is.na(justificativa_status_apuracao_previsoes)
    }
  )
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @export
check_indicadores_previsoes_zeradas <- function(indicadores_planejamento, output = FALSE, stop_on_failure = FALSE) {
  df <- indicadores_planejamento |>
    dplyr::filter(is_deleted_programa == FALSE &
                    is_deleted_indicador == FALSE)
  
  report <- validate::check_that(df, 
                                 if(is_em_apuracao_ano0 == FALSE) previsao_para_ano0 != 0,
                                 if(is_em_apuracao_ano1 == FALSE) previsao_para_ano1 != 0,
                                 if(is_em_apuracao_ano2 == FALSE) previsao_para_ano2 != 0,
                                 if(is_em_apuracao_ano3 == FALSE) previsao_para_ano3 != 0)
  
  check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}
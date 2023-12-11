#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de 
#' investimento das empresas controladas) coincide com a projeção do PPAG 
#' para o ano seguinte
#' @name check_total_orcamento
NULL

#' @export
check_equivalencia_qdd_fiscal <- function(base_qdd_fiscal, acoes_planejamento, stop_on_failure = FALSE, output = FALSE) {
  
  key <- c("uo_cod", "programa_cod", "acao_cod", "funcao_cod", "subfuncao_cod", "iag_cod")
  
  x <- base_qdd_fiscal |> 
    summarize(vlr_loa_desp = sum(vlr_loa_desp), .by = all_of(key))
  
  y <- acoes_planejamento |> 
    rename(uo_cod = uo_acao_cod) |> 
    filter(is_deleted_acao == FALSE & identificador_tipo_acao_cod %in% c(1, 2, 4, 7, 9)) |> 
    summarize(vr_meta_orcamentaria_ano0 = sum(vr_meta_orcamentaria_ano0), .by = all_of(key))
  
  df <- full_join(x, y, by = key) |> format_accounting()
  
  report <- validate::check_that(df, vlr_loa_desp == vr_meta_orcamentaria_ano0)
  
  format_check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

#' @export
check_equivalencia_qdd_plurianual <- function(base_qdd_plurianual, acoes_planejamento, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "programa_cod", "acao_cod", "funcao_cod", "subfuncao_cod", "iag_cod")
  
  x <- base_qdd_plurianual |> 
       summarize(across(starts_with("vlr_loa_desp"), sum), .by = all_of(key))

    y <- acoes_planejamento |> 
         rename(uo_cod = uo_acao_cod) |> 
         filter(is_deleted_acao == FALSE & identificador_tipo_acao_cod %in% c(1, 2, 4, 7, 9)) |> 
         summarize(across(starts_with("vr_meta_orcamentaria"), sum), .by = all_of(key))
           
    df <- full_join(x, y, by = key) |> 
          format_accounting()
    
    report <- validate::check_that(df, 
                                   vlr_loa_desp_ano0 == vr_meta_orcamentaria_ano0,
                                   vlr_loa_desp_ano1 == vr_meta_orcamentaria_ano1,
                                   vlr_loa_desp_ano2 == vr_meta_orcamentaria_ano2,
                                   vlr_loa_desp_ano3 == vr_meta_orcamentaria_ano3)
    
    format_check_result(df, report, stop_on_failure = stop_on_failure, output = output)
    
    }

#' @export
check_equivalencia_qdd_investimento <- function(base_qdd_investimento, acoes_planejamento, stop_on_failure = FALSE, output = FALSE) {
  
  key <- c("uo_cod", "programa_cod", "acao_cod", "funcao_cod", "subfuncao_cod", "iag_cod")
  
  x <- base_qdd_investimento |> 
       summarize(vlr_loa_desp_invest = sum(vlr_loa_desp_invest), .by = all_of(key))
    
  y <- acoes_planejamento |> 
       rename(uo_cod = uo_acao_cod) |> 
       filter(is_deleted_acao == FALSE & identificador_tipo_acao_cod %in% c(3, 5, 6, 8)) |> 
       summarize(vr_meta_orcamentaria_ano0 = sum(vr_meta_orcamentaria_ano0), .by = all_of(key))        
  
  df <- full_join(x, y, by = key) |> format_accounting()
  
  report <- validate::check_that(df, vlr_loa_desp_invest == vr_meta_orcamentaria_ano0)
  
  format_check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}


#' @export
check_equivalencia_qdd_plurianual_invest <- function(base_qdd_plurianual_invest, acoes_planejamento, stop_on_failure = FALSE, output = FALSE) {
  
  key <- c("uo_cod", "programa_cod", "acao_cod", "funcao_cod", "subfuncao_cod", "iag_cod")
  
  x <- base_qdd_plurianual_invest |> 
    summarize(across(starts_with("vlr_loa_desp_invest"), sum), .by = all_of(key))
  
  y <- acoes_planejamento |> 
    rename(uo_cod = uo_acao_cod) |> 
    filter(is_deleted_acao == FALSE & identificador_tipo_acao_cod %in% c(3, 5, 6, 8)) |> 
    summarize(across(starts_with("vr_meta_orcamentaria"), sum), .by = all_of(key))
  
  df <- full_join(x, y, by = key) |> 
    format_accounting()
  
  report <- validate::check_that(df, 
                                 vlr_loa_desp_invest_ano0 == vr_meta_orcamentaria_ano0,
                                 vlr_loa_desp_invest_ano1 == vr_meta_orcamentaria_ano1,
                                 vlr_loa_desp_invest_ano2 == vr_meta_orcamentaria_ano2,
                                 vlr_loa_desp_invest_ano3 == vr_meta_orcamentaria_ano3)
  
  format_check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}

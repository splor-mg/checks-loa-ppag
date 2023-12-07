#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de 
#' investimento das empresas controladas) coincide com a projeção do PPAG 
#' para o ano seguinte
#' @name check_total_orcamento
NULL

#' @export
check_equivalencia <- function(base_qdd_plurianual, acoes_planejamento, stop_on_failure = FALSE, output = FALSE) {
  key <- c("uo_cod", "programa_cod", "acao_cod", "funcao_cod", "subfuncao_cod", "iag_cod")
  
  x <- base_qdd_plurianual |> 
       summarize(across(starts_with("vlr_loa_desp"), sum), .by = all_of(key))

    y <- acoes_planejamento |> 
         rename(uo_cod = uo_acao_cod) |> 
         filter(is_deleted_acao == FALSE & identificador_tipo_acao_cod %in% c(1, 2, 4, 7, 9)) |> 
         summarize(across(starts_with("vr_meta_orcamentaria"), sum), .by = all_of(key))
           
    df <- full_join(x, y, by = key) |> 
          select(all_of(key) | ends_with("0") | ends_with("1") | ends_with("2") | ends_with("3")) |>
          format_accounting()
    
    report <- validate::check_that(df, 
                                   vlr_loa_desp_ano0 == vr_meta_orcamentaria_ano0,
                                   vlr_loa_desp_ano1 == vr_meta_orcamentaria_ano1,
                                   vlr_loa_desp_ano2 == vr_meta_orcamentaria_ano2,
                                   vlr_loa_desp_ano3 == vr_meta_orcamentaria_ano3)
    
    validate::summary(report)
    format_check_result(df, report, stop_on_failure = stop_on_failure, output = output)
    
    }

#' @export
check_total_orcamento_fiscal_qdd <- function(base_qdd_fiscal, acoes_planejamento) {

  x <- base_qdd_fiscal |> 
       summarize(BASE_QDD_FISCAL = sum(VL_LOA_DESP))
  
  y <- acoes_planejamento |> 
       filter(is_deleted_acao == FALSE & 
              identificador_tipo_acao_cod %in% c(1, 2, 4, 7, 9)) |> 
       summarize(ACOES_PLANEJAMENTO = sum(vr_meta_orcamentaria_ano0))
  
  df <- bind_cols(x, y) |> 
        mutate(across(everything(), pp))

  report <- validate::check_that(df, BASE_QDD_FISCAL == ACOES_PLANEJAMENTO)
  
  format_check_result(df, report)
}

#' @export
check_total_orcamento_investimento <- function(base_qdd_investimento, acoes_planejamento) {
  
  x <- base_qdd_investimento |> 
    group_by(COD_UO) |> 
    summarize(VL_LOA = sum(VALOR)) |> 
    rename(UO_COD = COD_UO) |> 
    arrange(UO_COD)
  
  y <- acoes_planejamento |> 
    filter(is_deleted_acao == FALSE & 
             identificador_tipo_acao_cod %in% c(3, 5, 6, 8)) |> 
    group_by(uo_acao_cod) |> 
    summarize(VL_LOA = sum(vr_meta_orcamentaria_ano0)) |> 
    rename(UO_COD = uo_acao_cod) |> 
    arrange(UO_COD)
  
  df <- full_join(x, y, by = "UO_COD") |> 
    mutate(across(starts_with("VL_LOA"), replace_na)) |> 
    mutate(across(starts_with("VL_LOA"), pp))
  
  rules <- validate::validator(VL_LOA.x == VL_LOA.y)
  report <- validate::confront(df, rules)
  
  info <- validate::summary(report)
  pass <- isTRUE(all.equal(info$items, info$passes))
  if (!pass) {
    info <- validate::violating(df, report)
    msg <- glue::glue_data(info, 
                           "O total do sisor (R$ {pp(VL_LOA.x)}) é diferente do sigplan (R$ {pp(VL_LOA.y)}) em R$ {pp(VL_LOA.x - VL_LOA.y)} para unidade {UO_COD}")
    log_warn(skip_formatter(msg))
  }
  list("pass" = pass, "info" = info)
}


#' @export
check_total_orcamento_fiscal_plurianual <- function(base_qdd_plurianual, acoes_planejamento) {
  
  x <- base_qdd_plurianual |> 
    group_by(UO_COD) |> 
    summarize(across(starts_with("VL_LOA_DESP"), sum)) |> 
    arrange(UO_COD)
  
  y <- acoes_planejamento |> 
    filter(is_deleted_acao == FALSE & 
             identificador_tipo_acao_cod %in% c(1, 2, 4, 7, 9)) |> 
    group_by(uo_acao_cod) |> 
    summarize(across(starts_with("vr_meta_orcamentaria"), sum)) |> 
    rename(UO_COD = uo_acao_cod) |> 
    arrange(UO_COD)
  
  df <- full_join(x, y, by = "UO_COD") |> 
        select("UO_COD" | ends_with("0") | ends_with("1") | ends_with("2") | ends_with("3")) |> 
        mutate(across(starts_with("VL_LOA_DESP"), replace_na)) |> 
        mutate(across(starts_with("VL_LOA_DESP"), pp)) |> 
        mutate(across(starts_with("vr_meta_orcamentaria"), replace_na)) |> 
        mutate(across(starts_with("vr_meta_orcamentaria"), pp))
  
  report <- validate::check_that(df, 
                                 VL_LOA_DESP_ANO0 == vr_meta_orcamentaria_ano0,
                                 VL_LOA_DESP_ANO1 == vr_meta_orcamentaria_ano1,
                                 VL_LOA_DESP_ANO2 == vr_meta_orcamentaria_ano2,
                                 VL_LOA_DESP_ANO3 == vr_meta_orcamentaria_ano3)
  
  format_check_result(df, report)
}
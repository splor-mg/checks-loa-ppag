#' Receitas e Despesas Intra-Orcamentárias
#'
#' Verifica se as operações intra-orçamentárias registradas na
#' base intraorçamentária de repasse estão adequadamente detalhadas no orçamento.
#'
#'
#' @export
check_intra_receita <- function(base_orcam_receita_fiscal,
                                base_intra_orcamentaria_repasse,
                                stop_on_failure = FALSE,
                                output = FALSE,
                                json_outfile = NULL,
                                log_level = "ERROR",
                                msg_template = NULL
                                ) {
  key <- c("uo_cod")

  x <- base_orcam_receita_fiscal |>
       aggregate("vlr_loa_rec$",
                 by = key,
                 filter = categoria %in% c(7, 8)
                 )

  y <- base_intra_orcamentaria_repasse |>
       aggregate("vlr_repassado",
                 by = key,
                 rename = list(uo_beneficiada_cod = "uo_cod")
                 )

  df <- merge(x, y,
              by = key,
              all = TRUE
              ) |>
        as_accounting()
  
  report <- check_that(df, vlr_loa_rec == vlr_repassado)
  
  default_message = paste0(
                    "A uo {uo_cod} foi indicada como beneficiária de um valor total de R$ ",
                    "{vlr_repassado} na base intra-repasse, porém a referida uo estimou valor ",
                    "diferente de receita intraorcamentária, R$ {vlr_loa_rec}, diferença de R$ ",
                    "{vlr_repassado - vlr_loa_rec}."
                    )
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(df, report,
               stop_on_failure = stop_on_failure,
               output = output,
               summary = aggregate(df, "vlr"),
               json_outfile = json_outfile,
               log_level = log_level,
               msg_template = msg_template
               )
}

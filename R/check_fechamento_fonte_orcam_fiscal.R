#' Fechamento receita e despesa por fonte do orçamento fiscal
#'
#'
#'
#'
#'
#' @export
check_fechamento_fonte_orcam_fiscal <- function(base_orcam_receita_fiscal,
                                                base_qdd_fiscal,
                                                stop_on_failure = FALSE,
                                                output = FALSE,
                                                json_outfile = NULL,
                                                log_level = "ERROR",
                                                msg_template = NULL
                                                ) {
  key <- c("fonte_cod")

  x <- base_orcam_receita_fiscal |>
       aggregate("vlr_loa_rec$",
                 by = key
                 )

  y <- base_qdd_fiscal |>
       aggregate("vlr_loa_desp$",
                 by = key
                 )

  df <- merge(x, y,
              by = key,
              all = TRUE
              ) |> 
        as_accounting()
  
  report <- check_that(df, vlr_loa_rec == vlr_loa_desp)
  
  default_message = "A Fonte de Recurso {fonte_cod} está com os valores totais de receita e despesa diferentes nas bases. R$ {vlr_loa_rec} na base orçamento fiscal da receita, e {vlr_loa_desp} na qdd fiscal. A diferença é de R$ {ifelse(is.na(vlr_loa_rec) | vlr_loa_rec == 0, vlr_loa_desp, ifelse(is.na(vlr_loa_desp) | vlr_loa_desp == 0,  vlr_loa_rec, vlr_loa_rec - vlr_loa_desp))}."
                    
  
  # prioritize the parameter error message if used
  msg_template = msg_template %||% default_message
  
  check_result(df, report,
               stop_on_failure = stop_on_failure,
               output = output,
               json_outfile = json_outfile,
               log_level = log_level,
               msg_template = msg_template
               )
}

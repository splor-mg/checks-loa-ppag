#' Fechamento receita e despesa por fonte do orçamento de investimento
#'
#'
#'
#'
#'
#' @export
check_fechamento_fonte_orcam_investimento <- function(base_orcam_receita_investimento,
                                                      base_qdd_investimento,
                                                      stop_on_failure = FALSE,
                                                      output = FALSE,
                                                      json_outfile = NULL,
                                                      log_level = "ERROR",
                                                      msg_template = NULL
                                                      ) {
  key <- c("fonte_cod")

  x <- base_orcam_receita_investimento |>
       mutate(fonte_cod = as.numeric(paste0(categoria, 
                                            subcategoria,
                                            alinea,
                                            str_pad(subalinea, 2, pad = "0")
                                            )
                                     )
              ) |>
       aggregate("vlr_loa_rec_invest$",
                 by = key
                 )

  y <- base_qdd_investimento |>
       aggregate("vlr_loa_desp_invest",
                 by = key
                 )

  df <- merge(x, y,
              by = key,
              all = TRUE
              ) |>
        as_accounting()

  report <- df |> check_that(vlr_loa_rec_invest  == vlr_loa_desp_invest)
  
  default_message = "Foram encontrados erros no teste."
  
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

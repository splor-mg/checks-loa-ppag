#' Verificar se os setores de governo estão corretos
#'
#' A referência correta dos setores de governo é disponibilizada
#' pela DCPPN via tabela auxiliar no recurso
#' [`volumes-loa-dados.desc_setor_governo`](https://github.com/splor-mg/volumes-loa-dados)
#'
#' @export
check_setor_governo <- function(acoes_planejamento,
                                aux_setor_governo,
                                output = FALSE,
                                stop_on_failure = FALSE,
                                json_outfile = NULL,
                                log_level = "ERROR",
                                msg_template = NULL
                                ) {
  
  x <- acoes_planejamento |>
       distinct(uo_acao_cod,
                setor_governo
                )

  y <- aux_setor_governo |>
       select(uo_acao_cod = uo_cod,
              setor_governo_referencia = setor_governo_desc
              )

  df <- merge(x, y,
              by = "uo_acao_cod",
              all = TRUE
              )

  report <- check_that(df, setor_governo == setor_governo_referencia)
  
  default_message = paste0(
                    "A uo {uo_acao_cod} está com setor divergente na base ações-planejamento ",
                    "({setor_governo}) em relação à tabela auxiliar setor-governo da loa ",
                    "({setor_governo_referencia})."
                    )
  
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

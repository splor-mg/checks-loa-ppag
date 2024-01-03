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
                                stop_on_failure = FALSE) {
  x <- acoes_planejamento |>
    distinct(uo_acao_cod, setor_governo)

  y <- aux_setor_governo |>
    select(uo_acao_cod = uo_cod, setor_governo_referencia = setor_governo_desc)

  df <- merge(x, y, by = "uo_acao_cod", all = TRUE)

  report <- check_that(df, setor_governo == setor_governo_referencia)

  check_result(
    df, report,
    stop_on_failure = stop_on_failure, output = output
  )
}

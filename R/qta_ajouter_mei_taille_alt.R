#' Ajouter à un df une colonne de longueur par dégroupage des lots
#'
#' Il s'agit d'une méthode alternative à celle utilisée par la base Aspe. Elle affecte à
#'     chaque individu d'un lot "S/L" qui n'a pas été mesuré une longueur tirée aléatoirement
#'     parmi celles des individus du sous-lot qui a été mesuré.
#'
#' @param df Dataframe de données avec des variables "lop_id", "mei_mesure_reelle" et "mei_taille".
#'
#' @return Le dataframe complété.
#' @export
#'
#' @importFrom dplyr pull filter mutate
#' @importFrom purrr map reduce
#'
#' @examples
#' \dontrun{
#' data <- data %>% qta_ajouter_mei_taille_alt()
#' }
qta_ajouter_mei_taille_alt <- function(df)

{
  mes_lots <- df %>%
    pull(lop_id) %>%
    unique()

  # fonction qui ajoute au df d'un lot la colonne mei_taille_alt qui, quand mei_mesure_reelle == "f",
  # tire aléatoirement une valeur parmi les mei_mesure_reelle == "t" du même lot.
  ajouter_mei_taille_alt_1_lot <- function(df, id_lot) {
    data_1lot <- df %>%
      filter(lop_id == id_lot)

    data_1lot_t <- data_1lot %>%
      filter(mei_mesure_reelle == "t") %>%
      mutate(mei_taille_alt = mei_taille)

    data_1lot_f <- data_1lot %>%
      filter(mei_mesure_reelle == "f")

    data_1lot_f$mei_taille_alt <- sample(
      x = data_1lot_t$mei_taille,
      size = nrow(data_1lot_f),
      replace = TRUE
    )

    data_1lot_ta <- rbind(data_1lot_t, data_1lot_f)

    data_1lot_ta

  }
  # application sur l'ensemble des lots du df
  map(.x = mes_lots,
      .f = ajouter_mei_taille_alt_1_lot,
      df = df) %>%
    reduce(rbind)
}

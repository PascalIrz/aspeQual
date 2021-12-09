#' Ajouter à un df une colonne de longueur par dégroupage des lots
#'
#' Il s'agit d'une méthode alternative à celle utilisée par la base Aspe. Elle affecte à
#'     chaque individu d'un lot "S/L" qui n'a pas été mesuré une longueur tirée aléatoirement
#'     parmi celles des individus du sous-lot qui a été mesuré.
#'
#' @param df Dataframe de données avec des variables "lop_id", "mei_mesure_reelle", "mei_id",
#'     "tyl_libelle" et "mei_taille".
#'
#' @return Le dataframe complété.
#' @export
#'
#' @importFrom dplyr group_by mutate ungroup filter tally left_join
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#' data <- data %>% qta_ajouter_mei_taille_alt()
#' }
qta_ajouter_mei_taille_alt <- function(df)

{
  # Fonction qui complète, dans un vecteur, les NA par tirage aléatoire parmi les non-NA
  replace_func <- function(vecteur)

  {

    indices_manquants <- is.na(vecteur)

    vecteur[indices_manquants] <-
      sample(vecteur[!indices_manquants],
             size = sum(indices_manquants),
             replace = TRUE)
    vecteur

  }

  # comptage des mesures réelles ou non
  comptage <- df %>%
    filter(tyl_libelle == "S/L") %>%
    group_by(lop_id,
             mei_mesure_reelle) %>%
    tally() %>%
    ungroup() %>%
    pivot_wider(names_from = mei_mesure_reelle,
                values_from = n)

  # calcul des longueurs par tirage aléatoire (slt surles lots avec des mesurés et non mesurés)
  mei_taille_alt <- df %>%
    left_join(comptage) %>%
    filter(t > 0, f > 0) %>% # sélection des lots avec des mesurés et non mesurés
    select(-t,-f) %>%
    mutate(mei_taille_alt = ifelse( # report des mesures réelles
      test = mei_mesure_reelle == 't',
      yes = mei_taille,
      no = NA
    )) %>%
    group_by(lop_id) %>% # par lot
    mutate(mei_taille_alt = replace_func(mei_taille_alt)) %>% # ajout mei_taille_alt par ta
    ungroup() %>%
    select(mei_id,
           mei_taille_alt)

  # ajout des mei_taille alt au df de départ
  df %>%
    left_join(mei_taille_alt)

}

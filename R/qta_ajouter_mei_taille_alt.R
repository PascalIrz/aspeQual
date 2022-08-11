#' Ajouter à un df une colonne de longueur par dégroupage des lots
#'
#' Il s'agit d'une méthode alternative à celle utilisée par la base Aspe. Elle affecte à
#'     chaque individu d'un lot "S/L" qui n'a pas été mesuré une longueur tirée aléatoirement
#'     parmi celles des individus du sous-lot qui a été mesuré. Deux méthodes sont proposées.
#'
#' @param df Dataframe de données avec des variables "lop_id", "mei_mesure_reelle", "mei_id",
#'     "tyl_libelle" et "mei_taille".
#' @param methode Caractère qui peut prendre les valeurs "ta_integral"ou "ta_partiel".
#'     Méthode d'attribution des tailles pour les individus non mesurées
#'     d'un lot S/L. Si methode = "ta_integral", il y a tirage aléatoire intégral avec remise.
#'     Chaque individu non mesuré se voit attribuer une longueur tirée aléatoirement parmi celles
#'     des individus mesurés. Si methode = "ta_partiel", soit NM le nombre d'individus mesurés et
#'     NNM le nombre d'individus non mesurés. Le vecteur des tailles mesurées est reporté
#'     autant de fois que possible (floor(NNM/NM)), puis les derniers individus se voient attribuer
#'     une taille par tirage aléatoire sans remise.
#' @param graine Nombre entier. Fixe la graine du générateur de nombre aléatoire. Par défaut la graine
#'     est NULL donc chaque tirage aléatoire est unique. Pour pouvoir répéter le même tirage il faut
#'     fixer par exemple graine = 123.
#'
#' @return Le dataframe complété.
#' @export
#'
#' @importFrom dplyr group_by mutate ungroup filter tally left_join select rename relocate last_col starts_with
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#' data <- data %>%
#'   qta_ajouter_mei_taille_alt(methode = "ta_partiel",
#'                              graine = 452)
#' }
qta_ajouter_mei_taille_alt <- function(df,
                                       methode = "ta_integral",
                                       graine = NULL)

{
  # Fonction qui complète, dans un vecteur, les NA par tirage aléatoire parmi les non-NA
  ta_integral <- function(vecteur)

  {

    indices_manquants <- is.na(vecteur)

    vecteur[indices_manquants] <-
      sample(vecteur[!indices_manquants],
             size = sum(indices_manquants),
             replace = TRUE)

    vecteur

  }

  # Fonction qui complète, dans un vecteur, les NA par une répétition des non-NA
  ta_partiel <- function(vecteur)

  {

    indices_manquants <- is.na(vecteur)
    vect_mesures_reelles <- vecteur[!indices_manquants]

    vecteur[indices_manquants] <-
      rep(vect_mesures_reelles,
          length.out = length(indices_manquants))

    vecteur

  }

  # set seed
  if(!is.null(graine)) {set.seed(graine)}

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
    select(-t, -f) %>%
    mutate(mei_taille_alt = ifelse( # report des mesures réelles
      test = mei_mesure_reelle == 't',
      yes = mei_taille,
      no = NA
    )) %>%
    group_by(lop_id) # par lot

  if(methode == "ta_integral") {
    mei_taille_alt <- mei_taille_alt %>%
      mutate(mei_taille_alt = ta_integral(mei_taille_alt))
  }

  if(methode == "ta_partiel") {
    mei_taille_alt <- mei_taille_alt %>%
      mutate(mei_taille_alt = ta_partiel(mei_taille_alt))
  }

  mei_taille_alt <- mei_taille_alt %>% # ajout mei_taille_alt par ta
    ungroup() %>%
    select(mei_id,
           mei_taille_alt)

  # ajout des mei_taille alt au df de départ
  df <- df %>%
    left_join(mei_taille_alt)

  if(methode == "ta_integral") {

    df <- df %>%
      rename(mei_taille_alt_ta_integral = mei_taille_alt)

  }

  if(methode == "ta_partiel") {

    df <- df %>%
      rename(mei_taille_alt_ta_partiel = mei_taille_alt)

  }

  df %>%
    relocate(starts_with("mei_taille"),
             .after = last_col())

}


#' Tester l'équivalence des distributions entre individus mesurés et dégroupées. Peut servir à
#'     comparer deux méthodes de dégroupage.
#'
#' Les tests sont Kolmogorov-Smirnov (comparaison des distributions), Student (comparaison des moyennes)
#'     et F-test (comparaison des variances).
#'
#' @param df Dataframe de données avec une variable "lop_id" et le nom de la (des) variable(s)
#'     choisie(s) à représenter.
#' @param lots Vecteur entier. Numéros des lots (lop_id).
#' @param var_taille1 Caractère. Nom de la variable à représenter, entre guillemets.
#' @param var_taille2 Caractère. Nom de la variable à représenter, entre guillemets (si deux méthodes
#'     de dégroupage à comparer).
#' @param sig Booléen. Si sig = TRUE (défaut), le tableau contient les niveaux de significativité
#'     des tests. Si sig = FALSE, il contient les p-values.
#'
#' @return Un dataframe avec une ligne par lot, contenant les résultats des tests.
#' @export
#'
#' @importFrom dplyr filter pull
#' @importFrom stats ks.test t.test var.test
#'
#' @examples
#' \dontrun{
#' tests <- qta_tests_distrib_sl(df = df, lot = 3156360, variable = "mei_taille")
#' }
qta_tests_distrib_sl <-
  function(df,
           lots = NA,
           var_taille1,
           var_taille2 = NA,
           sig = TRUE)

  {

    #----------------------------------------------------------------------------

    qta_tests_1lot_1var <- function(df, lot, variable)

    {

      lot_data <- df %>%
        filter(lop_id == lot)

      t <- lot_data %>%
        filter(mei_mesure_reelle == "t") %>%
        pull(get(variable))

      f <- lot_data %>%
        filter(mei_mesure_reelle == "f") %>%
        pull(get(variable))

      ks <- ks.test(t, f) %>%
        .$p.value

      student <- t.test(t, f) %>%
        .$p.value

      variance <- var.test(t, f) %>%
        .$p.value

      eff_mesure <- lot_data %>%
        filter(mei_mesure_reelle == "t") %>%
        nrow()

      eff_non_mesure <- nrow(lot_data) - eff_mesure

      lot <- lot

      station = lot_data$pop_libelle[1]

      espece = lot_data$esp_nom_commun[1]

      data.frame(lot,
                 station,
                 espece,
                 eff_mesure,
                 eff_non_mesure,
                 ks,
                 student,
                 variance)

    }

    # -----------------------------------------------------------------------------

    # identifiants des lots. Si non précisé ils sont tous conservés
    if (is.na(lots))
    {
      lots <- df %>%
        pull(lop_id) %>%
        unique()
    }

    # tests sur les données de la variable var_taille1 (eg dégroupées par Aspe)
    resultat <- map(
      .x = lots,
      .f = qta_tests_1lot_1var,
      df = df,
      variable = var_taille1
    ) %>%
      reduce(rbind)

  if(!is.na(var_taille2))
  {
    # tests sur les données par le dégroupage alternatif
    resultat2 <- map(
      .x = lots,
      .f = qta_tests_1lot_1var,
      df = df,
      variable = var_taille2
    ) %>%
      reduce(rbind)

    # assemblage
    resultat <- resultat %>%
      cbind(resultat2 %>%
              select(
                var2_ks = ks,
                var2_student = student,
                var2_variance = variance
              ))
  }

    if (sig)
    {
      # mise en forme avec des étoiles
      resultat <- resultat %>%
        mutate_at(vars(ks:last_col()),
                  function(x) {
                    x <- case_when(x < 1e-3 ~ "(***)",
                                   x < 1e-2 ~ "(**)",
                                   x < 0.05 ~ "(*)",
                                   TRUE ~ NA_character_)
                  })
    }

    resultat

  }

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
    # identifiants des lots. Si non précisé ils sont tous conservés
    if (is.na(lots))
    {
      lots <- df %>%
        pull(lop_id) %>%
        unique()
    }

    # tests sur les données de la variable var_taille1 (eg dégroupées par Aspe)
    degroupage_var1 <- map(
      .x = lots,
      .f = qta_tests_1lot_1var,
      df = df,
      variable = var_taille1
    ) %>%
      reduce(rbind)

    # tests sur les données par le dégroupage alternatif
    degroupage_var2 <- map(
      .x = lots,
      .f = qta_tests_1lot_1var,
      df = df,
      variable = var_taille2
    ) %>%
      reduce(rbind)

    # assemblage
    resultat <- degroupage_var1 %>%
      cbind(degroupage_var2 %>%
              select(
                var2_ks = ks,
                var2_student = student,
                var2_variance = variance
              ))

    if (sig)
    {
      # mise en forme avec des étoiles
      resultat <- resultat %>%
        mutate_at(vars(ks:var2_variance),
                  function(x) {
                    x <- case_when(x < 1e-3 ~ "(***)",
                                   x < 1e-2 ~ "(**)",
                                   x < 0.05 ~ "(*)",
                                   TRUE ~ NA_character_)
                  })
    }

    resultat

  }

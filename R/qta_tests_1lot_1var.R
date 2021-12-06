#' Tester sur un lot et une variable si les distributions diffèrent
#'
#' Les tests sont Kolmogorov-Smirnov (comparaison des distributions), Student (comparaison des moyennes)
#'     et F-test (comparaison des variances).
#'
#' @param df Dataframe de données avec une variable "lop_id" et le nom de la variable choisie
#'     à représenter.
#' @param lot Entier. Numéro du lot.
#' @param variable Caractère. Nom de la variable à représenter, entre guillemets.
#'
#' @return Un dataframe avec une ligne contenant les résultats des tests.
#' @export
#'
#' @importFrom dplyr filter pull
#' @importFrom stats ks.test t.test var.test
#'
#' @examples
#' \dontrun{
#' tests <- qta_tests_1lot_1var(df = df, lot = 3156360, variable = "mei_taille")
#' }
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

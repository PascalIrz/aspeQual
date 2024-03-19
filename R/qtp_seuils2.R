#' Calculer les seuils
#'
#' @param df Dataframe contenant les données.
#' @param var_taxon Variable contenant les identifiants des taxons (codes espèces, noms scientifiques ...).
#' @param var_a_tester Variable dont la distribution est examinée, par exemple mei_taille ou mei_poids.
#' @param seuil_densite Numérique. Seuil de densité de probabilité au sens
#'     de ggplot2::geom_density(). Par défaut il est de 1\%.
#'
#' @return Un dataframe contenant les seuils haut et bas, c'est-à-dire les tailles ou
#'     poids mini et maxi à retenir au seuil choisi.
#' @export
#'
#' @importFrom ggplot2 aes geom_density ggplot_build
#' @importFrom dplyr select filter pull
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' qtp_seuils (df = tp_data,
#'             var_taxon = esp_code_alternatif,
#'             var_a_tester = mei_taille,
#'             seuil_densite = 0.001)
#' }
qtp_seuils2 <- function(df,
                        var_taxon,
                        var_a_tester = mei_taille,
                        seuil_densite = 0.01) {

  # gestion de l'évaluation
  var_taxon <- enquo(var_taxon)
  var_a_tester <- enquo(var_a_tester)

  # fonction pour une espèce ----------------------
  qtp_seuils_1sp <- function(df,
                          var_taxon,
                          sel_taxon,
                          var_a_tester,
                          seuil_densite) {
    data_esp <- df %>%
      filter(!!var_taxon == sel_taxon,!!var_a_tester > 0)

    # graphique simplifié
    p <- ggplot(data = data_esp, aes(x = !!var_a_tester)) +
      geom_density()

    # extraction des données du graphique
    p <- ggplot_build(p)

    densite <- p$data[[1]] %>%
      as.data.frame() %>%
      select(x, scaled)

    # valeurs extremes
    densite <- densite %>%
      filter(scaled > seuil_densite) %>%
      pull(x) %>%
      range() %>%
      round() %>%
      as.data.frame()

    densite %>%
      mutate(esp_code_alternatif = sel_taxon) %>%
      cbind(variable = c("mini", "maxi"))

  }
  # fin fonction pour une espèce ----------------------

  # application sur un vecteur contenant les codes (ou nom) des espèces

  # vecteur des espèces
  mes_especes <- df %>%
    pull(!!var_taxon) %>%
    unique()

  # application
  map(
    .x = mes_especes,
    .f = qtp_seuils_1sp,
    df = df,
    var_taxon = var_taxon,
    var_a_tester = var_a_tester,
    seuil_densite = seuil_densite
  ) %>%
    reduce(rbind) %>%
    pivot_wider(names_from = "variable",
                values_from = 1)

}

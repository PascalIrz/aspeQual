#' Grapher l'histogramme comparé des deux méthodes de dégroupage
#'
#' S'applique typiquement pour comparer les distributions des longueurs entre individus
#'     mesurés et "dégroupés".
#'
#' @param df Dataframe de données avec les variables "ope_id", "esp_nom_commun" issues des tables Aspe
#'     ainsi que la variable choisie
#'     à représenter.
#' @param ope Entier. Numéro de l'opération.
#' @param espece Caractère. Nom commun de l'espèce à représenter.
#' @param x_var Caractère. Nom de la variable dont la distribution sera représentée, entre guillemets.
#' @param groupe_var Caractère. Nom de la variable de groupage, entre guillemets.
#' @param bins Entier. Nombre d'intervalles de l'histogramme (20 par défaut).
#'
#' @return L'histogramme ggplot de x_var pour cette espèce, lors de cette opération.
#' @export
#'
#' @importFrom dplyr filter group_by summarise slice select pull enquo
#' @importFrom grid grid.text gpar
#' @importFrom ggplot2 ggplot aes geom_density geom_vline labs theme scale_fill_brewer
#' @importFrom ggplot2 scale_color_brewer annotation_custom element_text element_blank
#'
#' @examples
#' \dontrun{
#' qta_gg_histo_comp_degroupage (df = df, ope = 6323, x_var = mei_taille, groupe_var = degroupage)
#' }
qta_gg_histo_comp_degroupage <- function(df,
                                         ope,
                                         espece,
                                         x_var,
                                         groupe_var,
                                         bins = 20)

{

  x_var <- enquo(x_var)
  groupe_var <- enquo(groupe_var)

  population_data <- df %>%
    filter(ope_id == ope &
           esp_nom_commun == espece)

  sous_titre <- population_data %>%
    slice(1) %>%
    select(pop_libelle,
           annee) %>%
    paste(collapse = " - ")

  titre <- paste0(
    "Op\u00e9ration ",
    population_data %>%
      slice(1) %>%
      select(ope_id, esp_nom_commun) %>%
      paste(collapse = " - ")
  )

  ggplot(data = population_data,
         aes(x = !!x_var,
             fill = !!groupe_var,
             col = !!groupe_var)) +
    geom_density(alpha = 0.3) +
    labs(x = "",
         y = "",
         fill = "D\u00e9groupage",
         col = "",
         title = titre,
         subtitle = sous_titre) +
    theme(plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 8)) +
    scale_fill_brewer(palette = "Set1",
                      labels = c("Aspe", "TA int\u00e9gral", "TA partiel")
    ) +
    scale_color_brewer(palette = "Set1",
                       guide = "none")

}

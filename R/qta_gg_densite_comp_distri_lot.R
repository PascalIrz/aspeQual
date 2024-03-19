#' Grapher le density plot de deux sous-lots
#'
#' S'applique typiquement pour comparer les distributions des longueurs entre individus
#'     mesurés et "dégroupés".
#'
#' @param df Dataframe de données avec une variable "lop_id" et le nom de la variable choisie
#'     à représenter.
#' @param lot Entier. Numéro du lot.
#' @param variable Caractère. Nom de la variable à représenter, entre guillemets.
#'
#' @return Le graphique ggplot pour ce lot et cette variable
#' @export
#'
#' @importFrom dplyr filter group_by summarise slice select pull
#' @importFrom grid grid.text gpar
#' @importFrom ggplot2 ggplot aes geom_density geom_vline labs theme scale_fill_brewer
#' @importFrom ggplot2 scale_color_brewer annotation_custom element_text element_blank
#'
#' @examples
#' \dontrun{
#' qta_gg_densite_comp_distri_lot (df = df, lot = 3156360, variable = "mei_taille")
#' }
qta_gg_densite_comp_distri_lot <- function(df,
                                           lot,
                                           variable)

{
  lot_data <- df %>%
    filter(lop_id == lot)

  mes_stats <- lot_data %>%
    group_by(mei_mesure_reelle) %>%
    summarise(moy = mean(get(variable)),
              effectif = n())

  sous_titre <- lot_data %>%
    slice(1) %>%
    select(pop_libelle, annee) %>%
    paste(collapse = " - ")

  titre <- paste0(
    "Lot ",
    lot_data %>%
      slice(1) %>%
      select(lop_id, esp_nom_commun) %>%
      paste(collapse = " - ")
  )

  texte <- mes_stats %>% pull(effectif)
  texte <- paste0("n=", texte)

  mon_grob <- grid.text(
    label = texte,
    x = c(0.80, 0.80),
    y = c(0.88, 0.95),
    just = "left",
    gp = gpar(col = c("#1B9E77", "#D95F02"),
    fontsize = 8
  ))

ggplot(data = lot_data,
       aes(x = get(variable),
           fill = mei_mesure_reelle,
           col = mei_mesure_reelle)) +
  geom_density(alpha = 0.2) +
  geom_vline(data = mes_stats,
             aes(xintercept = moy,
                 col = mei_mesure_reelle)) +
  labs(x = "",
       y = "",
       fill = "Individus",
       col = "",
       title = titre,
       subtitle = sous_titre) +
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_brewer(palette = "Dark2",
                     labels = c("Non mesur\u00e9s", "Mesur\u00e9s")) +
  scale_color_brewer(palette = "Dark2",
                      guide = "none") +
  annotation_custom(mon_grob)

}

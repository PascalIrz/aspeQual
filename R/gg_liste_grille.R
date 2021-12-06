#' Assembler des graphiques comparant les sous-lots
#'
#' S'applique typiquement sur des graphiques issus de qta_densite_comp_distri_lot().
#'
#' @param liste_graphiques Liste contenant les graphiques à assembler en grille.
#' @param n_colonnes Entier. Nombre de colonnes de la grille.
#' @param n_lignes Entier. Nombre de lignes de la grille.
#' @param titre_axe_x Caractère. Titre de l'axe des abscisses, commun aux graphiques de la grille.
#' @param titre_axe_y Caractère. Titre de l'axe des ordonnées, commun aux graphiques de la grille.
#'
#' @return Le graphique ggplot assemblé.
#' @export
#'
#' @importFrom ggpubr ggarrange text_grob annotate_figure
#'
#' @examples
#' \dontrun{
#' # Avec 12 graphiques à assembler
#' gg_liste_grille(liste_graphiques = graphiques, ncol = 3, nrow = 4)
#' }
gg_liste_grille <- function(liste_graphiques,
                            n_colonnes,
                            n_lignes,
                            titre_axe_x = "",
                            titre_axe_y = "")

{
  fig <- ggarrange(
    plotlist = liste_graphiques,
    ncol = n_colonnes,
    nrow = n_lignes,
    common.legend = TRUE,
    legend = "top"
  )

  annotate_figure(
    fig,
    bottom = ggpubr::text_grob(titre_axe_x),
    left = ggpubr::text_grob(titre_axe_y, rot = 90)
  )

}

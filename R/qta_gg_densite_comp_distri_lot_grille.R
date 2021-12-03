#' Assembler des graphiques comparant les sous-lots
#'
#' S'applique typiquement sur des graphiques issus de qta_densite_comp_distri_lot().
#'
#' @param liste_graphiques Liste contenant les graphiques à assembler en grille.
#' @param ncol Entier. Nombre de colonnes de la grille.
#' @param nrow Entier. Nombre de lignes de la grille.
#'
#' @return Le graphique ggplot assemblé
#' @export
#'
#' @importFrom ggpubr ggarrange text_grob annotate_figure
#'
#' @examples
#' \dontrun{
#' # Avec 12 graphiques à assembler
#' qta_gg_densite_comp_distri_lot_grille(liste_graphiques = graphiques, ncol = 3, nrow = 4)
#' }
qta_gg_densite_comp_distri_lot_grille <- function(liste_graphiques, ncol, nrow)

{
  fig <- ggarrange(
    plotlist = liste_graphiques,
    ncol = ncol,
    nrow = nrow,
    common.legend = TRUE,
    legend = "top"
  )

  annotate_figure(
    fig,
    bottom = ggpubr::text_grob("Longueur (mm)"),
    left = ggpubr::text_grob("Densit\u00e9", rot = 90)
  )

}

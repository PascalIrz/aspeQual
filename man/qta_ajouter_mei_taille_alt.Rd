% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qta_ajouter_mei_taille_alt.R
\name{qta_ajouter_mei_taille_alt}
\alias{qta_ajouter_mei_taille_alt}
\title{Ajouter à un df une colonne de longueur par dégroupage des lots}
\usage{
qta_ajouter_mei_taille_alt(df)
}
\arguments{
\item{df}{Dataframe de données avec des variables "lop_id", "mei_mesure_reelle", "mei_id",
"tyl_libelle" et "mei_taille".}
}
\value{
Le dataframe complété.
}
\description{
Il s'agit d'une méthode alternative à celle utilisée par la base Aspe. Elle affecte à
    chaque individu d'un lot "S/L" qui n'a pas été mesuré une longueur tirée aléatoirement
    parmi celles des individus du sous-lot qui a été mesuré.
}
\examples{
\dontrun{
data <- data \%>\% qta_ajouter_mei_taille_alt()
}
}

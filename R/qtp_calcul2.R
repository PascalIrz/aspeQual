#' Calculer les relations taille-poids sur un ensemble d'espèces
#'
#' @param df Dataframe contenant les données.
#' @param especes Vecteur texte contenant les codes espèces et trois lettres.
#' @param seuil_densite Numérique. Seuil de densité de probabilité probabilité au sens
#'     de ggplot2::gemp_density(). Par défaut il est de 1p1000.
#' @param seuil_poids_absolu Numérique. Poids minimum pour qu'un individu ne soit pas exclus,
#'     en grammes.
#'
#' @return Un dataframe contenant pour chaque espèce les coefficients et un résumé de la régression.
#' @export
#'
#' @importFrom purrr map reduce
#' @importFrom dplyr mutate_at vars arrange
#'
#' @examples
#' \dontrun{
#' qtp_calcul2 (df = tp_data,
#' sel_taxons = c("GAR", "SAN", "BRE"),
#' var_taxon = esp_code_alternatif,
#' seuil_poids_absolu = 10)
#' }
qtp_calcul2 <- function(df,
                        var_taxon,
                        sel_taxons = NA,
                        var_poids = mei_poids,
                        var_longueur = mei_taille,
                        seuil_densite = 0.001,
                        seuil_poids_absolu = 5)

{

  var_taxon <- enquo(var_taxon)
  var_poids <- enquo(var_poids)
  var_longueur <- enquo(var_longueur)

  if(is.na(sel_taxons)) {sel_taxons <- df %>% pull(!!var_taxon) %>% unique()}

  # fonction pour une espèce ----------------------
  qtp_calcul_1sp2 <- function(df,
                              var_taxon,
                              sel_taxon,
                              var_poids,
                              var_longueur,
                              seuil_densite,
                              seuil_poids_absolu)

  {
    # var_taxon <- enquo(var_taxon)
    # var_poids <- enquo(var_poids)
    # var_longueur <- enquo(var_longueur)

    df <- df %>%
      filter(!!var_taxon == sel_taxon &
               !!var_longueur > 0 &
               !!var_poids > 0)


    seuils_taille <- qtp_seuils2(df = df,
                                 var_taxon = !!var_taxon,
                                 var_a_tester = !!var_longueur,
                                 seuil_densite = seuil_densite)

    seuils_poids <- qtp_seuils2(df = df,
                                var_taxon = !!var_taxon,
                                var_a_tester = !!var_poids,
                                seuil_densite = seuil_densite)

    df <- df %>%
      filter(mei_taille > seuils_taille$mini,
             mei_taille < seuils_taille$maxi,
             mei_poids > seuils_poids$mini,
             mei_poids < seuils_poids$maxi,
             mei_poids > seuil_poids_absolu)

    if(nrow(df) > 2)

    {

      mod <- stats::lm(log(mei_poids) ~ log(mei_taille),
                       data = df)

      dist_cook <- stats::cooks.distance(mod)
      seuil_cook <- 4 / nrow(df)

      df <- df %>%
        cbind(dist_cook) %>%
        filter(dist_cook < seuil_cook)

      model_spec <- paste0("log(",
                           expr(mei_poids),
                           ") ~ log(",
                           expr(mei_taille),
                           ")")

      mod <- stats::lm(formula = model_spec,
                       data = df)

      a <- mod$coefficients["(Intercept)"] %>% exp()
      b <- mod$coefficients["log(mei_taille)"]
      r2_ajuste <- summary(mod)$adj.r.squared
      n_ind <- nrow(df)

      resultat <- cbind(sel_taxon,
                        a,
                        b,
                        r2_ajuste,
                        n_ind,
                        seuils_taille$mini,
                        seuils_taille$maxi,
                        seuils_poids$mini,
                        seuils_poids$maxi) %>%
        as.data.frame() %>%
        rename(taille_mini = V6,
               taille_maxi = V7,
               poids_mini = V8,
               poids_maxi = V9,
               esp_code_alternatif = sel_taxon)

      resultat

    }else{

      resultat <- cbind(sel_taxon,
                        a = NA,
                        b = NA,
                        r2_ajuste = NA,
                        n_ind = NA,
                        seuils_taille$mini,
                        seuils_taille$maxi,
                        seuils_poids$mini,
                        seuils_poids$maxi) %>%
        as.data.frame() %>%
        rename(taille_mini = V6,
               taille_maxi = V7,
               poids_mini = V8,
               poids_maxi = V9,
               esp_code_alternatif = sel_taxon)}

  }


  # fin fonction pour une espèce ----------------------

  # application au vecteur des espèces
  map(.x = sel_taxons,
      .f = qtp_calcul_1sp2,
      df = df,
      var_taxon = var_taxon,
      var_poids = var_poids,
      var_longueur = var_longueur,
      seuil_densite = seuil_densite,
      seuil_poids_absolu = seuil_poids_absolu) %>%
    reduce(rbind) %>%
    mutate_at(vars(-!!var_taxon),
              as.numeric) %>%
    arrange(-n_ind)

}

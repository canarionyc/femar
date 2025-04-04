#' @title Calculate Moran Index for values of points coords at a distance less or equal than radius
#' @param radius double
#' @param values vector
#' @param coords matrix
#' @param verbose logical
#' @return double
#'
#' @import spdep
#' @export

moran_sensitivity <- function(radius, values,  coords,verbose=FALSE) {
  # ?suppressWarnings
  # ?dnearneigh
  stopifnot(sf::sf_use_s2()==FALSE )
  if(is.matrix(coords))  {
    stopifnot(dim(coords)[1]==length(values))
    # coords <- coords[1:100L,]; values <- values[1:100L]
  } else {
    stopifnot(length(coords)==length(values))
  }
  suppressWarnings(dist.nb <- dnearneigh(coords, d1 = 0, d2 = radius, use_kd_tree = FALSE))
  # plot(dist.nb, coords)
  dist.lw <- nb2listw(dist.nb, style = "W", zero.policy = TRUE)
  # str(dist.lw)
  # ?moran
  out <- spdep::moran(values, listw = dist.lw, n=length(values),S0 =  Szero(dist.lw), zero.policy = TRUE)

  if(verbose) message(sprintf("radius=%8.2f Moran's I statistic=%.4f", radius, out$I))
  return(out$I)
}# ; moran_sensitivity_vec <- Vectorize(moran_sensitivity, vectorize.args = "radius")

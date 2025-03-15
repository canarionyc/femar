#' @export
get_DINS <- function() {
  DINS_2025_Palisades.shp <-"E:\\Datasets\\GOV\\CA\\CNRA\\DINS_2025_Palisades_Public_View.shp"; stopifnot(file.exists(DINS_2025_Palisades.shp))
  message(DINS_2025_Palisades.shp)
  DINS_2025_Palisades <- terra::vect(DINS_2025_Palisades.shp)
  show(DINS_2025_Palisades)
  summary(DINS_2025_Palisades)

  ext(DINS_2025_Palisades)

  table(DINS_2025_Palisades$STRUCTURET)
  table(DINS_2025_Palisades$DAMAGE)

  cat(crs(DINS_2025_Palisades))
DINS_2025_Palisades$DAMAGE <- factor(DINS_2025_Palisades$DAMAGE, levels = c('Destroyed (>50%)','Major (26-50%)','Minor (10-25%)','Affected (1-9%)','No Damage'))
str(DINS_2025_Palisades$DAMAGE )
return(DINS_2025_Palisades)
}

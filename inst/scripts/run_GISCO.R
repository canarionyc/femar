# setup -------------------------------------------------------------------
readRenviron("~/.Renviron")
Sys.getenv("GISCO_CACHE_DIR")
library(giscoR)
help(package="giscoR")
?gisco_detect_cache_dir
stopifnot(dir.exists(gisco_detect_cache_dir()))

library(sf)
library(dplyr)

# Different resolutions ----
(ES_res60 <- gisco_get_countries(resolution = "60", country = "ES") %>%
  mutate(res = "60M"))
ES_res20 <-
  gisco_get_countries(resolution = "20", country = "ES") %>%
  mutate(res = "20M")
ES_res10 <-
  gisco_get_countries(resolution = "10", country = "ES") %>%
  mutate(res = "10M")
(ES_res03 <-
  gisco_get_countries(resolution = "03", country = "ES") %>%
  mutate(res = "03M"))

plot(st_geometry(ES_res03))

debugonce(gisco_get_lau)
?gisco_get_lau
lau_es <- gisco_get_lau(epsg="3035"
                        , country="ES"
                        ,verbose = TRUE
                        )

lau_es

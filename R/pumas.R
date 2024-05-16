# pumas ----

# tigris includes a built-in data frame named fips_codes that used to match state and county names
# with Census FIPS codes in the various functions in the package. It can also be used to generate vectors
# of codes to be passed to tigris functions as in this example, so that the analyst does not have to generate
# the full list of state codes by hand.

us_states <- unique(fips_codes$state)[1:51]
continental_states <- us_states[!us_states %in% c("AK", "AS","MP", "HI","PR","VI","GU")]
us_pumas <- rbind_tigris(
  lapply(
    continental_states, function(x) {
      pumas(state = x, cb = TRUE)
    }
  )
)
saveRDS(us_pumas, file=file.path(Sys.getenv("WRKDIR"),"US_PUMAS.rds"))
plot(us_pumas)

ca_spdf <- pumas(state="CA")
show(ca_spdf@data)


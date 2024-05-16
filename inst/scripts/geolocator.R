
# call_geolocator ---------------------------------------------------------
# https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?address=330%203rd%20ave%2C%20NY%2010010&benchmark=4
?tigris::call_geolocator
debugonce(tigris::call_geolocator)
census_block <- tigris::call_geolocator(street = "330, 3rd avenue", city="New York", state="NY", zip="10010")
str(census_block)


# call_geolocator_latlon --------------------------------------------------

?tigris::call_geolocator_latlon
latlon <- tigris::call_geolocator_latlon(street = "330, 3rd avenue", city="New York", state="NY", zip="10010")
str(latlon)

# census blocks -----------------------------------------------------------

?tigris::blocks


# append_geoid ------------------------------------------------------------

airports <- data.frame(
  street = "700 Catalina Dr", city = "Daytona Beach", state = "FL"
)
append_geoid(airports, 'tract')


# fnma geo ----
FNM_MS_202010 <- readRDS("~/Finance/FNMA/Work_Dir/fnma-mbs-sf-singleclass/FNM_MS_202010.rds")
ms15 <- FNM_MS_202010[["Record_Type==15"]]
ms15.1 <- ms15[Security_Identifier=="MC0644"]
usms15 <- merge(states_sf, ms15.1, by.x='STUSPS', by.y='Property_State')
plot(usms15["Percentage_Investor_Loan_UPB"])

states_map %>% (usms15["Percentage_Investor_Loan_UPB"])

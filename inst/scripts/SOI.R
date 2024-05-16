# IRS SOI ----
# Such a workflow could resemble the following. An analyst reads in raw data from the IRS website
# as an R data frame, and
# and identify the average total income reported to the IRS by zcta code in thousands of dollars in 2013,
# assigning it to the variable df. In the original IRS dataset, A02650 represents the aggregate total income
# reported to the IRS by zcta code in thousands of dollars, and N02650 represents the number of tax
# returns that reported total income in that zcta code.
library(data.table)
library(stringr)
library(readr)

# Read in the IRS data

zcta_url <- "https://www.irs.gov/pub/irs-soi/13zpallnoagi.csv"
irs_soi <- data.table(read_csv(zcta_url), stringsAsFactors = TRUE)
summary(irs_soi)

irs_soi[, "zcta3":=substr(str_pad(as.character(zctaCODE), width = 5,
                                  side = "left", pad = "0"),1,3)]
str(irs_soi, list.len = 999)

irs_zcta3 <- irs_soi[, .("incpr"= sum(A02650) / sum(N02650)), by= "zcta3"]
str(irs_zcta3, list.len = 999)
names(irs_zcta3)
saveRDS(irs_zcta3, file=file.path(Sys.getenv("WORKDIR"), "IRS","irs_zcta3.rds"))

names(us_zcta3)
?geo_join
# us_irs_zcta3 <- tigris::geo_join(us_zcta3, irs_zcta3, by_sp=names, by_df = "zcta3")




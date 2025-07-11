
#' @import httr

get_DisasterDeclarations <- function(){
    datalist = list()       # a list that will hold the results of each call

    baseUrl <- "https://www.fema.gov/api/open/v2/FemaWebDisasterDeclarations?"

    # Determine record count. Specifying only 1 column here to reduce amount of data returned.
    #   Remember to add criteria/filter here (if you have any) to get an accurate count.
    result <- GET(paste0(baseUrl,"$inlinecount=allpages&$top=1&$select=id"))
    jsonData <- content(result)         # should automatically parse as JSON as that is mime type
    recCount <- jsonData$metadata$count

    # calculate the number of calls we will need to get all of our data (using the maximum of 1000)
    top <- 1000
    loopNum <- ceiling(recCount / top)

    # send some logging info to the console so we know what is happening
    print(paste0("START ",Sys.time(),", ", recCount, " records, ", top, " returned per call, ", loopNum," iterations needed."),quote=FALSE)

    # Loop and call the API endpoint changing the record start each iteration. Each call will
    # return results in a JSON format. The metadata has been suppressed as we no longer need it.
    skip <- 0
    for(i in seq(from=0, to=loopNum, by=1)){
        # As above, if you have filters, specific fields, or are sorting, add that to the base URL
        #   or make sure it gets concatenated here.
        result <- GET(paste0(baseUrl,"$metadata=off&$top=",top,"&$skip=",i * top))
        jsonData <- content(result)         # should automatically parse as JSON as that is mime type

        # Here we are adding the resulting JSON return to a list that can be turned into a combined
        #   dataframe later or saved. You may encounter memory limitations with very large datasets.
        #   For those, inserting into a database or saving chunks of data may be desired.
        datalist[[i+1]] <- jsonData

        print(paste0("Iteration ", i, " done)"), quote=FALSE)
    }

    # binds many items in our list to one data frame
    fullData <- dplyr::bind_rows(datalist)

    # Save as one R object - probably more useful (and storage efficient) than CSV or JSON if doing
    #   analysis within R.
    saveRDS(fullData, file = "output.rds")

    # open file just to verify that we got what we expect
    my_data <- readRDS(file = "output.rds")
    print(paste0("END ",Sys.time(), ", ", nrow(my_data), " records in file"))
}

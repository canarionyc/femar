

get_tigris_cache <- function(){
  tigris_cache_df <- data.frame(file_path=list.files(Sys.getenv("TIGRIS_CACHE_DIR"), full.names = TRUE))

  tigris_cache_df$mtime <- file.mtime(tigris_cache_df$file_path)
  tigris_cache_df[order(tigris_cache_df$mtime,decreasing = TRUE),]
}


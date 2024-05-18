(tabblock_stats <- tabblock[, .(CENSUS_BLOCK_COUNT=.N, HOUSING20 =sum(HOUSING20 ), POP20 =sum(POP20))]) %>% as_tibble()

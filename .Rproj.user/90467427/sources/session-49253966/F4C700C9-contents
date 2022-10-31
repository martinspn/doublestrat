library(tidyverse)

## Function - Double Stratification Analysis

dsa <- function(data,
                same, strata_same, tile_same_colname,
                ascending, strata_ascending, tile_ascending_colname,
                ties.method = "min"
                ){

        df <- data %>%
        mutate(!! (tile_same_colname) := ceiling(strata_same*rank(!!rlang::sym(same), ties.method = ties.method)/n())) %>%
        group_by(!!rlang::sym(tile_same_colname)) %>%
        mutate(!! (tile_ascending_colname) := ceiling(strata_ascending*rank(!!rlang::sym(ascending), ties.method = ties.method)/n())) %>%
        ungroup()

        return(df)
}

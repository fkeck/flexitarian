
#' Long format to community data matrix
#'
#' @param x a tibble.
#' @param counts column name of count data.
#' @param col.names column name for column names (usually taxonomic names).
#' @param row.names column name for row names (usually site names).
#' @param keep.order keep the original order of rows and columns.
#' If \code{FALSE} (default) rows and columns are reordered by alphabetical order.
#' @param fill.missing By default, missing values will be replaced by NA.
#' This is sometimes useful to replace with another value (typically zero).
#' Note that all the NAs will be affected.
#'
#' @return a dataframe in wide format (typical sites x species community data matrix).
#' @export
#' @examples
#' x <- data.frame(matrix(rpois(100, 10), nrow = 10))
#' rownames(x) <- paste("Site", 1:10)
#' colnames(x) <- paste("Species", LETTERS[1:10])
#' x
#' x_tidy <- tidy_cdm(x)
#' x_tidy
#' x_df <- spread_cdm(x_tidy, SITE, TAXON, COUNT)
#' x_df
#' x_df <- spread_cdm(x_tidy, SITE, TAXON, COUNT, keep.order = TRUE)
#' all.equal(x, x_df)
#'
spread_cdm <- function(x, row.names, col.names, counts, keep.order = FALSE, fill.missing = NA) {

  quo_counts <- dplyr::enquo(counts)
  quo_col.names <- dplyr::enquo(col.names)
  quo_row.names <- dplyr::enquo(row.names)
  x <- dplyr::ungroup(x)
  res <- dplyr::select(x, !!quo_counts, !!quo_col.names, !!quo_row.names)
  res <- tidyr::spread(res, key = !!quo_col.names, value = !!quo_counts, fill = fill.missing)
  row_names <- tibble::deframe(dplyr::select(res, !!quo_row.names))
  res <- dplyr::select(res, -!!quo_row.names)
  res <- as.data.frame(res)
  rownames(res) <- row_names

    if(keep.order){
  res <- res[unique(deframe(select(x, !!quo_row.names))),
             unique(deframe(select(x, !!quo_col.names)))]
    }

  return(res)
}



#' Community data matrix to tibble
#'
#' @param x a community matrix or dataframe.
#' @param key.name name of the key column.
#' @param value.name name of the value column.
#' @param row.name name of the column where row names are transfered.
#'
#' @return a tibble.
#' @export
#'
#' @examples
#' x <- matrix(rpois(100, 10), nrow = 10)
#' rownames(x) <- paste("Site", 1:10)
#' colnames(x) <- paste("Species", LETTERS[1:10])
#' x
#' x_tidy <- tidy_cdm(x)
#' x_tidy
#'
tidy_cdm <- function(x, row.name = "SITE", key.name = "TAXON", value.name = "COUNT"){
  x <- as.data.frame(x)
  x <- tibble::as_tibble(x, rownames = row.name)
  res <- tidyr::gather(x, !!key.name, !!value.name, -!!enquo(row.name))
  return(res)
}




#' Convert tibbles to dataframes
#'
#' @param x a tibble.
#' @param row.names name of the column to use as row names in the returned dataframe.
#'
#' @return a dataframe
#' @export
tbl_to_df <- function(x, row.names){
  quo_row.names <- dplyr::enquo(row.names)

  x <- dplyr::ungroup(x)

  row_names <- tibble::deframe(dplyr::select(x, !!quo_row.names))
  res <- dplyr::select(x, -!!quo_row.names)
  res <- as.data.frame(res)
  rownames(res) <- row_names
  return(res)
}

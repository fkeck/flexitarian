
#' Long format to community data matrix
#'
#' @param x a tibble.
#' @param counts column name of count data.
#' @param col.names column name for column names (usually taxonomic names).
#' @param row.names column name for row names (usually site names).
#'
#' @return a dataframe in wide format (typical sites x species community data matrix).
#' @export
#' @example
#' x <- matrix(rpois(100, 10), nrow = 10)
#' rownames(x) <- paste("Site", 1:10)
#' colnames(x) <- paste("Species", LETTERS[1:10])
#' x
#' x_tidy <- tidy_cdm(x)
#' x_tidy
#' y <- spread_cdm(x_tidy, SITE, TAXON, COUNT)
#' y
#'
spread_cdm <- function(x, row.names, col.names, counts) {

  quo_counts <- dplyr::enquo(counts)
  quo_col.names <- dplyr::enquo(col.names)
  quo_row.names <- dplyr::enquo(row.names)
  x <- dplyr::ungroup(x)
  res <- dplyr::select(x, !!quo_counts, !!quo_col.names, !!quo_row.names)
  res <- tidyr::spread(res, key = !!quo_col.names, value = !!quo_counts)
  row_names <- tibble::deframe(dplyr::select(res, !!quo_row.names))
  res <- dplyr::select(res, -!!quo_row.names)
  res <- as.data.frame(res)
  rownames(res) <- row_names
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
#' @example
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



#' Save multiple plots in one PDF.
#'
#' Convenient function for saving multiple plots stored in a list. The function
#' can also add bookmarks to the created pdf files.
#'
#' @param x a list of plots.
#' @param file a file path.
#' @param width width of the plots.
#' @param height height of the plots.
#' @param bookmarks a character vector for pdf bookmarks.
#' If \code{NULL} (default), no bookmarks are added.
#' @param gs.exec a path to your Ghostscript executable
#' (necessary to add bookmarks).
#' @param ... Other parameters passed on to the pdf function.
#'
#' @details
#' Bookmarks are added to pdf using Ghostscript, a third party program which
#' must be installed manually by the user. Tested on Linux only, probably not working
#' on Windows.
#'
#' @export
#'
save_plots <- function(.data, ...,
                        files, width = 8, height = 6,
                        bookmarks = NULL, gs.exec = "gs"){

  .data <- dplyr::arrange(.data, !!!bookmarks)
  plot_cols <- dplyr:::select.data.frame(.data, ...)
  bk_dat <- dplyr::select(.data, !!!bookmarks)

  #bk_dat <- arrange_all(bk_cols)
  bk_dat <- as.matrix(bk_dat)

  res <- flat_fac(bk_dat)

  bk_file <- tempfile(fileext = ".info")
  #bookmarks <- iconv(bookmarks, to = "ASCII//TRANSLIT")
  writeLines(res, bk_file)

  map2(plot_cols, files, function(plot_col, file){
    pdf(file, width = width, height = height)
    invisible(lapply(plot_col, print))
    dev.off()
    if(!is.null(bookmarks)){

      bridge_file <- tempfile(fileext = ".pdf")
      comm_gs <- paste0(gs.exec, " -sDEVICE=pdfwrite -q -dBATCH -dNOPAUSE ",
                        "-sOutputFile=", bridge_file,
                        " -dPDFSETTINGS=/prepress ", bk_file,
                        " -f ", file,
                        " && mv ", bridge_file, " ", file)
      system(comm_gs)
    }
  })
}


# library(tidyverse)
# zz <- iris %>%
#   as_tibble() %>%
#   group_by(Species) %>%
#   nest() %>%
#   mutate(Group = c("Group 1", "Group 2", "Group 1")) %>%
#   mutate(pl = map(data, ~ ggplot(.x) +
#            geom_point(aes(Sepal.Length, Sepal.Width))
#            ),
#          pl2 = map(data, ~ ggplot(.x) +
#                     geom_point(aes(Petal.Length, Petal.Width))
#          ))
#
# save_plots(zz, pl, pl2, files = c("pl.pdf", "pl2.pdf"), bookmarks = vars(Group, Species))
#
# zz

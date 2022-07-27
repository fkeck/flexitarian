
utils::globalVariables(c("n"))


#' Rarefaction for long format community data
#'
#' This function can be used to perform rarefaction (subsampling)
#' of community data.
#' The actual rarefaction is performed
#' by \code{\link[vegan]{rrarefy}} from \pkg{vegan}.
#'
#' @param x a tibble of community data in long format.
#' @param sites column name of the sites.
#' @param taxon column name of the taxa.
#' @param counts column name of the counts.
#' @param sample subsample size for rarefying community.
#' If \code{NULL} (default) the subsample size
#' will be the size of the smallest community.
#' @param verbose if \code{TRUE} (default), print the subsample size
#' and the number of sites discarded because below the subsample size.
#'
#' @return a tibble of rarefied community data in long format.
#' @export
#'
rarefy_long <- function(x, sites, taxon, counts, sample = NULL, verbose = TRUE) {

  quo_sites <- dplyr::enquo(sites)
  quo_taxon <- dplyr::enquo(taxon)
  quo_counts <- dplyr::enquo(counts)

  x <- dplyr::ungroup(x)

  sites_counts <- dplyr::count(x, !!quo_sites, wt = !!quo_counts, name = "n")

  if(is.null(sample)) {
    sample <- min(sites_counts$n)
  }

  sel_sites <- dplyr::filter(sites_counts, n >= sample)
  sel_sites <- tibble::deframe(dplyr::select(sel_sites, !!quo_sites))

  sites_rm <- nrow(sites_counts) - length(unique(sel_sites))

  if(verbose) {
    message("Performing rarefaction at ", sample, " individuals.\n",
            "Removing ", sites_rm, " sites with too few individuals.")
  }

  res <- spread_cdm(x, !!quo_sites, !!quo_taxon, !!quo_counts, fill.missing = 0L)
  res <- res[sel_sites, ]
  res <- vegan::rrarefy(res, sample)
  res <- tidy_cdm(res,
                  row.name = dplyr::as_label(quo_sites),
                  key.name = dplyr::as_label(quo_taxon),
                  value.name = dplyr::as_label(quo_counts))
  return(res)
}

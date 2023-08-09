#' Calculate distance matrix
#'
#' `calc_dist_matrix()` calculates a distance matrix from community data.
#'
#' @param commun_data Community data.
#' @param diss_index Dissimilarity index. See [vegan::vegdist()] for options.
#'
#' @export
calc_dist_matrix <- function(commun_data, diss_index = "jaccard") {
  rlang::check_installed("vegan")

  commun <- as.matrix(commun_data)
  vegan::vegdist(commun, method = diss_index, na.rm = TRUE)
}

#' Calculate MDS
#'
#' `calc_mds()` performs the MDS calculation from a distance matrix.
#'
#' @param dist_matrix Distance matrix.
#' @param n_dims Number of dimensions.
#' @param max_tries Maximum number of tries.
#'
#' @return Object of class `metaMDS`.
#' @export
calc_mds <- function(dist_matrix, n_dims, max_tries = 50) {
  rlang::check_installed("vegan")

  vegan::metaMDS(
    dist_matrix,
    autotransform = FALSE,
    k = n_dims,
    trymax = max_tries,
    trace = 0
  )
}

#' Get MDS list
#'
#' `get_mds_list()` calculates a series of ordination and produces a list of
#' `metaMDS` objects from a distance matrix and a range of dimensions.
#'
#' @param dist_matrix Distance matrix.
#' @param dims A length 2 vector describing the range of dimensions to ordinate
#'   over. Defaults to 2--8.
#' @param max_tries Maximum number of tries. Defaults to 50.
#'
#' @return Named list of objects of class `metaMDS`.
#' @export
get_mds_list <- function(dist_matrix, dims = seq(2, 8), max_tries = 50) {
  dims |>
    purrr::map(~ calc_mds(dist_matrix, .x, max_tries)) |>
    `names<-`(dims)
}

#' Get MDS information
#'
#' Retrieves MDS information from a list of MDSs and presents them in a tabular
#' format.
#'
#' `converg_status` column is derived from the `converged` part of the `metaMDS`
#' object. `converged` means the number of times the best version was repeated
#' (see
#' [here](https://github.com/vegandevs/vegan/blob/0ddfa4e52444de119bf5baae200eef0c05b12c23/R/print.metaMDS.R#L20-L24)).
#' It is originally initialized at 0, so it seems that 0 does mean no solution
#' was reached (see the internal `metaMDSiter()` [function
#' definition](https://github.com/vegandevs/vegan/blob/0ddfa4e52444de119bf5baae200eef0c05b12c23/R/metaMDSiter.R)).
#'
#' @param mds_list List of objects of class `metaMDS`.
#'
#' @return A [tibble()] with information about each ordination.
#' @export
get_mds_info <- function(mds_list) {
  tibble::tibble(mds = mds_list) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      n_dims = mds$ndim,
      stress = mds$stress,
      converg_status = as.logical(mds$converged),
      mds = NULL
    ) |>
    dplyr::ungroup()
}

#' Plot stress vs. number of dimensions
#'
#' `plot_stress_dim()` plots the stress of the iteration run for each of its
#' dimensions. The color of the point represents whether the run converged or
#' not.
#'
#' @param mds_info A [tibble()] with information from each ordination run.
#'   Output from [get_mds_info()].
#'
#' @return A ggplot2 plot.
#' @export
plot_stress_dim <- function(mds_info) {
  ggplot2::ggplot(mds_info, ggplot2::aes(n_dims, stress)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(color = converg_status), size = 5) +
    ggplot2::scale_color_manual(
      values = c("red", "green"),
      labels = c("No solution reached", "Solution reached")
    ) +
    ggplot2::labs(
      x = "Number of dimensions", y = "Stress",
      color = "Convergence status"
    ) +
    ggplot2::theme(
      legend.position = c(1, 1),
      legend.justification = c(1, 1),
      legend.title = ggplot2::element_blank()
    )
}

#' Add text to ggplot2 plot by NPC
#'
#' `add_text_npc()` wraps [ggplot2::annotation_custom()] to create a text grob
#' at location specified by Normalized Parent Coordinates (NPC) in a ggplot2
#' plot.
#'
#' @param text Annotation text
#' @param x,y Position within plot. Defaults to 0.95.
#' @param just Numeric vector of length 2 that specifies justification relative
#'   to position. Defaults to upper right.
#' @param graph_params List of graphical parameters passed to [grid::gpar()].
#'   Sets font size to 10 by default.
#'
#' @export
add_text_npc <- function(text,
                         x = 0.95, y = 0.95,
                         just = c(1, 1),
                         graph_params = list(fontsize = 10)) {
  text_grob <- grid::textGrob(
    label = text,
    x = grid::unit(x, "npc"),
    y = grid::unit(y, "npc"),
    just = just,
    gp = rlang::exec(grid::gpar, !!!graph_params)
  )

  ggplot2::annotation_custom(text_grob)
}


#' Axis guide
#'
#' Axis guides are the visual representation of position scales like those
#' created with [scale_(x|y)_continuous()][scale_x_continuous()] and
#' [scale_(x|y)_discrete()][scale_x_discrete()].
#'
#' @inheritParams guide_legend
#' @param line.theme An [element_line()] to use as a template for the line
#'   along the axis. Usually set with [theme(axis.line = ...)][theme()].
#' @param tick.theme An [element_line()] to use as a template for the ticks
#'   along the axis. Usually set with [theme(axis.ticks = ...)][theme()].
#' @param tick.length A [grid::unit()]
#'
#' @export
#'
guide_axis <- function(# label (axis.text*)
                       label = TRUE,
                       label.theme = NULL,

                       # axis line (axis.line*)
                       line.theme = NULL,

                       # axis ticks (axis.ticks*)
                       tick.theme = NULL,
                       tick.length = NULL,

                       # general
                       order = 0,

                       ...
) {
  structure(
    list(
      # label
      label = label,
      label.theme = label.theme,

      # axis line (axis.line*)
      line.theme = line.theme,

      # axis ticks (axis.ticks*)
      tick.theme = tick.theme,
      tick.length = tick.length,

      # general
      order = order,

      # parameter
      available_aes = c("x", "y"),
      ...,

      name = "axis"
    ),
    class = c("guide", "axis")
  )
}

#' @export
guide_train.axis <- function(guide, scale, aesthetic = NULL) {

  aesthetic <- aesthetic %||% scale$aesthetics[1]
  breaks <- scale$get_breaks()

  empty_ticks <- new_data_frame(
    list(aesthetic = numeric(0), .value = numeric(0), .label = character(0))
  )
  names(empty_ticks) <- c(aesthetic, ".value", ".label")

  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    warning(
      "axis guide needs appropriate scales: ",
      paste(guide$available_aes, collapse = ", "),
      call. = FALSE
    )
    guide$key <- empty_ticks
  } else if (length(breaks) == 0) {
    guide$key <- empty_ticks
  } else {
    ticks <- new_data_frame(setNames(list(scale$map(breaks)), aesthetic))
    ticks$.value <- breaks
    ticks$.label <- scale$get_labels(breaks)

    if (is.list(ticks$.label)) {
      if (any(sapply(ticks$.label, is.language))) {
        ticks$.label <- do.call(expression, ticks$.label)
      } else {
        ticks$.label <- unlist(ticks$.label)
      }
    }

    guide$key <- ticks
  }

  guide$name <- paste0(guide$name, "_", aesthetic)
  guide$hash <- digest::digest(list(guide$title, guide$key$.value, guide$key$.label, guide$name))
  guide
}

# discards the new guide
#' @export
guide_merge.axis <- function(guide, new_guide) {
  guide
}

# axis guides don't care which geometry uses these aesthetics
#' @export
guide_geom.axis <- function(guide, layers, default_mapping) {
  guide
}

# Unlike other guides,
# axis guides don't draw the guide title, and require a `position`
# in c("top", "bottom", "right", "left")
#' @export
guide_gengrob.axis <- function(guide, theme, position = "bottom", ...) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]

  match.arg(aesthetic, c("x", "y"))
  position <- match.arg(position, c("top", "bottom", "right", "left"))

  is_vertical <- position %in% c("left",  "right")
  is_second <- position %in% c("right", "top") # refers to positive npc coordinates
  is_first_gtable <- position %in% c("left", "top") # refers to position in gtable
  n_breaks <- nrow(guide$key)
  opposite_positions <- c("top" = "bottom", "bottom" = "top", "right" = "left", "left" = "right")
  position_opposite <- unname(opposite_positions[position])

  # resolve elements
  line_element_name <- paste0("axis.line.", aesthetic, ".", position)
  tick_element_name <- paste0("axis.ticks.", aesthetic, ".", position)
  tick_length_element_name <- paste0("axis.ticks.length.", aesthetic, ".", position)
  label_element_name <- paste0("axis.text.", aesthetic, ".", position)

  line_element <- guide$line.theme %||% calc_element(line_element_name, theme) %||% element_blank()
  tick_element <- guide$tick.theme %||% calc_element(tick_element_name, theme) %||% element_blank()
  tick_length <- guide$tick.length %||% calc_element(tick_length_element_name, theme) %||% element_blank()
  label_element <- guide$label.theme %||% calc_element(label_element_name, theme) %||% element_blank()

  if (is_vertical) {
    position_dim <- "y"
    non_position_dim <- "x"
    position_size <- "height"
    non_position_size <- "width"
    label_margin_name <- "margin_x"
    gtable_element <- gtable_row
    measure_gtable <- gtable_width
    measure_labels <- grobWidth
  } else {
    position_dim <- "x"
    non_position_dim <- "y"
    position_size <- "width"
    non_position_size <- "height"
    label_margin_name <- "margin_y"
    gtable_element <- gtable_col
    measure_gtable <- gtable_height
    measure_labels <- grobHeight
  }

  if (is_second) {
    tick_direction <- 1
    non_position_panel <- unit(0, "npc")
    tick_coordinate_order <- c(2, 1)
  } else {
    tick_direction <- -1
    non_position_panel <- unit(1, "npc")
    tick_coordinate_order <- c(1, 2)
  }

  if (is_first_gtable) {
    table_order <- c("labels", "ticks")
  } else {
    table_order <- c("ticks", "labels")
  }

  # draw elements
  line_coords <- list(
    position = unit(c(0, 1), "npc"),
    non_position = unit.c(non_position_panel, non_position_panel)
  )
  names(line_coords) <- c(position_dim, non_position_dim)
  line_grob <- do.call(element_grob, c(list(line_element), line_coords))

  if (!guide$label || n_breaks == 0) {
    return(
      absoluteGrob(
        gList(line_grob),
        width = grobWidth(line_grob),
        height = grobHeight(line_grob)
      )
    )
  }

  label_coords <- list(
    position = unit(guide$key[[aesthetic]], "native"),
    label = guide$key$.label,
    margin = TRUE
  )

  tick_coords <- list(
    position = rep(label_coords$position, each = 2),
    non_position = rep(
      unit.c(non_position_panel + (tick_direction * tick_length), non_position_panel)[tick_coordinate_order],
      times = n_breaks
    ),
    id.lengths = rep(2, times = n_breaks)
  )

  names(label_coords) <- c(position_dim, "label", label_margin_name)
  names(tick_coords) <- c(position_dim, non_position_dim, "id.lengths")

  grobs <- list(
    line = line_grob,
    labels = do.call(element_grob, c(list(label_element), label_coords)),
    ticks = do.call(element_grob, c(list(tick_element), tick_coords))
  )

  # assemble elements
  gt_element_order <- match(table_order, c("labels", "ticks"))
  gt_dims <- list(
    dims = unit.c(measure_labels(grobs$labels), tick_length),
    dim = unit(1, "npc")
  )
  gt_dims$dims <- gt_dims$dims[gt_element_order]
  names(gt_dims) <- c(paste0(non_position_size, "s"), position_size)

  gt <- do.call(
    gtable_element,
    c(list(name = "axis", grobs = grobs[table_order]), gt_dims)
  )

  justvp_args <- list(
    non_position_dim = non_position_panel,
    just = position_opposite,
    non_position_size = measure_gtable(gt)
  )
  names(justvp_args) <- c(non_position_dim, "just", non_position_size)

  justvp <- do.call(viewport, justvp_args)

  absoluteGrob(
    gList(grobs$line, gt),
    width = gtable_width(gt),
    height = gtable_height(gt),
    vp = justvp
  )
}


#' Grob for axes
#'
#' For compatibility with previous internal code that draws a grob
#' directly.
#'
#' @param at position of ticks
#' @param labels at ticks
#' @param position position of axis (top, bottom, left or right)
#' @param theme theme object
#'
#' @keywords internal
#' @noRd
#' @return A grob
#'
draw_axis <- function(at, labels, position = "right", theme) {
  match.arg(position, c("top", "bottom", "right", "left"))
  if(position %in% c("top", "bottom")) {
    aesthetic <- "x"
  } else {
    aesthetic <- "y"
  }

  scale <- list(
    get_breaks = function() at,
    map = function(breaks) at,
    get_labels = function(breaks) labels,
    aesthetics = aesthetic
  )

  guide <- guide_train.axis(guide_axis(), scale)
  guide_gengrob.axis(guide, theme, position)
}

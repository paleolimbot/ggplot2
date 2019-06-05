#' Ranges
#'
#' Mutable ranges have a two methods (`train()` and `reset()`), and make
#' it possible to build up complete ranges with multiple passes.
#' Immutable ranges have a set range and cannot be trained or reset.
#'
#' These range objects should be instantiated with
#' [continuous_range()], [discrete_range()], and [immutable_range()].
#'
#' @noRd
Range <- ggproto("Range", NULL,
  range = NULL,
  reset = function(self) {
    self$range <- NULL
  },
  set_range = function(self, range) {
    self$range <- range
    invisible()
  },
  clone = function(self) {
    ggproto(NULL, self, range = self$range)
  },
  finalize = function(self) {
    immutable_range(self$range)
  }
)

RangeDiscrete <- ggproto("RangeDiscrete", Range,
  train = function(self, x, drop = FALSE, na.rm = FALSE) {
    self$range <- scales::train_discrete(x, self$range, drop = drop, na.rm = na.rm)
  }
)

RangeContinuous <- ggproto("RangeContinuous", Range,
  train = function(self, x) {
    self$range <- scales::train_continuous(x, self$range)
  }
)

RangeImmutable <- ggproto("RangeImmutable", Range,
  train = function(...) stop("Cannot train an immutable range", call. = FALSE),
  reset = function(...) stop("Cannot reset an immutable range", call. = FALSE),
  set_range = function(...) stop("Cannot set range on an immutable range", call. = FALSE)
)

continuous_range <- function(range = NULL) {
  ggproto(NULL, RangeContinuous, range = range)
}

discrete_range <- function(range = NULL) {
  ggproto(NULL, RangeDiscrete, range = range)
}

immutable_range <- function(range = NULL) {
  ggproto(NULL, RangeImmutable, range = range)
}

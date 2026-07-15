# Implementation from: https://github.com/r-lib/rlang/blob/master/R/compat-purrr.R

#' @export
#' @rdname map
pmap <- function(.l, .f, ...) {
  .f <- as_function(.f)
  args <- .args_recycle(.l)
  out <- do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
  if (obj_is_list(args)) {
    if (is_named(args[[1]])) {
      names(out) <- names(args[[1]])
    }
  }
  out
}

#' @export
#' @rdname map
pmap_lgl <- function(.l, .f, ...) {
  list_simplify(pmap(.l, .f, ...), logical())
}

#' @export
#' @rdname map
pmap_int <- function(.l, .f, ...) {
  list_simplify(pmap(.l, .f, ...), integer())
}

#' @export
#' @rdname map
pmap_dbl <- function(.l, .f, ...) {
  list_simplify(pmap(.l, .f, ...), double())
}

#' @export
#' @rdname map
pmap_chr <- function(.l, .f, ...) {
  list_simplify(pmap(.l, .f, ...), character())
}

#' @export
#' @rdname map
pmap_dfc <- function(.l, .f, ...) {
  result_list <- pmap(.l, .f, ...)
  bind_cols(result_list)
}

#' @export
#' @rdname map
pmap_dfr <- function(.l, .f, ..., .id = NULL) {
  result_list <- pmap(.l, .f, ...)
  bind_rows(result_list, .id = .id)
}

#' @export
#' @rdname map
pmap_df <- pmap_dfr

#' @export
#' @rdname map
pmap_vec <- function(.l, .f, ..., .ptype = NULL) {
  list_simplify(pmap(.l, .f, ...), .ptype)
}

.args_recycle <- function(args) {
  args <- as.list(args)
  vec_recycle_common(!!!args, .arg = ".l", .call = caller_env())
}

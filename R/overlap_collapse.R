#' Collapsing overlapped ranges
#' @description A function to collapse different ranges
#' @importFrom data.table :=
#' @export
overlap_collapse <- function(x,...)
{
  UseMethod('overlap_collapse')
}

#' @rdname overlap_collapse
#' @param .data a data.frame, tbl, or grouped_df. If grouped, collapsing will be performed by group.
#' @param range_cols unquoted column names that specify the beginning and stopping of the range, wrapped in dplyr::vars()
#' @param method a string, either:
#'
#' - outer: the collapsed range is the widest one
#'
#' - inner: the collapsed range is the closest one
#'
#' - left: the collapsed range is skew to the left
#'
#' - right: the collapsed range is skew to the right
#'
#' @return For data.frame method: a tbl or a grouped_df
#' @export
overlap_collapse.data.frame <- function(.data, range_cols,  method = c('outer', 'inner', 'left', 'right'))
{
  # browser()
  original_class <- class(.data)
  group_names <- dplyr::group_vars(.data)
  range_cols <- lapply(range_cols, rlang::quo_get_expr)
  range_names <- unlist(lapply(range_cols, rlang::quo_name))
  .data <- data.table::as.data.table(.data)
  # browser()
  expr <-
    rlang::quo_get_expr(
      rlang::quo(
        .data[,
              overlap_collapse(!!range_cols[[1]], !!range_cols[[2]], method = !!{{method}}),
              by = group_names])
    )
  .data <- rlang::eval_tidy(expr)

  names(.data)[names(.data) %in% c('starts', 'ends')] <- range_names
  if (length(group_names)) return(.data %>% group_by_at(vars(!!!{{group_names}})))
  .data <- as(.data, original_class)
  .data
}

#' @rdname overlap_collapse
#' @param start,end start and end of a period
#' @return For Date/POSIXct/POSIXlt method: a vector of the same class
#' @export
overlap_collapse.Date <- function(start, end, method = c('outer', 'inner', 'left', 'right')){
  ._overlap_collapse_DateTime(start, end, method = method)
}

#' @rdname overlap_collapse
#' @export
overlap_collapse.POSIXt <- function(start, end, method = c('outer', 'inner', 'left', 'right')){
  ._overlap_collapse_DateTime(start, end, method = method)
}

._overlap_collapse_DateTime <- function(start, end, method = c('outer', 'inner', 'left', 'right')){
  start <- unlist(start)
  end <- unlist(end)
  if (any(sapply(seq_along(start), function(i) start[i] > end[i])))
    stop('start must lower or equal to end')

  attrib.start <- attributes(start)
  attrib.end <- attributes(end)

  collapsed <- overlap_collapse.numeric(as.numeric(start), as.numeric(end), method = method)
  new_start <- collapsed$starts
  new_end <- collapsed$ends
  attributes(new_start) <- attrib.start
  attributes(new_end) <- attrib.end
  list(starts = new_start, ends = new_end)
  # names(range) <- c(deparse(substitute(x)), deparse(substitute(y)))
}


#' @rdname overlap_collapse
#' @param x,y a numeric vector specifying the two borders of each range
#' @return For numeric method: a numeric vector
#' @export
overlap_collapse.numeric <- function(x, y, method = c('outer', 'inner', 'left', 'right')){
  x.name <- deparse(substitute(x))
  y.name <- deparse(substitute(y))
  if (length(x) != length(y))
    stop(deparse(substitute(x)), 'and', deparse(substitute(y)), ' must be off the same length.')
  if (!is.numeric(y)) stop(deparse(substitute(y)), ' must be numeric.')
  x <- unlist(x)
  y <- unlist(y)
  rebuilt <- ._rebuild_start_end(x, y)
  start <- rebuilt$x
  end <- rebuilt$y
  end.sort_map <- data.frame(from = start, to = end)
  start <- sort(start)
  end <- C306::simple_sort(end, order = C306::simple_relevel(end, by = start, map = end.sort_map, value = 'level'))
  is.overlaps <- function(x, y){
    if (is.atomic(x)) x <- list(x)
    sapply(x, function(.x) max(.x) >= min(y) & min(.x) <= max(y))
  }

  method <- match.arg(method)
  if (method == 'outer') {
    ranges <- list(starts = start[1], ends = end[1])
    k <- 1
    for (i in seq_along(end)[-1]){
      # browser()
      range_i <- c(start[i], end[i])
      range_k <- c(ranges$start[k], ranges$ends[k])
      if (is.overlaps(range_i, range_k)){
        new_range <- ._range_outer(c(range_k[1], range_i[1]), c(range_k[2], range_i[2]))
        ranges$starts[k] <- new_range[[1]]
        ranges$ends[k] <- new_range[[2]]
      } else {
        ranges$starts <- c(ranges$starts, start[[i]])
        ranges$ends <- c(ranges$ends, end[[i]])
        k <- k + 1
      }
    }
    # names(ranges) <- c(x.name, y.name)
    return(ranges)
  }
  #else:
  method_fn <- switch(method, inner =._range_inner, left = ._range_left, right = ._range_right)
  ranges_outer <- overlap_collapse.numeric(start, end, method = 'outer')
  n_groups <- n_ranges(ranges_outer)
  ranges_outer <- transpose(ranges_outer)

  # browser()
  range_group <- sapply(seq_along(x),
                        function(i){
                          which(is.overlaps(ranges_outer, c(x[i], y[i])))
                        })

  ranges <- sapply(seq_len(n_groups), function(g){
    .start <- start[range_group == g]
    .end <- end[range_group == g]
    out <- method_fn(.start, .end)
    names(out) <- c('starts', 'ends')
    out
  })

  ranges <- list(starts = unname(ranges['starts',]), ends = unname(ranges['ends',]))
  # names(ranges) <- c(x.name, y.name)
  ranges
}

._rebuild_start_end <- function(x, y){
  x_y <- rbind(x,y)
  out_mat <-
    sapply(seq_len(ncol(x_y)), function(i){
      x.y <- x_y[,i]
      return(c(x = min(x.y),y = max(x.y)))
    })
  list(x = out_mat['x',], y = out_mat['y', ])
}

#' @export
overlap_collapse.default <- function(x, y, method = c('outer', 'inner', 'left', 'right')){
  range <- overlap_collapse.numeric(as.numeric(x), as.numeric(y), method = method)
  range$starts <- as(range$starts, typeof(start))
  range$ends <- as(range$ends, typeof(end))
  attributes(range$starts) <- attributes(start)
  attributes(range$ends) <- attributes(end)
  # names(range) <- c(deparse(substitute(x)), deparse(substitute(y)))
  range
}

._range_inner <- function(x, y){
  x <- sort(x)
  y <- sort(y)
  if (max(x) > min(y)) return(c(as(NA, class(max(x))), as(NA, class(min(y)))))
  c(max(x), min(y))
}

._range_outer <- function(x, y){
  z <- sort(c(x, y))
  c(min(z), max(z))
}

._range_left <- function(x, y){
  x <- sort(x)
  y <- sort(y)
  c(min(x), min(y))
}

._range_right <- function(x, y){
  x <- sort(x)
  y <- sort(y)
  c(max(x), max(y))
}

n_ranges <- function(ranges) {
  if (length(ranges[[1]]) != length(ranges[[2]])) stop('Not a formal range')
  return(length(ranges[[1]]))
}


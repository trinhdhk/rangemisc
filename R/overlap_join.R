load#' Overlap join of 2 data frames
#' @description This function is a wrapper of data.table's foverlaps, following the dplyr's _join grammar.
#' @param x,y data.frames
#' @param by,by.x,by.y
#' a character vector of variables to join by, passed to \link[data.table]{foverlaps}' by.x ad by.y.
#'
#' If NULL, the default,  will do a natural join, using all variables with common names across the two tables.
#' A message lists the variables so that you can check they're right
#' (to suppress the message, simply explicitly list the variables that you want to join).
#'
#' Otherwise, the last two columns must be the start and end time of the event specified.
#'
#' @param type character-typed value, either 'left' (default), right, or 'inner',
#' representing left_join, right_join, and inner_join
#' @param overlap character-typed value, passed to \link[data.table]{foverlaps}' type
#'
#' Allowed values are 'any' (default), 'within', 'start', 'end' and 'equal'.
#'
#' The types shown here are identical in functionality
#' to the function findOverlaps in the bioconductor package IRanges.
#'
#' Let [a,b] and [c,d] be intervals in x and y with a<=b and c<=d.
#' For type="start", the intervals overlap iff a == c.
#' For type="end", the intervals overlap iff b == d.
#' For type="within", the intervals overlap iff a>=c and b<=d.
#' For type="equal", the intervals overlap iff a==c and b==d.
#' For type="any", as long as c<=b and d>=a, they overlap.
#' In addition to these requirements, they also have to satisfy the min_overlap argument.
#'
#' @param multiple_match a character-typed value, passed to \link[data.table]{foverlaps}' mult
#'
#' When multiple rows in y match to the row in x,
#' multiple_match controls which values are returned - "all" (default), "first" or "last".
#'
#' @param max_gap a character-typed value, passed to \link[data.table]{foverlaps}' maxgap.
#' It should be a non-negative integer value, >= 0. Default is 0 (no gap). For intervals [a,b] and [c,d],
#' where a<=b and c<=d, when c > b or d < a, the two intervals don't overlap.
#' If the gap between these two intervals is <= maxgap, these two intervals are considered as overlapping.
#'
#' @param min_overlap a character-typed value, passed to \link[data.table]{foverlaps}' minoverlap
#'
#' It should be a positive integer value, > 0. Default is 1. For intervals [a,b] and [c,d],
#' where a<=b and c<=d, when c<=b and d>=a, the two intervals overlap.
#' If the length of overlap between these two intervals is >= minoverlap,
#' then these two intervals are considered to be overlapping.
#'
#' @return a tibble with relevant columns from x, y
#' @importFrom dplyr %>%
#' @seealso \link[data.table]{foverlaps}
#' @export
overlap_join <- function(x, y, by = NULL,
                         type = c('left', 'right', 'inner', 'full'),
                         overlap = c('any', 'within', 'start', 'end', 'equal'),
                         multiple_match = c('all', 'first', 'last'),
                         max_gap = 0L, min_overlap = 1L)
{
  type <- match.arg(type)
  overlap <- match.arg(overlap)
  mutl <- match.arg(multiple_match)
  if (type == 'full'){
    # browser()
    left_ <- overlap_join(x, y, by = by, type = 'left', overlap = overlap, multiple_match = multiple_match,
                          max_gap = max_gap, min_overlap = min_overlap)
    right_ <- overlap_join(x, y, by = by, type = 'right', overlap = overlap, multiple_match = multiple_match,
                           max_gap = max_gap, min_overlap = min_overlap)
    full_ <- dplyr::arrange_at(unique(dplyr::bind_rows(left_, right_)), dplyr::vars(!!!by))
    return(full_)
  }
  arg.x <- switch(type, left = x, y)
  arg.y <- switch(type, left = y, x)

  by.x <- ifelse(names(by) == '', by, names(by))
  by.y <- unname(by)

  arg.by.x <- switch(type, left = by.x, by.y)
  arg.by.y <- switch(type, left = by.y, by.x)
  nomatch <- switch(type, inner = NULL, NA)

  arg.x <- data.table::as.data.table(arg.x)
  arg.y <- data.table::as.data.table(arg.y)
  data.table::setkeyv(arg.x, arg.by.x)
  data.table::setkeyv(arg.y, arg.by.y)

  # browser()
  merged_dt <- data.table::foverlaps(x = arg.x, y = arg.y, by.x = arg.by.x, by.y = arg.by.y,
                                     maxgap = max_gap, minoverlap = min_overlap, nomatch = nomatch,
                                     which = FALSE, verbose = FALSE)
  merged_dt <- dplyr::select(merged_dt, !!!union(names(x), names(y)))
  as.data.frame(merged_dt)
}

#' Summarise events overlapping of 2 event data frames.
#' @description A function to summarise the overlapping status of 2 event data frames
#' @param x,y data.frames.
#' @param ids the id col(s) wrapped in dplyr::vars() distinguishing different events.
#' Based on this is the summarisation taken.
#' If missing, except for the last 2, every columns will be automatically included into ids.
#' @param cols,cols.x,cols.y variables wrapped in dplyr::vars(), passed to overlap_join.
#' @param sstable logical value specifying whether to return in sstable format. Default is FALSE.
#' @param flextable logical value specifying whether to build flextable object. Default it FALSE. Set to TRUE forces sstable to TRUE.
#' @return A summary table that has at least 6 columns.
#'
#' - id column(s): identification of each event.
#'
#' - X: total time when X happens, regardless Y
#'
#' - Y: total time when Y happens, regardless X
#'
#' - X_Y: time duration when X and Y happen simultaneously.
#'
#' - X_notY: time duration when only X happens
#'
#' - notX_Y: time duration when only Y happens
#' @importFrom dplyr %>%
#' @seealso \link[dplyr]{vars}, \link[data.table]{foverlaps}, \link{overlap_join}
#' @export
overlap_summary <- function(x, y, by, sstable = FALSE, flextable = FALSE)
{
  # browser()
  include.x <- ifelse(names(by) == '', by, names(by))
  include.y <- unname(by)
  summary_vars.x <- intersect(include.x[1:(length(by) - 2)], names(x))
  summary_vars.y <- intersect(include.y[1:(length(by) - 2)], names(y))
  summary_vars <- union(summary_vars.x, summary_vars.y)

  joining_by <- by[!by %in% union(setdiff(summary_vars.x, summary_vars.y), setdiff(summary_vars.y, summary_vars.x))]
  by.x <- ifelse(names(joining_by) == '', joining_by, names(joining_by))
  by.y <- unname(joining_by)

  time_start.x <- by.x[length(by.x)-1]
  time_end.x <- by.x[length(by.x)]
  time_start.y <- by.y[length(by.y)-1]
  time_end.y <- by.y[length(by.y)]

  # browser()
  if (any(!by.x %in% names(x))) stop('Column name ', paste(by.x[!by.x %in% names(x)], collapse = ', '),
                                          ' not exist in ', deparse(substitute(x)), sep = '')
  if (any(!by.y %in% names(y))) stop('Column name ', paste(by.y[!by.y %in% names(y)], collapse = ', '),
                                          ' not exist in ', deparse(substitute(y)), sep = '')

  # browser()

  if (any(!summary_vars.x %in% names(y)))
    ._summary_vars.warning(summary_vars.x[!summary_vars.x %in% names(y)], deparse(substitute(y)))
  if (any(!summary_vars.y %in% names(x)))
    ._summary_vars.warning(summary_vars.y[!summary_vars.y %in% names(x)], deparse(substitute(x)))

  x_collapsed <- x %>%
    dplyr::group_by_at(dplyr::vars(!!!{{summary_vars.x}})) %>%
    overlap_collapse(range_cols = vars(!!rlang::sym(time_start.x), !!rlang::sym(time_end.x))) %>%
    ungroup()
  y_collapsed <- y %>%
    dplyr::group_by_at(dplyr::vars(!!!{{summary_vars.y}})) %>%
    overlap_collapse(range_cols = vars(!!rlang::sym(time_start.y), !!rlang::sym(time_end.y))) %>%
    ungroup()

  join_tbl <-
    overlap_join(x_collapsed, y_collapsed, by = joining_by, type = 'full', overlap = 'any', multiple_match = 'all') %>%
    dplyr::mutate(
      .x_y_start = pmax(.[[time_start.x]], .[[time_start.y]]),
      .x_y_end = pmin(.[[time_end.x]], .[[time_end.y]]),
      .x_y_dur = .x_y_end - .x_y_start,
      .x_dur = .[[time_end.x]] - .[[time_start.x]],
      .y_dur = .[[time_end.y]] - .[[time_start.y]],
      .x_y_dur = ifelse(is.na(.x_y_dur), 0, .x_y_dur)
    ) %>%
    dplyr::mutate_at(dplyr::vars('.x_y_dur', '.x_dur', '.y_dur', '.x_y_dur'), tidyr::replace_na, 0)

  x_tbl <-
    join_tbl %>%
    dplyr::select({{include.x}}, .x_dur) %>%
    dplyr::distinct() %>%
    dplyr::group_by_at(dplyr::vars(!!!{{summary_vars}})) %>%
    dplyr::summarise(X = sum(.x_dur))%>%
    dplyr::ungroup()

  y_tbl <-
    join_tbl %>%
    dplyr::select({{include.y}}, .y_dur) %>%
    dplyr::distinct() %>%
    dplyr::group_by_at(dplyr::vars(!!!{{summary_vars}})) %>%
    dplyr::summarise(Y = sum(.y_dur)) %>%
    dplyr::ungroup()

  x_y_tbl <-
    join_tbl %>%
    dplyr::group_by_at(dplyr::vars(!!!{{summary_vars}})) %>%
    dplyr::summarise(X_Y = sum(.x_y_dur))%>%
    dplyr::ungroup()

  summary_tbl <-
    x_tbl %>%
    dplyr::left_join(y_tbl, by = summary_vars) %>%
    dplyr::left_join(x_y_tbl, by = summary_vars) %>%
    dplyr::mutate(X_notY = X - X_Y, notX_Y = Y - X_Y)

  # browser()
  # not_is.na <- function(x) !is.na(x)
  # summary_tbl <-
  #   join_tbl %>%
  #   dplyr::mutate(
  #     .x_y_start = pmax(join_tbl[[time_start.x]], join_tbl[[time_start.y]]),
  #     .x_y_end = pmin(join_tbl[[time_end.x]], join_tbl[[time_end.y]]),
  #     .x_y_dur = .x_y_end - .x_y_start,
  #     .x_dur = join_tbl[[time_end.x]] - join_tbl[[time_start.x]],
  #     .y_dur = join_tbl[[time_end.y]] - join_tbl[[time_start.y]],
  #     .x_y_dur = ifelse(is.na(.x_y_dur), 0, .x_y_dur)
  #   ) %>%
  #   dplyr::group_by_at(dplyr::vars(!!!{{summary_vars}})) %>%
  #   dplyr::summarise(X_Y = sum(.x_y_dur),
  #                    X = list(unique(na.omit(.x_dur))), Y = list(unique(na.omit(.y_dur))),
  #                    X_notY = lapply(X, `-`, X_Y), notX_Y = lapply(Y, `-`, X_Y)) %>%
  #   dplyr::mutate_at(dplyr::vars('X', 'Y', 'X_Y', 'X_notY', 'notX_Y'), tidyr::replace_na, 0) %>%
  #   dplyr::ungroup()

  class(summary_tbl) <- c('overlap_summary', class(summary_tbl))
  attr(summary_tbl, 'footer') <-
    c('X: total time when X happens, regardless Y',
      'Y: total time when Y happens, regardless X',
      'X_Y: time duration when both X and Y happen',
      'X_notY: time duration when only X happens',
      'notX_Y: time duration when only Y happens')
  if (sstable) return(as_sstable.overlap_summary(summary_tbl, flextable = flextable))
  summary_tbl
}

._summary_vars.warning <- function(vars, tab_name){
  for (v in vars){
    message('- "', v, '" not exist in ', tab_name, '. Some values can be missing')
  }
}

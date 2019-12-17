#' Convert from absolute date/time to relative numeric number
#'
#' @description A function to convert calendar date/time to relative day/time(s). Useful in survival analyses
#' @param ... variables of type Date/POSIXt
#' @param zero A value specifying the date or time zero
#' @param shift If zero is not date/time 0, but is forward/backward-shifted. Changing the shift function can correct that.
#' @return Vectors of type double. Number of outputted vectors is equal to number of inputted ones
#' @export
to_relative <- function(..., zero = min(...), shift = 0)
  UseMethod('to_relative')

#' @rdname to_relative
#' @export
to_relative.Date <- function(..., zero = min(...), shift = 0)
{
  .dates <- list(...)
  sapply(unlist(.dates), assertthat::is.date)


  converted <- sapply(.dates, ._convert_to_rl, zero = zero, USE.NAMES = TRUE, simplify = FALSE)
  if (shift != 0) converted <- sapply(converted, `+`, 1, USE.NAMES = TRUE, simplify = FALSE)

  return(converted)
}

#' @rdname to_relative
#' @export
to_relative.POSIXt <- function(..., zero = min(...), shift = 0)
{
  .times <- list(...)
  sapply(unlist(.times), function(x)
    assertthat::assert_that(is.POSIXt(x), msg = paste0(deparse(substitute(x)), " is not a POSIXt object")))


  converted <- sapply(.times, ._convert_to_rl, zero = zero, USE.NAMES = TRUE, simplify = FALSE)
  if (shift != 0) converted <- sapply(converted, `+`, 1, USE.NAMES = TRUE, simplify = FALSE)

  return(converted)
}

._convert_to_rl <- function(obj, zero){
  sapply(obj, `-`, zero)
}


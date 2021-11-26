#' Split a Second
#'
#' @param x A character vector
#' @param split what to split by
#'
#' @return A character vector
#' @export
#'
#' @examples
#'  x <- "alfa,bravo,charlie,delta"
#'  strsplit(x, split = ",")

strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}

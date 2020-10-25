#' extract_prefix
#'
#' Extract and return the word prefix based on a string separator
#'
#' extract_prefix split a word an returns the word prefix based on a string.
#'
#' @param word string, the word from which the prefix is to be extracted.
#' @param separator string, the separator between the prefix and the word.
#'
#' @importFrom stringr str_split
#'
#' @return returns the word prefix.
#' @export
#'
#' @examples
#' str <- "abc"
#' extract_prefix(str, "b")
#'
#' str <- "B_column"
#' extract_prefix(str, "_")
extract_prefix <- function(word, separator) {
  unlist(str_split(word, separator))[1]
}

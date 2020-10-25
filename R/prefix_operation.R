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
#' prefix <- extract_prefix(str, "_")
extract_prefix <- function(word, separator) {
  unlist(str_split(word, separator))[1]
}


#' prefix_vector
#'
#' Extracts and returns a vector of column prefixes of a dataframe according to a separator
#'
#' prefix_vector returns a vector of column prefixes of a dataframe according to a separator
#'
#' @param separator string. The prefix separator.
#' @param dataframe data.frame. The dataframe from which the prefix vector is to be extracted.
#'
#' @return returns a vector of column prefixes.
#' @export
#'
#' @examples
#' A_column <- c(1, 2, 3)
#' B_column <- c(4, 5, 6)
#' C_column <- c(7, 8, 9)
#' df <- data.frame(A_column, B_column, C_column)
#'
#' pref_vec <- prefix_vector(df, "_")
prefix_vector <- function(dataframe, separator) {
  unique(sapply(names(dataframe), function(column_name) extract_prefix(column_name, separator)))
}

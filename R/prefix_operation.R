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


#' substract_prefix
#'
#' Subtracts the prefix of the word entered as a parameter
#'
#' subtracts the prefix of the word entered as a parameter and returns the same word without prefix.
#'
#' @param word string. The word from which the prefix must be removed.
#' @param prefix string. The prefix to be removed from the word.
#'
#' @return returns the word without the prefix.
#' @export
#'
#' @examples
#' str <- "abcd"
#' word <- substract_prefix(str, "ab")
#'
#' column_name <- "R_column"
#' word <- substract_prefix(column_name, "R_")
substract_prefix <- function(word, prefix) {
  substr(word, nchar(prefix) + 1, nchar(word))
}


#' substract_prefix_from_all_column
#'
#' Subtracts the indicated prefix from all the columns of the dataframe.
#'
#' substract_prefix_from_all_column subtracts the specified prefix from all the
#' columns of the dataframe and returns a new dataframe.
#'
#' @param dataframe data.frame. The dataframe from which the column prefixes must be removed.
#' @param prefix string. The prefix to subtract from column names.
#'
#' @return returns a new dataframe identical but without prefix to the column names.
#' @export
#'
#' @examples
#' A_Column1 <- c(1, 2)
#' A_Column2 <- c(3, 4)
#' A_Column3 <- c(5, 6)
#' df <- data.frame(A_Column1, A_Column2, A_Column3)
#'
#' newdf <- substract_prefix_from_all_column(df, "A_")
substract_prefix_from_all_column <- function(dataframe, prefix) {
  temp_dataframe <- dataframe
  names(temp_dataframe) <- sapply(
    X = names(temp_dataframe),
    FUN = function(column_name) substract_prefix(column_name, prefix))
  temp_dataframe
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

#' separate_data
#'
#' Splits a dataframe in half and returns a new dataframe whose column names
#' match a certain naming pattern.
#'
#' separate_data splits a dataframe  in half according to the names of the columns.
#' This function returns a new dataframe whose column names match a certain naming pattern.
#' Its behaviour can also be reversed. It is based on column selection using the grep function.
#' See documentation grep function for more information about regex.
#'
#' @param dataframe a dataframe.
#' @param column_regex_pattern character string containing a regular expression that represente the column selection.
#' If an empty chain is passed the dataframe will be returned intact.
#' @param inverse logical. The function returns the inverse of the result if this parameter is set to true.
#'
#' @return returns a part of the original dataframe
#' @export
#'
#' @examples
#' A_column <- c(1, 2, 3)
#' B_column <- c(4, 5, 6)
#' C_column <- c(7, 8, 9)
#' df <- data.frame(A_column, B_column, C_column)
#'
#' Bs <- separate_data(df, "^B")
#' notBs <- separate_data(df, "^B", inverse = TRUE)
separate_data <- function(dataframe, column_regex_pattern, inverse=FALSE) {
  if (!inverse)
    dataframe[grep(column_regex_pattern, names(dataframe))]
  else
    dataframe[-grep(column_regex_pattern, names(dataframe))]
}

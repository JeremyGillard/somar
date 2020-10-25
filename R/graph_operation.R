#' node_vector_from_dataframe_unit
#'
#' Allows to retrieve the list of nodes to give to the igraph constructor from a dataframe.
#'
#' node_vector_from_dataframe_unit returns a vector containing in the order
#' of the dataframe columns the name of each of the nodes to be displayed on the graph.
#'
#' @param dataframe data.frame. The dataframe from which the node names must be retrieved
#'
#' @return returns a vector containing the name of all the nodes to be added to the graph.
#' @export
#'
#' @examples
#' SelIndex_SelThumb <- c(1, 2)
#' SelIndex_SelPinky <- c(3, 4)
#'
#' df <- data.frame(SelIndex_SelThumb, SelIndex_SelPinky)
#' nodes.vec <- node_vector_from_dataframe_unit(df)
node_vector_from_dataframe_unit <- function(dataframe) {
  names.vec <- names(dataframe)
  unlist(lapply(names.vec, function(str) { unlist(str_split(str, "_")) }))
}



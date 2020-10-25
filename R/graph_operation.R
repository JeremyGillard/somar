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

#' layout_in_semicircle
#'
#' Graph layout with vertices on a semicircle.
#'
#' Layout in semicircle places vertices on a semicircle, in the order of their vertex ids.
#'
#' @param graph the input graph.
#'
#' @importFrom igraph vcount
#'
#' @return A numeric matrix with two columns, and one row for each vertex.
#' @export
#'
#' @examples
#' \dontrun{
#'     layout_in_semicircle(graph)
#' }
layout_in_semicircle <- function(graph) {
  number_of_vertex <- vcount(graph)
  layout_matrix <- matrix(ncol = 2)
  for (i in 1:number_of_vertex) {
    x <- cos((i * pi / number_of_vertex) - (pi / number_of_vertex) / 2)
    y <- sin((i * pi / number_of_vertex) - (pi / number_of_vertex) / 2)
    if (is.na(layout_matrix[1, 1])) {
      layout_matrix[1,] <- c(x, y)
    } else {
      layout_matrix <- rbind(layout_matrix, c(x, y))
    }
  }
  layout_matrix
}


#' sequence_order_nodes_according_somatotpy
#'
#' Orders the nodes to reflect the real position of the areas in somatotopy.
#'
#' The use of this function is specific to a precise naming convention
#' (see vignette guide) and to the field of somatotopy.
#' sequence_order_nodes_according_somatotpy returns an order sequence of
#' the nodes (name vector entered as a parameter) to reflect the real position
#' of the zones in somatotopy.
#'
#' @param names vector. string. the names vector
#'
#' @importFrom stringr str_length
#'
#' @return returns a position sequence in order to reorder the nodes correctly.
#' @export
#'
#' @examples
#' \dontrun{
#'     sequence_order_nodes_according_somatotpy(columns names)
#' }
sequence_order_nodes_according_somatotpy <- function(names) {
  prefix <- ""
  if (grepl(names[1], pattern="^S")) {
    prefix <- "Sel"
  } else {
    prefix <- "NnSel"
  }
  normal_names_cols <- substr(names, str_length(prefix) + 1, str_length(names))
  f_cols <- factor(normal_names_cols, ordered = TRUE, levels = somar::somatotopy.fac)
  sequence <- order(f_cols)
  if (prefix == "NnSel") {
    for (i in 1:length(sequence)) {
      sequence[i] <- ((length(sequence) * 2) + 1) - sequence[i]
    }
  }
  sequence
}

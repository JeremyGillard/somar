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


#' graph_structure_construction
#'
#' Graph structure construction returns an igraph::graph structure with all
#' the attributes of the dataframe unit
#'
#' graph_structure_construction returns an igraph::graph structure with all
#' the attributes of the dataframe unit
#'
#' @param dataframe_unit data.frame. The dataframe unit built using data
#' separation and prefix functions
#'
#' @importFrom igraph graph set.edge.attribute
#'
#' @return igraph::graph. A graph structure
#' @export
#'
#' @examples
#' \dontrun{
#'     graph_structure_construction(dataframe_unit)
#' }
graph_structure_construction <- function(dataframe_unit) {
  nodes.vec <- node_vector_from_dataframe_unit(dataframe_unit)

  g <- graph(nodes.vec, directed = FALSE)

  for (i in 1:nrow(dataframe_unit)) {
    g <- set.edge.attribute(g, row.names(dataframe_unit)[i], value = as.vector(t(dataframe_unit[i,])))
  }

  g
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

#' nodes_permutation_somatotopy
#'
#' Nodes Permutation Somatotopy basé sur la fonction sequence_order_nodes_according_somatotpy,
#' reordonne les noeds du graph afin qu'il respecte la disposition selon les zones en somatotopie.
#' For this function to work correctly, it is necessary that the columns respect the naming convention
#' as follow "zoneName"_NnSel "zoneName" or Salt "zoneName"_Sel "zoneName"
#' or SelNn "zoneName"_Sel "zoneName" be respected beforehand.
#'
#' nodes_permutation_somatotopy reorders the graph noodes so that it respects
#' the layout according to the somatotopic areas and returns a new graph.
#'
#' @param g igraph::graph. The graph to be reordered.
#'
#' @importFrom igraph as_ids V permute
#'
#' @return igraph::graph. The reordered graph.
#' @export
#'
#' @examples
#' \dontrun{
#'     nodes_permutation_somatotopy(graph)
#' }
nodes_permutation_somatotopy <- function(g) {
  columns_name <- as_ids(V(g))

  Sels <- columns_name[sapply(columns_name, grepl, pattern="^Sel")] # Could be a function
  NnSels <- columns_name[sapply(columns_name, grepl, pattern="^NnSel")]

  sequence <- c(sequence_order_nodes_according_somatotpy(Sels),
                sequence_order_nodes_according_somatotpy(NnSels))

  permute(g, sequence)
}


#' default_edge_color_aes
#'
#' default_edge_color_aes applies a default edge color aesthetics for the graph
#'
#' @param g the graph concerned
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom igraph E E<-
#'
#' @return igraph::graph. The the graph with the ethic.
#' @export
#'
#' @examples
#' \dontrun{
#'     default_edge_color_aes(graph)
#' }
default_edge_color_aes <- function(g) {
  precision_scale <- 1000
  color_scale <- colorRampPalette(c("red4", "red1", "yellow", "lawngreen",  "forestgreen"))
  color_pallet <- color_scale(precision_scale);

  edgeColor <- function(r) {
    color_pallet[abs(signif(r, 3)) * precision_scale]
  }

  E(g)$color <- edgeColor(E(g)$R)
  g
}




#' default_edge_width_aes
#'
#' default_edge_width_aes applies a default edge width aesthetics for the graph
#'
#' @param g the graph concerned
#'
#' @importFrom igraph E E<-
#'
#' @return igraph::graph. The the graph with the ethic.
#' @export
#'
#' @examples
#' \dontrun{
#'     default_edge_width_aes(graph)
#' }
default_edge_width_aes <- function(g) {
  precision_scale <- 1000
  edge_max_width <- 50
  edge_min_width <- 1

  edgeWidth <- function(r, minWidth, maxWidth) {
    range_of_values <- maxWidth - minWidth
    unit <- range_of_values / precision_scale
    minWidth + unit * abs(signif(r, 2)) * precision_scale
  }

  E(g)$width <- edgeWidth(E(g)$R, edge_min_width, edge_max_width)
  g
}


#' default_edge_continuity_aes
#'
#' default_edge_continuity_aes applies a default edge continuity aesthetics for
#' the graph
#'
#' @param g the graph concerned
#'
#' @importFrom igraph E E<-
#'
#' @return igraph::graph. The the graph with the ethic.
#' @export
#'
#' @examples
#' \dontrun{
#'     default_edge@_continuity_aes(graph)
#' }
default_edge_continuity_aes <- function(g) {
  E(g)$lty <- ifelse(E(g)$p > 0.05, "dotted", "solid")
  g
}


#' default_edge_curve_aes
#'
#' default_edge_curve_aes applies a default edge curve aesthetics for
#' the graph
#'
#' @param g the graph concerned
#'
#' @importFrom igraph E E<- ecount
#'
#' @return igraph::graph. The the graph with the ethic.
#' @export
#'
#' @examples
#' \dontrun{
#'     default_edge_curve_aes(graph)
#' }
default_edge_curve_aes <- function(g) {
  E(g)$curved <- seq(0, 0.7, length = ecount(g))
  g
}


#' default_selNnSel_vertex_aes
#'
#' default_selNnSel_vertex_aes applies a default Sel and NnSel aesthetics for
#' the vertices of the graph
#'
#' @param g the graph concerned
#'
#' @importFrom igraph V V<-
#'
#' @return igraph::graph. The the graph with the ethic.
#' @export
#'
#' @examples
#' \dontrun{
#'     default_selNnSel_vertex_aes(graph)
#' }
default_selNnSel_vertex_aes <- function(g) {
  V(g)$selected <- grepl(as_ids(V(g)), pattern="^S")
  V(g)$label <- ifelse(V(g)$selected, substract_prefix(V(g)$name, "Sel"), substract_prefix(V(g)$name, "NnSel"))
  V(g)$color <- ifelse(V(g)$selected, "gray60", "gray90")
  g
}


#' default_vertex_aes
#'
#' default_vertex_aes applies a default vertex aesthetics for
#' the graph
#'
#' @param g the graph concerned
#'
#' @importFrom igraph V V<- degree
#'
#' @return igraph::graph. The the graph with the ethic.
#' @export
#'
#' @examples
#' \dontrun{
#'     default_vertex_aes(graph)
#' }
default_vertex_aes <- function(g) {
  V(g)$size <- 40
  V(g)$frame.color <- ifelse(degree(g) > 1, "forestgreen", "gray")
  V(g)$frame.width <- 2
  V(g)$label.cex <- 2
  V(g)$label.family=""
  V(g)$label.cex=1.4
  V(g)$label.color="black"
  g
}


#' default_graph_aesthetic
#'
#' default_graph_aesthetic applies a default aesthetics for the graph
#'
#' @param g the graph concerned
#'
#' @return igraph::graph. The the graph with the ethic.
#' @export
#'
#' @examples
#' \dontrun{
#'     default_graph_aesthetic(graph)
#' }
default_graph_aesthetic <- function(g) {
  g <- default_edge_color_aes(g)
  g <- default_edge_width_aes(g)
  g <- default_edge_continuity_aes(g)
  g <- default_edge_curve_aes(g)
  g <- default_selNnSel_vertex_aes(g)
  g <- default_vertex_aes(g)
  g
}

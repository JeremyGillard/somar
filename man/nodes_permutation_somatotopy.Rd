% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_operation.R
\name{nodes_permutation_somatotopy}
\alias{nodes_permutation_somatotopy}
\title{nodes_permutation_somatotopy}
\usage{
nodes_permutation_somatotopy(g)
}
\arguments{
\item{g}{igraph::graph. The graph to be reordered.}
}
\value{
igraph::graph. The reordered graph.
}
\description{
Nodes Permutation Somatotopy basé sur la fonction sequence_order_nodes_according_somatotpy,
reordonne les noeds du graph afin qu'il respecte la disposition selon les zones en somatotopie.
For this function to work correctly, it is necessary that the columns respect the naming convention
as follow "zoneName"_NnSel "zoneName" or Salt "zoneName"_Sel "zoneName"
or SelNn "zoneName"_Sel "zoneName" be respected beforehand.
}
\details{
nodes_permutation_somatotopy reorders the graph noodes so that it respects
the layout according to the somatotopic areas and returns a new graph.
}
\examples{
\dontrun{
    nodes_permutation_somatotopy(graph)
}
}

#' grid_graph_automatic_construction
#'
#' grid_graph_automatic_construction provides a quick overview of each of
#' the graphs that can be represented using the dataframe.
#'
#' Beware the graph ethic depends strongly on the attributes that have been added to your dataframe.
#' So it goes without saying that this part will certainly have to be customized according to your needs.
#' This is why there is a detailed guide for more details.
#' As well as a design thumbnail if you want to know the construction logic behind this package.
#'
#' @param df the dataframe concerned.
#'
#' @importFrom graphics par plot
#' @export
#'
#' @examples
#' df <- somar::experimental_data.df
#'
#' grid_graph_automatic_construction(df)
grid_graph_automatic_construction <- function(df) {
  metaframe <- separate_data(df, "\\_[A-z]+\\_", inverse = TRUE)
  dataframe <- separate_data(df, "\\_[A-z]+\\_")

  params <- prefix_vector(dataframe, "_")
  params_dataframes_list <- list()
  for (param in params) {
    params_dataframes_list[[param]] <- separate_data(dataframe, paste("^", param, sep=""))
  }

  dataframe_units <- list()
  for (n in 1:nrow(metaframe)) {
    dataframe_units[[n]] <- list()
    for (metadata in names(metaframe)) {
      dataframe_units[[n]][[metadata]] <- as.numeric(metaframe[n, metadata])
    }
    final_dataframe_unit <- data.frame()
    for (i in 1:length(params)) {
      temp_dataframe <- params_dataframes_list[[i]][n,]
      row.names(temp_dataframe) <- params[i]
      temp_dataframe <- substract_prefix_from_all_column(temp_dataframe, paste(params[i], "_", sep=""))
      final_dataframe_unit <- rbind(final_dataframe_unit, temp_dataframe)
    }
    dataframe_units[[n]][["data"]] <- final_dataframe_unit
  }

  dim <- ceiling(sqrt(length(dataframe_units)))
  par(mfrow=c(dim, dim))

  for(i in 1:length(dataframe_units)) {
    list_unit <- dataframe_units[[i]]
    tmp_df <- list_unit$data
    g <- graph_structure_construction(tmp_df)
    layout <- layout_in_semicircle(g)
    g <- nodes_permutation_somatotopy(g)
    g <- default_graph_aesthetic(g)
    plot(g, layout = layout, asp = 0.4)
  }
}

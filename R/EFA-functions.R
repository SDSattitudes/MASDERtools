#' Title
#'
#' @param loadings Factor loadings from EFA
#' @param cutoff Minimum loading value used to determine if an item loads on a factor
#' @param scale_names Vector of scale names; optional (must be based on common part of item names)
#' @param factor_names Vector of factor names; optional
#'
#' @return
#' @export
#'
#' @examples
create_links_EFA <- function(loadings, 
                             cutoff = 0.40, 
                             scale_names = NULL, 
                             factor_names = NULL){
  
  links_df <- data.frame(source = character(),
                         target = character(),
                         value = integer())
  
  # create matrix of true/false indicating whether loading is above cutoff
  loadings_tf <- apply(loadings, 2, function(x){x >= cutoff})
  
  if (is.null(scale_names)){
    scale_names <- unique(gsub(pattern = "_[0-9]*",
                               replacement = "",
                               x = rownames(loadings)))
  }
  if (is.null(factor_names)){
    factor_names <- paste("Factor",1:ncol(loadings_tf),sep="")
  }
  
  for (i in 1:length(scale_names)){
    cur_rows_tf <- grepl(scale_names[i], rownames(loadings_tf))
    for (j in 1:ncol(loadings_tf)){
      tmp_df <- data.frame(source = scale_names[i],
                           target = factor_names[j],
                           value = sum(loadings_tf[cur_rows_tf, j]))
      if (tmp_df$value[1] > 0){
        links_df <- rbind(links_df, tmp_df)
      }
    }
  }
  nodes <- unique(data.frame(name=c(as.character(links_df$source), 
                                    as.character(links_df$target))))
  
  links_df$IDsource <- match(links_df$source, nodes$name) - 1 ### necessary step so the graph actually prints
  links_df$IDtarget <- match(links_df$target, nodes$name) - 1 ### necessary step so the graph actually prints
  return(list(links = links_df,
              nodes = nodes))
}

#' Title
#'
#' @param loadings Factor loadings from EFA
#' @param ... Options to be passed to create_links_EFA
#'
#' @return
#' @export
#'
#' @examples
make_sankey_EFA <- function(loadings, ...){
  if ("fa" %in% class(efa_out5_f5)){
    warning("This function expects factor loadings. Guessing that this is an fa object and continuing.")
    loadings <- loadings$loadings
  }
  sank_out <- create_links_EFA(loadings, ...)
  p <- sankeyNetwork(Links = sank_out[[1]], Nodes = sank_out[[2]],
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", 
                     fontSize = 14, nodeWidth = 30, 
                     sinksRight = FALSE) 
  print(p)
}


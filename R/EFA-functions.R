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
                         value = integer(),
                         tooltip = character())
  
  # create matrix of true/false indicating whether loading is above cutoff
  loadings_tf <- apply(loadings, 2, function(x){abs(x) >= cutoff})
  multi_loading <- sum(rowSums(loadings_tf) > 1) > 0
  
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
      tmp_tooltip <- paste(rownames(loadings_tf[cur_rows_tf,])[which(loadings_tf[cur_rows_tf, j] == 1)], collapse = "\n")
      tmp_df <- data.frame(source = scale_names[i],
                           target = factor_names[j],
                           value = sum(loadings_tf[cur_rows_tf, j]),
                           tooltip = tmp_tooltip)
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
              nodes = nodes,
              multi_loading = multi_loading,
              cutoff = cutoff))
}

# based on some stack overflow code
# https://stackoverflow.com/questions/47591824/link-not-node-tooltips-in-networkd3s-forcenetwork-and-htmlwidgets
# https://stackoverflow.com/questions/47215310/r-customized-tooltip-in-networkd3sankeynetwork
#' Title
#'
#' @param loadings Factor loadings from EFA
#' @param custom_html Logical; should custom tooltips, titles, captions, etc. be added to the diagram?
#' @param sankey_title A string indicating the title to be used. Default is NULL to work with next option.
#' @param guess_title Logical; if sankey_title is not specified, should a title be created? Default is "Sankey Diagram (cutoff = VALUE)".
#' @param multi_loading_caption Logical; if any items load on more than one factor, should a note be added to the diagram?
#' @param ... Options to be passed to create_links_EFA
#'
#' @return
#' @export
#'
#' @examples
make_sankey_EFA <- function(loadings, 
                            custom_html = TRUE,
                            sankey_title = NULL,
                            guess_title = TRUE,
                            multi_loading_caption = TRUE,
                            ...){
  if ("fa" %in% class(loadings)){
    warning("This function expects factor loadings. Guessing that this is an fa object and continuing.")
    loadings <- loadings$loadings
  }
  sank_out <- create_links_EFA(loadings, ...)
  p <- networkD3::sankeyNetwork(Links = sank_out[[1]], Nodes = sank_out[[2]],
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", 
                     fontSize = 14, nodeWidth = 30, 
                     sinksRight = FALSE) 
  if (custom_html){
    p$x$links$tooltip <- sank_out$links$tooltip
    p <- htmlwidgets::onRender(p,
                               '
                             function(el, x) {
                               d3.selectAll(".link").select("title foreignObject body pre")
                               .text(function(d) { return d.tooltip; });
                             }
                             '
    )
    p <- htmlwidgets::appendContent(p, htmltools::tags$p("Hover over links to see which items loaded onto each factor."))
    if (guess_title & is.null(sankey_title)){
      sankey_title <- paste("Sankey Diagram (cutoff = ", sank_out[[4]], ")", sep = "")
    }
    if (!is.null(sankey_title)){
      p$sizingPolicy$viewer$fill <- FALSE
      p <- htmlwidgets::prependContent(p, htmltools::tags$h1(sankey_title))
    }
    # in the future, we can set a flag in the create_links_EFA function if an item loads onto more than one factor
    if (multi_loading_caption & sank_out[[3]] > 0){
      p$sizingPolicy$viewer$fill <- FALSE
      p <- htmlwidgets::appendContent(p, htmltools::tags$p("Note: items may load onto more than one factor."))
    }
  }
  #print(p)
  return(p)
}


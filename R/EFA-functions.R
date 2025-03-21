#' Create Links for Sankey Diagram from EFA Loadings
#'
#' @param loadings Factor loadings from EFA
#' @param cutoff Minimum loading value used to determine if an item loads on a factor
#' @param scale_names Vector of scale names; optional (must be based on common part of item names)
#' @param factor_names Vector of factor names; optional
#' @param display_dnl Logical; should items that do not load be added to the diagram? If TRUE, a group called "Did not load" will be included on the diagram. Intended to be inherited from make_sankey_EFA

#'
#' @return
#' @export
#'
#' @examples
create_links_EFA <- function(loadings, 
                             cutoff = 0.40, 
                             scale_names = NULL, 
                             factor_names = NULL,
                             #display_notes = TRUE,
                             display_dnl = FALSE,
                             dnl_string = "Did not load"){
  # Some variables we can toggle later if needed
  adding_dnl_column <- FALSE
  nonloading_items <- FALSE 
  
  links_df <- data.frame(source = character(),
                         target = character(),
                         value = integer(),
                         tooltip = character())
  
  # create matrix of true/false indicating whether loading is above cutoff
  loadings_tf <- apply(loadings, 2, function(x){abs(x) >= cutoff})
  
  # check to see if any items load on more than one factor (cross-loading)
  multi_loading <- sum(rowSums(loadings_tf) > 1) > 0
  
  # check to see if any items did not load (based on user choice about displaying)
  
  items_that_do_not_load <- (rowSums(loadings_tf) == 0)
  
  if (sum(items_that_do_not_load) > 0){ # if there are any...
    nonloading_items <- TRUE
    if (display_dnl){
    # check to see if any did not load
      adding_dnl_column <- TRUE
      loadings_tf <- cbind(loadings_tf, DNL = items_that_do_not_load) # bind the vector to the array 
    }
  }
  
  if (is.null(scale_names)){
    scale_names <- unique(gsub(pattern = "_[0-9]*",
                               replacement = "",
                               x = rownames(loadings)))
  }
  if (is.null(factor_names)){
    factor_names <- paste("Factor", 1:ncol(loadings_tf), sep = "")
    if (adding_dnl_column){ # only if the column was actually cbinded onto the array...
      factor_names[ncol(loadings_tf)] <- dnl_string # ... do we overwrite the last factor name 
    }
  } else { # if the user is supplying factor names...
    if (adding_dnl_column){ # ... and we are adding a DNL column...
      factor_names <- c(factor_names, dnl_string) # ... add the appropriate string to the factor names
    }
  }
  
  # # we create a fictitious column for variables that "load" on the "Do not load" factor
  # if (display_dnl){
  #   loadings_tf <- cbind(loadings_tf, c(rowSums(loadings_tf) == 0))
  #   factor_names <- c(factor_names, "Did not load")
  # }
  
  for (i in 1:length(scale_names)){
    cur_rows_tf <- grepl(scale_names[i], rownames(loadings_tf))
    for (j in 1:ncol(loadings_tf)){
      tmp_tooltip <- paste(rownames(loadings_tf[cur_rows_tf, ])[which(loadings_tf[cur_rows_tf, j] == 1)], collapse = "\n")
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
              nonloading_items = nonloading_items,
              cutoff = cutoff))
}

# based on some stack overflow code
# https://stackoverflow.com/questions/58786387/how-to-not-display-values-in-the-nodes-or-the-links-in-sankeydiagram-using-netwo
# https://stackoverflow.com/questions/47591824/link-not-node-tooltips-in-networkd3s-forcenetwork-and-htmlwidgets
# https://stackoverflow.com/questions/47215310/r-customized-tooltip-in-networkd3sankeynetwork
# This code was essentially 2-9 lines (depending on how you count) illustrating the use of htmlwidgets::onRender to do the tooltips
# While this code is likely not copyrightable, CJ Yetman's contribution on 2019-11-10 would by under the CC-BY-SA-4.0 license, 
# which is one-way compatible with GPLv3. 
#' Make Sankey Diagram from EFA Loadings
#'
#' @param loadings Factor loadings from EFA
#' @param display_dnl Logical; should items that do not load be added to the diagram? If TRUE, a group called "Did not load" will be included on the diagram.
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
                            display_dnl = FALSE,
                            custom_html = TRUE,
                            sankey_title = NULL,
                            guess_title = TRUE,
                            display_text_notes = TRUE,
                            ...){
  if ("fa" %in% class(loadings)){
    warning("This function expects factor loadings. Guessing that this is an fa object and continuing.")
    loadings <- loadings$loadings
  }
  sank_out <- create_links_EFA(loadings, display_dnl = display_dnl, ...)
  # it would be better to reference the named objects from sank_out rather than the indices
  p <- 
    networkD3::sankeyNetwork(
      Links = sank_out$links, 
      Nodes = sank_out$nodes,
      Source = "IDsource",
      Target = "IDtarget",
      Value = "value", 
      NodeID = "name", 
      fontSize = 14, 
      nodeWidth = 30, 
      sinksRight = FALSE
    ) 
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
    if (guess_title & is.null(sankey_title)){
      sankey_title <- paste("Sankey Diagram (cutoff = ", sank_out$cutoff, ")", sep = "")
    }
    if (!is.null(sankey_title)){
      p$sizingPolicy$viewer$fill <- FALSE
      p <- htmlwidgets::prependContent(p, htmltools::tags$h1(sankey_title))
    }
    # in the future, we can set a flag in the create_links_EFA function if an item loads onto more than one factor
    if (display_text_notes){
      p <- htmlwidgets::appendContent(p, htmltools::tags$p("Hover over links to see which items loaded onto each factor."))
      
      if (sank_out$multi_loading == TRUE){
        p$sizingPolicy$viewer$fill <- FALSE
        p <- htmlwidgets::appendContent(p, htmltools::tags$p("Note: some items load onto more than one factor (cross-loading)."))
      }
      if (sank_out$nonloading_items == TRUE){
        p <- htmlwidgets::appendContent(p, htmltools::tags$p("Note: some items did not load onto any factor (DNL)."))
      }
    }

  }
  #print(p)
  return(p)
}


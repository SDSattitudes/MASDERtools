model_string_builder <- function(dat = NULL, 
                                 scale_names = NULL, kperscale = NULL, 
                                 drop_items = NULL, keep_items = NULL,
                                 str_type = c("cfa", "mirt", "bfactor"),
                                 combo_scales = NULL){
  
  # Get the scale names from the stem of the item
  # This assumes that the items are of the form ScaleName_ItemNumber, e.g.,
  #   AcadSC_3, Cost_10, IntEnj_5, Expectancy_4
  # This is the naming convention used by MASDER - it must be followed for 
  # the functions in MASDERtools to work.
  if (is.null(scale_names) & !is.null(dat)){
    scale_names <- unique(gsub(pattern = "_[0-9]*",
                               replacement = "",
                               x = names(dat)))
  }
  else {
    stop("Scale names must be specified or raw data provided.")
  }
  # Identify the number of items per scale
  if (is.null(kperscale) & !is.null(dat)){
    kperscale <- table(gsub(x = names(dat),
                            pattern = "_[0-9]*",
                            replacement = ""))
  }
  else {
    stop("Number of items per scale must be specified or raw data provided.")
  }
  
  # Now we obtain the items to include in the analysis.
  # Best case scenario: the user specifies keep_items and we have a list to use.
  # If they specify drop_items we convert that to a keep_items list.
  
  # If both drop_items and keep_items are used, return an error.
  if (!is.null(drop_items) & !is.null(keep_items)){
    stop("Cannot specify both drop_items and keep_items.")
  }
  # drop_items specified
  else if (!is.null(drop_items) & is.null(keep_items)){
    if (typeof(drop_items) == "list"){
      keep_items <- convert_drop_to_keep_list(scale_names = scale_names, 
                                              kperscale = kperscale,
                                              drop_items = drop_items)
    }
    else {
      stop("Only a list type object is supported for drop_items.")
    }
  }
  # nothing specified - assume user wants to keep all items
  else {
    for (i in 1:length(scale_names)){
      keep_items[[scale_names[i]]] <- c(1:kperscale[[scale_names[i]]])
    }
  }
  
  # We now have the list of items we want to keep. 
  # Now we generate the requested string.
  
  if ("cfa" %in% str_type){
    make_cfa_string()
  }
  if ("mirt" %in% str_type){
    make_mirt_string()
  }
  if ("bfactor" %in% str_type){
    
  }
}

convert_drop_to_keep_list <- function(scale_names, kperscale, drop_items){
  keep_items <- list()
  # Loop through each scale
  for (i in 1:length(scale_names)){
    # If items are to be dropped, the list entry should not be NULL
    if (!is.null(drop_items[[i]])){
      # Create a vector of length k, then just drop the requested items
      keep_items[[scale_names[i]]] <- 
        c(1:kperscale[[scale_names[i]]])[-drop_items[[i]]]
    }
    # If the list entry is NULL, then no items are to be dropped
    else {
      keep_items[[scale_names[i]]] <- c(1:kperscale[[scale_names[i]]])
    }
  }
  return(keep_items)
}

mirt_string_builder <- function(dat, drop_items = NULL, keep_items = NULL){
  
}
#' Create Model Strings for Analysis Functions Based on List of Items
#'
#' @param dat Dataset to be analyzed. If provided, scale_names and kpersecale will be computed from this (assumes data uses internal MASDER naming convention). This is necessary if str_type is "mirt" or "bfactor".
#' @param scale_names A vector of scale names. Determined from dat if provided.
#' @param kperscale A table of the number of items per scale. Determined from dat if provided.
#' @param drop_items A list of vectors of item numbers to drop for each scale; named components are okay. Any scales that will not have any items dropped should be indicated with NULL instead of a vector of item numbers to drop.
#' @param keep_items A list of vectors of item numbers to keep for each scale; named components are okay. This format is preferred to drop_items. If no items to drop or keep are provided, all items in dat will be kept.
#' @param str_type A string indicating what type of analysis the function should build a string for: "cfa" indicates a simple factor structure for use in lavaan, "mirt" indicates a correlated traits model for use with the mirt::mirt.model function, and "bfactor" indicates a bifactor model for use with mirt::bfactor.
#' @param combo_scales A list of additional scales and items. This is needed if scales will be created that do not use the typical naming convention. 
#' @param simplify Logical; if TRUE and str_type is of length 1, an outer list will not be returned, just the value of the component of the list.
#'
#' @return List with named components if simplify = FALSE or str_type has length 2 or more.
#' @export
#'
#' @examples
model_string_builder <- function(dat = NULL, 
                                 scale_names = NULL, kperscale = NULL, 
                                 drop_items = NULL, keep_items = NULL,
                                 str_type = c("cfa", "mirt", "bfactor"),
                                 combo_scales = NULL,
                                 simplify = TRUE){
  
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
  else if (is.null(drop_items) & is.null(keep_items)){
    for (i in 1:length(scale_names)){
      keep_items[[scale_names[i]]] <- c(1:kperscale[[scale_names[i]]])
    }
  }
  # No else needed - the remaining condition is only keep_items is specified
  
  # We now have the list of items we want to keep. 
  # Now we generate the requested string.
  
  output <- list()
  
  # Confirmatory Factor Analysis
  
  if ("cfa" %in% str_type){
    # Make basic string
    cfa_string <- make_cfa_string(scale_names = scale_names,
                                  keep_items = keep_items)
    # Account for custom scale definitions
    if (!is.null(combo_scales)){
      newstr <- ""
      for (i in 1:length(combo_scales)){
        tmpstr <- paste(names(combo_scales)[i], " =~ ",
                        paste(combo_scales[[i]], sep = "", collapse = " + "),
                        "\n", sep = "")
        newstr <- paste(newstr, tmpstr)
      }
      cfa_string <- paste(cfa_string, newstr, sep = "")
    }
    # Prepare object for return
    output[["cfa"]] <- cfa_string
  }
  
  # Multidimensional Item Response Theory (for mirt.model function)
  
  if ("mirt" %in% str_type){
    # Make basic string
    mirt_string_and_dat <- make_mirt_string(scale_names = scale_names,
                                            keep_items = keep_items,
                                            dat = dat)
    # Account for custom scale definitions
    if (!is.null(combo_scales)){
      newstr <- ""
      for (i in 1:length(combo_scales)){
        tmpstr <- paste(names(combo_scales)[i], " = ",
                        paste(combo_scales[[i]], sep = "", collapse = ","),
                        "\n", sep = "")
        newstr <- paste(newstr, tmpstr)
        
        # Now we add back in missing columns needed for combo_scales
        # First check for missing columns
        if (sum(!(combo_scales[[i]] %in% 
                  colnames(mirt_string_and_dat[["mirt_subdat"]]))) > 0){
          mirt_string_and_dat[["mirt_subdat"]] <- 
            cbind(mirt_string_and_dat[["mirt_subdat"]],
                  dat[,colnames(dat) %in% 
                        combo_scales[[i]][!(
                          combo_scales[[i]] %in% 
                            colnames(mirt_string_and_dat[["mirt_subdat"]]))]])
        }
      }
      mirt_string_and_dat[["mirt_string"]] <- 
        paste(mirt_string_and_dat[["mirt_string"]], newstr, sep = "")
    }
    output[["mirt"]] <- mirt_string_and_dat
  }
  
  # Multidimensional Item Response Theory Bi-Factor model (for bfactor function)
  if ("bfactor" %in% str_type){
    bfactor_vec_and_dat <- make_bfactor_vec(scale_names = scale_names,
                                            keep_items = keep_items,
                                            dat = dat)
    # Account for custom scale definitions
    # This probably will NOT work if items included in the new scale definitions
    #   are already included in another scale. That is, if one or more items are
    #   are set to load on more than one factor then the bfactor vector will not
    #   have the length equal to the number of columns in the dataset. This 
    #   might very well be an intentional limit based on the bifactor theory.
    if (!is.null(combo_scales)){
      tmpvec <- c()
      for (i in 1:length(combo_scales)){
        tmpvec <- c(tmpvec, 
                    rep(length(scale_names) + i, length(combo_scales[[i]])))
        
        # Now we add back in missing columns needed for combo_scales
        # First check for missing columns
        # This might be worth moving to its own helper function considering
        #   that it is almost identical to code above.
        if (sum(!(combo_scales[[i]] %in% 
                  colnames(bfactor_vec_and_dat[["bfactor_subdat"]]))) > 0){
          bfactor_vec_and_dat[["bfactor_subdat"]] <- 
            cbind(bfactor_vec_and_dat[["bfactor_subdat"]],
                  dat[,colnames(dat) %in% 
                        combo_scales[[i]][!(
                          combo_scales[[i]] %in% 
                            colnames(
                              bfactor_vec_and_dat[["bfactor_subdat"]]))]])
        }
      }
      bfactor_vec_and_dat[["bfactor_vec"]] <- 
        c(bfactor_vec_and_dat[["bfactor_vec"]], tmpvec)
      if (length(bfactor_vec_and_dat[["bfactor_vec"]]) != 
          ncol(bfactor_vec_and_dat[["bfactor_subdat"]])){
        warning(paste("Mismatch between vector length and number of columns.", 
                      "Check combo_scales specification."))
      }
    }
    output[["bfactor"]] <- bfactor_vec_and_dat
    
  }
  
  if (simplify & length(output) == 1){
    return(output[[1]])
  }
  else{
    return(output)
  }
}


# Makes the vector for using mirt::bfactor (not technically a string)
# As with the other function for mirt, this creates a data subset
make_bfactor_vec <- function(scale_names, keep_items, dat){
  scale_vec <- item_vec <- c()
  for (i in 1:length(scale_names)){
    # Create the vector of scale codes 
    scale_vec <- c(scale_vec, rep(i, length(keep_items[[i]])))
    # Create a vector to use in selecting the data columns
    item_vec <- c(item_vec, 
                  paste(scale_names[i], "_", keep_items[[i]], sep = ""))
  }
  subdat <- dat[,colnames(dat) %in% item_vec] 
  
  return(list(bfactor_vec = scale_vec,
              bfactor_subdat = subdat))
}

# Helper function to build the mirt.model string
# This will also subset the dataset appropriately (keep correct columns)
make_mirt_string <- function(scale_names, keep_items, dat){
  
  outstr <- "\n"
  item_vec <- c()
  for (i in 1:length(scale_names)){
    # Create the string
    tmpstr <- paste(scale_names[i], " = ",
                    paste(scale_names[i], "_", keep_items[[i]], # Item name
                          sep = "", collapse = ","), # Separate items with +
                    "\n", sep = "")
    outstr <- paste(outstr, tmpstr)
    
    # Create a vector to use in selecting the data columns
    item_vec <- c(item_vec, 
                  paste(scale_names[i], "_", keep_items[[i]], sep = ""))
  }
  
  # Now we subset the data (order of columns should not matter)
  subdat <- dat[,colnames(dat) %in% item_vec] 
  
  return(list(mirt_string = outstr,
              mirt_subdat = subdat))
}

# Helper function to build the CFA string for lavaan from keep_items
make_cfa_string <- function(scale_names, keep_items){
  outstr <- "\n"
  for (i in 1:length(scale_names)){
    tmpstr <- paste(scale_names[i], " =~ ",
                    paste(scale_names[i], "_", keep_items[[i]], # Item name
                          sep = "", collapse = " + "), # Separate items with +
                    "\n", sep = "")
    outstr <- paste(outstr, tmpstr)
  }
  return(outstr)
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


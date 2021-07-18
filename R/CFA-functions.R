

#' Title
#'
#' @param drop.items A list of vectors of item numbers to be dropped WITHIN each scale. These are the column numbers within each scale's sub-data.frame, not the overall column numbers.
#' @param dat The raw data so that kperscale and scale.names can be calculated instead of specified. NOT YET IMPLEMENTED.
#'
#' @return
#' @export
#'
#' @examples
cfa_string_builder <- function(dat, drop.items = NULL){

  scale.names <- unique(gsub(pattern = "_[0-9]*",
                             replacement = "",
                             x = names(dat)))
  kperscale <- table(gsub(x = names(dat),
                          pattern = "_[0-9]*",
                          replacement = ""))

  
  outstr <- "\n"
  for (i in 1:length(scale.names)){
    if (typeof(drop.items) == "list"){
      if (!is.null(drop.items[[i]])){
        itemnumbers <- c(1:kperscale[[scale.names[i]]])[-drop.items[[i]]]
      }
      else{
        itemnumbers <- c(1:kperscale[[scale.names[i]]])
      }
    }
    else{
      itemnumbers <- c(1:kperscale[[scale.names[i]]])
    }
    tmpstr <- paste(scale.names[i], " =~ ",
                    paste(scale.names[i],"_",
                          itemnumbers,
                          sep = "", collapse = " + "),
                    "\n", sep = "")
    outstr <- paste(outstr, tmpstr)
  }
  return(outstr)
}

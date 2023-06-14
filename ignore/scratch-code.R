make_tier2_constraints <- function(bfactor_vec){
  mod_str <- paste("\nF = 1-", length(bfactor_vec), "\n", sep = "")
  mod_str <- paste(mod_str, "CONSTRAIN = ", sep = "")
  for (i in 1:length(bfactor_vec)){
    tmpstr <- paste("(", i, ", a1, a", bfactor_vec[i] + 1, ")", sep = "")
    if (i != length(bfactor_vec)){
      tmpstr <- paste(tmpstr, ", ", sep = "")
    }
    if (i < length(bfactor_vec)){
      if (bfactor_vec[i] != bfactor_vec[i+1]){
        tmpstr <- paste(tmpstr, "\n", sep = "")
      }
    }
  }
  mod_str <- 
}

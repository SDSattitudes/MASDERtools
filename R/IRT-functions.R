

#' Runs princals and can compare linear vs. ordinal
#'
#' @param dat Data from items; usually a data.frame. 
#' @param scale.names Vector of scale names; must be based on common part of item names.
#' @param items A vector of column numbers (corresponding to columns in dat) to be plotted. Rarely used - usually leave as NULL.
#' @param method If method = "both" the output is a list containing the first two eigenvalues and the loss function; if it "linear" or "ordinal" the primary output is loadings plots
#' @param verbose Logical for debug purposes.
#' @param plot_both Logical; If method = "both" should the loadings plots be created?
#'
#' @return
#' @export
#'
#' @examples
princaller <- function(dat, 
                       scale.names = NULL, 
                       items = NULL, 
                       method = "both",
                       verbose = FALSE,
                       plot_both = FALSE,
                       return_objs = TRUE){
  # scale.names is a vector of strings to pattern match with column names
  # items is a vector of TRUE or FALSE values to match specific columns (must match the number of columns in dataset)
  
  if (method == "both"){
    out.lin <- out.ord <- matrix(data = NA, nrow = length(scale.names), ncol = 2)
    loss.lin <- loss.ord <- c()
  }
  
  # Advantage of returning these objects is so that one could loop over them and graph outside of this function
  # princaller can be rather slow! No reason to run it twice unnecessarily
  prlin_list <- prord_list <- list()
  
  if (!is.null(scale.names)){
    for (i in 1:length(scale.names)){
      if (verbose){
        print(scale.names[i])
      }
      
      if (method %in% c("both", "linear")){
        knotsexp <- Gifi::knotsGifi(dat, type = "E")
      }
      
      # There's a lot of duplicated code here
      # Might be better to do if (method == "both" | method == "linear") and the like
      if (method == "both"){
        prlin <- Gifi::princals(dat[,grepl(scale.names[i],names(dat))], knots = knotsexp, degrees = 1)
        prlin_list[[scale.names[i]]] <- prlin
        out.lin[i,] <- prlin$evals[1:2] # store eigenvalues for first two components for comparison of methods
        loss.lin[i] <- prlin$f
        prord <- Gifi::princals(dat[,grepl(scale.names[i],names(dat))])
        prord_list[[scale.names[i]]] <- prord
        out.ord[i,] <- prord$evals[1:2] # store eigenvalues for first two components for comparison of methods
        loss.ord[i] <- prord$f
        
        if (plot_both) {
          plot(prord, main = paste("Ordinal Loadings Plot: ", scale.names[i], sep = "")) 
          plot(prlin, main = paste("Linear Loadings Plot: ", scale.names[i], sep = "")) 
        }
      }
      else if (method == "linear"){
        prlin <- Gifi::princals(dat[,grepl(scale.names[i],names(dat))], knots = knotsexp, degrees = 1)
        prlin_list[[scale.names[i]]] <- prlin
        plot(prlin, main = paste("Linear Loadings Plot: ", scale.names[i], sep = "")) 
      }
      else if (method == "ordinal"){
        prord <- Gifi::princals(dat[,grepl(scale.names[i],names(dat))])
        prord_list[[scale.names[i]]] <- prord
        plot(prord, main = paste("Ordinal Loadings Plot: ", scale.names[i], sep = "")) 
      }
    }
  }
  
  if (!is.null(items)){
    if (verbose){
      print(items)
    }
    plot(Gifi::princals(dat[,items]))
  }
  
  if (method == "both"){
    return(list(out.lin=out.lin,
                out.ord=out.ord,
                loss.lin=loss.lin,
                loss.ord=loss.ord,
                prlin_list = prlin_list,
                prord_list = prord_list))
  } else if (method == "linear"){
    return(list(out.lin=out.lin,
                loss.lin=loss.lin,
                prlin_list = prlin_list))
  }
  else if (method == "ordinal"){
    return(list(out.ord=out.ord,
                loss.ord=loss.ord,
                prord_list = prord_list))
  }
  else{
    return(0) # probably not needed anymore 
  }
}

#' Runs the GRM model for each scale using mirt
#'
#' @param dat Data from items; usually a data.frame. 
#' @param scale.names Vector of scale names; must be based on common part of item names.
#' @param drop.items A list of vectors of item numbers to be dropped WITHIN each scale. These are the column numbers within each scale's sub-data.frame, not the overall column numbers.
#' @param verbose Logical for debug purposes.
#'
#' @return
#' @export
#'
#' @examples
grmit <- function(dat, # full dataset
                  scale.names = NULL, # names of scales to be analyzed
                  drop.items = NULL, # column numbers (e.g., 1, 2, 3, 4, 5, 6, 7) to be dropped WITHIN each scale as a list
                  verbose = TRUE){
  if (!is.null(scale.names)){
    out <- list()
    
    for (i in 1:length(scale.names)){
      if (verbose){
        print(scale.names[i])
      }
      
      dat.tmp <- dat[,grepl(scale.names[i],names(dat))]
      if (!is.null(drop.items)){
        if(!is.null(drop.items[[i]])){
          dat.tmp <- dat.tmp[,-drop.items[[i]]]
        }
      }
      out.grm.tmp <- mirt::mirt(dat.tmp, model = 1, itemtype="graded", technical=list(removeEmptyRows=TRUE))
      if (verbose){
        mirt::coef(out.grm.tmp)
        mirt::itemfit(out.grm.tmp, fit_stats = c("S_X2","infit"), na.rm = TRUE)
      }
      
      out.tmp <- list(scale=scale.names[i],
                      drop.items=drop.items,
                      mirt.out=out.grm.tmp,
                      coef=mirt::coef(out.grm.tmp),
                      itemfit=mirt::itemfit(out.grm.tmp, fit_stats = c("S_X2","infit"), na.rm = TRUE),
                      personscores=mirt::fscores(out.grm.tmp))
      out[[i]] <- out.tmp
    }
    return(out) # maybe do this as an if statement based on length of scale.names (1 or not)
  }
}

# CURRENTLY NOT WORKING RIGHT
# The par mfrow in this isn't working
#' Generate the item plots from grmit 
#'
#' @param out A list of objects created by mirt (one object per scale). This function loops over this list.
#' @param guesspar Old attempt at creating a grid array. NOT WORKING.
#' @param layoutcol Integer number of figures to print per row (number of columns in a grid of images). This does not work well; better to use knitr options fig.show = "hold" and out.width = "50%" (with appropriate adjustments, e.g., 3 figures per row is 33%).
#' @param ... Options to be passed to mirt::itemplot
#'
#' @return
#' @export
#'
#' @examples
itemplotter <- function(out, guesspar=FALSE, layoutcol = NULL, ...){
  for (i in 1:length(out)){
    ncur <- out[[i]]$mirt.out@Data$nitems
    tmp_plot <- list()
    for (j in 1:ncur){
      if (guesspar){
        oldpar <- graphics::par()$mfrow
        if (ncur %% 3){
          graphics::par(mfrow=c(ncur/3,3))
        }
        #        else if(ncur %% 4){
        else{
          graphics::par(mfrow=c(ncur/4,4))
        }
      }
      tmp_plot[[j]] <- mirt::itemplot(out[[i]]$mirt.out,
                                      j,
                                      main = paste("Trace Lines for",
                                                   names(out[[i]]$coef[j])),
                                      ...)
      if (is.null(layoutcol)){
        print(tmp_plot[[j]])
      }
    }
    if (!is.null(layoutcol)){
      #layoutrows <- ceiling(out[[i]]$mirt.out@Data$nitems / layoutcol)
      spillover <- out[[i]]$mirt.out@Data$nitems %% layoutcol
      #if (spillover != 0){
      layoutmat <- matrix(c(1:out[[i]]$mirt.out@Data$nitems, 
                            rep(0, layoutcol - spillover)),
                          ncol = layoutcol,
                          byrow = TRUE)
      #}
      #layout(layoutmat)
      print(gridExtra::grid.arrange(grobs = tmp_plot, layout = layoutmat))
    }
  }
  if (guesspar){
    graphics::par(mfrow=oldpar)
  }
}

#' Build a table of item fit statistics.
#'
#' @param out A list of mirt output objects. This function will loop over this list. 
#'
#' @return
#' @export
#'
#' @examples
itemfitbuilder <- function(out){
  fitstats <- out[[1]]$itemfit
  if (length(out) > 1){
    for(i in 2:length(out)){
      fitstats <- rbind(fitstats, out[[i]]$itemfit)
    }
  }
  return(fitstats)
}

#' Compare PCM, GPCM, and GRM models
#'
#' @param dat Data from items; usually a data.frame. 
#' @param scale.names Vector of scale names; must be based on common part of item names.
#' @param drop.items A list of vectors of item numbers to be dropped WITHIN each scale. These are the column numbers within each scale's sub-data.frame, not the overall column numbers.
#' @param pval The p-value to use in the chi-squared test for nest models (comparing PCM and GPCM). Default value 0.01.
#' @param verbose Logical; debug purposes.
#'
#' @return
#' @export
#'
#' @examples
comparer <- function(dat, # full dataset
                     scale.names = NULL, # names of scales to be analyzed
                     drop.items = NULL, # column numbers (e.g., 1, 2, 3, 4, 5, 6, 7) to be dropped within each scale. Only useful if length(scale.names) is 1.
                     pval = 0.01,
                     verbose = TRUE){
  # Need to update this to run all items if scale.names = NULL 
  # Basically just set dat.tmp <- dat and then scale.names to anything of length 1
  if (!is.null(scale.names)){
    out <- list()
    out.comp <- data.frame(LR.p.value=NULL,pcm.aic=NULL, gpcm.aic=NULL,grm.aic=NULL)
    
    for (i in 1:length(scale.names)){
      if (verbose){
        print("====================================")
        print(scale.names[i])
      }
      
      dat.tmp <- dat[,grepl(pattern = scale.names[i], x = colnames(dat))]
      if (!is.null(drop.items)){
        if(!is.null(drop.items[[i]])){
          dat.tmp <- dat.tmp[,-drop.items[[i]]]
        }
      }
      
      #####
      # RSM
      
      # this is the mirt version
      #out.grm.tmp <- mirt(dat[,grepl(scale.names[i],names(dat))],model = 1, itemtype="graded", technical=list(removeEmptyRows=TRUE))
      if (verbose) { print("Fitting GRM") }
      fitgrm <- ltm::grm(dat.tmp)
      
      ##############
      # PCM vs. GPCM
      if (verbose) { print("Fitting PCM") }
      fitpcm <- ltm::gpcm(dat.tmp, constraint = "rasch")
      if (verbose) { print("Fitting GPCM") }
      fitgpcm <- ltm::gpcm(dat.tmp)
      a.out <- stats::anova(fitpcm, fitgpcm)
      out.comp <- rbind(out.comp, data.frame(a.out$p.value,a.out$aic0,a.out$aic1,summary(fitgrm)$AIC))
      rownames(out.comp)[nrow(out.comp)] <- paste(scale.names[i],ifelse(is.null(drop.items),"",paste(" drop ",drop.items,sep="")),sep="")
      
      if (verbose){
        
        prefmodel <- ifelse(a.out$p.value < pval, "GPCM", "PCM")
        nonprefmodel <- ifelse(a.out$p.value < pval, "PCM", "GPCM")
        print(paste(scale.names[i], ": ", prefmodel, " preferred based on LR test (to the ",nonprefmodel,").", sep=""))
        
        print(paste(scale.names[i], ": AIC for PCM = ", a.out$aic0, sep=""))
        print(paste(scale.names[i], ": AIC for GPCM = ", a.out$aic1, sep=""))
        print(paste(scale.names[i], ": AIC for GRM = ", summary(fitgrm)$AIC, sep=""))
        
        aics <- out.comp[nrow(out.comp),c(2:4)]
        whichminaic <- which(aics == min(aics))
        if (whichminaic == 1){ # note that these indices are from WHICH on a 3-tuple, not the columns from the output!
          print("PCM preferred based on AIC")
        }
        else if (whichminaic == 2){
          print("GPCM preferred based on AIC")
        }
        else if (whichminaic == 3){ # in case we add more models
          print("GRM preferred based on AIC")
        }
      }
      out.tmp <- list(scale=scale.names[i],
                      drop.items=drop.items,
                      comps=out.comp,
                      fitpcm=fitpcm,
                      fitgpcm=fitgpcm,
                      fitgrm=fitgrm)
      out[[i]] <- out.tmp
    }
  }
  return(out)
}
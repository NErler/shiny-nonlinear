# set reference category
# x: column of dataset
set.to.ref <- function(x) {
  factor(#rep(
    levels(x)[1], #length(x)),
    levels = levels(x))
}


# get data frame for prediction (for CIs)
# nlin: name of a variable with non-linear effect
# Dat: dataset
# form: model formula
# predx: vector of values of the varying variable (necessary for residuals)
get.DF <- function(nlin, Dat, form, predx = NULL) {

  print("get.DF called!")
  # extract all relevant variable names
  l <- list()
  print(length(l))

  # set numerical variables to their median
  numvars <- sapply(Dat[, all.vars(as.formula(form))], is.numeric)
  print(numvars)

  if (sum(numvars) > 0) {
    l <- c(l, lapply(Dat[, names(numvars)[numvars], drop = F], median, na.rm = T))
  }
  cat("length(l) after numeric: ", length(l), "\n")

  # set categorical variables to their reference category
  if (sum(!numvars) > 0) {
    l <- c(l, lapply(Dat[, names(numvars)[!numvars], drop = F], set.to.ref))
  }

  cat("length(l) after catvars: ", length(l), "\n")


  if (is.null(predx)) {
    l[[nlin]] <- seq(min(Dat[, nlin], na.rm = T),
                     max(Dat[, nlin], na.rm = T), length = 200)
  } else {
    l[[nlin]] <- predx
  }
  DF <- expand.grid(l)
  print(dim(DF))
  return(DF)
}

# function to predict fit & get CIs
# nonlin: vector with names of variables with non-lin. effect
# mod: model
# Dat: Data
# form: model formula
# predx:
# type: "lp" (Cox model) or "link"
get.fitCI <- function(nonlin, mod, Dat, form, predx = NULL, type) {
  L <- as.list(nonlin)
  for (i in 1:length(nonlin)) {
    DF <- get.DF(nonlin[i], Dat, form, predx = predx)
    print(names(DF))
    predCI <- predict(mod, DF, se.fit = T, type = type)
    predCI$lwr <- predCI$fit - 1.96 * predCI$se.fit
    predCI$upr <- predCI$fit + 1.96 * predCI$se.fit
    L[[i]] <- cbind(DF, predCI)
  }
  return(L)
}


#  get label for y-axis
get_ylab <- function(nlin){
    # M <- list("lin" = paste("f(", nlin, ")", sep = ""),
    #           "log" = paste("Pr(", nlin, " = 1)", sep = ""),
    #           "poi" = paste("log(f(", nlin, "))", sep = ""),
    #           "cox" = paste("log(Hazard ratio)", sep = "")
    # )
    return(paste0("f(", nlin, ")"))
}



plotfunc <- function(k, predDF, nonlin, model, Dat, modType,
                     plotResid, plotKnots, quants = NULL, type) {
  print("Start plotting.")

  cat(deparse(formula(model)))

  if (length(nonlin) < 1 | is.null(model)) {
    return("Please select variables to be fitted with splines.")
  } else {
    if (modType == "lin") {
      if (plotResid) {
        resids <- residuals(model) +
          get.fitCI(nonlin, model, Dat, form = formula(model), type = type,
                    predx = Dat[, nonlin[k]])[[k]][,"fit"]

        yrange <- range(resids, predDF[[k]][,"fit"])
      } else {
        yrange <- range(c(predDF[[k]][,c("lwr", "upr")]))
      }
    } else {
      yrange <- range(c(predDF[[k]][,c("lwr", "upr")]))
    }

    par(mfrow = c(1,1), mar = c(4.3, 4, 0.1, 0.1), mgp = c(2, 0.6, 0), bg = "transparent")
    plot(1, type = "n",
         xlab = nonlin[k], ylab = get_ylab(nonlin[k]),
         ylim = yrange, xlim = range(Dat[, nonlin[k]]),
         cex.lab = 1, bg = "transparent")

    if (plotKnots) {
      abline(v = quantile(Dat[, nonlin[k]],
                          c(0.05, c(1:(quants[[k]] - 1))/quants[[k]], 0.95)),
             col = grey(0.5), lty = 2)
    }

    polygon(c(predDF[[k]][ ,nonlin[k]], predDF[[k]][200:1, nonlin[k]]),
            c(predDF[[k]][,"lwr"], predDF[[k]][200:1, "upr"]),
            col = "lightsteelblue1", border = NA)

    if (modType == "lin") {
      if (plotResid) {
        points(Dat[, nonlin[k]],
               resids,
               col = grey(0.7), cex = 0.3)
      }
    }
    AGEblue <- "#004681"
    lines(predDF[[k]][, nonlin[k]], predDF[[k]][,"fit"], lwd = 3, col = AGEblue)
    axis(side = 1, at = Dat[, nonlin[k]], tck = 0.02, labels = F)
    box(which = "plot", bg = "transparent")
  }
}




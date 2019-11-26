library(fda) # fbplot
library(fda.usc) # 
library(rainbow) # library rainbow for fboxplot
library("matrixcalc")
library("ccaPP")
#library("ggplot2", warn.conflicts = FALSE, quietly=TRUE)
#require("grid")
source("funciones_competencia/OutGram.R")
source("funciones_competencia/msplot.R")
source("funciones_competencia/DirOut.R")
source("funciones_competencia/facCal_num.R")
source("funciones_competencia/fom_dos.R")
###Functional Boxplots#####
get_fbplot_outliers <- function(X){
  fbplot(X, plot = FALSE)$outpoint
}

###Outliergram########
get_outliergram_outliers <- function(X){
  fD = roahd::fData(grid = 1:ncol(X), X)
  roahd::outliergram(fD, display = F)$ID_outliers
}


###Functional Highest Region########

# plot.type = "functional"
# type = "bag"
# fboxplot uses the hidden function fbag.
# I create mybag
#myfbag = getAnywhere(fbag)$objs[[1]]

# plot.type = "functional"
# type = "hdr"
# fboxplot uses the hidden function fhdr
# I create myfhdr
#myfhdr = getAnywhere(fhdr)$objs[[1]]

# plot.type = "bivariate"
# type = "bag"
# fboxplot uses the hidden function bbag
# I create mybbag 
#mybbag = getAnywhere(bbag)$objs[[1]]

# plot.type = "bivariate"
# type = "hdr"
# fboxplot uses the hidden function bhdr
# I create mybhdr
mybhdr = getAnywhere(bhdr)$objs[[1]]

myfboxplot = function(data, plot.type = c("functional", "bivariate"),
                      type = c("bag", "hdr"), alpha = c(0.05, 0.5),
                      projmethod = "PCAproj",
                      factor = 1.96, na.rm = TRUE,
                      xlab = data$xname, ylab = data$yname,
                      shadecols = gray((9:1)/10), pointcol = 1, plotlegend = TRUE,
                      legendpos = "topright", ncol = 2, ...){
  projmethod = match.arg(projmethod)
  op <- par(no.readonly = TRUE)
  type <- match.arg(type)
  plot.type <- match.arg(plot.type)
  y = t(data$y)
  if (na.rm) 
    y <- na.omit(y)
  if (plot.type == "functional") {
    if (type == "bag") 
      res = myfbag(data, factor, xlab = xlab, ylab = ylab, plotlegend = plotlegend,
                   legendpos = legendpos, ncol = ncol, projmethod = projmethod,
                   ...)
    else res = myfhdr(data, alpha, xlab = xlab, ylab = ylab, plotlegend = plotlegend,
                      legendpos = legendpos, ncol = ncol, projmethod = projmethod,
                      ...)
  }
  if (plot.type == "bivariate") {
    par(pty = "s")
    sco = PCAproj(t(data$y), center = median)$scores
    if (type == "bag") 
      res = mybbag(data, factor, projmethod = projmethod, ...)
    else res = mybhdr(data, alpha, shadecols = shadecols, pointcol = pointcol,
                      projmethod = projmethod, ...)
    exit.restore <- function() {
      par(op)
    }
    on.exit(exit.restore())
  }
  return(res)
}


get_fhdr_outliers <- function(X){
  argvals <- 1:ncol(X)
  colnames(X) <- argvals
  rownames(X) <- seq(1:nrow(X))
  X_fds <- fds(argvals, t(X))
  as.integer(myfboxplot(X_fds, plot.type = "bivariate", type = "hdr"))
}

###Magnitude Shape Plot######
# X must be n by p
get_msplot_outliers <- function(X){
  msplot(data = X, plot = F, dirout = T)$out.dir
}

###Functional Outlier Map#####
get_fom_outliers <- function(X){
  # Get Data
  DO = DO_functional(data = X, univ=TRUE)
  fdo = fDO(DO)
  vdo = vDO(DO)
  CFO       = log(0.1 + sqrt((fdo/median(fdo))^2 +
                             (vdo/median(vdo))^2))
  fence     = qnorm(0.995)*mad(CFO)+median(CFO)
  return(outliers <- which(CFO > fence))
}



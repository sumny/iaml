#' @import data.table
#' @import checkmate
#' @import lgr
#' @import qs
#' @import iml
#' @import mlr3
#' @import mlr3misc
#' @import mlr3learners
#' @import mlr3pipelines
#' @import mlr3oml
#' @importFrom stats .lm.fit coef predict qlogis weighted.mean
#' @importFrom utils object.size
"_PACKAGE"

.onLoad = function(libname, pkgname) { # nolint
  suppressWarnings(RNGversion("4.0.5"))
  options(warn = 1, mlr3oml.cache = TRUE)
}

utils::globalVariables(c(".", "..feature", "Predictor", "ale", "alev", "factor_to_dataframe",
  "interval", "lrn.regr", "lvl", "n", "setHyperPars", "sparkline", "task", "task.dat", "train", "xv"))


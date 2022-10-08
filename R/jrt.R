#' Fit ordinal IRT models on judgment data and return factor scores and
#' statistics.
#'
#' @description This function automatically selects appropriate polytomous IRT
#'   models based on an information criterion (e.g. Corrected AIC), then returns
#'   factor scores, standard errors and various IRT psychometric information, as
#'   well as more traditionnal ("CTT") psychometric information. All IRT
#'   estimation procedures are executed with the package \code{mirt} (Chalmers,
#'   2012). The non-IRT procedures use packages \code{psych} and \code{irr}.
#'
#' @param data A dataframe or matrix including the judgments to be scored. Note
#'   that so far missing data are not supported. This is the only required
#'   argument for the function.
#' @param irt.model A string value with the name of the model to fit. It can be:
#'   \itemize{
#'   \item{\code{"auto"} (default) or \code{NULL} : Empirically select the model
#'   based on an information criterion (see \code{selection.criterion} argument).
#'  \item{Difference models (more or less constrained versions of the
#'  Graded Response Model)}
#'  \itemize{
#'   \item{\code{"GRM"}: Graded Response Model}
#'   \item{\code{"CGRM"}: Constrained Graded Response Model (equal discriminations)}
#'   \item{\code{"GrRSM"}: Graded Rating Scale Model (same category structures)}
#'   \item{\code{"CGrRSM"}: Constrained Graded Rating Scale Model (same category
#'   structures and equal discriminations)}
#'            }
#'   \item{Divide-by-total models (more or less constrained versions of the
#'  Generalized Partial Credit Model)}
#'   \itemize{
#'   \item{\code{"GPCM"}: Generalized Partial Credit Model}
#'   \item{\code{"PCM"}: Partial Credit Model (equal discriminations)}
#'   \item{\code{"GRSM"}: Generalized Rating Scale Model (same
#'   category structures)}
#'   \item{\code{"RSM"}: Rating Scale Model (same category structures and
#'   equal discriminations)}
#'            }
#'            }}
#'   For convenience, models can also be called by their full names
#'   (e.g. \code{"Generalized Rating Scale Model"}
#'   or \code{"Generalized Rating Scale"} work.)\cr
#'   \itemize{
#'   \item{Note: Models where judges are constrained to same category structures (Graded Rating Scale Model,
#'   Constrained Graded Rating Scale Model, Generalized Rating Scale Model and Rating Scale Model) cannot be fit
#'   if judges have different observed categories. Judges with unobserved categories are automatically removed
#'   if these models are called. If the automatic model selection is used,
#'   these models are ignored in the comparison by default, but this behavior can
#'   be modified to removing judges in the comparison with \code{remove.judges.with.unobserved.categories = T}.}}
#' @param summary A logical to indicate if summary statistics should be
#'   displayed as messages (default is \code{TRUE}).
#' @param selection.criterion A string with the criterion for the automatic
#'   selection. The default is the Akaike Information Criterion
#'   (\code{AIC}), but other criteria may be used (\code{AICc}, \code{BIC} and
#'   \code{SABIC}).
#' @param response.categories A numeric vector to indicate the possible score
#'   values. For example, use \code{1:7} for a Likert-type score from 1 to 7.
#'   The default, \code{auto} automatically detects the possible values based on
#'   the dataset provided.
#' @param remove.judges.with.unobserved.categories A logical value to indicate whether to only keep the
#'   judges with all categories observed (based on the \code{response.categories}
#'   argument). The Rating Scale Model (RSM) and Graded Rating Scale Model
#'   (GRSM) can only be estimated if the same categories are observed for all
#'   judges. If set to \code{TRUE}, "incomplete judges" are removed only to fit
#'   models that require it (RSM and GRSM), and for other models when they are compared to them (to allow
#'   meaningful model comparisons). It defaults to \code{FALSE} to keep all the data available, and has no effect
#'   if models that do not require "complete judges" are called.
#' @param additional.stats A logical to indicate whether to report other
#'   ("non-IRT") reliability statistics (based on computations from packages
#'   `psych` and `irr`). Defaults to \code{FALSE}.
#' @param method.factor.scores A string to indicate the method used to compute
#'   the factor scores. Bayesian methods (\code{EAP}, \code{MAP}) are
#'   recommended. Defaults to Expected A Posteriori (\code{EAP}) based on a Standard
#'   Normal \eqn{N(0,1)} prior distribution. Alternatively, Maximum A Posteriori
#'   (\code{MAP}) with a Standard Normal \eqn{N(0,1)} prior may be used. Maximum
#'   Likelihood (\code{ML}) is also possible (it is equivalent to using a
#'   uniform prior), but it is discouraged as can produce -Inf and +Inf
#'   factor scores (for which standard errors will be missing). Alternatively,
#'   Weighted Likelihood Estimation (\code{WLE}) may be used.
#' @param return.mean.scores A logical to indicate whether to return the mean
#'   scores in the output (defaults to \code{TRUE}).
#' @param prefix.for.outputs A character used as prefix to name the vectors in the
#'   output data frames. Default is \code{"Judgments"}.
#' @param column.names A character to indicate the preferred name to give to a Judge. Defaults to \code{"Judge"}.
#' @param maximum.iterations A numeric indicating the maximum number of
#'   iterations used to fit the model (default is \code{2000}).
#' @param convergence.threshold A numeric to indicate the threshold used to
#'   tolerate convergence (default is \code{.001}). Reduce for increased
#'   precision (but slower or non convergent results).
#' @param estimation.algorithm A string indicating the estimation algorithm. Can
#'   notably be \code{EM} for Bock and Atkin's Expected-Maximization (default)
#'   or \code{MHRM} for the Metropolis-Hastings Robbins-Monro algorithm (usually
#'   slower for unidimensional models).
#' @param method.item.fit A character value to indicate which fit statistic to use
#' for the item fit output. Passed to the \code{itemfit} function of the
#' \code{mirt} package. Can be \code{S_X2}, \code{Zh}, \code{X2}, \code{G2},
#' \code{PV_Q1}, \code{PV_Q1}, \code{X2*}, \code{X2*_df}, \code{infit}. Note that some are not be computable if there are missing data.
#' @param status.verbose A logical to indicate whether to output messages
#'   indicating what the package is doing. Defaults to \code{FALSE}.
#' @param estimation.package.warnings A logical to indicate whether to output
#'   the warnings and messages of the estimation package. Defaults to
#'   \code{FALSE} for a cleaner output, but set to \code{TRUE} if experiencing
#'   issues with the estimation.
#' @param digits A numeric to indicate the number of digits to round output
#'   statistics by (default is \code{3}).
#' @param plots A logical to indicate whether to plot the total information plot
#' and judge category curves (\code{TRUE}, default) or not (\code{FALSE}).
#' @param greyscale A logical to indicate whether the plots should be in greyscale (\code{TRUE}) or color (\code{FALSE}, default).
#' @param progress.bar A logical to indicate whether to show a progress bar
#' during the automatic model selection. Defaults to \code{TRUE}.
#' @param select.variables.that.contain A character string to use as data the
#' variables in the original dataset that contain the string. Based on the
#' \code{select} function of \code{dplyr}. For example, if all your judgment
#' data includes "Rater", use "Rater" to filter your dataset here.
#' @param silent A logical (defaults to \code{FALSE}) to ask no output (no message or plot) but the \code{jrt} object.
#' This uses other parameters (\code{progress.bar}, \code{estimation.package.warnings},
#' \code{plots}, \code{summary}) in order to return a silent output.
#' Useful if only using the package for factor scoring, for example.
#' @param show.calls A logical to report the calls made to fit the different models. This is meant as a didactic options for users who may be interested in switching over to \code{mirt} directly. Defaults to \code{FALSE}.
#' @param debug A logical to report debug messages (used in development).
#' Defaults to \code{FALSE}.
#' @return An object of S4-class \code{jrt}. The factor scores can be accessed
#'   in slot \code{@output.data}.
#'
#' @references Chalmers, R., P. (2012). mirt: A Multidimensional Item Response
#' Theory Package for the R Environment. \emph{Journal of Statistical Software,
#' 48}(6), 1-29. \doi{10.18637/jss.v048.i06}
#' @references Myszkowski, N., & Storme, M. (2019). Judge Response Theory? A call to upgrade our psychometrical account of creativity judgments. \emph{Psychology of Aesthetics, Creativity and the Arts, 13}(2), 167-175. \doi{10.1037/aca0000225}
#' @references Myszkowski, N. (2021). Development of the R library “jrt”: Automated item response theory procedures for judgment data and their application with the consensual assessment techniques. \emph{Psychology of Aesthetics, Creativity and the Arts, 15}(3), 426-438. \doi{10.1037/aca0000287}
#'
#' @import mirt
#' @importClassesFrom mirt SingleGroupClass
#' @import methods
#' @examples
#'
#' # Load dataset
#' data <- jrt::ratings
#'
#' # Fit models
#' fit <- jrt(data,
#'   irt.model = "GRM", # to manually select a model
#'   plots = FALSE) # to remove plots
#'
#' # Extract the factor scores
#' fit@factor.scores # In a dataframe with standard errors
#' fit@factor.scores.vector # As a numeric vector
#'
#' # See vignette for more options
#'
#' @export
jrt <- function(data,
                irt.model = "auto",
                summary = T,
                selection.criterion = "AIC",
                response.categories = "auto",
                remove.judges.with.unobserved.categories = F,
                additional.stats = F,
                method.factor.scores = "EAP",
                return.mean.scores = T,
                prefix.for.outputs = "Judgments",
                column.names = "Judge",
                maximum.iterations = 2000,
                convergence.threshold = .001,
                estimation.algorithm = "EM",
                status.verbose = F,
                estimation.package.warnings = F,
                digits = 3,
                plots = T,
                greyscale = F,
                progress.bar = T,
                method.item.fit = "X2",
                select.variables.that.contain = NULL,
                silent = F,
                show.calls = F,
                debug = F) {



  # Check if data exists
  if (is.data.frame(data) == FALSE && is.matrix(data) == FALSE) {stop(call. = F, paste0("The dataset does not exist in the environment as a matrix or data frame."))}





  #Recode irt model to auto if NULL
  if (is.null(irt.model) == TRUE) { irt.model <- "auto"}

  #Recode irt models to a standard name
  if (irt.model == "grm" | irt.model == "GRM" | irt.model == "Graded Response Model"| irt.model == "GR"
      | irt.model == "graded") { irt.model <- "Graded Response Model"} else {
  if (irt.model == "cgrm" | irt.model == "CGRM" | irt.model == "Constrained Graded Response Model"| irt.model == "Constrained GRM") { irt.model <- "Constrained Graded Response Model"} else {
  if (irt.model == "grrsm" | irt.model == "GrRSM" | irt.model == "Graded Rating Scale Model"| irt.model == "Graded Rating Scale" | irt.model == "Graded RSM") { irt.model <- "Graded Rating Scale Model"} else {
  if (irt.model == "cgrsm" | irt.model == "CGRSM" | irt.model == "CGrRSM" | irt.model == "Constrained Graded Rating Scale Model"| irt.model == "Constrained GRM") { irt.model <- "Constrained Graded Rating Scale Model"} else {
  if (irt.model == "gpcm" | irt.model == "GPCM" | irt.model == "Generalized Partial Credit Model"| irt.model == "Generalized Partial Credit") { irt.model <- "Generalized Partial Credit Model"} else {
  if (irt.model == "pcm" | irt.model == "PCM" | irt.model == "Partial Credit Model"| irt.model == "Partial Credit"
      | irt.model == "PC") { irt.model <- "Partial Credit Model"} else {
  if (irt.model == "grsm" | irt.model == "GRSM" | irt.model == "Generalized Rasch Rating Scale"| irt.model == "Generalized Rating Scale" | irt.model == "Rating Scale Model") { irt.model <- "Generalized Rating Scale Model"} else {
  if (irt.model == "rsm" | irt.model == "RSM" | irt.model == "Rasch Rating Scale"| irt.model == "Rating Scale"
      | irt.model == "Rating Scale Model") { irt.model <- "Rating Scale Model"} else {
  if (irt.model == "NRM" | irt.model == "nominal" | irt.model == "nrm"| irt.model == "Nominal Response Model"
      | irt.model == "Nominal") { irt.model <- "Nominal Response Model"}
      }}}}}}}}


# Defaults not specificed as arguments
  missing.data.message <- TRUE

# Silent mode
  if(silent == T){
    summary <- FALSE
    plots <- FALSE
    status.verbose <- FALSE
    estimation.package.warnings <- FALSE
    additional.stats <- FALSE
    progress.bar <- FALSE
    missing.data.message <- FALSE
  }


  originaldata <- data

  original.data.as.data.frame <- as.data.frame(originaldata)

  data <- original.data.as.data.frame

  ## Create an index variable to merge later
  index.vector <- seq(seq(1, dim(data)[1]))
  original.data.as.data.frame$index.for.jrt <- index.vector



  # Count

  # Select data that contains
  if (is.null(select.variables.that.contain) != TRUE) {
    data <- dplyr::select(data, dplyr::contains(select.variables.that.contain))
  }

  # Count number of judges (number of vectors)
  numberofjudges <- dim(data)[2]



  # Filter out complete missing cases
  vector.of.missing.data.per.case <- rowSums(is.na(data))
  logical.vector.of.complete.missing <- vector.of.missing.data.per.case == numberofjudges
  logical.vector.of.not.complete.missing <- vector.of.missing.data.per.case != numberofjudges
  count.of.complete.missing.cases <- sum(logical.vector.of.complete.missing)




  ##### PART BELOW PROBABLY NOT NECESSARY ! NEEDS TESTING
  ## If there are complete missing, output a message, and create index for merging after analysis
  if (count.of.complete.missing.cases > 0) {
    message(paste(sep = "", count.of.complete.missing.cases, " case(s) with completely missing data detected! They were skipped for analysis." ))
    vector.of.cases.complete.missing <- which(logical.vector.of.complete.missing)
    ## Add index variable to data
    data.with.index <- data
    index.vector <- seq(seq(1, dim(data)[1]))
    data.with.index$index.for.jrt <- index.vector
    data.complete.missing <- data[vector.of.cases.complete.missing,]
    data.not.complete.missing <- data[logical.vector.of.not.complete.missing,]
    data.complete.missing.with.index <- data.with.index[vector.of.cases.complete.missing,]
    data.not.complete.missing.with.index <- data.with.index[logical.vector.of.not.complete.missing,]
    data <- data.not.complete.missing
    }



  # Count sample size (this is the sample size for data that is not complete missing)
  sample.size <- dim(data)[1]

  # If RSM or GRSM is requested directly, then set to remove.judges.with.unobserved.categories = T
  if (irt.model == "Rating Scale Model"| irt.model == "Graded Rating Scale Model" | irt.model == "Generalized Rating Scale Model"| irt.model == "Constrained Graded Rating Scale Model") {
    remove.judges.with.unobserved.categories <- T }



  # If response categories is set to auto, estimate it based on the overall min to max (integer sequence)
    if (length(response.categories) == 1) {
      response.categories <- min(psych::describe(data)$min):max(psych::describe(data)$max)
      if (summary == T) {message("The possible responses detected are: ", paste(response.categories, collapse = "-"))}
    }
   else {
     if (summary == T) {message("Your selected possible responses are: ", paste(response.categories, collapse = "-"))}
}


  # Function to return a logical (TRUE FALSE) for inclusion of judges based on same unique categories
  shouldwekeepthejudge <- function(judge) {
    if(length(sort(unique(judge))) == length(response.categories)){
      logicaltokeepjudge <- unique((sort(unique(judge)) == response.categories)) == TRUE
    } else (logicaltokeepjudge <- FALSE)
    return(logicaltokeepjudge)
  }



  # Create a function to only keep judges if shouldwekeepthejudge==T
  keepjudgeswithsamecategories <- function(data, message = T) {
    x <- data.frame(data)
    # Remove the judges
    data <- data[, which(sapply(X = data, FUN = shouldwekeepthejudge))]
    numberofjudgesremoved <- dim(x)[2] - dim(data)[2]
    if (message == T) {
      if(numberofjudgesremoved > 0){
        message(paste(sep="", numberofjudgesremoved, " ",column.names,"(s) did not have all categories observed: It/they was/were removed for model comparisons (this is necessary to fit the Rating Scale models)."))}
    }
    return(data)
  }

  # Create a new "sanitized" data frame (not necessarily used)
  datasanitized <- keepjudgeswithsamecategories(data, message = F)

  # Function to return the number of judges to potentially remove for RSM/GRSM
  numberofjudgestoremove <- numberofjudges - dim(datasanitized)[2]

  # Ratio of judges to potentially remove
  ratioofjudgestoremove <- numberofjudgestoremove/dim(data)[2]

  # Function to return number as percent
  percent <- function(x, digits = 1, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }

  if (silent == F) {
    # If judges have different observed categories
    if(numberofjudgestoremove > 0 && irt.model == "auto"){
      if (remove.judges.with.unobserved.categories == F) {
        message(paste(sep="", percent(ratioofjudgestoremove)," ",column.names,"s (",numberofjudgestoremove," out of ",numberofjudges, ") did not have all categories (", paste(response.categories, collapse = "-")," observed). Rating scale models were ignored. See documentation (argument remove.judges.with.unobserved.categories) for details."))
      } else if (remove.judges.with.unobserved.categories == T) {
        message(paste(sep="", percent(ratioofjudgestoremove)," ",column.names,"s (",numberofjudgestoremove," out of ",numberofjudges, ") did not have all categories (", paste(response.categories, collapse = "-")," observed). ", "Incomplete ",column.names, "s were removed for model comparison, and in subsequent analyses if a rating scale model is selected. See documentation (argument remove.judges.with.unobserved.categories) for details."))
      }
    }
  }


# Compute the number of judges kept for analysis in Rating Scale etc.
number.of.judges.kept <- numberofjudges-numberofjudgestoremove
# For the constrained GRM (slopes constrained equal)
model.for.constrained.equal.slopes <- paste(sep = "", "G = 1-",numberofjudges, " \n ", "CONSTRAIN = (1-", numberofjudges, ", a1)")
model.for.constrained.equal.slopes.incomplete.judges.removed <- paste(sep = "", "G = 1-",number.of.judges.kept, " \n ", "CONSTRAIN = (1-", number.of.judges.kept, ", a1)")

# For the generalized rating scale (uses the GPCMirt model, with category structure constrained)
### Base example : mirt::mirt(data, item = "gpcmIRT", model = "G = 1-6 \n CONSTRAIN = (1-6, b1),(1-6, b2), (1-6, b3), (1-6, b4) \n FREE = (1-6, c)")


# DEPRECATED
#parameters.to.fix.in.category.structure <- response.categories[1:length(response.categories)-1]
# FIX for missing data
parameters.to.fix.in.category.structure <- seq(from = 1, to = length(response.categories) - 1)
category.structure.constrain <- paste(sep="", "(1-", number.of.judges.kept, ", b", parameters.to.fix.in.category.structure, ")", collapse = ",")
category.structure.constrain <- paste(sep="", "CONSTRAIN = ", category.structure.constrain)
model.for.constrained.category.structure <- paste(sep = "",
                                                  "G = 1-",number.of.judges.kept, " \n ",
                                                  category.structure.constrain, " \n ",
                                                  "FREE = (1-",number.of.judges.kept, ", c)")





# Function to decide of


if (irt.model == "auto") {
  ########## COMPARE MODELS AUTOMATICALLY
  if(remove.judges.with.unobserved.categories == T | numberofjudgestoremove == 0){
    irt.modelvector <- c(
      "rsm",
      "gpcmIRT",
      "Rasch",
      "gpcm",

      "grsm",
      "grsm",
      "graded",
      "graded")
    name.of.model.internal <- c(
      "RSM",
      "GeRSM",
      "PCM",
      "GPCM",

      "CGRSM",
      "GRSM",
      "CGRM",
      "GRM")

    irt.modelvectorwellnamed <- c(
      "Rating Scale Model",
      "Generalized Rating Scale Model",
      "Partial Credit Model",
      "Generalized Partial Credit Model",

      "Constrained Graded Rating Scale Model",
      "Graded Rating Scale Model",
      "Constrained Graded Response Model",
      "Graded Response Model")

    model.list <- list(
      1,
      model.for.constrained.category.structure,
      1,
      1,

      model.for.constrained.equal.slopes.incomplete.judges.removed,
      1,
      model.for.constrained.equal.slopes.incomplete.judges.removed,
      1)
    datainmodelcomparison <- datasanitized

  } else {
    # Models with different category structures can be run with incomplete judges
    irt.modelvector <- c(
      "graded",
      "graded",
      "Rasch",
      "gpcm")
    name.of.model.internal <- c(
      "GRM",
      "CGRM",
      "PCM",
      "GPCM")
    model.list <- list(1, model.for.constrained.equal.slopes, 1, 1, 1)
    irt.modelvectorwellnamed <- c("Graded Response Model", "Constrained Graded Response Model", "Partial Credit Model", "Generalized Partial Credit Model")
    datainmodelcomparison <- data
  }

#return(model.list)

  if(progress.bar == T){
  #Message when comparison starts
  message(paste("\nComparing models...",sep=""))}


  comparisonstatistic <- c()
  iterations.for.model.comparison <- maximum.iterations

 #return(model.list)
#return(irt.modelvector)
 # return(paste(("Fitting model", unlist(model.list[i]),irt.modelvector[i])))

  #message for debug
  if (debug == TRUE) {
    message(paste0("\nDEBUG| Data in model comparison: ", dim(datainmodelcomparison)[1], " rows x ", dim(datainmodelcomparison)[2], " columns \n"))
  }


  # Set progressbar
if (progress.bar == T) {
  pb <- utils::txtProgressBar(min = 0, max = length(irt.modelvector), initial = 0, style = 3)
}





  for (i in 1:length(irt.modelvector)){
    Sys.sleep(.01)
    #set.seed(123)
    paste(unlist(model.list[i]))

    #message for debug
    if (debug == TRUE) {
      message(paste0("\n",
                     "DEBUG| model= ",irt.modelvectorwellnamed[i],"\n",
                     "DEBUG| structural= ", unlist(model.list[i]),"\n",
                     "DEBUG| itemtype=", irt.modelvector[i],"\n\n"))
    }


    # To show the model calls
    if (show.calls == TRUE) {
      if (is.character(unlist(model.list[i])) == FALSE) {
        character.for.structural.model <- ""
      } else {character.for.structural.model <- "\""}
      message(paste0("\n",
                     "Call for ",irt.modelvectorwellnamed[i], "\n",
                     "mirt::mirt(data = data, ",
                     "itemtype = \"",irt.modelvector[i],"\", ",
                     "model = ", character.for.structural.model, unlist(model.list[i]), character.for.structural.model,")"))
    }



    fit <- mirt::mirt(data = datainmodelcomparison,
                      model = unlist(model.list[i]),
                      itemtype = irt.modelvector[i],
                      verbose = F,
                      TOL = convergence.threshold,
                      technical = list(NCYCLES = iterations.for.model.comparison,
                                       warn = F,
                                       message = F))
      modelAIC <- as.numeric(fit@Fit$AIC)
      modelAICc <- as.numeric(fit@Fit$AICc)
      modelBIC <- as.numeric(fit@Fit$BIC)
      modelSABIC <- as.numeric(fit@Fit$SABIC)
      if (selection.criterion == "AIC" | selection.criterion == "aic") {
        fullnameofcomparisonstatistic <- "Akaike Information Criterion"
        shortnameofcomparisonstatistic <- "AIC"
        citationofcomparisonstatistic <- "citationneeded" #####!!!!
        comparisonstatistic[i] <- as.numeric(modelAIC)
      }
      if (selection.criterion == "BIC" | selection.criterion == "bic") {
        fullnameofcomparisonstatistic <- "Bayesian Information Criterion"
        shortnameofcomparisonstatistic <- "BIC"
        citationofcomparisonstatistic <- "citationneeded" #####!!!!
        comparisonstatistic[i] <- as.numeric(modelBIC)
      }
      if (selection.criterion == "SABIC" | selection.criterion == "sabic") {
        fullnameofcomparisonstatistic <- "Sample-Adjusted Bayesian Information Criterion"
        shortnameofcomparisonstatistic <- "SABIC"
        citationofcomparisonstatistic <- "citationneeded" #####!!!!
        comparisonstatistic[i] <- as.numeric(modelSABIC)
      }
    if (selection.criterion == "AICc" | selection.criterion == "aicc" | selection.criterion == "AICC") {
      fullnameofcomparisonstatistic <- "Akaike Information Criterion corrected"
      shortnameofcomparisonstatistic <- "AICc"
      citationofcomparisonstatistic <- "citationneeded" #####!!!!
      comparisonstatistic[i] <- as.numeric(modelAICc)
    }
      if (progress.bar == T) {
        utils::setTxtProgressBar(pb,i)
      }

      #cat("\r")




  }


  if (progress.bar == T) {
   close(pb)
  }

if (summary == T) {
  #Message when comparison starts
  message(paste("\n-== Model Selection (", dim(datainmodelcomparison)[2]," judges) ==-",sep=""))
}


#Build a data.frame with comprison statistics and model names
fit.indices.data.frame <- data.frame(cbind(irt.modelvectorwellnamed, comparisonstatistic))

#Compute delta AIC
delta.ic <- comparisonstatistic-min(comparisonstatistic)



#Compute relative likelihood L
rel.likelihood <- exp((-1/2)*delta.ic)

# Compute sum of relative likelihoods (for denominator)
sum.rel.likelihood <- sum(rel.likelihood)

#Compute Akaike weights
fit.indices.data.frame$Weights <- rel.likelihood/sum(rel.likelihood)

#Rename columns of data.frame
colnames(fit.indices.data.frame) <- c("Model", fullnameofcomparisonstatistic, "Weights")






positionofbestfittingmodelinlist <- which.min(comparisonstatistic)

  if (summary == T) {
  for (i in 1:length(irt.modelvectorwellnamed)) {
    message(paste(sep=""), shortnameofcomparisonstatistic, " for ",  irt.modelvectorwellnamed[i], ": ",round(comparisonstatistic[i], digits), " | Model weight: ", sprintf("%.3f", round(fit.indices.data.frame$Weights[i], digits)))
  }

  message(paste(" -> The best fitting model is the ",irt.modelvectorwellnamed[positionofbestfittingmodelinlist] ,".\n",sep = ""))
  }


  selectedmodelnameformirt <- name.of.model.internal[positionofbestfittingmodelinlist]
  selected.structural.model <- model.list[positionofbestfittingmodelinlist]
  irt.model <- selectedmodelnameformirt

}




  #Create message to say that we need to use the "sanitized data"
    messageestimationwithsanitizeddata  <- paste(sep = "", "- Note : The selected model requires to use only the judges with all categories observed. ", numberofjudgestoremove, " judge(s) out of ",numberofjudges,  " removed.\n")



  ##########
    number.of.judges.removed.for.outputs <- 0





  if (irt.model == "rsm" | irt.model == "RSM" | irt.model == "Rating Scale Model"| irt.model == "Rating Scale") {
    irt.modelnameformirt <- "rsm"
    irt.modelshortname <- "RSM"
    irtfullmodelnameforoutputs <- "Rating Scale Model"
    doiofirt.model <- "doi: 10.1007/BF02293814"
    authorofmodelforoutputs <- "(Andrich, 1978)"
    structural.model <- 1
    if (numberofjudgestoremove != 0) {
      data <- datasanitized
      message(paste(sep = "", "\n- Note : The ",irtfullmodelnameforoutputs," requires to use only the judges with all categories observed. ", numberofjudgestoremove, " judge(s) out of ",numberofjudges,  " removed."))
      # To return in outputs
      number.of.judges.removed.for.outputs <- numberofjudgestoremove
    }
  }
  if (irt.model == "grsm" | irt.model == "GrRSM" | irt.model == "Graded Rating Scale Model"| irt.model == "Graded Rating Scale") {
    irt.modelnameformirt <- "grsm"
    irt.modelshortname <- "GRSM"
    irtfullmodelnameforoutputs <- "Graded Rating Scale Model"
    doiofirt.model <- "doi: 10.1177/014662169001400106"
    authorofmodelforoutputs <- "(Muraki, 1990)"
    structural.model <- 1
    if (ratioofjudgestoremove != 0) {

      data <- datasanitized
      message(paste(sep = "", "\n- Note : The ",irtfullmodelnameforoutputs," requires to use only the judges with all categories observed. ", numberofjudgestoremove, " judge(s) out of ",numberofjudges,  " removed."))
      # To return in outputs
      number.of.judges.removed.for.outputs <- numberofjudgestoremove
      }
  }
    if (irt.model == "gersm" | irt.model == "GeRSM" | irt.model == "GRSM" | irt.model == "Generalized Rating Scale Model"| irt.model == "Generalized Rating Scale") {
      irt.modelnameformirt <- "gpcmIRT"
      irt.modelshortname <- "GeRSM"
      irtfullmodelnameforoutputs <- "Generalized Rating Scale Model"
      doiofirt.model <- "doi: 10.1002/j.2333-8504.1992.tb01436.x"
      authorofmodelforoutputs <- "(Muraki, 1992)"
      structural.model <- model.for.constrained.category.structure
      if (ratioofjudgestoremove != 0) {
        data <- datasanitized
        message(paste(sep = "", "\n- Note : The ",irtfullmodelnameforoutputs," requires to use only the judges with all categories observed. ", numberofjudgestoremove, " judge(s) out of ",numberofjudges,  " removed."))
        # To return in outputs
        number.of.judges.removed.for.outputs <- numberofjudgestoremove
      }
    }
  if (irt.model == "cgrsm" | irt.model == "cGRSM" | irt.model == "CGRSM" | irt.model == "Constrained Graded Rating Scale Model"| irt.model == "Constrained Graded Rating Scale") {
    irt.modelnameformirt <- "grsm"
    irt.modelshortname <- "CGRSM"
    irtfullmodelnameforoutputs <- "Constrained (equal slopes) Graded Rating Scale Model"
    doiofirt.model <- "doi: 10.1177/014662169001400106"
    authorofmodelforoutputs <- "(Muraki, 1990)"
    structural.model <- model.for.constrained.equal.slopes
    if (ratioofjudgestoremove != 0) {

      data <- datasanitized
      message(paste(sep = "", "\n- Note : The ",irtfullmodelnameforoutputs," requires to use only the judges with all categories observed. ", numberofjudgestoremove, " judge(s) out of ",numberofjudges,  " removed."))
      # To return in outputs
      number.of.judges.removed.for.outputs <- numberofjudgestoremove
    }
  }
  if (irt.model == "GRM" | irt.model == "graded" | irt.model == "Graded Response Model") {
    irt.modelnameformirt <- "graded"
    irt.modelshortname <- "GRM"
    irtfullmodelnameforoutputs <- "Graded Response Model"
    doiofirt.model <- "doi: 10.1007/BF03372160"
    authorofmodelforoutputs <- "(Samejima, 1969)"
    structural.model <- 1  }
  if (irt.model == "CGRM" | irt.model == "Constrained Graded Response Model" | irt.model == "cgrm"| irt.model == "cGRM") {
    irt.modelnameformirt <- "graded"
    irt.modelshortname <- "CGRM"
    irtfullmodelnameforoutputs <- "Constrained (equal slopes) Graded Response Model"
    doiofirt.model <- "doi: 10.1007/BF03372160"
    authorofmodelforoutputs <- "(Samejima, 1969)"
    structural.model <- model.for.constrained.equal.slopes
  }
  if (irt.model == "PCM" | irt.model == "pcm" | irt.model == "Partial Credit Model"| irt.model == "Rasch") {
    irt.modelnameformirt <- "Rasch"
    irt.modelshortname <- "PCM"
    irtfullmodelnameforoutputs <- "Partial Credit Model"
    doiofirt.model <- "doi: 10.1007/BF02296272"
    authorofmodelforoutputs <- "(Masters, 1982)"
    structural.model <- 1  }
  if (irt.model == "GPCM" | irt.model == "gpcm" | irt.model == "Generalized Partial Credit Model") {
    irt.modelnameformirt <- "gpcm"
    irt.modelshortname <- "GPCM"
    irtfullmodelnameforoutputs <- "Generalized Partial Credit Model"
    doiofirt.model <- "doi: 10.1002/j.2333-8504.1992.tb01436.x"
    authorofmodelforoutputs <- "(Muraki, 1992)"
    structural.model <- 1  }
  if (irt.model == "Nominal" | irt.model == "NRM" | irt.model == "nominal" | irt.model == "Nominal Response Model") {
    irt.modelnameformirt <- "nominal"
    irt.modelshortname <- "NRM"
    irtfullmodelnameforoutputs <- "Nominal Response Model"
    doiofirt.model <- "doi: 10.1007/BF02291411"
    authorofmodelforoutputs <- "(Bock, 1972)"
    structural.model <- 1  }
  if (estimation.algorithm == "EM" | estimation.algorithm == "Expectation-Maximization") {
    estimationalgorithmfullname <- "Expectation-Maximization"
    doiofestimationalgorithm <- "doi: 10.1007/BF02293801"
    authorofestimationalgorithm <- "(EM; Bock & Atkin, 1981)"
  }
  if (estimation.algorithm == "MHRM" | estimation.algorithm == "MH-RM" | estimation.algorithm == "Metropolis-Hastings Robbins-Monro") {
    estimationalgorithmfullname <- "Metropolis-Hastings Robbins-Monro"
    doiofestimationalgorithm <- "doi: 10.3102/1076998609353115"
    authorofestimationalgorithm <- "(MHRM; Cai, 2010)"
  }


  # Report on method for factor scores
  if (method.factor.scores == "EAP" | method.factor.scores == "eap") {
    methodfactorscoresformirt <- "EAP"
    methodfactorscoresfortext <- "Expected A Posteriori (EAP)"
    methodfactorscoresfullname <- "Expected A Posteriori (EAP)"
    methodfactorscoresexplanation <- "mean of the posterior distribution of a subject's ability, assuming a prior standard normal N(0,1) distribution"
  }
  if (method.factor.scores == "MAP" | method.factor.scores == "map") {
    methodfactorscoresformirt <- "MAP"
    methodfactorscoresfortext <- "Maximum A Posteriori (MAP)"
    methodfactorscoresfullname <- "Maximum A Posteriori (MAP)"
    methodfactorscoresexplanation <- "mode of the posterior distribution of a subject's ability, assuming a prior standard normal N(0,1) distribution"
  }
  if (method.factor.scores == "ML" | method.factor.scores == "ml") {
    methodfactorscoresformirt <- "ML"
    methodfactorscoresfortext <- "Maximum Likelihood (ML)"
    methodfactorscoresfullname <- "Maximum Likelihood estimation (MLE)"
    methodfactorscoresexplanation <- "mode of the likelihood function"
  }
  if (method.factor.scores == "WLE" | method.factor.scores == "WL" | method.factor.scores == "wl") {
    methodfactorscoresformirt <- "WLE"
    methodfactorscoresfortext <- "Weighted Likelihood (WL)"
    methodfactorscoresfullname <- "Weighted Likelihood Estimation (WLE)"
    methodfactorscoresexplanation <- "mode of the likelihood function" ## !!! to change
  }




  # Storing name of package used
  estimationpackage <- "mirt"
  citationofestimationpackage <- utils::citation(package = estimationpackage)[1]
  authorofestimationpackage <- citationofestimationpackage$author$family
  yearofestimationpackage <- citationofestimationpackage$year
  doiofestimationpackage <- paste("doi:",citationofestimationpackage$doi)
  apastyleintextcitationofestimationpackage <- paste(sep="", "(",authorofestimationpackage,", ", yearofestimationpackage, ")")


  # Initial message before estimation
  if (status.verbose == T) {
    message(paste("Fitting ",
                  irtfullmodelnameforoutputs, " ", authorofmodelforoutputs, "...",
                  sep = "")
    )}


# !!! return(structural.model)

# Message for debug mode
  if (debug == T) {
    message(paste0("\nDEBUG| Fitting main model\n",
                   "DEBUG| data= ", dim(data)[1], " rows x ", dim(data)[2], " columns\n",
                   "DEBUG| model= ", structural.model, "\n",
                   "DEBUG| itemtype= ", irt.modelnameformirt,"\n\n"))
  }

  # To show the model call
  if (show.calls == TRUE) {
    if (is.numeric(structural.model)==T) {
      character.for.structural.model <- ""
    } else {character.for.structural.model <- "\""}
    message(paste0("\n",
                   "Call for selected model:\n","mirt::mirt(data = data, ",
                   "itemtype = \"",irt.modelnameformirt,"\", ",
                   "model = ", character.for.structural.model, structural.model, character.for.structural.model,")"))
  }



  #set.seed(123)
  model <- mirt::mirt(data = data,
                      model = structural.model,
                      itemtype = irt.modelnameformirt,
                      method = estimation.algorithm,
                      SE = T,
                      TOL = convergence.threshold,
                      #SE.type = method.standard.errors,
                      verbose = estimation.package.warnings,
                      technical = list(NCYCLES = maximum.iterations,
                                       warn = estimation.package.warnings,
                                       message = estimation.package.warnings)
  )

  #Store info to be displayed
  ## Time elapsed
  timeelapsed <- round(unname(model@time[1]), 2)
  ## Converged
  modelconverged <- model@OptimInfo$converged
  ## Fit indices
  modelAIC <- model@Fit$AIC
  modelAICc <- model@Fit$AICc
  modelBIC <- model@Fit$BIC
  modelSABIC <- model@Fit$BIC
  modelDIC <- model@Fit$DIC






  #Store empirical reliability
  empiricalreliability <- mirt::fscores(model, returnER = T, method = methodfactorscoresformirt)
  attr(empiricalreliability, "names") <- NULL
  marginalreliability <- mirt::marginal_rxx(mod = model)



  # Compute conditionnal reliabilities
  lowlowlowdensity <- mirt::marginal_rxx(model, density = function(x) {stats::dnorm(x, mean = -3, sd = 1)})
  lowlowdensity <- mirt::marginal_rxx(model, density = function(x) {stats::dnorm(x, mean = -2, sd = 1)})
  lowdensity <- mirt::marginal_rxx(model, density = function(x) {stats::dnorm(x, mean = -1, sd = 1)})
  averagedensity <- mirt::marginal_rxx(model, density = function(x) {stats::dnorm(x, mean = 0, sd = 1)})
  highdensity <- mirt::marginal_rxx(model, density = function(x) {stats::dnorm(x, mean = +1, sd = 1)})
  highhighdensity <- mirt::marginal_rxx(model, density = function(x) {stats::dnorm(x, mean = +2, sd = 1)})
  highhighhighdensity <- mirt::marginal_rxx(model, density = function(x) {stats::dnorm(x, mean = +3, sd = 1)})


  #Create AIC object name with IRT model name
  AICname <- paste(sep="", "AIC.", irt.model)
  assign(AICname, modelAIC)
  AICcname <- paste(sep="", "AICc.", irt.model)
  assign(AICcname, modelAICc)
  BICname <- paste(sep="", "BIC.", irt.model)
  assign(BICname, modelBIC)
  SABICname <- paste(sep="", "SABIC.", irt.model)
  assign(SABICname, modelSABIC)

  nonconvergencemessage <- paste("!!! Model did not successfully converge !!!",
                "...but don't give up: ", "\n",
                "- Check for aberrant data", "\n",
                "- Try increasing maximum number of iterations (estimation may be long!) (e.g. maximum.iterations = 10000)","\n",
                "- Try increasing the tolerance for convergence, but not too much (e.g. maximum.iterations = .001)","\n",
                "- Try a different (more constrained) model (e.g. model = \"PCM\")","\n",
                sep = "")

  ## Display estimation info
  if (status.verbose == TRUE) {
    if(modelconverged == TRUE) {
    message(paste("Done! Model successfully converged in ",
                  timeelapsed,
                  " seconds.",
                  #modelconverged ???
                  sep = ""))
    } else {
      message(nonconvergencemessage)
    }
  }








  # Deal with prefix of output data
  if (prefix.for.outputs != "") {
    prefix.for.outputs <- paste(prefix.for.outputs, ".", sep="")
  }




  #Calculate mean scores
  meanscores <- as.data.frame(as.numeric(rowMeans(data, na.rm = T)))

  names(meanscores) <- paste(prefix.for.outputs, "Mean.Score", sep = "")

if (additional.stats == T) {
  #Calculate Cronbach's Alpha
  cronbach <- psych::alpha(data)
  cronbachalpha <- cronbach$total[1]
  cronbachalphastd <- cronbach$total[2]
  suppressMessages(splithalf<- psych::splitHalf(data, check.keys = FALSE))
  alpha <- splithalf$alpha
  lambda6 <- splithalf$lambda6
  lambda4maxsplithalf <- splithalf$maxrb
  averagesplithalf <- splithalf$meanr


  #Calculate IRR coefficients
  fleisskappaexactconger <- irr::kappam.fleiss(ratings = data, exact = TRUE, detail = FALSE)
  fleisskappa <- irr::kappam.fleiss(ratings = data, exact = FALSE, detail = FALSE)
  #lightskappa <- irr::kappam.light(ratings = originaldata)
  #krippendorfordinal <- irr::kripp.alpha(x = originaldata, method = "ordinal")
  #krippendorfnominal <- irr::kripp.alpha(x = originaldata, method = "nominal")
  iccconsistencyoneway <- irr::icc(ratings = data, model = "oneway", type = "consistency") #
  iccconsistencytwoway <- irr::icc(ratings = data, model = "twoway", type = "consistency") # randomly chosen raters (recommended)
  iccagreementoneway <- irr::icc(ratings = data, model = "oneway", type = "agreement") #
  iccagreementtwoway <- irr::icc(ratings = data, model = "twoway", type = "agreement") # randomly chosen raters (recommended) + means are of interest
  textabouticc <- "If means are important in you analysis : Report Agreement ICC (Shrout & Fleiss, 1979).\n If judges are randomly sampled from an existing or theoretical population of judges, use Two-Way ICC. (Shrout & Fleiss, 1979)"

}





  #Polychoric Alpha
#  polycor <- psych::polychoric(data, )
#  alphapolycor <- psych::alpha(polycor$rho)
  #Compute factor scores
  factorscores <- mirt::fscores(object = model, full.scores.SE = T, method = methodfactorscoresformirt)

  # Transform into dataframe
  factorscores <- as.data.frame(factorscores)

  #Rename columns
  factorscoresname <- paste(prefix.for.outputs, "Factor.Score", sep = "")
  standarderrorsname <- paste(prefix.for.outputs, "Standard.Error", sep = "")
  colnames(factorscores) <- c(factorscoresname, standarderrorsname)

  # Extract a vector for factor scores and one for SE
  factorscoresvector <- c(factorscores[[1]])
  standarderrorsvector <- c(factorscores[[2]])



####### INPUTE MISSING DATA
cases.with.missing.data <- data[!stats::complete.cases(data),]
missingness.logical.matrix <- is.na.data.frame(as.data.frame(data))



if (sum(missingness.logical.matrix) > 0) {
  # Number of products with missing data
  number.of.incomplete.observations <- sum(missingness.logical.matrix)
  maximum.observations <- dim(data)[1]*dim(data)[2]
  percent.of.incomplete.observations <- percent(x = number.of.incomplete.observations/maximum.observations)

  # Message to signal inputation
  if (missing.data.message == T) {
    message(paste(sep = "" , "\nThere are ",number.of.incomplete.observations," (", percent.of.incomplete.observations, ") missing observations! Inputing..."))
  }

  # Inpute with mirt
  factorscoresmatrixforinpute <- mirt::fscores(object = model, method = methodfactorscoresformirt)
  input.missing.data <- mirt::imputeMissing(x = model, Theta = factorscoresmatrixforinpute)
  imputed <- TRUE
} else {
  imputed <- FALSE
  input.missing.data <- data
  }



  objectstoreturn <- list(Factor.Scores = factorscores,
                          get(AICname),
                          get(AICcname),
                          get(BICname),
                          get(SABICname)
  )
  names(objectstoreturn) <- c("Factor.Scores",
                              AICname,
                              AICcname,
                              BICname,
                              SABICname
  )

  #Create function to remove leading zeroes
  numformat <- function(x, digits = 3) {
    ncode <- paste0("%.", digits, "f")
    sub("^(-?)0.", "\\1.", sprintf(ncode, x))
  }



judges.used.in.estimated.model <- numberofjudges - number.of.judges.removed.for.outputs

if (number.of.judges.removed.for.outputs > 0) {
  additionnal.message.for.judges.removed <- paste(sep = "", " | ", number.of.judges.removed.for.outputs, " ", column.names, "(s) with unobserved categories removed.")
} else {additionnal.message.for.judges.removed <- NULL}

  if (summary == T) {
    message(paste(sep="", "\n -== General Summary ==-\n",
                  "- ", judges.used.in.estimated.model, " ", column.names, "s", additionnal.message.for.judges.removed, "\n",
                  "- ", sample.size, " Products\n",
                  "- ", length(response.categories), " response categories (",paste(response.categories, collapse = "-"),")\n"),
                  "- Mean judgment = ", round(mean(as.numeric(rowMeans(data, na.rm = T))), digits = digits), " | SD = ",round(stats::sd(as.numeric(rowMeans(data, na.rm = T))), digits = digits))
    message(paste(sep="", "\n-== IRT Summary ==-\n",
                  "- Model: ", irtfullmodelnameforoutputs," ", authorofmodelforoutputs," | ", doiofirt.model, "\n",
                  "- Estimation package: ", estimationpackage, " ",apastyleintextcitationofestimationpackage," | ", doiofestimationpackage,"\n",
                  "- Estimation algorithm: ", estimationalgorithmfullname, " ", authorofestimationalgorithm, " | ", doiofestimationalgorithm, "\n",

                  "- Method of factor scoring: ", methodfactorscoresfullname,"\n",
                  "- AIC = ", round(as.numeric(modelAIC), digits = digits)," |",
                  " AICc = ", round(as.numeric(modelAICc), digits = digits)," |",
                  " BIC = ", round(as.numeric(modelBIC), digits = digits)," |",
                  " SABIC = ", round(as.numeric(modelSABIC), digits = digits),"\n",
                  "\n-== Model-based reliability ==-\n",
                  "- Empirical reliability | Average in the sample: ", numformat(empiricalreliability, digits), "\n",
                  "- Expected reliability | Assumes a Normal(0,1) prior density: ", numformat(averagedensity, digits), "\n"#,
               #   "- Normal(-3,1) prior density | Extremely Low abilities : ", numformat(lowlowlowdensity, digits), "\n",
                #  "- Normal(-2,1) prior density | Very low abilities: ", numformat(lowlowdensity, digits), "\n",
                #  "- Normal(-1,1) prior density | Low abilities: ", numformat(lowdensity, digits), "\n",

                 # "- Normal(+1,1) prior density | High abilities: ", numformat(highdensity, digits), "\n",
                #  "- Normal(+2,1) prior density | Very high abilities: ", numformat(highhighdensity, digits), "\n",
                 # "- Normal(+3,1) prior density | Extremely high abilities: ", numformat(highhighhighdensity, digits)
#                  "- Pearson correlation between Factor scores and Mean Scores :  ",round(pearson, 3), "\n",
                  ))
 #   if (example.write.up == T) {
      # message(paste(sep="", "\n-== Example write-up ==-\n",
      #               "To obtain composite judgment scores, ", methodfactorscoresfortext ," factor scores were estimated through a ", irtfullmodelnameforoutputs," ", authorofmodelforoutputs," ",
      #               " using the R package \"jrt\" (Myszkowski, 2018), with the ", estimationalgorithmfullname, " estimation method ", authorofestimationalgorithm, " implemented in the Item-Response Theory estimation package \"", estimationpackage, "\" ", apastyleintextcitationofestimationpackage,". The estimated scores had an empirical reliability of ", numformat(empiricalreliability, digits), ")."))
#    }
    if (additional.stats == T) {
      message(paste(sep="",
                    "-== Other reliability statistics (packages \"irr\" and \"psych\") ==-\n",
                    "- Cronbach's Alpha: ", numformat(cronbachalpha, digits),"\n",
                    "- Standardized Cronbach's Alpha : ", numformat(cronbachalphastd, digits), "\n",
                    "- Guttman's Lambda 4 :", numformat(lambda4maxsplithalf,digits), "\n",
                    "- Guttman's Lambda 6 :", numformat(lambda6,digits), "\n",
                    #"Average split-half reliability :", numformat(averagesplithalf,digits), "\n",
                   # "- Omega :", numformat(omegatotal,digits), "\n",
                  #  "- Omega Hierarchical :", numformat(omegahierarchical,digits), "\n",
                    "- Fleiss' Kappa : ", numformat(fleisskappa$value, digits),"\n",
                    "- Fleiss-Conger's Exact Kappa : ", numformat(fleisskappaexactconger$value, digits),"\n",
                   # "- Ordinal Krippendorf's Alpha : ", numformat(krippendorfordinal$value, digits),"\n",
                    "- Intraclass Correlation Coefficient (One-Way Consistency model): ", numformat(iccconsistencyoneway$value, digits),"\n",
                    "- Intraclass Correlation Coefficient (Two-Way Consistency model): ", numformat(iccconsistencytwoway$value, digits),"\n",
                    "- Intraclass Correlation Coefficient (One-Way Agreement model): ", numformat(iccagreementoneway$value, digits),"\n",
                    "- Intraclass Correlation Coefficient (Two-Way Agreement model): ", numformat(iccagreementtwoway$value, digits),"\n"))
    }

}


##### Outputs in jrt object

## Combine outputs from mirt
#If there were partial missing, add imputed data
if (imputed == TRUE) {
  factor.scores.with.appended.original.index <- cbind(input.missing.data, factorscores, meanscores, index.for.jrt = index.vector[logical.vector.of.not.complete.missing])
} else { # if not don't include it
  factor.scores.with.appended.original.index <- cbind(factorscores, meanscores, index.for.jrt = index.vector[logical.vector.of.not.complete.missing])
}


# Merge with original dataframe by index.for.jrt
full.scores <- merge(x = original.data.as.data.frame,
                     y = factor.scores.with.appended.original.index,
                     no.dups = T,
                     suffixes = c(".Original", ".Imputed"),
                     by = "index.for.jrt", all = T, sort = T)
full.scores$index.for.jrt <- NULL


# Merge with original dataframe to get order of observations but just keep factor scores
index.for.jrt <- index.vector
full.scores.without.data <- merge(x = as.data.frame(index.for.jrt),
                                  y =  cbind(factorscores, meanscores, index.for.jrt = index.vector[logical.vector.of.not.complete.missing]),
                                  no.dups = T,
                                  by = "index.for.jrt", all = T, sort = T)
full.scores.without.data$index.for.jrt <- NULL



# Merge with original dataframe to get order of observations but just keep imputed data
if (imputed == TRUE) {
  imputed.without.data <- merge(x = as.data.frame(index.for.jrt),
                                y =  cbind(input.missing.data, index.for.jrt = index.vector[logical.vector.of.not.complete.missing]),
                                no.dups = T,
                                by = "index.for.jrt", all = T, sort = T)
  # Remove index.for.jrt
  imputed.without.data$index.for.jrt <- NULL
  imputed.data <- imputed.without.data
} else { imputed.data <- NULL}


# Output : Full scores with original, imputed, fscores and standard errors
output.data <- as.data.frame(full.scores)


# Output : Only factor scores
factor.scores <- as.data.frame(full.scores.without.data)

# Output : Only factor scores vector
factor.scores.vector <- unname(unlist(dplyr::select(factor.scores, dplyr::contains("Factor"))))

# Output : Only standard errors vector
standard.errors.vector <- unname(unlist(dplyr::select(factor.scores, dplyr::contains("Standard"))))

# Output : Only mean scores vector
mean.scores.vector <- unname(unlist(dplyr::select(factor.scores, dplyr::contains("Mean"))))

# Output : imputed data
imputed.data <- as.data.frame(imputed.data)




  # Warn for non finite values if using ML estimates
  if (methodfactorscoresformirt == "ML") {
    warning("Maximum Likelihood factor scores are not recommended. Your factor scores may include non finite (-Inf and/or +Inf) estimates with non-computable standard errors. We suggest to use Bayesian estimates instead (EAP, MAP).")
  }



if (sum(missingness.logical.matrix)>0) {


  person.fit <- as.data.frame(NULL)

  message("- Note : Person fit statistics based on imputed data! Use with caution!")

  model.complete <- mirt::mirt(data = as.data.frame(input.missing.data),
                      model = 1,
                      itemtype = irt.modelnameformirt,
                      method = estimation.algorithm,
                      SE = T,
                      TOL = convergence.threshold,
                      #SE.type = method.standard.errors,
                      verbose = estimation.package.warnings,
                      technical = list(NCYCLES = maximum.iterations,
                                       warn = estimation.package.warnings,
                                       message = estimation.package.warnings)
  )

  person.fit <- as.data.frame(mirt::personfit(x = model.complete, method = method.factor.scores, Theta = NULL, stats.only = TRUE))

} else {
  person.fit <- mirt::personfit(x = model, method = method.factor.scores, Theta = NULL, stats.only = TRUE)
  }

item.fit <- as.data.frame(mirt::itemfit(x = model, fit_stats = method.item.fit,
                                        method = method.factor.scores, Theta = NULL, stats.only = TRUE))
local.dependence <- mirt::residuals(object = model, df.p = T, verbose = F)


outputobject <- methods::new("jrt",
                    input.data = as.data.frame(originaldata),
                    output.data = output.data,
                    factor.scores = factor.scores,
                    factor.scores.vector = factor.scores.vector,
                    standard.errors.vector = standard.errors.vector,
                    mean.scores.vector = mean.scores.vector,
                    imputed.data = as.data.frame(imputed.data),
#                    partially.missing.data = sum(missingness.logical.matrix),
                    response.categories = response.categories,
                    sample.size = sample.size,
                    method.factor.scores = methodfactorscoresfullname,
                    fitted.model = irtfullmodelnameforoutputs,
                    empirical.reliability = empiricalreliability,
                    marginal.reliability = marginalreliability,
                    item.fit = item.fit,
                    person.fit = person.fit, ### Will have to account there for complete missing
                    local.dependence = local.dependence,
                    column.names = column.names,
                    mirt.object = model
                    )




#outputobject

if (plots == T) {
  print(info.plot(outputobject, type = "ir", legend.position = "bottom", mirt.object.input = F))
  print(jcc.plot(outputobject, overlay.reliability = T, labelled = greyscale, greyscale = greyscale, legend.position = "bottom"))
}

#show(outputobject)

return(outputobject)

}

#' Object returned by the \link{jrt} function.
#' @slot input.data The original data
#' @slot output.data The output data with factor scores.
#' @slot fitted.model The selected model.
#' @slot response.categories The count of response categories.
#' @slot method.factor.scores The method used to compute factor scores.
#' @slot imputed.data The data with imputation.
#' @slot factor.scores The factor scores with standard errors as a data.frame.
#' @slot factor.scores.vector The factor scores as a vector.
#' @slot standard.errors.vector The standard errors as a vector.
#' @slot mean.scores.vector The mean scores as a vector.
#' @slot empirical.reliability The empirical reliability.
#' @slot marginal.reliability The marginal reliability.
#' @slot item.fit Tests of item fit.
#' @slot person.fit Tests of person fit.
#' @slot local.dependence Tests of local dependence.
#' @slot sample.size The sample size used in the model.
#' @slot number.of.judges.in.model The number of judges (or items) in the model.
#' @slot column.names The name used for the columns.
#' @slot mirt.object The mirt object of the model.
methods::setClass(Class = "jrt", slots = c(
  input.data = "data.frame",
  output.data = "data.frame",
  fitted.model = "character",
  response.categories = "numeric",
  method.factor.scores = "character",
  imputed.data = "data.frame",
  factor.scores = "data.frame",
  factor.scores.vector = "numeric",
  standard.errors.vector = "numeric",
  mean.scores.vector = "numeric",
  empirical.reliability = "numeric",
  marginal.reliability = "numeric",
  item.fit = "data.frame",
  person.fit = "data.frame",
  local.dependence = "list",
  sample.size = "numeric",
  number.of.judges.in.model = "numeric",
  column.names = "character",
  mirt.object = "SingleGroupClass"
))



#' anova method for objects returned by the \link{jrt} function.
#'@param object An object returned by \link{jrt}.
methods::setMethod("anova", "jrt", function(object) {mirt::anova(object@mirt.object)})







# Documentation for data "ratings"
#'A simulated dataset with 300 products judged by 6 judges.
#'
#' @format A data.frame with 300 rows and 6 columns:
#' \describe{
#'   \item{Judge_1}{Judgments of judge 1}
#'   \item{Judge_2}{Judgments of judge 2}
#'   \item{Judge_3}{Judgments of judge 3}
#'   \item{Judge_4}{Judgments of judge 4}
#'   \item{Judge_5}{Judgments of judge 5}
#'   \item{Judge_6}{Judgments of judge 6}
#' }
"ratings"

# Documentation for data "ratings"
#'A simulated dataset with 350 cases judged by 5 judges, using a planned missingness design.
#'
#' @format A data.frame with 350 rows (cases) and 5 columns (judges):
#' \describe{
#'   \item{Judge_1}{Judgments of judge 1}
#'   \item{Judge_2}{Judgments of judge 2}
#'   \item{Judge_3}{Judgments of judge 3}
#'   \item{Judge_4}{Judgments of judge 4}
#'   \item{Judge_5}{Judgments of judge 5}
#' }
"ratings_missing"

#### CODE TO SIMULATE RATINGS DATASET
# set.seed(123)
# N <- 300
# judges <- 6
# diffs <- t(apply(matrix(runif(judges*4, .4, 5), judges), 1, cumsum))
# d <- -(diffs - rowMeans(diffs)) + stats::rnorm(judges, mean = 0, sd= 1)
# data <- mirt::simdata(matrix(rlnorm(judges,1,0)), d, N,
#                       itemtype = 'graded') + 1
# colnames(data) <- paste("Judge_", 1:dim(data)[2], sep = "")
# data <- as.data.frame(data)




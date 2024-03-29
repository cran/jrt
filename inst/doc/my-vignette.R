## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.height = 5,
	fig.width = 7.3,
  collapse = TRUE,
  comment = "#>"
)

## ----include=TRUE-------------------------------------------------------------
data <- jrt::ratings

## -----------------------------------------------------------------------------
head(data)

## -----------------------------------------------------------------------------
library(jrt)

## -----------------------------------------------------------------------------
fit <- jrt(data, progress.bar = F)

## -----------------------------------------------------------------------------
fit <- jrt(data, silent = T)

## -----------------------------------------------------------------------------
fit <- jrt(data, "PCM")

## -----------------------------------------------------------------------------
head(fit@factor.scores)

## -----------------------------------------------------------------------------
head(fit@output.data)

## -----------------------------------------------------------------------------
jcc.plot(fit, judge = 3)

## -----------------------------------------------------------------------------
jcc.plot(fit)

## -----------------------------------------------------------------------------
jcc.plot(fit, judge = c(1,6))

## -----------------------------------------------------------------------------
jcc.plot(fit, facet.cols = 2)

## -----------------------------------------------------------------------------
jcc.plot(fit, 1, greyscale = T)

## -----------------------------------------------------------------------------
jcc.plot(fit, 1, overlay.reliability = TRUE)

## -----------------------------------------------------------------------------
jcc.plot(fit, overlay.reliability = T, labelled = F)

## -----------------------------------------------------------------------------
jcc.plot(fit, overlay.reliability = T, labelled = F, legend.position = "bottom")

## -----------------------------------------------------------------------------
jcc.plot(fit, 2, column.names = "Expert")

## -----------------------------------------------------------------------------
jcc.plot(fit, 3:4,
         manual.facet.names = paste("Expert ", c("A", "B", "C", "D", "E", "F")),
         manual.line.names = c("Totally disagree", "Disagree", "Neither agree\nnor disagree", "Agree", "Totally agree"),
         labelled = F)

## -----------------------------------------------------------------------------
jcc.plot(fit, 1, title = "")

## -----------------------------------------------------------------------------
jcc.plot(fit, 1, theta.span = 5)

## -----------------------------------------------------------------------------
jcc.plot(fit, 1:4,
         labelled = F,
         line.opacity = c(0,0,0,1,0,0) # Highlighting the 4th category
         )

## -----------------------------------------------------------------------------
jcc.plot(fit, 1, color.palette = "Dark2", theme = "classic", line.width = 1.5, font.family = "serif", overlay.reliability = T, name.for.reliability = "Reliability")

## -----------------------------------------------------------------------------
jcc.plot(fit, 1:3, labelled = F, line.opacity = c(0,0,0,1,0,0))

## -----------------------------------------------------------------------------
jcc.plot(fit, 1, color.palette = "Blues", theme = "grey", line.width = 3, labelled = F)

## -----------------------------------------------------------------------------
jcc.plot(fit, 1, color.palette = "npg", overlay.reliability = T)

## -----------------------------------------------------------------------------
info.plot(fit, 1)

## -----------------------------------------------------------------------------
info.plot(fit)

## -----------------------------------------------------------------------------
info.plot(fit, type = "r")

## -----------------------------------------------------------------------------
info.plot(fit, type = "se")

## -----------------------------------------------------------------------------
info.plot(fit, type = "r", y.limits = c(0,1))

## -----------------------------------------------------------------------------
info.plot(fit, type = "r", y.line = .70)

## -----------------------------------------------------------------------------
info.plot(fit, type = "ise")

## -----------------------------------------------------------------------------
info.plot(fit, type = "ir", y.line = .7)

## -----------------------------------------------------------------------------
info.plot(fit, type = "ir", y.line = .7, color.palette = "Dark2")

## -----------------------------------------------------------------------------
info.plot(fit, 1, "ir",
          column.names = "Rater",
          theta.span = 5,
          theme = "classic",
          line.width = 2,
          greyscale = T,
          font.family = "serif")

## ----include=FALSE------------------------------------------------------------
set.seed(123)
N <- 100
judges <- 8
diffs <- t(apply(matrix(runif(judges*4, .4, 5), judges), 1, cumsum))
d <- -(diffs - rowMeans(diffs)) + stats::rnorm(judges, mean = 0, sd= 1)
data <- mirt::simdata(matrix(rlnorm(judges,1,0)), d, N,
itemtype = 'graded') + 1
colnames(data) <- paste("Judge_", 1:dim(data)[2], sep = "")


## -----------------------------------------------------------------------------
fit <- jrt(data, 
           progress.bar = F, #removing the progress bar for the example
           plots = F) 

## -----------------------------------------------------------------------------
fit <- jrt(data, 
           remove.judges.with.unobserved.categories = T,
           progress.bar = F, #removing the progress bar for the example
           plots = F) 

## -----------------------------------------------------------------------------
fit <- jrt(data,
           additional.stats = T,
           progress.bar = F,
           plots = F) #removing the progress bar for the example

## -----------------------------------------------------------------------------
# Get more fit indices and compare models
mirt::anova(fit@mirt.object, verbose = F)
# Get total information for a given vector of attributes
mirt::testinfo(fit@mirt.object, Theta = seq(from = -3, to = 3, by = 1))
# Get the test information for case 1
mirt::testinfo(fit@mirt.object, Theta = fit@factor.scores.vector[1])
# Get marginal reliability for high abilities – using a Normal(1,1) prior
mirt::marginal_rxx(fit@mirt.object,
                   density = function(x) {dnorm(x, mean = 1, sd = 1)})

## -----------------------------------------------------------------------------
model1 <- jrt(data, "GRM", silent = T) # Fitting a GRM
model2 <- jrt(data, "CGRM", silent = T) # Fitting a Constrained GRM
mirt::anova(model1@mirt.object, model2@mirt.object, verbose = F) #Comparing them

## -----------------------------------------------------------------------------
fit <- jrt(ratings_missing, irt.model = "PCM", silent = T) #fit model

## -----------------------------------------------------------------------------
fit <- mirt::mirt(data = mirt::Science, 
                  model = 1, 
                  itemtype = "gpcm",
                  verbose = F)

## -----------------------------------------------------------------------------
jcc.plot(fit)

## -----------------------------------------------------------------------------
info.plot(fit)

## -----------------------------------------------------------------------------
jcc.plot(fit, item = 3)

## -----------------------------------------------------------------------------
# SAT data from mirt
## Convert to binary
data <- mirt::key2binary(mirt::SAT12,
    key = c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5))
## Fit 2PL model in mirt
fit <- mirt::mirt(data = data, model = 1, itemtype = "2PL", verbose = F)
## Plotting an item response function
jcc.plot(fit, item = 2)
## Plotting the item response functions of the first 12 items with a larger theta range
jcc.plot(fit, facet.cols = 4, item = 1:12, theta.span = 5)


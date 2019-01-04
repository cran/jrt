#' Plot the information function for a judge or for the entire set of judges.
#'
#' @description This function returns the Judge Information Function plot from a \code{jrt} object and the judge number. Information can be plotted as such, as reliability or as standard error. The function may also be used for the information of the entire set of judges. This is a wrapper function and adaptation of the \code{itemplot} function in the package \code{mirt} (Chalmers, 2012). It also uses the plotting functions of the packages \code{directlabels} and \code{ggplot2}.
#'
#' @param jrt.object A object of the \code{jrt} class (created by the function \code{jrt}).
#' @param judge A numerical to indicate which judge to plot. Be careful : If a (Generalized) Rating Scale Model was used, then judges may have been removed for the model to be fitted. Provide \code{"all"} to get the information plot for all judges.
#' @param type A character to indicate what to plot on the y axis, \code{"information"} for Information, \code{"reliability"} for Reliability, or \code{"SE"} for standard errors. Alternatively, use \code{"infose"} (or \code{ise}) to plot information and standard error of measurement in the same plot. Use \code{"inforxx"} (or \code{ir}) to plot information and reliability in the same plot.
#' @param title A character title for the plot. By default it is created automatically based on the judge number.
#' @param column.names A character to indicate how a judge should be called (Defaults to \code{"auto"}, which uses what was set in the estimation function \code{jrt}, whose default is \code{"judge"}, but you may use \code{"Rater"}, \code{"Expert"}, etc.). This is used to create automatic titles.
#' @param theta.span A numeric indicating the maximum \eqn{\theta}. The minimum is automatically adjusted to \code{-theta.span}.
#' @param y.limits A numeric vector to manually adjust the minimum and maximum of the y axis (may notably be useful if using reliability). Set to \code{NULL} (default) to automatically set with \code{ggplot2}.
#' @param y.line A numeric to add a (dashed) horizontal line on the plot at the y value indicated (for example for a threshold of acceptable reliability). Defaults to \code{NULL}, which does not plot the line. Note that the y level is in reference to the primary axis, if there are two y axes.
#' @param y.line.refers.to.secondary.axis A logical to indicate if the y.line should refer to a value on the secondary axis (\code{TRUE}, default) or the primary (\code{FALSE}). Only used if there is a secondary axis. The default is \code{TRUE} because threshold values for interpretation are more often used for reliability or standard error than information.
#' @param name.for.y.line A character to indicate how to call the y line in the legend. Default is \code{"Threshold"}.
#' @param greyscale A logical (default is \code{FALSE}) to indicate whether to use greyscale graphics (useful for publication). Uses variations in linetype as opposed to variations in line colors.
#' @param color.palette A character value to indicate the colour palette to use. Defaults to \code{"D3"} from "ggsci". Use \code{""} for the default of \code{ggplot2}. The palettes are supplied as arguments in the \code{scale_fill_brewer()} function of \code{ggplot2}. See here for a list of palettes: \url{http://ggplot2.tidyverse.org/reference/scale_brewer.html}. In addition, most palettes from the pacakge \code{ggsci} are available (e.g., \code{"npg"}, \code{"aas"}, \code{"nejm"}, \code{"lancet"}, \code{"jama"}, \code{"d3"}). Use \code{vignette("ggsci")} for details.
#' @param line.width A numeric indicating the width for the information function curve (default is \code{2.5}).
#' @param key.width A numeric to indicate the width of the legend key (default is \code{3}).
#' @param legend.position A character string or vector of coordinates to position the legend key. Defaults to \code{"right"}. Other possibilities include notably \code{"bottom"}.
#' @param legend.columns A numeric to indicate after how many legend key elements to add a line break. Especially useful if using \code{legend.position = "bottom"} if you want line breaks between each key. Defaults to \code{""}, which automatically saves space based on the legend position (line breaks are used if the legend in positioned on the side of the graph).
#' @param line.type A numeric indicating the line type for the information function curve (default is \code{1} for a plain line. This would be used if overlaying multiple plots.
#' @param theme A character value to indicate the background color theme used by \code{ggplot2}. Defaults to \code{"bw"}. Can be \code{"light"}, \code{"dark"}, \code{"minimal"}, \code{"classic"}, \code{"gray"}, \code{"bw"} or \code{"linedraw"}.
#' @param text.size A numeric value to control the size of the text on the plot.
#' @param title.size A numeric value to control the size of the plot title (defaults to \code{text.size+4}).
#' @param font.family A character value to control the font family used on the graph. Defaults to \code{"sans"}. Other possible values include \code{"serif"} or \code{"mono"}.
#' @param remove.gridlines A logical value to remove the gridlines (default is \code{TRUE}).
#' @param precision A numeric to indicate the degree of precision used to plot the curves. Higher values will increase the accuracy of the graph and make the curves look smoother, but the data generated to plot the graph will be bigger, which will slow down the function. Lower values will do the opposite. Values between \code{10} and \code{100} are recommended, \code{20} is the default and sufficient for most uses.
#' @param mirt.object.input A logical allowing to input directly an \code{mirt} object as a \code{jrt.object} argument, even though this should be detected automatically. See \code{mirt} package documentation, and note that this is a secondary use that may lead to inconsistent results at this point.
#' @param item For convenience, this argument, more standard to IRT packages, can be used instead of the \code{judge} argument.
#' @references
#' Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory
#' Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' \doi{10.18637/jss.v048.i06}
#' @references Myszkowski & Storme (in press). Judge Response Theory? A call to upgrade our psychometrical account of creativity judgments. \emph{Psychology of Aesthetics, Creativity and the Arts.} \doi{10.17605/OSF.IO/9WC34}
#' @return A plot of the information, reliability or standard error function.
#' @import directlabels
#' @import mirt
#' @import ggplot2

#'
#' @export
#'
#' @examples
#'
#' # Simulate data with package mirt
#' # (6 judges, 300 productions, ordinal ratings from 1 to 5)
#' set.seed(123)
#' N <- 300
#' judges <- 6
#' diffs <- t(apply(matrix(runif(judges*4, .4, 5), judges), 1, cumsum))
#' d <- -(diffs - rowMeans(diffs)) + stats::rnorm(judges, mean = 0, sd= 1)
#' data <- mirt::simdata(matrix(rlnorm(judges,1,0)), d, N,
#' itemtype = 'graded') + 1
#' colnames(data) <- paste("Judge_", 1:dim(data)[2], sep = "")
#' data <- as.data.frame(data)
#' ### --> See mirt documentation for details
#'
#' # Fit model
#' fit <- jrt(data, irt.model = "PCM")
#'
#' # Information function of the first judge
#' info.plot(fit, 1)
#'
#' # Reliability function of the second judge
#' info.plot(fit, 2, type = "reliability")
#'
#' # Standard error function of the entire set of judges
#' info.plot(fit, "all", type = "SE")
#'
#' # See vignette for more options
#'
info.plot <- function(jrt.object,
                      judge = "all",
                      type = "information",
                      title = "auto",
                      column.names = "auto",
                      theta.span = 3.5,
                      y.limits = NULL,
                      y.line = NULL,
                      name.for.y.line = "Threshold",
                      y.line.refers.to.secondary.axis = TRUE,
                      greyscale = FALSE,
                      color.palette = "D3",
                      line.type = 1,
                      line.width = 1,
                      key.width = 3,
                      legend.position = "right",
                      legend.columns = "",
                      theme = "bw",
                      text.size = 10,
                      title.size = text.size+4,
                      remove.gridlines = TRUE,
                      font.family = "sans",
                      precision = 20,
                      mirt.object.input = F,
                      item = NULL) {




  # If user supplies item instead of judge, replace judge by value for item
  if (is.null(item) == FALSE) {
    judge <- item
  }

  # Detect if input is mirt type
  if (class(jrt.object)[1] == "SingleGroupClass") {
    mirt.object.input <- TRUE
  } else {mirt.object.input <- FALSE}




  #get the mirt object from the jrt object
  if (mirt.object.input == F) {
    mirtobject <- slot(object = jrt.object, name = "mirt.object")
  } else { mirtobject <- jrt.object }



  # Set auto name of judges if using mirt input
  if (mirt.object.input == T && column.names == "auto"){
    column.names <- "Item"
  }

  # Set auto name of judges for "auto" (uses what's in the jrt object)
  if (mirt.object.input == F && column.names == "auto"){
    column.names <- jrt.object@column.names
  }



Theta <- matrix(seq(from = -theta.span,to =  theta.span,by = 1/precision))



if (type == "information" | type == "Information" | type == "i" | type == "I") {
  type.for.titles <- "Information"
  transformationfunction <- function(information) {
    return(information)
  }
  expressionforyaxis <- expression(Information)
}

if (type == "reliability" | type == "Reliability" | type == "r" | type == "R") {
  type.for.titles <- "Reliability"
  transformationfunction <- function(information) {
    return(information/(information+1))
  }
  expressionforyaxis <- expression(Reliability)
}

if (type == "SE" | type == "Standard Error" | type == "se" | type == "s" | type == "stderr" | type == "standarderror") {
  type.for.titles <- "Standard Error"
  transformationfunction <- function(information) {
    return(1/sqrt(information))
  }
  expressionforyaxis <- expression(Standard~~Error)
}

if (type == "inforxx" | type == "inforeliability" | type == "ir" | type == "IR") {
  type.for.titles <- "Information"
  transformationfunction <- function(information) {
    return(information)
  }
  expressionforyaxis <- expression(Information)
}

if (type == "infose" | type == "infoSE" | type == "ise" | type == "IS") {
  type.for.titles <- "Information"
  transformationfunction <- function(information) {
    return(information)
  }
  expressionforyaxis <- expression(Information)
}



  if (is.numeric(judge) == T) {
    # Setting auto title
    if(title == "auto"){title <- paste(sep="", column.names," ", type.for.titles," Function for ",column.names," ", judge)}
info <- mirt::iteminfo(mirt::extract.item(mirtobject, judge), Theta)
  } else if (judge == "all") {
    # Set title
    if(title == "auto"){title <- paste(sep="", "Total ", type.for.titles," Function")}
    #get test info
    info <- mirt::testinfo(mirtobject, Theta)
  }


    # plot chart
    dataforplot <- as.data.frame(cbind(Theta, info))
    names(dataforplot) <- c("Theta", "Information")
   # return(dataforplot)

    # To ease cran check
    Information <- dataforplot$Information


    Reliability <- Information/(Information+1)
    StandardError <- 1/sqrt(Information)








# Count the lines to plot or not a legend
how.many.lines <- 1
if (type == "inforxx" | type == "inforeliability" | type == "ir" | type == "IR" | type == "infose" | type == "infoSE" | type == "ise" | type == "IS") {
  how.many.lines <- how.many.lines + 1
}
if (is.null(y.line) == FALSE) { how.many.lines <- how.many.lines + 1}



# If there is only 1 line, do not add line color or width in aes to avoid showing useless legend
if (how.many.lines == 1) {
  p <- ggplot2::ggplot(dataforplot, ggplot2::aes(Theta, transformationfunction(Information)))
} else {
  # If using greyscale, use line types
  if (greyscale == TRUE) {
    p <- ggplot2::ggplot(dataforplot, ggplot2::aes(Theta, transformationfunction(Information), linetype = type.for.titles))
  } else {
    # Else use colors
    p <- ggplot2::ggplot(dataforplot, ggplot2::aes(Theta, transformationfunction(Information), colour = type.for.titles))
  }
}

# Color palettes
# plot chart
# For color palettes from ggsci (and default)
if (color.palette == "" | color.palette == "npg" | color.palette == "aaas" | color.palette == "nejm" | color.palette == "lancet" | color.palette == "jama" | color.palette == "jco" | color.palette == "D3" | color.palette == "locuszoom" | color.palette == "igv" | color.palette == "uchicago" | color.palette == "startrek" | color.palette == "tron" | color.palette == "futurama") {
  # Additional palettes with ggsci
  if (color.palette == "npg") {
    p <- p + ggsci::scale_color_npg()
  } else {
  if (color.palette == "aaas") {
    p <- p + ggsci::scale_color_aaas()
  } else {
  if (color.palette == "nejm") {
    p <- p + ggsci::scale_color_nejm()
  } else {
  if (color.palette == "lancet") {
    p <- p + ggsci::scale_color_lancet()
  } else {
  if (color.palette == "jama") {
    p <- p + ggsci::scale_color_jama()
  } else {
  if (color.palette == "jco") {
    p <- p + ggsci::scale_color_jco()
  } else {
  if (color.palette == "jama") {
    p <- p + ggsci::scale_color_jama()
  } else {
  if (color.palette == "D3") {
    p <- p + ggsci::scale_color_d3()
  } else {
  if (color.palette == "locuszoom") {
    p <- p + ggsci::scale_color_locuszoom()
  } else {
  if (color.palette == "igv") {
    p <- p + ggsci::scale_color_igv()
  } else {
  if (color.palette == "uchicago") {
    p <- p + ggsci::scale_color_uchicago()
  } else {
  if (color.palette == "startrek") {
    p <- p + ggsci::scale_color_startrek()
  } else {
  if (color.palette == "tron") {
    p <- p + ggsci::scale_color_tron()
  } else {
  if (color.palette == "futurama") {
    p <- p + ggsci::scale_color_futurama()
  } else {
  if (color.palette == "rickandmorty") {
    p <- p + ggsci::scale_color_rickandmorty()
  }}}}}}}}}}}}}}}
  # Else use the color brewer palette
} else {
  p <- p + ggplot2::scale_colour_brewer(palette = color.palette, direction = 1)
}






# Specify line.width
p <- p + ggplot2::geom_line(size = line.width) +
  ggplot2::ggtitle(title) +
  ggplot2::xlab(expression(theta)) +
  ggplot2::ylab(expressionforyaxis) +
  ggplot2::scale_x_continuous(breaks=seq(round(-theta.span), round(theta.span), 1))






    # Info + Reliability
    if (type == "inforxx" | type == "inforeliability" | type == "ir" | type == "IR") {
      p <- p + ggplot2::coord_cartesian(ylim = c(0, max(Information)))
      yaxistransformation.lefttoright <- function(x) {x*max(Information)}
      yaxistransformation.righttoleft <- function(x) {x/max(Information)}
      # If greyscale is true, use linetypes
      if (greyscale == TRUE) {
        p <- p + ggplot2::geom_line(ggplot2::aes(y = yaxistransformation.lefttoright(Reliability), linetype = "Reliability"), size = line.width)
      } else {
        # If not, use colour
        p <- p + ggplot2::geom_line(ggplot2::aes(y = yaxistransformation.lefttoright(Reliability), colour = "Reliability"), size = line.width)
      }
        # Add the secondary axis
      p <-  p + ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(trans = ~yaxistransformation.righttoleft(.), name = "Reliability", breaks = seq(from = 0, to = 1, by = .1)))

    }


    # Info + Standard error
    if (type == "infose" | type == "infoSE" | type == "ise" | type == "IS") {
      maximuminfose <- max(c(max(Information), max(StandardError)))

      # If information is higher than standard error, the secondary axis is a division of the primary
      if (max(Information) > max(StandardError)) {
        yaxistransformation.lefttoright <- function(x) {x*maximuminfose}
        yaxistransformation.righttoleft <- function(x) {x/maximuminfose}
      } else {
        yaxistransformation.lefttoright <- function(x) {x/maximuminfose}
        yaxistransformation.righttoleft <- function(x) {x*maximuminfose}
      }
        if (greyscale == TRUE) {
          # If greyscale
          p <- p + ggplot2::geom_line(ggplot2::aes(y = yaxistransformation.lefttoright(StandardError), linetype = "Standard Error"), size = line.width)
        } else {
          # If not, use colour
          p <- p + ggplot2::geom_line(ggplot2::aes(y = yaxistransformation.lefttoright(StandardError), colour = "Standard Error"), size = line.width)
        }

        p <- p + ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(trans = ~yaxistransformation.righttoleft(.), name = "Standard Error"))
      }






# Adjust manually the y maximum.
if (is.null(y.limits) == FALSE) {
  p <- p + ggplot2::ylim(y.limits)
}

# Add horizontal line
if (is.null(y.line) == FALSE) {

  if (how.many.lines > 2 & y.line.refers.to.secondary.axis == TRUE) {
    #if y.line in reference to secondary axis, then transform y.line value
    y.line <- yaxistransformation.lefttoright(y.line)
  }

  vectorforyline <- rep(y.line, length(Information))
    if (greyscale == TRUE) {
      # If greyscale
      p <- p + ggplot2::geom_line(ggplot2::aes(y = vectorforyline, linetype = name.for.y.line), size = line.width)
    } else {
      # If not, use colour
      p <- p + ggplot2::geom_line(ggplot2::aes(y = vectorforyline, colour = name.for.y.line), size = line.width)
    }

  }





    if (theme == "light") {
      p <- p + ggplot2::theme_light()
    } else if (theme == "dark") {
      p <- p + ggplot2::theme_dark()
    } else if (theme == "minimal") {
      p <- p + ggplot2::theme_minimal()
    } else if (theme == "classic") {
      p <- p + ggplot2::theme_classic()
    } else if (theme == "gray") {
      p <- p + ggplot2::theme_gray()
    } else if (theme == "bw") {
      p <- p + ggplot2::theme_bw()
    } else if (theme == "linedraw") {
      p <- p + ggplot2::theme_linedraw()
    }







    p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = title.size)) +
      ggplot2::theme(text = ggplot2::element_text(size = text.size, family = font.family),
                     axis.text.x=ggplot2::element_text(colour = "black", size = text.size),
                     axis.text.y=ggplot2::element_text(colour = "black", size = text.size),
                     legend.key.width = ggplot2::unit(key.width,"line"),
                     legend.title=ggplot2::element_blank(),
                     legend.text=ggplot2::element_text(size=text.size, family = font.family),
                     legend.position = legend.position
      )



    # To manually change the number of legend columns
      if (legend.columns != "" & legend.columns != "auto" & legend.columns != 0) {
        p <- p + ggplot2::guides(colour=ggplot2::guide_legend(ncol=legend.columns,byrow=TRUE))
      } else {
        # If legend is bottom use one line
        if (legend.position == "bottom" | legend.position == "top") {
          p <- p + ggplot2::guides(colour=ggplot2::guide_legend(nrow = 1,byrow=TRUE))
        } # else do nothing
      }


     if (remove.gridlines == T) {
      p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
    }



    return(p)


}



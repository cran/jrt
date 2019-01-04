#' Plot the category curves for a judge.
#'
#' @description This function returns the Judge Category Curves (JCC) plot from a \code{jrt} object and the judge number. This is a wrapper function and adaptation of the \code{itemplot} function in the package \code{mirt} (Chalmers, 2012). It also uses the plotting functions of the packages \code{directlabels} and \code{ggplot2}.
#'
#' @param jrt.object A object of the \code{jrt} class (created by the function \code{jrt}).
#' @param judge A numerical to indicate which judge(s) to plot. Default is \code{all} which plots all category curves of all judges. Alternatively, a single integer may be used to plot the JCC for one judge, or a vector of integers to plot multiple judges in a faceted plot. Note that, if a (Generalized) Rating Scale Model was used, then judges may have been removed for the model to be fitted.
#' @param labelled A logical to indicate whether the curves should be labelled with boxed labels (\code{TRUE}, default) or whether a legend should be used instead (\code{FALSE}). This uses the package \code{directlabels}. Note that the rendering is slower (and may take more time to show in GUI) when the plot is labelled.
#' @param greyscale A logical to indicate whether to plot in greyscale (\code{TRUE}, default) as opposed to color (\code{FALSE}).
#' @param vertical.labels A logical to indicate whether the labels should be vertically oriented (\code{TRUE}), as opposed to oriented inthe angle of the trace curve (\code{FALSE}, the default).
#' @param title A character title for the plot. By default it is created automatically based on the judge number.
#' @param column.names A character to indicate how a judge should be called (Defaults to \code{"auto"}, which uses what was set in the estimation function \code{jrt}, whose default is \code{"judge"}, but you may use \code{"Rater"}, \code{"Expert"}, etc.). This is used to create automatic titles.
#' @param overlay.reliability A logical to indicate whether to overlay the reliability function of the item (default is \code{FALSE}). If overlayed (\code{TRUE}), the reliability function will be contrast with the category curves by being in color if the category curves are in blackandwhite, and in black dashed if the category curves are in color.
#' @param color.palette A character value to indicate the colour palette to use. Defaults to \code{"D3"} from "ggsci". Use \code{""} for the default of \code{ggplot2}. The palettes are supplied as arguments to \code{ggplot2}. See here for a list of palettes: \url{http://ggplot2.tidyverse.org/reference/scale_brewer.html}. In addition, most palettes from the pacakge \code{ggsci} are available (e.g., \code{"npg"}, \code{"aas"}, \code{"nejm"}, \code{"lancet"}, \code{"jama"}, \code{"d3"}). Use \code{vignette("ggsci")} for details. Make sure there are enough colors in the palette. Alternatively, you can pass a vector of colors.
#' @param category.name.for.legend A character to indicate how to call categories in the legend. Default to \code{"Category"} but for example you may try \code{"Cat."} or even \code{""} to save space.
#' @param name.for.reliability A character to indicate a preferred name for reliability in the legend or labels. Defaults to \code{"auto"}, which adapts to whether labels are used.
#' @param theta.span A numeric indicating the maximum \eqn{\theta}.
#' @param line.width A numeric indicating the width of the trace lines (default is \code{2.5}).
#' @param key.width A numeric to indicate the width of the legend key (default is \code{3}).
#' @param legend.position A character string or vector of coordinates to position the legend key. Defaults to \code{"right"}. Other possibilities include notably \code{"bottom"}.
#' @param legend.columns A numeric to indicate after how many legend key elements to add a line break. Especially useful if using \code{legend.position = "bottom"} if you want line breaks between each key. Defaults to \code{""}, which automatically saves space based on the legend position (line breaks are used if the legend in positioned on the side of the graph).
#' @param theme A character value to indicate the background color theme used by \code{ggplot2}. Defaults to \code{"bw"}. Can be \code{"light"}, \code{"dark"}, \code{"minimal"}, \code{"classic"}, \code{"gray"}, \code{"bw"} or \code{"linedraw"}.
#' @param text.size A numeric value to control the size of the text on the plot.
#' @param title.size A numeric value to control the size of the plot title (defaults to \code{text.size+4}).
#' @param font.family A character value to control the font family used on the graph. Defaults to \code{"sans"}. Other possible values include \code{"serif"} or \code{"mono"}.
#' @param remove.gridlines A logical value to remove the gridlines (default is \code{TRUE}).
#' @param facet.rows A numeric to change the number of rows for faceted plots. Use this one or \code{facet.cols}, not both. Defaults to \code{NULL}, which uses \code{ggplot2}'s automatic layout.
#' @param facet.cols A numeric to change the number of columns for faceted plots. Use this one or \code{facet.row}, not both. Defaults to \code{NULL}, which uses \code{ggplot2}'s automatic layout.
#' @param facet.title.position A character string to indicate the position of the facet titles for faceted plts. Defaults to \code{"top"}, but can be \code{"bottom"}, \code{"left"}, or \code{"right"}.
#' @param precision A numeric to indicate the degree of precision used to plot the category curves. Higher values will increase the accuracy of the graph and make the curves look smoother, but the data generated to plot the graph will be bigger, which will slow down the function. Lower values will do the opposite. Values between \code{10} and \code{100} are recommended, \code{20} is the default and sufficient for most uses.
#' @param debug A logical to report debug messages (used in development).
#' Defaults to \code{FALSE}.
#' @param mirt.object.input A logical allowing to input directly an \code{mirt} object as a \code{jrt.object} argument, even though this should be detected automatically. See \code{mirt} package documentation, and note that this is a secondary use that may lead to inconsistent results at this point.
#' @param item For convenience, this argument, more standard to IRT packages, can be used instead of the \code{judge} argument.
#' @references
#' Chalmers, R., P. (2012). mirt: A Multidimensional Item Response Theory
#' Package for the R Environment. \emph{Journal of Statistical Software, 48}(6), 1-29.
#' \doi{10.18637/jss.v048.i06}
#' @references Myszkowski & Storme (in press). Judge Response Theory? A call to upgrade our psychometrical account of creativity judgments. \emph{Psychology of Aesthetics, Creativity and the Arts.} \doi{10.17605/OSF.IO/9WC34}
#' @return A plot of the category curves.
#' @import directlabels
#' @import mirt
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import ggsci
#'
#' @export
#'
#' @examples
#'
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
#' # JCC of the first judge
#' jcc.plot(fit, 1)
#'
#' # See vignette for more options
#'
jcc.plot <- function(jrt.object,
                     judge = "all",
                     labelled = T,
                     greyscale = F,
                     vertical.labels = F,
                     title = "auto",
                     column.names = "auto",
                     overlay.reliability = F,
                     color.palette = "D3",
                     category.name.for.legend = "",
                     name.for.reliability = "auto",
                     theta.span = 3.5,
                     line.width = .8,
                     key.width = 3,
                     legend.position = "right",
                     legend.columns = "",
                     theme = "bw",
                     text.size = 10,
                     title.size = text.size+4,
                     font.family = "sans",
                     remove.gridlines = T,
                     facet.rows = NULL,
                     facet.cols = NULL,
                     facet.title.position = "top",
                     precision = 20,
                     debug = F,
                     mirt.object.input = F,
                     item = NULL) {




  #-------------------------
  # For time diagnostics
  if (debug == T) {
    start.time <- Sys.time()
    time.point <- 0
  }
  #-------------------------




# If user supplies item instead of judge, replace judge by value for item
if (is.null(item) == FALSE) {
  judge <- item
}


# Detect if input is mirt type
  if (class(jrt.object)[1] == "SingleGroupClass") {
    mirt.object.input <- TRUE
  } else {mirt.object.input <- FALSE}



# Get the mirt object from the jrt object
  if (mirt.object.input == F) {
    mirtobject <- slot(object = jrt.object, name = "mirt.object")
  } else { mirtobject <- jrt.object }


# For mirt input, use prefix item instead of judge by default
if (column.names == "auto" && mirt.object.input == T) {
  column.names <- "Item"
}


# Define color themes



# Set auto name of judges
  if (mirt.object.input == F && column.names == "auto"){
    column.names <- jrt.object@column.names
  } else
    if (mirt.object.input == T && column.names == "auto"){
    column.names <- "Judge"
  }


# Judge names vector
  judgeenumerationfortitle <- paste(judge, sep = ", ", collapse = ', ')



  #-------------------------
  # Print time diagnostics
  if (debug == T) {
    time <- Sys.time() - start.time
    time.point <- round(time.point + 1, 2)
    message(paste0("Time ",time.point, ": ", time))
  }
  #-------------------------


# Create theta data

Theta <- matrix(seq(from = -theta.span, to = theta.span, by = 1/precision))

if (mirt.object.input == FALSE) {
  responsecategoriesside <- jrt.object@response.categories
}




  #-------------------------
  # Print time diagnostics
  if (debug == T) {
    time <- Sys.time() - start.time
    time.point <- round(time.point + 1, 2)
    message(paste0("Time ",time.point, ": ", time))
  }
  #-------------------------



    number.of.items <- mirtobject@Data$nitems

    type <- "reliability"

    # Functions to transform information to get relaibility or SE
    if (type == "information" | type == "Information" | type == "i" | type == "I") {
      type.for.titles <- "Information"
      transformationfunction <- function(information) {
        return(information)
      }
      expressionforyaxis <- expression(Information~~(theta))

    }

    if (type == "reliability" | type == "Reliability" | type == "r" | type == "R") {
      type.for.titles <- "Reliability"
      transformationfunction <- function(information) {
        return(information/(information+1))
      }
      expressionforyaxis <- expression(Reliability~~(theta))
      # Make automatic reliability name
      if (name.for.reliability == "auto") {
        if (labelled == T) {
          name.for.reliability <- "rxx'"
        } else {name.for.reliability <- "Reliability"}
      }
    }

    if (type == "SE" | type == "Standard Error" | type == "se" | type == "s" | type == "stderr" | type == "standarderror") {
      type.for.titles <- "Standard Error"
      transformationfunction <- function(information) {
        return(1/sqrt(information))
      }
      expressionforyaxis <- expression(Standard~~Error~~(theta))
    }



  #-------------------------
  # Print time diagnostics
  if (debug == T) {
    time <- Sys.time() - start.time
    time.point <- round(time.point + 1, 2)
    message(paste0("Time ",time.point, ": ", time))
  }
  #-------------------------


###Initialise lists
    traceline <- list(rep(NULL,number.of.items))
    info <- list(rep(NULL,number.of.items))

  for(i in 1:number.of.items){
    extr.2 <- mirt::extract.item(mirtobject, i)
    info[[i]] <- transformationfunction(mirt::iteminfo(extr.2, c(Theta), total.info = TRUE))
    traceline[[i]] <- as.data.frame(mirt::probtrace(extr.2, Theta))
  }


    #return(traceline)

    traceline.df <- dplyr::bind_rows(traceline)

    number.of.categories <- dim(traceline.df)[2]



    #-------------------------
    # Print time diagnostics
    if (debug == T) {
      time <- Sys.time() - start.time
      time.point <- round(time.point + 1, 2)
      message(paste0("Time ",time.point, ": ", time))
    }
    #-------------------------



# Detect if dichotomous items
if (number.of.categories == 2) {
  dichotomous <- TRUE
} else { dichotomous <- FALSE }

if (dichotomous == TRUE) {
  traceline.df[,1] <- NULL
}

    #return(traceline.df)

    names(traceline) <- paste(column.names, " ",1:length(traceline), sep = "")



    # create item names length based on length of theta provided
    item <- rep(names(traceline),each=length(Theta))

    nameforcategory <- "Category"

    # put them all together into a dataframe
    l.format <- cbind.data.frame(Theta, item, traceline.df)
    names(l.format) <- c("Theta", "Item", paste(nameforcategory, " ", 3:dim(l.format)[2] - 2, sep=""))

    # Appeasing Cran checks (waiting on a better fix...)
    Category <- y <- Item <- NULL


    # wide to long format.
    dataforplot <- tidyr::gather(data = l.format,
                                 key = Category,
                                 value = y, dplyr::contains(nameforcategory))
    dataforplot$Item <- as.character(dataforplot$Item)
    dataforplot$Category <- as.character(dataforplot$Category)


    #-------------------------
    # Print time diagnostics
    if (debug == T) {
      time <- Sys.time() - start.time
      time.point <- round(time.point + 1, 2)
      message(paste0("Time ",time.point, ": ", time))
    }
    #-------------------------

    # Create a function that takes the long format data and append information for item i
    appendinformation <- function(itemnumber, dat) {
      thetaasdata <- as.data.frame(Theta)
      bindthetaandinfo <- cbind(thetaasdata, info[[itemnumber]])
      bindthetaandinfo <- cbind(bindthetaandinfo, as.character(rep(type.for.titles, length(Theta))))
      names(bindthetaandinfo) <- c("Theta", "y", nameforcategory)
      bindthetaandinfo$Category <- as.character(bindthetaandinfo$Category)
      bindthetaandinfo$Item <- rep(paste(column.names, " ",itemnumber, sep=""))
      mergeddata <- suppressMessages(dplyr::full_join(dat, bindthetaandinfo))
      return(mergeddata)
    }
    #



    #-------------------------
    # Print time diagnostics
    if (debug == T) {
      time <- Sys.time() - start.time
      time.point <- round(time.point + 1, 2)
      message(paste0("Time ",time.point, ": ", time))
    }
    #-------------------------

    #
    # Apply to add information
    for (i in 1:number.of.items) {
      dataforplot <- appendinformation(itemnumber = i, dat = dataforplot)
    }

    dataforplot <- stats::na.omit(dataforplot)



# Transform Item vector into factor type with appropriate order
factor.levels.item <- paste0(column.names, " ",1:number.of.items)
dataforplot$Item <- factor(dataforplot$Item, levels=factor.levels.item)


#-------------------------
# Print time diagnostics
if (debug == T) {
  time <- Sys.time() - start.time
  time.point <- round(time.point + 1, 2)
  message(paste0("Time ",time.point, ": ", time))
}
#-------------------------

# Filter based on user input for selected judges

if (length(judge) == 1){
if (judge != "all") {
  dataforplot <- dplyr::filter(dataforplot, as.integer(as.factor(Item))  %in% judge)
}} else {
    dataforplot <- dplyr::filter(dataforplot, as.integer(as.factor(Item))  %in% judge)
}


#
# if (length(judge) != 1 && judge != "all" | length(judge) == 1 && judge != "all") {
#   dataforplot <- dplyr::filter(dataforplot, as.integer(as.factor(Item))  %in% judge)
# }


#-------------------------
# Print time diagnostics
if (debug == T) {
  time <- Sys.time() - start.time
  time.point <- round(time.point + 1, 2)
  message(paste0("Time ",time.point, ": ", time))
}
#-------------------------



    # replace "Category by"
    name.for.category <- category.name.for.legend




    dataforplot$Category <- gsub(pattern = paste(nameforcategory, sep = "", " "), replacement = name.for.category, x = dataforplot$Category)
    dataforplot$Category <- gsub(pattern = type.for.titles, replacement = name.for.reliability, x = dataforplot$Category)

    dataforplot$Category <- as.factor(dataforplot$Category)

    #return(levels(dataforplot$Category))

    #gsub(pattern = nameforcategory, replacement = "", x = )

    #-------------------------
    # Print time diagnostics
    if (debug == T) {
      time <- Sys.time() - start.time
      time.point <- round(time.point + 1, 2)
      message(paste0("Time ",time.point, ": ", time))
    }
    #-------------------------


    if (overlay.reliability == F) {
    dataforplot <- dplyr::filter(dataforplot, Category != name.for.reliability)
    }

   # return(dataforplot)


    if (overlay.reliability == TRUE) {
      expressionforyaxis <- expression(Category~~Probability~~and~~Reliability)
      if (dichotomous == T) {expressionforyaxis <- expression(Probability~~and~~Reliability)}
    } else if (overlay.reliability == F){
      expressionforyaxis <- expression(Category~~Probability)
      if (dichotomous == T) {expressionforyaxis <- expression(Probability)}
    }


    #-------------------------
    # Print time diagnostics
    if (debug == T) {
      time <- Sys.time() - start.time
      time.point <- round(time.point + 1, 2)
      message(paste0("Time ",time.point, ": ", time))
    }
    #-------------------------



    # Set title
    if (dichotomous == TRUE) {
      name.of.curves <- "Item Response Function"
    } else {name.of.curves <- "Category Curves"}

    if(title == "auto"){
      if (length(judge) == 1 && judge == "all") {
        title <- paste(sep="", name.of.curves," for all ", column.names,"s")



      } else {if(length(judge) == 1){
        title <- paste(sep="", name.of.curves," for ",column.names," ", judge) }
        else{title <- paste(sep="", name.of.curves," for ",column.names,"s ", judgeenumerationfortitle)}
      }
      if (overlay.reliability == TRUE) {
        title <- paste(sep="", title, " (with Reliability Function)")
      }
    }



    # Set color palettes

    # plot chart
    if (greyscale == T) {
      p <- ggplot2::ggplot(dataforplot, ggplot2::aes(Theta, y, colour = Category, linetype = Category, group = Category)) +
        ggplot2::scale_color_manual(values = rep("black", 100))
    } else {
      p <- ggplot2::ggplot(dataforplot, ggplot2::aes(Theta, y, colour = Category, group = Category))
      if (length(color.palette)>1) {
        p <- p + ggplot2::scale_colour_manual(values=color.palette)
      } else {
      # For color palettes from ggsci (and default)
      if (color.palette == "" |
          color.palette == "npg" |
          color.palette == "aaas" |
          color.palette == "nejm" |
          color.palette == "lancet" |
          color.palette == "jama" |
          color.palette == "jco" |
          color.palette == "D3" |
          color.palette == "locuszoom" |
          color.palette == "igv" |
          color.palette == "uchicago" |
          color.palette == "startrek" |
          color.palette == "tron" |
          color.palette == "futurama") {
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
      } else {p <- p + ggplot2::scale_colour_brewer(palette = color.palette, direction = 1) #+

        #ggplot2::scale_color_manual(values = ['#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd'])
        }

      }
    }

    #-------------------------
    # Print time diagnostics
    if (debug == T) {
      time <- Sys.time() - start.time
      time.point <- round(time.point + 1, 2)
      message(paste0("Time ",time.point, ": ", time))
    }
    #-------------------------

    p <- p +
      ggplot2::geom_line(size = line.width) +
      ggplot2::ggtitle(title) +
      ggplot2::xlab(expression(theta)) +
      ggplot2::ylab(expressionforyaxis) +
      # geom_hline(aes(yintercept = 0.5)) +
      # scale_color_manual(labels = c("T999", "T888")) +
      #ggplot2::scale_y_continuous(breaks=seq(0,1.1,.1)) + ## change the auto scaling of y?
      ggplot2::scale_x_continuous(breaks=seq(round(-theta.span), round(theta.span), 1))

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


    #-------------------------
    # Print time diagnostics
    if (debug == T) {
      time <- Sys.time() - start.time
      time.point <- round(time.point + 1, 2)
      message(paste0("Time ",time.point, ": ", time))
    }
    #-------------------------



    # To manually change the number of legend columns
     # Only evaluate if legend
    if (labelled == FALSE) {
      if (legend.columns != "" & legend.columns != "auto" & legend.columns != 0) {
        p <- p + ggplot2::guides(colour=ggplot2::guide_legend(ncol=legend.columns,byrow=TRUE))
      } else {
        # If legend is bottom use one line
        if (legend.position == "bottom" | legend.position == "top") {
          p <- p + ggplot2::guides(colour=ggplot2::guide_legend(nrow = 1,byrow=TRUE))
        } # else do nothing
      }
    }




    #-------------------------
    # Print time diagnostics
    if (debug == T) {
      time <- Sys.time() - start.time
      time.point <- round(time.point + 1, 2)
      message(paste0("Time ",time.point, ": ", time))
    }
    #-------------------------


    # theme(
    #   strip.background = element_blank(),
    #   strip.text.x = element_blank()
    # )


    if (length(judge) == 1) {
      if (judge == "all") {
        p <- p + ggplot2::facet_wrap(~Item, nrow = facet.rows, ncol = facet.cols, strip.position = facet.title.position)
        p <- p + ggplot2::theme(strip.text.x = ggplot2::element_text(size = text.size))### Add if condition for facet
      }}

    if (length(judge) > 1) {
      p <- p + ggplot2::facet_wrap(~Item, nrow = facet.rows, ncol = facet.cols, strip.position = facet.title.position)
      p <- p + ggplot2::theme(strip.text.x = ggplot2::element_text(size = text.size))### Add if condition for facet
    }




    # To remove the gridlines
    if (remove.gridlines == T) {
      p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
    }



    #-------------------------
    # Print time diagnostics
    if (debug == T) {
      time <- Sys.time() - start.time
      time.point <- round(time.point + 1, 2)
      message(paste0("Time ",time.point, ": ", time))
    }
    #-------------------------



    if (labelled == T) {
      p <- p + ggplot2::guides(fill=FALSE, linetype = FALSE)
      # For option that verticalises text in labels
   #   if(vertical.labels == T){angled.boxes <-list(angled.boxes, rot=90)}

      custom.draw.rects <- list(directlabels::dl.trans(box.color = "black", fill = "white", fontface="bold", alpha = 1, cex = .4),"draw.rects", fontfamily = font.family)
      angled.boxes <-
        list("angled.boxes", custom.draw.rects,
             cex=1, color = "blue")#Controls size of text in labels
      outputplot <- directlabels::direct.label(p, method = angled.boxes, debug=F)
    } else {outputplot <- p }


    if (dichotomous == T && overlay.reliability == F) {
      outputplot <- p + ggplot2::theme(legend.position="none")
    }

    #-------------------------
    # Print time diagnostics
    if (debug == T) {
      time <- Sys.time() - start.time
      time.point <- round(time.point + 1, 2)
      message(paste0("Final time ",time.point, ": ", time))
    }
    #-------------------------

  return(outputplot)



}





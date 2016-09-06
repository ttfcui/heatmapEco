############################################################
####  R heatmap script
####  Author: TC
############################################################

# %< Native R function to generate residuals of the dep. var
# after regression on controls and FE (UNDER CONSTRUCTION)
makeResid <- function(data, x, y, time, controls, absorb) {
    # Procedure: Use LFE to within transform y, x and controls
    # Then run partitioned regression of y, x on controls
    # Returns y, x residuals for heatmap plotting
}

# %>

# TODO: Add compatibility for days.
# %< Time conversion closure for flexible parsing of x axis arguments
#' Parse time variable to machine-readable formats.
#'
#' \code{timeConv} is a closure allowing flexible parsing of a time column
#' into an appropriate format for the heatmap x-axis.
#'
#' To increase ease of use, the \code{heatmap} package can parse
#' date and time strings, so long as the format of the string is specified.
#' Furthermore, the time interval represented by each block in the heatmap
#' can be set using \code{timeConv}. The function transforms the string
#' into a date class using native R functions, then converts it into integers
#' so the plot can be built properly.
#'
#' @param period A string indicating the time interval. One of "yearmon",
#' "year" or "yearqtr."
#' @param ft R date format string as in as.Date.
#' @return Function that converts string of format ft into an internal numeric
#' representing period.
#' @importFrom zoo as.yearmon as.yearqtr
#' @export
#' @examples
#' timeConv("yearmon")(c("2008-01-01", "1934-12-15"))
#' \dontrun{
#' timeConv("yearmon")("1993") # Have to put month there as well, even if dummy
#' timeConv("yearmon", "%d%b%Y")("jan112011") # wrong formatting order
#' }
timeConv <- function(period, ft="") {
    if (!is.null(period)) {
        if (period == "yearmon") {
            function(x) 12 * as.numeric(as.yearmon(x, format=ft))
        } else if (period == "year") {
            function(x) as.numeric(format(as.yearmon(x, format=ft), "%Y"))
        } else if (period == "yearqtr") {
            function(x) 4 * as.numeric(as.yearqtr(x, format=ft))
        }
    } else {
        as.numeric
    }
}

# %>

# %< Native R function for data into some grouped function.
# TODO: Fix binning problem for discrete but not time variables
#' Grouping data into form suitable for heatmap
#'
#'\code{compilation} groups primitive data into a heatmap-suitable form,
#'i.e. aggregation along the time/index and factor/quantile axes.
#'
#' @details
#' This function first searches for the time/index column and retains it.
#' Then it drop duplicates of ID, either binning the IDs into quantiles
#' (analogous to Stata's xtile function) or preserving a factor variable
#' to be converted later.
#'
#' This part of the code is run in Stata/Python for their respective ports.
#' One difference is that the aggregation function allowed in R can be much
#' more creative than what are admissible in Stata.
#' @param data A data.frame, or data.table.
#' @param y The factor/quantile column (y-axis on heatmap).
#' @param Ident The individual identifier, creating unique key with time.
#' @param z The dependent variable plotted (variation in heatmap)
#' @param index The time/index column (x-axis in heatmap)
#' @param t.sort If instrument is time-varying, the time period where sorting
#' is made.
#' @param N Number of quantiles on y-axis.
#' @param not.q Default \code{FALSE}, set to \code{TRUE} if y is character,
#' at which point the column will be set to a factor later.
#' @param t.func Output from \code{\link{timeConv}}.
#' @param q.probs Optional, set quantiles at which instrument bins are cut.
#' @param grp.func Aggregation function, default is \code{mean}.
#' @param q.wgt Weighted quantiles using column from the dataset.
#' @return Collapsed dataset of same class as input, with columns "time", "y",
#' "quantile."
#' @keywords internal
compilation <- function(data, y, Ident, z, index, t.sort=NA, N, not.q=F,
                        t.func, q.probs, grp.func = mean, q.wgt = NA)
    {

    # Pctile and Xtile ripped from Matthieu Gomez's statar package
    pctile <- function (x, probs = c(0.25, 0.5, 0.75), w = NA, na.rm = FALSE)
    {
        if (is.na(w)) {
            quantile(x = x, type = 2, probs = probs, na.rm = na.rm)
        }
        else {
            if (anyNA(x) | anyNA(w)) {
                if (na.rm) {
                    na <- is.na(x) | is.na(w)
                    x <- x[!na]
                    w <- w[!na]
                }
                else {
                    stop("Missing values not allowed when na.rm is FALSE",
                      call. = FALSE)
                }
            }
            order <- order(x)
            cumsum <- cumsum(w[order])
            n <- cumsum[length(cumsum)]
            index <- n * probs
            j <- floor(index)
            low <- x[order[pmin(length(x), .bincode(j, c(-Inf, cumsum)))]]
            high <- x[order[pmin(length(x), .bincode(j + 1, c(-Inf,
                cumsum)))]]
            ifelse(j == index, 0.5 * low + 0.5 * high, high)
        }
    }

    xtile <- function (x, N, probs, w)
    {
        if (!missing(N)) {
            probs <- seq(1 / N, 1 - 1 / N, length = N - 1)
        }
        if (!missing(probs)) {
            cutpoints <- pctile(x, probs, w = w, na.rm = TRUE)
        }
        .bincode(x, c(-Inf, cutpoints, +Inf), include.lowest = TRUE)
    }

    colnames(data)[which(colnames(data) == z)] <- "z"

    # Regrouping the index variable if continuous
    if (is.na(t.func("1960-01-01"))) {
        indexOld <- data[[index]]
        data$index <- cut(indexOld, eval(indexLab <- quantile(
                          indexOld, seq(0, 1,
                          length.out = floor(1.375 * N) + 1))), indexLab)
    } else colnames(data)[which(colnames(data) == index)] <- "index"
    data$index <- t.func(data$index)

    # Building a table with unique id index and the instrument/group
    # Also allows sorting for time-varying variables
    data <- data[order(data$index),]
    measures <- if (Ident != y) c(Ident, y) else c(y)
    if (is.na(t.sort)) {
        quantile <- subset(data[!duplicated(data[[Ident]]),], T, measures)
    } else {
        quantile <- subset(eval(quantile <- data[data$index == t.sort,])[
                           !duplicated(quantile[[Ident]]),], T, measures)
    }
    # Build the instrument quantiles, or retain as groups.
    quantile$quantile <- if (not.q == F) xtile(quantile[[y]],
                         N=N, probs=q.probs, w=q.wgt) else quantile[[y]]
    # Merge and aggregate.
    `%>%` <- dplyr::`%>%`
    data.frame(dplyr::inner_join(data, quantile, by=Ident)) %>%
        dplyr::group_by(quantile, index) %>% dplyr::summarise(z= grp.func(z))
}

# %< Setup x axis
# TODO: Make sure date formatting works for multiple types of time data.
# TODO: Does split mean "tick every n units" or "create n ticks"?
#' Setting up X axis of heatmap
#'
#' \code{setupX} adds bells and whistles to aspects of the X axis.
#'
#' Some of the actions done include spacing out labels; adjusting bounds given
#' the actual minima and maxima of the time variable; and plotting vertical
#' lines that show when policy changes occur.
#'
#' @param t.col Input column of time values.
#' @param split If set, determines the number of gaps between each X-axis
#' label. Default spacings exist for each time type specified by
#' \code{\link{timeConv}}.
#' @param period Same as argument \code{period} from \code{\link{timeConv}}.
#' @param pol.break Vector of character/integers expressing time in the same
#' format specified by \code{\link{timeConv}}.
#' @return List of ggplot2 formulae to be parsed by \code{\link{heatmapBuild}}.
#' @importFrom zoo as.yearmon as.yearqtr
#' @keywords internal
setupX <- function (t.col, split, period, pol.break) {

    if (any(is.na(t.col))) {
        stop("Formatting failed in X: cannot accept NA values!")
    }

    tval <- unique(t.col); splCrit <- missing(split)
    if (!is.null(period)) {
        if (period == c("yearmon") ) {
            div <- function(x) as.yearmon(x / 12); if (splCrit) split <- 6
        } else if (period == "year") {
            div <- identity; if (splCrit) split <- 2
        } else if (period == "yearqtr") {
            div <- function(x) as.yearqtr(x / 4); if (splCrit) split <- 6
        }
    } else {
        div <- identity
        if (splCrit & eval(space <- floor(length(tval) / 6)) == 0) {
            split <- length(tval)
        } else if (splCrit) {
          split <- space
        } else split <- floor(length(yval) / as.numeric(split))
    }

    t.break <- tval[seq(1, length(tval), split)]
    t.lab <- as.character(div(t.break))

    # Converting human-readable time breaks into numeric x axis
    pol.intercept <- timeConv(period)(
                     gsub("^\\s+|\\s+$", "", pol.break)) - 0.5
    list(tick=bquote(scale_x_continuous(breaks=.(t.break), labels=.(t.lab))),
         xl=bquote(coord_cartesian(xlim=c(.(min(t.col) + .1), .(max(t.col))))),
         int=bquote(geom_vline(xintercept=.(pol.intercept), linetype="4132",
                      col="gray40", size=.9)))
}
# %>

# %< Setup y axis
# TODO: Sorting options for factor variables.
#' Setting up Y axis of heatmap
#'
#' \code{setupY} adds bells and whistles to aspects of the X axis.
#'
#' Actions currently possible include spacing out of labels and indication of
#' whether the Y-axis are factors (in which case every row is labelled).
#'
#' @param y.col Input column of categorical/quantile values.
#' @param split If set, determines the number of gaps between each Y-axis
#' label. The default spacing is to have 5 evenly-spaced labels.
#' @param factor.ax If \code{TRUE}, interprets column as a factor.
#' @return List of ggplot2 formulae to be parsed by \code{\link{heatmapBuild}}.
#' @keywords internal
setupY <- function (y.col, split, factor.ax=F) {

    if (any(is.na(y.col))) {
        stop("Formatting failed in Y: cannot accept NA values!")
    }

    if (factor.ax == F) {
        yval <- unique(y.col); splCrit <- missing(split)
        if (splCrit & eval(space <- floor(length(yval) / 4)) == 0) {
            split <- length(yval)
        } else if (splCrit) {
          split <- space
        } else split <- floor(length(yval) / as.numeric(split))
        y.break <- yval[seq(0, length(yval), split)]
        y.lab <- as.character(y.break)
        list(tick=
             bquote(scale_y_discrete(breaks=.(y.break), labels=.(y.lab))),
             ylab="Quantile of instrument")
    }
    else {
        list(tick=bquote(scale_y_discrete()), ylab="")
    }
}
# %>

# %< Setup fill specs
# TODO: Fluctuating fill area based on some statistic.
#' Setting up fill aesthetic of heatmap
#'
#' \code{setupFill} adds bells and whistles to the fill aesthetic (in ggplot
#' terms).
#'
#' Heatmap accepts two colour gradient; a divergent red-blue palette
#' and a sequential red-yellow-pale blue palette. By default, the blue palette
#' has unequal variation biased to lower quantiles in order to accentuate
#' "Ashenfelter dips". Apart from the default palettes, other options are
#' adjustable.
#'
#' @param fill.col Input column of time values.
#' @param outliers If set, will add two low-luminosity colours to both ends of
#' the gradient, intended to help perceive observations exceeding the
#' 1.5*IQR heuristic.
#' @param zlab Label for the fill variable to be placed above the legend.
#' @param count Vector of character/integers expressing time in the same
#' format specified by \code{\link{timeConv}}.
#' @param custom Percentiles at which the principal palette colours are placed.
#' @return List of ggplot2 formulae to be parsed by \code{\link{heatmapBuild}}.
#' @keywords internal
setupFill <- function(fill.col, outliers, zlab, count=F, custom) {

    zlab <- gsub("\\s", "\n", zlab)

    # Outlier visualization
    Fn <- ecdf(fill.col)
    bottom <- quantile(fill.col, .25, na.rm=T)
    top <- quantile(fill.col, .75, na.rm=T)
    IQR <- top - bottom

    outlier <- function(col, quote, val) {
        tryCatch({list(out=col, cut=Fn(quote + val))},
                 error = function (x) list(out=col, cut=val))
    }

    ccol <- if (count == T) list(b="#D6E9FF", t="#D7301F") else
            list(b="#25779D", t="#DA2022")

    bottom <- if (count == T & missing(custom)) (
                    outlier("#F5F0FA", 1e-5, 1e-5)
                  ) else if (Fn(bottom - 1.5 * IQR) > 0 & !missing(outliers)) (
                    outlier("#1D5F7D", bottom, - 1.5 * IQR)
                  ) else outlier(ccol[["b"]], "none", 0)
    top <- if (Fn(top + 1.5 * IQR) < 1 & !missing(outliers)) (
                    outlier("#881314", top, 1.5 * IQR)
                  ) else outlier(ccol[["t"]], "none", 1)

    # Colour gradient
    cgrad <- if (count == T) c(rep(bottom[["out"]], 4), # F1EEF6
              "#FCFCFC", "#FFF7BC", "#FEC44F", "#D95F0E", top[["out"]]) else
              c(bottom[["out"]], "#25779D", "#6FA6C0", "#BBD7E4", "#F2F2F2",
                "#F2C1B5","#E66C65", "#DA2022", top[["out"]])

    # Colour distribution (customizable)
    if (count != T & missing(custom)) {
        custom <- c(.15, .3, .51, .677, .843)
    } else if (count == T & missing(custom)) {
        custom <- rep(bottom[["cut"]], 5) + c(0, 0, 1e-5,
                  .25*(1-bottom[["cut"]]), .5*(1-bottom[["cut"]]))
    }
    cval <- c(0, bottom[["cut"]], custom, top[["cut"]], 1)
    list(zlab=zlab,
         fill=bquote(scale_fill_gradientn(colours=.(cgrad), values=.(cval),
                     na.value="gray77")))
}
# %>

# %<
#' Construct and export heatmap
#'
#' \code{heatmapBuild} compiles the heatmap given settings.
#'
#' This is an internal function. Its arguments are the collapsed data and
#' elements of the list outputs from \code{\link{setupX}} or
#' \code{\link{setupFill}}
#'
#' @param data The collapsed dataset.
#' @param xtick Placement of ticks and labels on the X-axis, set with setupX.
#' @param xl X-axis boundaries, set with setupX.
#' @param int Shaded vertical lines (x-intercepts), set with setupX.
#' @param ytick Placement of ticks and labels on the Y-axis, set with setupY.
#' @param factor.ax If \code{TRUE}, interprets variable column as a factor for
#' aggregation and options.
#' @param fill Colour gradient derived from palette, set with setupFill.
#' @param zlab Label of dependent variable for legend, set with setupFill.
#' @param save File to which graph is exported using ggsave.
#' @import ggplot2
#' @keywords internal
heatmapBuild <- function(data, xtick, xl, int, ytick, ylab, factor.ax,
                          fill, zlab, save)
{
    tryCatch(attachNamespace("ggplot2"), error = function(x) eval(x))
    m <- as.formula
    ylbf <- levels(factor(data$quantile))
    O <- if (factor.ax == T) rev else identity

    ggplot(data, aes(x=index, y=factor(quantile, levels=O(ylbf)), fill=z)) +
        geom_tile(colour="gray92") + m(fill) + theme_classic() +
        theme(axis.line=element_blank(), title=element_text(size=12)) +
        labs(x="", y=ylab, fill=zlab) +
        m(int) + m(xtick) + m(ytick) + m(xl)
    if (!missing(save)) ggsave(save, width=11, height=8)
}

# %>

# %<
# TODO: Move all the N stuff into formula, weights out.
#' Formula parsing function for heatmap.
#'
#' \code{heatmap} needs to interpret a particular kind of formula to understand
#' which columns contain data to be added on the axes, and so forth.
#' \code{parseHeatform} handles this.
#'
#' Internal function. When inputted, "CrS" could be replaced by any other name,
#' so long as the formula looks like it has a named function within.
#' @param relation Formula of the form Y ~ CrS(X,ID,[w]):i[(t)]. Y is the
#' dependent variable to be plotted in colour. The arguments of CrS are,
#' left to right, the instrument/factor variable, the cross-sectional identifier
#' which should form a unique key along with time, and a weighing variable
#' for each observation. i is the index/time column, and if (t) is specified i
#' is assumed to be time varying, with data being sorted on values of i at
#' time t.
#' @return A list of parsed arguments for entry in \code{\link{heatmapEco}}.
#' @keywords internal
#' @examples
#' \dontrun{
#' parseHeatform(Y ~ CrS(x,ID):time) # Sorting on time-invariant instrument
#' # Sorting on time varying instrument at time 2001
#' parseHeatform(Y ~ CrS(x,ID):time(2001))
#' # The following will not parse correctly
#' parseHeatform(Y ~ (x,ID):time)
#' parseHeatform(Y ~ CrS(x,ID):.)
#' }
parseHeatform <- function(relation) {
  stopifnot(class(relation) == "formula")
  all.args <- unlist(strsplit(deparse(relation), "\\s*~\\s*|:|\\(|\\)"))
  crs.args <- unlist(strsplit(all.args[3], "\\s*(,|=)\\s*"))
  list(cross=crs.args, z.t.args=all.args[c(1,5,6)])
}
# %>

# %<
#' Master heatmap function.
#'
#' \code{heatmapEco} takes in (panel) data, aggregates it, and customizes a
#' heatmap output according to function options.
#'
#' @details
#' While heatmap can perform much of the data cleaning, it is still important
#' to clean the data to the point where the values of the instrument/factor
#' are all within one column, and to unite date columns into one time format
#' if time is to be used as an axis.
#'
#' \code{heatmapEco} can still be called to handle already cleaned data stored
#' outside of R: use the fname and xq arguments. Those two arguments also
#' facilitate compatability with Stata and Python.
#' @param relation Parsed formula. See \code{\link{parseHeatform}} for details.
#' @param data data.frame or data.table object. Should have columns
#' corresponding to the X, Y and fill axes at the minimum.
#' @param xq The name of the instrument/factor variable, if importing an
#' external cleaned file. Mostly relevant for other programs calling this
#' package in a script.
#' @param controls Controls from which the dependent variable and the instrument
#' are residualized (NOT IMPLEMENTED).
#' @param absorb Factor variables from which the dependent variable and the
#' instrument are residualized (NOT IMPLEMENTED).
#' @param N Number of quantiles to split a continuous instrument.
#' @param q.probs Vector of probabilities through which custom cutpoints could
#' be used for dividing the instrument.
#' @param grp.func Aggregation function over each
#' time/index X instrument/factor bin. Most summary functions in R, mathematical
#' transformations and compositions thereof are admissible.
#' @param t.fmt If a time axis is to be used, the string formatting in the
#' time column. Ensure all values in the column have the same formatting.
#' @param t.per Time interval - should each division of the X-axis represent
#' a month, quarter or year?
#' @param pol.break A vector of strings in the same formatting as the X axis
#' that lists where vertical lines should be added.
#' @param outliers If \code{TRUE}, expands colour palette to emphasize
#' outliers. See \code{\link{setupFill}} for more details.
#' @param factor.ax If \code{TRUE}, interprets variable column as a factor for
#' aggregation and options.
#' @param count If \code{TRUE}, uses an alternative palette for count data.
#' See \code{\link{setupFill}} for more details.
#' @param split.x Custom value declaring spacing between X axis ticks.
#' @param split.y Custom value declaring spacing between Y axis ticks.
#' @param custom.f Vector of percentages where the palette composing
#' the colour gradient should be placed. Most useful for data with
#' many zero values.
#' @param zlab Label for the fill variable to be placed above the legend.
#' @param fname Location of an external, collapsed CSV dataset to be entered into R for plotting.
#' @param save File to which graph is exported using ggsave.
#' @return Collapsed data.frame with columns "time", "y", "quantile," to be
#' reused for tweaking purposes.
#' @export
#' @examples
#' # Generate a rather silly panel
#' library(data.table)
#' b1 <- rep(rnorm(3e3, 0, 0.8), 10)
#' b2 <- rep(rnorm(3e3, 0, 0.2), 10)
#' test <- data.table(id=rep(1:3e3, 20), X=c(b1, b2), Y=rnorm(6e4,
#'                    c(- b1 / 2, -2.5 * b2), c(rep(1, 3e4), rep(5, 3e4))))
#' test$t <- as.vector(sapply(1:20, function(x) rep(x, 3e3)))
#'
#' # Plot using heatmap, dividing instrument "X" in percentiles
#' heatmapEco(Y ~ CrS(X,id):t, test, N=100,
#'                     pol.break=c(11,16), outliers=TRUE)
heatmapEco <- function(relation, data, xq, controls=NULL, absorb=NULL,
                       N=0, q.probs, grp.func=mean, t.fmt="%Y",
                       t.per="year", factor.ax=F, pol.break="", outliers,
                       count=F, split.x, split.y, custom.f,
                       zlab="Mean Outcomes", fname, save)  {

    time.axis <- timeConv(t.per, t.fmt)
    if (missing(fname)) {
        if (!is.null(c(controls, absorb))) {
            # data <- residuals(...)
        }
        cross <- parseHeatform(relation)[["cross"]]
        oth <- parseHeatform(relation)[["z.t.args"]]

        collapsed <- compilation(data, cross[1], cross[2], oth[1],
                                 oth[2], oth[3], N, factor.ax, time.axis,
                                 q.probs, grp.func, cross[3])
    } else {
         tryCatch({
             collapsed <- read.csv(fname, stringsAsFactors=F)
             colnames(collapsed)[which(colnames(collapsed) == xq)] <- "quantile"
             collapsed$index <- time.axis(collapsed$index)
         }, error = function(x) {
            stop("Compatibility check failed!")
         })
    }
    x.opt <- setupX(collapsed$index, split.x, t.per, pol.break)
    y.opt <- setupY(collapsed$quantile, split.y, factor.ax)
    f.opt <- setupFill(collapsed$z, outliers, zlab, count, custom.f)
    heatmapBuild(collapsed, x.opt[["tick"]], x.opt[["xl"]], x.opt[["int"]],
                 y.opt[["tick"]], y.opt[["ylab"]], factor.ax, f.opt[["fill"]],
                 f.opt[["zlab"]], save)
    collapsed
}

# %>

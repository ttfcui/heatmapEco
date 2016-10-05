{smcl}
{* *! version 1.0.2  23mar2016}{...}
{vieweralsosee "" "--"}{...}
{viewerjumpto "Syntax" "heatmap##syntax"}{...}
{viewerjumpto "Description" "heatmap##description"}{...}
{viewerjumpto "Installation" "heatmap##installation"}{...}
{viewerjumpto "Options" "heatmap##options"}{...}
{viewerjumpto "Examples" "heatmap##examples"}{...}
{viewerjumpto "Author" "heatmap##author"}{...}
{viewerjumpto "Acknowledgements" "heatmap##acknowledgements"}{...}
{title:Title}

{phang}
{bf:heatmap} {hline 2} Heatmap plotter suited for economic data


{marker syntax}{...}
{title:Syntax}

{p 8 15 2}
{cmd:heatmap} {it:{help varname:z}} {it:{help varname:y}} {it:{help varname:x}} 
{if}
{weight} {cmd:,}
{cmd:id(}{it:{help varname:varname}}{cmd:)}
{cmd:save(}{it:{help varname:varname}}{cmd:)}
[{it:options}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Main}
{synopt:{opth id(varname)}} cross-sectional identifier,  which should form a unique key
by itself or along with time {p_end}
{synopt:{opt save(filename)}} file to which graph is exported after compilation {p_end}
{synopt:{opt n:quantiles(#)}} number of quantiles into which a 
continuous covariate or instrument is divided, if applicable {p_end}

{syntab:Aggregation}
{synopt:{opt grpf:unc(funcname)}} aggregation function over each bin {p_end}
{synopt:{opt tper:iod(string)}} format X axis values to represent a certain 
time interval {p_end}
{synopt:{opt tsort(string)}} sorting time-varying {it:y} on values at one time period {p_end}
{synopt:{opth control:s(varlist)}} residualize {it:z, y} on controls {p_end}
{synopt:{opth a:bsorb(varname)}} residualize {it:z, y} on factor variables {p_end}
{synopt:{opth probs(numlist)}} percentiles defining custom quantiles for binning {p_end}
{synopt:{opt keep:agg}} saves the aggregated data as a CSV for
further use {p_end}

{syntab:Visualization}
{synopt:{opt polbr:eak(strings)}} string list in the same formatting as the X axis
that lists where vertical lines should be added{p_end}
{synopt:{opt xlab:el(string)}} declares spacing between X axis ticks {p_end}
{synopt:{opt ylab:el(string)}} declares spacing between Y axis ticks {p_end}
{synopt:{opth customf(numlist)}} percentiles defining colour gradient composition
for gradient construction {p_end}
{synopt:{opt count}} use alternative palette for count data {p_end}
{synopt:{opt out:liers}} expands colour palette to emphasize outliers {p_end}
{synopt:{opt isf:actor}} interprets {it:y} as a factor variable for
visualization {p_end}
{synopt:{opt zti:tle(string)}} label for the fill variable to be placed
above the legend {p_end}

{synoptline}
{p2colreset}{...}
{p 4 6 2}
{cmd:aweight}s, {cmd:fweight}s, {cmd:iweight}s, and {cmd:pweight}s are
allowed; see {help weight}.{p_end}


{marker description}{...}
{title:Description}

{pstd} {cmd:heatmap} takes in (panel) data, aggregates it, and customizes a
heatmap output according to function options.

{pstd} A heatmap is a two-dimensional data visualization, plotted in matrix form, 
using colour to represent magnitude. An aggregated dependent variable {it:z} 
is plotted over axes {it:y, x}, which are factor variables or 
discretizations of continuous variables. It is an effective way to plot
high-dimensional data and compare levels of a variable between groups.

{pstd} This is the Stata interface for the R package {bf:heatmapEco}. While
figure production still takes place in R, this program handles the binning and
aggregation of {it:z}. Most commands and options in the R package can be
declared in Stata, with the exception of complex aggregation functions.

{pstd} Specifically, the work pipeline involves Stata exporting an aggregated 
CSV, R loading it and associated arguments, then plotting the file using 
{bf:heatmapEco}. See {it:{help heatmap##installation:installation}} for details.


{marker installation}{...}
{title:Installation}
{pstd} Ensure you have installed the following files:

{p 10 13 2} 1) R executable of minimum version 3.0.0; with {bf:heatmapEco},
and its dependent packages, already installed.

{p 10 13 2} 2) The R package {bf:heatmapEco}, along with its dependent R packages. A file that comes with the Stata files, heatmapSetup.R, can do this step,
but read the caveat below.

{pstd} It is advised that, as soon as R has been installed to a directory,
that directory is added to the PATH environment variable. Different operating
systems use different procedures for this, so consult a guide just for
your OS.

{pstd} That said, {cmd:heatmap} can run successfully without adding
directories to PATH. Instead, when Stata is booted, you must define the global
"linkdir" as the directory in which R is installed. However, heatmapSetup.R
will not execute successfully without adding directories to PATH.

{pstd} A sample R installation directory for windows is
{it:C:\Program Files\R\R-3.3.0\bin}. A sample directory for Unix systems
(Mac, Linux) is {it: /usr/local/bin} or {it: /usr/bin}. For Windows, the
directory {it:does} change with each updated version of R.

{pstd} Install the R packages by calling heatmapSetup.R from the command line,
with one argument: the full directory to where {bf:heatmapEco}'s source file,
which ends in ".tar.gz"., was downloaded.
To test the installation works, try running the example given in this manual.


{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
{opth id(varname)} specifies the cross-sectional identifier in the data,
which is either unique or is unique joint with a time variable. While this
always needs to be declared, it is a superfluous option when aggregation
is not involved and should be the same variable as {it: y} or {it: x}.

{phang}
{opt save(filename)} declares the heatmap file name. Specify extensions
in the filename. All file types supported by the R package {bf:ggplot2},
including PNG, JPEG and PDF, can be used.

{phang}
{opt n:quantiles(#)} sets the number of even quantiles into which
a continuous covariate is split. Weighted quantiles can be set using
the weights options in syntax. That is the weight option's {it:only} use.


{dlgtab:Aggregation}

{phang}
{opt grpf:unc(funcname)} indicates which summary statistic is to be used
when aggregating the dependent variable within a bin. Currently all summary
functions allowed by {help collapse:collapse} are available, which is less than
what is possible with the R package. Default function is {bf:mean}.

{phang}
{opt tper:iod(string)} indicates the time interval represented by a X axis
time bin. Can be one of {it:year, quarter,} a year-month pair {it:yearmon}, where where the third shows year and month. The default is to turn off this option
and interpret values on the X axis as arbitrary numeric values.

{phang}
{opt tsort(string)} sorts a continuous covariate by its values at {it:one} 
binned value on the X-axis. It illustrates research designs where
the researcher has a time-varying independent variable and wishes to
arrange the heatmap in the variable's values at a certain point in time.
Use this option with caution: the heatmap is misleading if selection
effects exist prior to the time period.

{phang}
{opth control:s(varlist)} lists which variables are used to residualize
the plotted {it:z, y} variables, in the Frisch-Waugh-Lovell manner. This
procedure is nearly identical as that in {bf:binscatter}, by Michael Steppner. 

{phang}
{opth a:bsorb(varname)} lists which factor variables are used to residualize
the plotted {it:z, y} variables, using the within transformation supported
by {help areg:areg}. Currently, factor variables are not supported as
arguments. This procedure is nearly identical as that in {bf:binscatter},
by Michael Steppner. 

{phang}
{opth probs(numlist)} inputs a list of percentiles used to generate
the quantiles for a continuous covariate. Use as you would the
{help pctile:_pctile} function in base Stata, except enter numbers from 0 to 1
instead of percentages.

{phang}
{opt keep:agg} preserves the outputted CSV with aggregated data, whereas
the default is to delete the file after the R script is complete. Because
the aggregation may be time-consuming, if this option continues to be
turned on after the aggregated data are saved, new heatmaps will be created
without rerunning any of the aggregation.


{dlgtab:Visualization}

{phang}
{opt polbr:eak(strings)} parses a list of strings that will indicate where
dotted vertical lines are drawn, signifying policy breaks. Ensure they are
in the same format as what was indicated in {bf:tperiod}, i.e. if {it:yearmon},
write dates in the form {it:Mar 2016, 2016-03-01} and so on.


{phang}
{opt xlab:el(string)} places a number of equally spaced ticks onto
the X axis, i.e. calling xlabel(4) will yield 5 ticks. Default is 6 equal
ticks, or a tick every few periods if the axis is declared as a time axis.


{phang}
{opt ylab:el(string)} places a number of equally spaced ticks onto
the Y axis. Default is 4 equal ticks. This option is ignored if
{bf:isfactor} is on.

{phang}
{opth customf(numlist)} indicates which values on the empirical CDF
are the cutoff points for the paletete composing the colour gradient.
composing the colour gradient should be placed.

{phang}
{opt count} uses the alternative palette for count data, which replaces
a divergent red-blue palette with a "semi-sequential" yellow-orange one,
with a lavender shade wherever the aggregated value is zero. 

{phang}
{opt out:liers} expands the colour palette to emphasize outliers, where
values above or below the 1.5 IQR heuristic are significantly darker than other
cells.

{phang}
{opt isf:actor} interprets {it: y} as a factor variable for aggregation and
options. Labels for these factors cannot be hidden, and they can be sorted
according to another variable's values (TODO).

{phang}
{opt zti:tle(string)} changes the label for the fill variable to be placed 
above the legend. It is advised to keep this label short, at no more than five
words.


{marker examples}{...}
{title:Examples}

{com}. global linkdir "C:/Program Files/R/R-3.2.4revised/bin/" // Windows ex.
{com}. global linkdir /usr/local/bin/ // Mac ex.

{com}. sysuse xtline1, clear
{com}. gen month = mofd(day)
{com}. tostring person, gen(id)
{com}. heatmap calories id month, id(id) isfactor tperiod(yearmon) ///
> save(test.pdf)
{res}

The following example uses a cleaned dataset from Project Tycho, which
can be downloaded from the Github project page for heatmapEco.

{com}. use MEASLES_Incidence_long, clear // Project Tycho data in slides
{com}. * Getting rid of state-years with only NAs
{com}. bys year variable: egen nonmiss = count(value)
{com}. drop if nonmiss==0
{com}. heatmap value variable year, id(variable) isfactor count grpfunc(sum) ///
> tper(year) polbr(Jan 1963) out xlab(10) zti("Measles Incidence (p100,000)") ///
> save(measlesRep.pdf)
{res}

{marker author}{...}
{title:Author}

{pstd}Tom Cui{p_end}
{pstd}Tom.Cui@chicagobooth.edu{p_end}

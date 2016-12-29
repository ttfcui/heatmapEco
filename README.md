# heatmapEco
Package to build heatmaps for economic analysis, v. 0.4.

## About
**heatmapEco** is a R package that handles both the creation of heatmaps - colour-coded, high-dimensional data visualizations -
as well as any data munging needed to produce the visualization.

This package is an **alpha** build. Bugs are possible, and major changes to the interface should be expected in the future.

Why **heatmapEco** over other heatmap-making packages? This package recognizes that heatmaps can also be used to plot the
relationshio between a categorical/factor variable and a *continuous* variable, adequately discretized. The goal is to create
a package with an easy-to-use interface that processes all the discretization, aggregation and relabelling involved with a heatmap
automatically.

**heatmapEco** also has a companion interface in Stata, just called **heatmap**. The interface communicates with the R package for
a more robust program than Stata's own heatmap program.

## Installation

*1) Of the R package*

Pull the repo to get the source package, heatmapEco_xxx.tar.gz. Install it from source in R, or run heatmapSetup.R from the repo to automate
installation. To run heatmapSetup.R, make sure to input the directory to which the repo was downloaded as the command line argument.

Upload to CRAN is coming eventually.

*2) Of the Stata package*

Run *sysdir* in Stata. Copy the ADO and STHLP files in subdirectory stataHeatmap to one of the folders listed by that command
(the "PERSONAL" or "PLUS" folder is advised). Reboot Stata; the command should be loaded afterwards.

The Stata help file goes through more details about the installation of the R package if you are an R novice.

Upload to SSC is tentative.

## Getting started

Check out the slides in docs/heatmaps.pdf for an example of what can be done with heatmaps.

For more information read Tom Cui and Eric Zwick, [NBER ARTICLE HERE]

## Contribute

The subdirectory /RHeatmap has the project file for the R package, so with RStudio you can start editing right away.

There is only one ado file for Stata Heatmap, which can be edited directly.

## Future updates

v 1.0 should include a much more "functional" interface in both the Stata
and R interfaces, where settings to an axis are called as suboptions to
a main option for each axis.

Allow easy aggregation of data on just one axis, not both axes, and display
those aggregated data as sideplots to the heatmap.

## Contact

Tom Cui: E-mail Tom.Cui [at] chicagobooth.edu.

**heatmapEco** is licensed under GPLv3.

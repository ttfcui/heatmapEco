library(heatmapEco)
context("Compilation collapses datasets correctly")

test_that("Compilation options pass through", {

 test <- data.frame(y=c(1, 3.5, 8, 2), ID=c(1, 1, 2, 2),
                    time=c("2008", "2009", "2008", "2009"), x=c(5, 5, 4, 4), 
                    x2=c("5", "5", "4", "4"), wgt=c(1.2, 1, 2, 0.6))
 args <- list(data=test, y="x", Ident="ID", z="y", index="time", N=1,
              t.func=timeConv("year", ft="%Y"))
 out <- do.call("compilation", args)
 expect_equal(out$z, c(4.5, 2.75))
 expect_equal(out$index, c(2008, 2009))
 args[["N"]] <- 2; args[["grp.func"]] <- max; args[["not.q"]] <- T
 out <- do.call("compilation", args)
 expect_equal(out$z, c(8, 2, 1, 3.5))
 args[["grp.func"]] <- log
 out <- do.call("compilation", args)
 expect_equal(out$z, log(c(8, 2, 1, 3.5)))

})

test_that("Data.table compatible", {

 library(data.table)
 test <- data.table(y=c(1, 3.5, 8, 2), id=c(1, 1, 2, 2),
                    time=c("2008", "2009", "2008", "2009"), x=c(5, 5, 4, 4), 
                    x2=c("5", "5", "4", "4"), wgt=c(1.2, 1, 2, 0.6))
 args <- list(data=test, y="x", Ident="id", z="y", index="time", N=1,
              t.func=timeConv("year", ft="%Y"))
 out <- do.call("compilation", args)
 expect_equal(out$z, c(4.5, 2.75))
 expect_equal(out$index, c(2008, 2009))
 args[["N"]] <- 2; args[["grp.func"]] <- max; args[["not.q"]] <- T
 out <- do.call("compilation", args)
 expect_equal(out$z, c(8, 2, 1, 3.5))
 args[["grp.func"]] <- log
 out <- do.call("compilation", args)
 expect_equal(out$z, log(c(8, 2, 1, 3.5)))

})

test_that("Compilation passes through after residualization", {

})

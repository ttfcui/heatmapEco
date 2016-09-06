library(heatmapEco)
context("Heatmap API stops at right times.")

test_that("Formula parsing works as expected", {
    expect_equal(parseHeatform(Y ~ CrS(x,ID,w):time)[["cross"]],
                 c("x", "ID", "w"))
    expect_equal(parseHeatform(~e(x,ID,w):time)[["cross"]],
                 c("x", "ID", "w"))
    expect_equal(parseHeatform(Y ~ CrS(x,ID,w):time("2001"))[["z.t.args"]],
                 c("Y", "time", "\"2001\""))
    expect_equal(parseHeatform(Y ~ CrS(x,ID,w,,,,,):time)[["cross"]],
                 c("x", "ID", "w", "", "", "", ""))

})

test_that("Correct error raising", {
    expect_error(heatmapEco(fname="stuff.csv"), "Compatibility check failed!")
})

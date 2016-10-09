library(heatmapEco)
context("Proper axis displays")

test_that("Time_axis converts dates properly", {
    expect_equal(
        timeConv("yearmon")(c("2008-09-07 11:11:11", "2008-09-08")),
        c(24104, 24104))
    expect_equal(timeConv("yearmon", "%d%b%Y")("13jan2008"), 24096)
    expect_equal(timeConv("year", "%d%b%Y")("21aug1993"), 1993)
    expect_equal(timeConv("yearqtr", "%Ym%m")("2008m3"), 8032)
    expect_identical(timeConv(NULL, "%d%b%Y")(196), 196)
})

test_that("X axis setup is working", {
    out <- setupX(c(24096, 24104, 24096), period="yearmon",
                  time.axis=timeConv("yearmon"), pol.break="January 2008")
    expect_equal(as.character(out[["tick"]])[2], "24096")
    expect_equal(as.character(out[["tick"]])[3], "Jan 2008")
    expect_equal(as.character(out[["xl"]])[2], "c(24096.1, 24104)")
    expect_equal(as.character(out[["int"]])[2], "24095.5")
    out <- setupX(c(24096, 24096, 24104), split=1, period="yearmon",
                  time.axis=timeConv("yearmon"), pol.break="")
    expect_equal(as.character(out[["tick"]])[2], 'c(24096, 24104)')
    expect_error(setupX(timeConv("yearmon")("200809  08")),
                 "Formatting failed in X: cannot accept NA values!")
    expect_error(setupX(c(24096, 24096, 24104), period="yearmon"),
                 "argument \"time.axis\" is missing, with no default")
    expect_error(setupX(c(24096, 24096, 24104), period="yearmon",
                        time.axis=timeConv("yearmon")),
                 "argument \"pol.break\" is missing, with no default")
})

test_that("Y axis setup is working", {
    # Uniques
    out <- setupY(c(24096, 24104, 24112))
    expect_equal(as.character(out[["tick"]])[2], "24112")
    expect_equal(as.character(out[["tick"]])[3], "24112")
    # Duplicates
    out <- setupY(c(24096, 24104, 24096))
    expect_equal(as.character(out[["tick"]])[3], "24104")
    # Factors (Duplicates admissible)
    out <- setupY(c(24096, 24104, 24096), factor.ax=T)
    expect_equal(as.character(out[["tick"]]), "scale_y_discrete")
    expect_error(setupY(c("Alaska", "California", NA)),
                 "Formatting failed in Y: cannot accept NA values!")
})

test_that("Fill setup is working", {
    base <- rnorm(1000, 2, 10)
    out <- setupFill(base, ztitle="Test")[["fill"]]
    expect_match(as.character(out)[2],
                 '"#25779D", "#25779D", "#6FA6C0", "#BBD7E4", "#F2F2F2", "#F2C1B5", "#E66C65", "#DA2022"')
    expect_match(as.character(out)[3], "0, 0.15, 0.3, 0.51, 0.677, 0.843, 1")
    expect_is(setupFill(base, outliers=T, count=T, ztitle="Test", 
              custom=c(0, 0, 0, 0, 1)), "list")
    expect_error(setupFill(base),'argument "ztitle" is missing, with no default')
})

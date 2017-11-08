# Add comment
# 
# Author: Andrie
#----------------------------------------------------------------------------------


context("True Driver")

test_that("truedriver returns a valid object", {
      data(diamonds, package="ggplot2")
      x <- truedriver(price~cut+color+carat+clarity, data=diamonds)
      expect_is(x, "truedriver")
    })

test_that("truedriver returns a valid object with formula=price~.", {
      data(diamonds, package="ggplot2")
      x <- truedriver(price~., data=diamonds[, c("price", "cut", "color", "clarity", "carat")])
      expect_is(x, "truedriver")
    })


test_that("plot.truedriver prints object", {
      data(diamonds, package="ggplot2")
      x <- truedriver(price~cut+color+carat+clarity, data=diamonds)
      p <- plot(x)
      
      expect_is(p, "ggplot")
      
      
    })

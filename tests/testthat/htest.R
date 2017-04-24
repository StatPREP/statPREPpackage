context("htest")

library(dplyr)

test_that("single sample t-test", {
  t1 <- htest( ~ cyl, data= mtcars)
  expect_equal(t1$estimate, 6.1875, tolerance = 0.0002)
  expect_equal(t1$statistic, 19.59872, tolerance = 0.0002)
  expect_equal(t1$p.value, 5.048147e-19, tolerance = 0.0002)
  expect_equal(t1$conf.low, 5.543607, tolerance = 0.0002)
  expect_equal(t1$conf.high, 6.831393, tolerance = 0.0002)
})

test_that("two-sample t-test works", {
  t2 <- mtcars %>% mutate(four = cyl == 4) %>% htest(hp ~ four)
  expect_equal(t2$estimate, 97.60173, tolerance = 0.0002)
  expect_equal(t2$statistic, 6.69313, tolerance = 0.0002)
  expect_equal(t2$p.value, 3.247086e-7, tolerance = 0.0002)
  expect_equal(t2$conf.low, 67.70034, tolerance = 0.0002)
  expect_equal(t2$conf.high, 127.5031, tolerance = 0.0002)
})

test_that("1-way anova works", {
  t3 <- mtcars %>% htest(hp ~ cyl, test = "anova")
  expect_equal(t3$term, c("cyl", "Residuals"))
  expect_equal(t3$df, c(1,30))
  expect_equal(t3$sumsq, c(100984.2, 44742.7), tolerance = 0.0002)
  expect_equal(t3$meansq, c(100984.2, 1491.423), tolerance = 0.0002)
  expect_equal(t3$statistic, c(67.70993, NA), tolerance = 0.0002)
  expect_equal(t3$p.value, c(3.477861e-9, NA), tolerance = 0.0002)
})

t4 <- mtcars %>% htest(~ cyl == 4)
test_that("test of proportion works (as t-test)" {
  expect_equal(t4$estimate, 0.34375, tolerance = 0.0002)
  expect_equal(t4$statistic, 4.029652, tolerance = 0.0002 )
  expect_equal(t4$p.value, 0.0003363794, tolerance = 0.0002)
  expect_equal(t4$conf.low, 0.169769, tolerance = 0.0002)
  expect_equal(t4$conf.high, 0.517731, tolerance = 0.0002)
})

test_that("test of proportion works with dpyr", {
  # as TRUE/FALSE
  t5 <- mtcars %>% mutate(cyl = cyl == 4) %>% htest(~ cyl)
  # or as a 0,1 variable
  t6 <- mtcars %>% mutate(cyl = (cyl==4) + 0) %>% htest( ~ cyl)
  expect_equal(t4, t5)
  expect_equal(t4, t6)
  })

## ----message=FALSE, warning=FALSE----------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library("devtools")
library(repmis)
library(dplyr)
install_github('xiaozhouw/ATC')

## ----message=FALSE, warning=FALSE----------------------------------------
test_med = "fentanyl (pf) 50 mcg/ml injection solution"
test_tier = 4
findClass(test_med,test_tier)

## ----message=FALSE, warning=FALSE----------------------------------------
test_med = "fentanyl (pf) 500 mcg/ml injection solution"
test_tier = 4
findClass(test_med,test_tier)

## ----message=FALSE, warning=FALSE----------------------------------------
test_med = "fentanyl-acetaminophen 50mg"
test_tier = 4
findClass(test_med,test_tier)


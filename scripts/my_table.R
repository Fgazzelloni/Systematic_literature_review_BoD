
# table datasource: IHME BoE--------

# http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html

library(tidyverse)
library(gtsummary)
library(gt)
library(survival)

review %>%
  select(trt,age,grade) %>%
  tbl_summary(by=trt,
              missing="no",
              statistic=all_continous()~"")





# install.packages(c("survival", "lubridate", "ggsurvfit", "gtsummary", "tidycmprsk"))
# remotes::install_github("zabore/condsurv")
# remotes::install_github("zabore/ezfun")
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condsurv)
library(dplyr)
lung <- lung %>% mutate(status = recode(status, `1` = 0, `2` = 1))
# ?recode
head(lung[, c("time", "status", "sex")])
date_ex <- tibble(
  sx_date = c("2007-06-22", "2004-02-13", "201-10-27"),
  last_fup_date = c("2017-04-15", "2018-07-04", "2016-10-31")
)
date_ex
date_ex <-
  date_ex %>%
  mutate(
    sx_date = ymd(sx_date),
    last_fup_date = ymd(last_fup_date)
  )
date_ex
?ymd
date_ex <-
  date_ex %>%
  mutate(
    os_yrs = as.duration(sx_date %--% last_fup_date) / dyears(1)
  )
date_ex
Surv(lung$time, lung$status)[1:10]
s1 <- survfit(Surv(time, status) ~ 1, data = lung)
str(s1)
survfit2(Surv(time, status) ~ 1, data = lung) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() +
  add_risktable()
summary(survfit(Surv(time, status) ~ 1, data = lung), time = 365.25)

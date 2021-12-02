library(lubridate)
library(tidyverse)

mypredict = function() {
  n.comp = 7
  train = spread(train[, -5], Date, Weekly_Sales)
  train[is.na(train)] = 0
  train_svd = NULL
  for (mydept in unique(train$Dept)) {
    dept_data = filter(train, Dept == mydept)
    if (nrow(dept_data) > n.comp) {
      store_means = rowMeans(dept_data[, -c(1, 2)])
      z = svd(dept_data[, -c(1, 2)] - store_means, nu = n.comp, nv = n.comp)
      dept_data[, -c(1:2)] =
        z$u %*% diag(z$d[1:n.comp]) %*% t(z$v) + store_means
    }
    train_svd = rbind(train_svd, dept_data)
  }
  train_svd = gather(train_svd, Date, Weekly_Sales, -Store, -Dept)
  
  start_date = ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date = ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_current = test[test$Date >= start_date &
                        test$Date < end_date, -4]
  train_pairs = train_svd[, 1:2] %>% count(Store, Dept) %>% filter(n != 0)
  test_pairs = test_current[, 1:2] %>% count(Store, Dept) %>% filter(n != 0)
  unique_pairs = intersect(train_pairs[, 1:2], test_pairs[, 1:2])
  train_split = unique_pairs %>%
    left_join(train_svd, by = c("Store", "Dept")) %>%
    mutate(Wk = factor(ifelse(
      year(Date) == 2010, week(Date) - 1, week(Date)
    ), levels = 1:52)) %>%
    mutate(Yr = year(Date)) %>%
    group_split(Store, Dept)
  test_split = unique_pairs %>%
    left_join(test_current, by = c("Store", "Dept")) %>%
    mutate(Wk = factor(ifelse(
      year(Date) == 2010, week(Date) - 1, week(Date)
    ), levels = 1:52)) %>%
    mutate(Yr = year(Date)) %>%
    group_split(Store, Dept)
  test_pred = NULL
  for (i in 1:nrow(unique_pairs)) {
    test_pred =
      rbind(test_pred, cbind(test_split[[i]][, 1:3], Weekly_Pred = predict(
        lm(Weekly_Sales ~ Yr + Wk, train_split[[i]]), test_split[[i]]
      )))
  }
  test_pred
}
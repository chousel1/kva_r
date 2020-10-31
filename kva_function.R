KVA_multi <- function(no_emp, learn_time, pctauto, times_perf_year, avg_time_complete, wage, revenue) {
  df <- data.frame(no_emp = no_emp, learn_time = learn_time,
                   pctauto = pctauto, 
                   times_perf_year = times_perf_year, 
                   avg_time_complete = avg_time_complete, wage = wage,
                   revenue = revenue)
  df$tlearn_time <- df$no_emp * df$learn_time
  total_knoweldge <- list()
  for (i in 1:length(df$times_perf_year)) {
    total_knoweldge[i] <- (df[i, 8] * df[i, 4]) * (1-df[i, 3])
  }
  tst <- 1
  for (i in 1:length(total_knoweldge)) {
    tst <- rbind(tst, total_knoweldge[[i]])
  }
  tst <- tst[2:length(tst)]
  df$total_knoweldge <- tst
  total_knoweldge <- list()
  for (i in 1:length(df$no_emp)){
    total_knoweldge[i] <- (df[i, 6] * df[i, 5] * df[i, 4]) * df[i, 1]
  }
  tst <- 1
  for (i in 1:length(total_knoweldge)) {
    tst <- rbind(tst, total_knoweldge[[i]])
  }
  tst <- tst[2:length(tst)]
  df$expenses <- tst
  rev_per_unit <- sum(revenue)/sum(df$total_knoweldge)
  rok <- list()
  # Second argument to round specifies number of digits to round by
  for (i in 1:length(df$expenses)) {
    rok[i] <- round(((df[i, 9]/df[i, 10]) * 100), 2)
  }
  out <- list()
  out[[1]] <- rev_per_unit
  out[[2]] <- rok
  out[[3]] <- df
  return(out)
}
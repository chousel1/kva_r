KVA_lines_code <- function(no_prog_used, lc, times_perf_year, avg_time_complete, cost_sec, revenue) {
  df <- data.frame(no_prog_used = no_prog_used, lc = lc,
                   times_perf_year = times_perf_year, 
                   avg_time_complete = avg_time_complete, cost_sec = cost_sec,
                   revenue = revenue)
  df$tlc <- df$no_prog_used * df$lc
  total_code <- list()
  for (i in 1:length(df$times_perf_year)) {
    total_code[i] <- (df[i, 7] * df[i, 3])
  }
  tst <- 1
  for (i in 1:length(total_code)) {
    tst <- rbind(tst, total_code[[i]])
  }
  tst <- tst[2:length(tst)]
  df$total_code <- tst
  total_knoweldge <- list()
  for (i in 1:length(df$no_prog_used)){
    total_knoweldge[i] <- (df[i, 5] * df[i, 4] * df[i, 3]) * df[i, 1]
  }
  tst <- 1
  for (i in 1:length(total_knoweldge)) {
    tst <- rbind(tst, total_knoweldge[[i]])
  }
  tst <- tst[2:length(tst)]
  df$expenses <- tst
  rev_per_unit <- sum(revenue)/sum(df$total_code)
  rok <- list()
  # Second argument to round specifies number of digits to round by
  for (i in 1:length(df$expenses)) {
    rok[i] <- round(((df[i, 8]/df[i, 9]) * 100), 2)
  }
  out <- list()
  out[[1]] <- rev_per_unit
  out[[2]] <- rok
  out[[3]] <- df
  return(out)
}

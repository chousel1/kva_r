#' @title Knowledge Value Added - Training Time
#'
#' @description Using Training time as a unit of input, generates a 3 item list containing [[1]]: Revenue per Unit of knowledge, [[2]]: Return on Knowledge by task, and [[3]]: a dataframe compiling all calculations and input data
#'
#' @param no_emp Number of employees
#' @param learn_time Learning time by task
#' @param pctauto Percent of task automated (enter as Decimal)
#' @param times_perf_year Times a task is performed in a year
#' @param avg_time_complete Average time to completion of a task
#' @param wage Wages paid by task
#' @param revenue Revenue by task
#'
#' @return a 3 item list containing [[1]]: Revenue per Unit of knowledge, [[2]]: Return on Knowledge by task, and [[3]]: a dataframe compiling all calculations and input data
#'
#' @export KVA
#' @examples KVA(c(1, 15, 1), c(11, 47, 16), c(0.9, 0.14, 0.6), c(690, 6500, 230), c(0.5, .33, 1), c(9.5, 12, 20), c(4916.25,  579150, 6900))
#'

KVA <- function(no_emp, learn_time, pctauto, times_perf_year, avg_time_complete, wage, revenue) {
  df <- data.frame(no_emp = no_emp, learn_time = learn_time,
                   pctauto = pctauto,
                   times_perf_year = times_perf_year,
                   avg_time_complete = avg_time_complete, wage = wage,
                   revenue = revenue)
  df$tlearn_time <- df$no_emp * df$learn_time
  total_knowledge <- list()
  for (i in 1:length(df$times_perf_year)) {
    total_knowledge[i] <- (df[i, 8] * df[i, 4]) * (1-df[i, 3])
  }
  tst <- total_knowledge[[1]]
  for (i in 2:length(total_knowledge)) {
    tst <- rbind(tst, total_knowledge[[i]])
  }
  df$total_knowledge <- tst
  expenses <- list()
  for (i in 1:length(df$no_emp)){
    expenses[i] <- (df[i, 6] * df[i, 5] * df[i, 4]) * df[i, 1]
  }
  tst <- expenses[[1]]
  for (i in 2:length(expenses)) {
    tst <- rbind(tst, expenses[[i]])
  }
  df$expenses <- tst
  rev_per_unit <- sum(revenue)/sum(df$expenses)
  rok <- list()
  # Second argument to round specifies number of digits to round by
  for (i in 1:length(df$expenses)) {
    rok[i] <- round(((df[i, 9]/df[i, 10]) * 100), 2)
  }
  out <- list()
  out$rev_per_unit <- rev_per_unit
  out$rok <- rok
  out$dataframe <- df
  return(out)
}

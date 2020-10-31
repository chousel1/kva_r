library(shiny)
library(tidyverse)
library(plotly)
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

string_input_to_numer <- function(brks) {
  nums <- strsplit(brks, ',')
  nums <- nums[[1]]
  nums <- trimws(nums, which='both')
  nums <- as.numeric(nums)
  return(nums)
}

server <- function(input, output) {   
  # Revnenue Per Knowledge Unit
  output$rev <- renderPrint({
    brks <- input$no_emp
    no_emp <- string_input_to_numer(brks)
    brks1 <- input$learn_time
    learn_time <- string_input_to_numer(brks1)
    brks2 <- input$pctauto
    pctauto <- string_input_to_numer(brks2)
    brks3 <- input$times_perf_year
    times_perf_year <- string_input_to_numer(brks3)
    brks4 <- input$avg_time_complete
    avg_time_complete <- string_input_to_numer(brks4)
    brks5 <- input$wage
    wage <- string_input_to_numer(brks5)
    brks6 <- input$revenue
    revenue <- string_input_to_numer(brks6)
    result <- KVA_multi(no_emp, learn_time, pctauto, times_perf_year, avg_time_complete, wage, revenue)
    print(paste0("Revenue Per Knowledge Unit: ", round(result[[1]], 5), " (Rounded to the 5th Decimal)"))
  })
  # Return on Knowledge
  output$rok <- renderPrint({
    brks <- input$no_emp
    no_emp <- string_input_to_numer(brks)
    brks1 <- input$learn_time
    learn_time <- string_input_to_numer(brks1)
    brks2 <- input$pctauto
    pctauto <- string_input_to_numer(brks2)
    brks3 <- input$times_perf_year
    times_perf_year <- string_input_to_numer(brks3)
    brks4 <- input$avg_time_complete
    avg_time_complete <- string_input_to_numer(brks4)
    brks5 <- input$wage
    wage <- string_input_to_numer(brks5)
    brks6 <- input$revenue
    revenue <- string_input_to_numer(brks6)
    brks8 <- input$proc_name
    proc_name <- strsplit(brks8, ',')
    proc_name <- proc_name[[1]]
    proc_name <- trimws(proc_name, which="both")
    result <- KVA_multi(no_emp, learn_time, pctauto, times_perf_year, avg_time_complete, wage, revenue)
    print("Return on Knowledge by Process (listed in order of Input)")
    for (i in 1:length(result[[2]])){
      print(paste0(proc_name[i], ": ", result[[2]][i], "%"))
    }
  })
  # Total Knoweldge
  output$totl <- renderPrint({
    brks <- input$no_emp
    no_emp <- string_input_to_numer(brks)
    brks1 <- input$learn_time
    learn_time <- string_input_to_numer(brks1)
    brks2 <- input$pctauto
    pctauto <- string_input_to_numer(brks2)
    brks3 <- input$times_perf_year
    times_perf_year <- string_input_to_numer(brks3)
    brks4 <- input$avg_time_complete
    avg_time_complete <- string_input_to_numer(brks4)
    brks5 <- input$wage
    wage <- string_input_to_numer(brks5)
    brks6 <- input$revenue
    revenue <- string_input_to_numer(brks6)
    brks8 <- input$proc_name
    proc_name <- strsplit(brks8, ',')
    proc_name <- proc_name[[1]]
    proc_name <- trimws(proc_name, which="both")
    result <- KVA_multi(no_emp, learn_time, pctauto, times_perf_year, avg_time_complete, wage, revenue)
    print("Total Knowledge by Process")
    nw <- result[[3]]
    nw <- nw$total_knoweldge
    for (i in 1:length(nw)){
      print(paste0(proc_name[i], ": ", nw[i]))
    }
  })
  # Total Learning Time
  output$tlt <- renderPrint({
    brks <- input$no_emp
    no_emp <- string_input_to_numer(brks)
    brks1 <- input$learn_time
    learn_time <- string_input_to_numer(brks1)
    brks2 <- input$pctauto
    pctauto <- string_input_to_numer(brks2)
    brks3 <- input$times_perf_year
    times_perf_year <- string_input_to_numer(brks3)
    brks4 <- input$avg_time_complete
    avg_time_complete <- string_input_to_numer(brks4)
    brks5 <- input$wage
    wage <- string_input_to_numer(brks5)
    brks6 <- input$revenue
    revenue <- string_input_to_numer(brks6)
    brks8 <- input$proc_name
    proc_name <- strsplit(brks8, ',')
    proc_name <- proc_name[[1]]
    proc_name <- trimws(proc_name, which="both")
    result <- KVA_multi(no_emp, learn_time, pctauto, times_perf_year, avg_time_complete, wage, revenue)
    print("Total Learning Time by Process")
    nw <- result[[3]]
    nw <- nw$tlearn_time
    for (i in 1:length(nw)){
      print(paste0(proc_name[i], ": ", nw[i]))
    }
  })
  ## Graphs
  # ROK Graph
  # Return on Knowledge
  output$rokplot <- renderPlotly({
    brks <- input$no_emp
    no_emp <- string_input_to_numer(brks)
    brks1 <- input$learn_time
    learn_time <- string_input_to_numer(brks1)
    brks2 <- input$pctauto
    pctauto <- string_input_to_numer(brks2)
    brks3 <- input$times_perf_year
    times_perf_year <- string_input_to_numer(brks3)
    brks4 <- input$avg_time_complete
    avg_time_complete <- string_input_to_numer(brks4)
    brks5 <- input$wage
    wage <- string_input_to_numer(brks5)
    brks6 <- input$revenue
    revenue <- string_input_to_numer(brks6)
    brks8 <- input$proc_name
    proc_name <- strsplit(brks8, ',')
    proc_name <- proc_name[[1]]
    proc_name <- trimws(proc_name, which="both")
    result <- KVA_multi(no_emp, learn_time, pctauto, times_perf_year, avg_time_complete, wage, revenue)
    rokk <- result[[2]][[1]]
    rokk <- as.data.frame(rokk)
    for (i in 2:length(result[[2]])){
      rokk <- rbind(rokk, result[[2]][[i]])
    }
    rokk$proc_name <- proc_name
    if (input$logg == "Yes"){
    rokp <- ggplot(rokk) + aes(x=proc_name, y=log(rokk), fill=proc_name, 
                       text = paste0('Process Name: ', proc_name, "\n", 
                                    'Return on Knowledge: ', rokk, '%')) + 
      geom_bar(stat="identity") + 
      theme_bw() + labs(x="Process", y="Natural Log of: Return on Knowledge", fill="Process", title="Return on Knowledge by Process")
    }
    else {
      rokp <- ggplot(rokk) + aes(x=proc_name, y=rokk, fill=proc_name, 
                                 text = paste0('Process Name: ', proc_name, "\n", 
                                               'Return on Knowledge: ', rokk, '%')) + 
        geom_bar(stat="identity") + 
        theme_bw() + labs(x="Process", y="Return on Knowledge (in %)", fill="Process", title="Return on Knowledge by Process")
    }
    ggplotly(rokp, tooltip=c("text"))
  })
}

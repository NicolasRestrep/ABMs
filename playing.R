
after_each_round <- matrix(0, 30, 1000)

report_data <- list()

for (k in 1:1000) {
  
  disp <- matrix(0, 30)
  
  for (i in 1:30) {
    
    # Draw from each distribution according to the weight of each actor 
    
    w1 <- rnorm(1, 3, n = data$h1[i])
    
    w2 <- rnorm(1.25, 3, n = data$h2[i])
    
    w3 <- rnorm(1.5, 3, n = data$h3[i])
    
    w4 <- rnorm(1.75, 3, n = data$h4[i])
    
  
    # Concatenate all values into a list
    
    draw_list <- list("w1" = w1, 
                      "w2" = w2, 
                      "w3" = w3, 
                      "w4" = w4)
    
    # Take the absolute value of each draw
    
    absolute_draws <- map(draw_list, abs)
    
    # What is the minimum? 
    
    draws <- map_dbl(absolute_draws, min)
    
    # add that to the dispositions 
    
    disp[i, ] <-names(which.min(draws))
    
  }
  
  for (j in 1:30) {
    
    
    # Find the neighbors of my agent
    
    neighbors <- which(net_matrix[j,] != 0)
    
    # Dispositions that should be updated
    
    to_update <- disp[neighbors]
    
    w_w1 <- sum(str_detect("w1", to_update))
    
    w_w2 <- sum(str_detect("w2", to_update))
    
    w_w3 <- sum(str_detect("w3", to_update))
    
    w_w4 <- sum(str_detect("w4", to_update))
    
    # Update the weights on the original dataframe 
    
    data$h1[j] <- data$h1[j] + w_w1
    data$h2[j] <- data$h2[j] + w_w2
    data$h3[j] <- data$h3[j] + w_w3 
    data$h4[j] <- data$h4[j] + w_w4
    
  }  
  
  d_graph <- data %>% 
    mutate(conviction = case_when(h1 > h2 & h1 > h3 & h1 > h4 ~ "h1", 
                                  h2 > h1 & h2 > h3 & h2 > h4 ~ "h2", 
                                  h3 > h1 & h3 > h2 & h3 > h4 ~ "h3", 
                                  h4 > h1 & h4 > h2 & h4 > h3 ~ "h4", 
                                  h1 == h2 | h1 == h3 | h1 == h4 | h2 == h3 | h2 == h4 | h3 == h4 ~ "undecided"))
  
  
  after_each_round[, k] <- d_graph$conviction
  
  report_data[[k]] <- d_graph
  
  if (k %% 100) { 
    
    V(sw_network)$conviction <- d_graph$conviction
    
    
    
    V(sw_network)$color <- ifelse(V(sw_network)$conviction == "h1", "#117ed3", 
                                  ifelse(V(sw_network)$conviction == "h2", "#d3116c", 
                                         ifelse(V(sw_network)$conviction == "h3", "#1e9231", 
                                                ifelse(V(sw_network)$conviction == "h4", "#d13010", 
                                                       ifelse(V(sw_network)$conviction == "undecided", "#7412c7", "NA")))))
    
    
    
    
    plot(sw_network, vertex.color = V(sw_network)$color, col="#777777", vertex.label=V(sw_network)$conviction)
  }
  
  print(as.character(k))
  
  
}


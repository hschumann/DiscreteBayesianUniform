## server.R
## this is the background for the
## Discrete Bayes' applet for Uniform Prior

library(shiny)
library(LearnBayes)

shinyServer(function(input,output) {
  ## get the type of population distribution from input
  distbn <- eventReactive(input$distribution, {
    if (input$distribution == "binom") {
      "binom"
    }
    else if (input$distribution == "pois") {
      "pois"
    }
  })
  
  ## this code will produce the 4 plots seen in the applet
  output$plot <- renderPlot({
    ## create template for 4 plots and set the window size
    par(mfrow = c(2,2))
    op <- par(mar = c(5,7,4,2) + 0.1)
    ## if/else statements are for the type of distribution for population
    if (distbn() == "binom") {
      # for the prior distribution
      p <- seq(input$bin_begin,input$bin_end,
               by = (input$bin_end - input$bin_begin)/(input$bin_by - 1))
      prior <- (1 / length(p)) + 0*p
      names(prior) <- p
      ## create the plot of the uniform prior
      plot(p,prior,type = 'h',
           col = "cyan4",lwd = 2,
           xlim = c(input$bin_begin,input$bin_end),
           xlab = "Proportion of Successes",
           ylab = "",
           main = "Uniform Prior Distribution",
           las = 1)
      mtext(text="Probability", side=2, line=4)
      # plot the population distribution for sampling
      x <- seq(0,input$num_trials)
      hx.pop <- dbinom(x,size = length(x) - 1,prob = input$pop_prop)
      plot(x,hx.pop,col = "midnightblue",lwd = 2,type = 'h',
           main = paste0("True Population Sampling Distribution\nfor ",input$num_trials," trials"),
           xlab = "Number of Successes",
           ylab = "",
           las = 1)
      mtext(text="Probability", side=2, line=4)
      # plot the prior and posterior on same plot
      posterior <- pdisc(p,prior,
                         c(input$binom_success,input$binom_failures))
      plot(p,posterior,xlim = c(input$bin_begin,input$bin_end),
           main = "Prior vs Posterior Distributions",
           xlab = "Proportion of Successes",
           ylab = "",
           col = "darkorange",
           pch = 20,cex = 1.8,las = 1)
      mtext(text="Probability", side=2, line=4)
      points(p,prior,col = "cyan4",pch = 20,cex = 1.8)
      # plot for the credible interval
      names(posterior) <- p
      d <- discint(cbind(p,posterior),as.numeric(input$cred.int)/100)
      plot(p,posterior,
           xlim = c(input$bin_begin,input$bin_end),
           main = paste0(input$cred.int,"% Credible Interval for Mean"),
           xlab = "Proportion of Successes",
           ylab = "",
           col = "darkorange",
           pch = 20,cex = 1.8,las = 1)
      mtext(text="Probability", side=2, line=4)
      segments(x0 = d$set,y0 = rep(0,length(d$set)),
               y1 = posterior[as.character(d$set)],
               lwd = 2)
    }
    else if (distbn() == "pois") {
      # for the prior distribution
      p <- seq(input$pois_begin,input$pois_end)
      prior <- (1 / length(p)) + 0*p
      names(prior) <- p
      plot(p,prior,
           type = 'h',col = "cyan4",lwd = 2,
           xlim = c(input$pois_begin,input$pois_end),
           xlab = "Mean Number of Occurrences",
           ylab = "",
           main = "Uniform Prior Distribution",
           las = 1)
      mtext(text="Probability", side=2, line=4)
      # for the population sampling data
      n <- seq(0,8 + 2*input$pop_lambda)
      hn <- dpois(n,lambda = input$pop_lambda)
      plot(n,hn,pch = 20,col = "midnightblue",type = 'h',lwd = 2,
           main = "Population Distribution of Occurrences \nfor Sampling",
           xlab = "Occurrences",
           ylab = "Probability",
           las = 1)
      # for the prior vs posterior distributions
      post <- discrete.bayes(dpois,prior,input$pois_sample_n)
      plot(post,col = "darkorange",
           main = "Prior vs Posterior Distributions",
           xlab = "Mean Number of Occurances",
           ylab = "",
           las = 1)
      mtext(text="Probability", side=2, line=4)
      points((p - min(p))*1.2 + 0.7,prior,col = "cyan4",pch = 20,cex = 1.8)
      # for the credible interval
      ## bar.colors is to change the bar colors for the correct interval to display
      bar.colors <- function(post.dist) {
        v <- rep("darkorange",length(post.dist$prob))
        hts <- post.dist$prob
        area <- 0
        while (area < as.numeric(input$cred.int)/100) {
          area <- area + max(hts)
          v[which.max(hts)] <- "midnightblue"
          hts[which.max(hts)] <- 0
        }
        return (v)
      }
      plot(post,col = bar.colors(post),
           main = paste0(input$cred.int,"% Credible Interval for Mean"),
           xlab = "Proportion of Successes",
           ylab = "",
           las = 1)
      mtext(text="Probability", side=2, line=4)
    }
  })
  
  ## this will display the actual values of the credible interval
  output$text3 <- renderText({
    if (distbn() == "binom") {
      # for the prior distribution
      p <- seq(input$bin_begin,input$bin_end,
               by = (input$bin_end - input$bin_begin)/(input$bin_by - 1))
      prior <- (1 / length(p)) + 0*p
      names(prior) <- p
      # create the posterior dsitribution
      posterior <- pdisc(p,prior,
                         c(input$binom_success,input$binom_failures))
      names(posterior) <- p
      d <- discint(cbind(p,posterior),as.numeric(input$cred.int)/100)
      paste0(input$cred.int,"% Credible Interval: ",round(min(d$set),4)," to ",round(max(d$set),4))
    }
    else if (distbn() == "pois") {
      # for the prior distribution
      p <- seq(input$pois_begin,input$pois_end)
      prior <- (1 / length(p)) + 0*p
      names(prior) <- p
      # create posterior dist.
      post <- discrete.bayes(dpois,prior,input$pois_sample_n)
      # for the credible interval
      bar.colors <- function(post.dist) {
        v <- rep("darkorange",length(post.dist$prob))
        hts <- post.dist$prob
        area <- 0
        while (area < as.numeric(input$cred.int)/100) {
          area <- area + max(hts)
          v[which.max(hts)] <- "midnightblue"
          hts[which.max(hts)] <- 0
        }
        return (v)
      }
      cols <- bar.colors(post)
      paste0(input$cred.int,"% Credible Interval: ",min(which(cols == "midnightblue")) + input$pois_begin - 1,
             " to ",max(which(cols == "midnightblue")) + input$pois_begin - 1)
    }
  })
  
  ## this text acts as a legend below the window for the plots
  output$text1 <- renderText({
    paste0("Blue represents the prior distribution")
  })
  output$text2 <- renderText({
    paste0("Orange represents the posterior distribution")
  })
})

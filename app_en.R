# Install and load required libraries
#library(rsconnect)
#library(shiny)
#library(ggplot2)
#library(reshape2)

#install.packages("here")
# Declare the location of the current script
#here::i_am("Shiny_App_en/app.R")

#library(here)
# Check if location was declared correctly
#here()

# Connect to shinyapps.io
rsconnect::setAccountInfo(name='nicorei', token='589039CEA158185A2079B1E8546F0107', secret='1Lc2wJXqx8uHgkUbV7tMXRF5u/nNgiUtz1p21wCI')
rsconnect::deployApp(here::here("Shiny_App_en"))

# Define UI for application
ui <- fluidPage(
  titlePanel("Health Microsimulation Model"),
  sidebarLayout(
    sidebarPanel(
      h4("Introduction"),
      p("This tool performs a microsimulation based on the Sick-Sicker Model, where individuals move between the states healthy, sick, sicker, and dead. For the given inputs, the tool generates an incremental cost-effectiveness analysis as well as three helpful diagrams on the proportion of individuals in different health states and the distribution of costs and quality-adjusted life years (QALYs) across individuals."),
      tabsetPanel(
        tabPanel("General", 
                 numericInput("n.i", "Number of Individuals:", 100000, min = 1000, step = 1000),
                 numericInput("n.t", "Number of Cycles:", 30, min = 10, step = 1),
                 numericInput("d.c", "Discount Rate for Costs:", 0.03, min = 0, step = 0.01),
                 numericInput("d.e", "Discount Rate for QALYs:", 0.03, min = 0, step = 0.01)
        ),
        tabPanel("Transition Probabilities", 
                 numericInput("p.HD", "Probability to Die when Healthy:", 0.005, min = 0, max = 1, step = 0.001),
                 numericInput("p.HS1", "Probability to Become Sick when Healthy:", 0.15, min = 0, max = 1, step = 0.01),
                 numericInput("p.S1H", "Probability to Become Healthy when Sick:", 0.5, min = 0, max = 1, step = 0.01),
                 numericInput("p.S1S2", "Probability to Become Sicker when Sick:", 0.105, min = 0, max = 1, step = 0.01),
                 numericInput("rr.S1", "Rate Ratio of Death when Sick vs Healthy:", 3.0, min = 0, step = 0.1),
                 numericInput("rr.S2", "Rate Ratio of Death when Sicker vs Healthy:", 10.0, min = 0, step = 0.1),
                 numericInput("rp.S1S2", "Mortality Rate Increase with Every Additional Year Being Sick:", 0.2, min = 0, step = 0.01)
        ),
        tabPanel("Costs & Utilities", 
                 numericInput("c.H", "Cost in € of Remaining Healthy per Cycle:", 2000, min = 0, step = 100),
                 numericInput("c.S1", "Cost in € of Being Sick per Cycle:", 4000, min = 0, step = 100),
                 numericInput("c.S2", "Cost in € of Being Sicker per Cycle:", 15000, min = 0, step = 500),
                 numericInput("c.Trt", "Cost in € of Treatment per Cycle:", 12000, min = 0, step = 500),
                 numericInput("u.H", "Utility when Healthy:", 1, min = 0, max = 1, step = 0.01),
                 numericInput("u.S1", "Utility when Sick:", 0.75, min = 0, max = 1, step = 0.01),
                 numericInput("u.S2", "Utility when Sicker:", 0.5, min = 0, max = 1, step = 0.01),
                 numericInput("u.Trt", "Utility when Treated:", 0.95, min = 0, max = 1, step = 0.01)
        ),
        tabPanel("Advanced Settings", 
                 numericInput("ru.S1S2", "Decrease in Utility per Year Being Sick (ru.S1S2):", 0.03, min = 0, step = 0.001),
                 numericInput("x.lower", "Lower Bound for Individual Effect Modifier (x.lower):", 0.95, min = 0, max = 1, step = 0.01),
                 numericInput("x.upper", "Upper Bound for Individual Effect Modifier (x.upper):", 1.05, min = 1, max = 10, step = 0.01)
        )
      ),
      actionButton("run", "Run Simulation")
    ),
    mainPanel(
      h3("Incremental Cost-Effectiveness Analysis"),
      tableOutput("table_micro"),
      p("QALYs = Quality-Adjusted Life Years"),
      p("ICER = Incremental Costs Effectiveness Ratio"),
      p("MCSE = Monte-Carlo Standard Error"),
      h3("Diagrams"),
      plotOutput("proportions_plot"),
      plotOutput("hist_trt"),
      plotOutput("hist_no_trt")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$run, {
    
    ##################################### Model input #######################################
    n.i   <- input$n.i                # number of simulated individuals
    n.t   <- input$n.t                # time horizon, 30 cycles
    v.n   <- c("H","S1","S2","D")     # the model states: Healthy (H), Sick (S1), Sicker (S2), Dead (D)
    n.s   <- length(v.n)              # the number of health states
    v.M_1 <- rep("H", n.i)            # everyone begins in the healthy state 
    d.c   <- input$d.c                # discounting of costs
    d.e   <- input$d.e                # discounting of QALYs
    v.Trt <- c("No Treatment", "Treatment") # store the strategy names
    
    # Transition probabilities (per cycle)
    p.HD    <- input$p.HD             # probability to die when healthy
    p.HS1   <- input$p.HS1            # probability to become sick when healthy
    p.S1H   <- input$p.S1H            # probability to become healthy when sick
    p.S1S2  <- input$p.S1S2           # probability to become sicker when sick
    rr.S1   <- input$rr.S1            # rate ratio of death when sick vs healthy
    rr.S2   <- input$rr.S2            # rate ratio of death when sicker vs healthy 
    r.HD    <- -log(1 - p.HD)         # rate of death when healthy 
    r.S1D   <- rr.S1 * r.HD           # rate of death when sick
    r.S2D   <- rr.S2 * r.HD           # rate of death when sicker
    p.S1D   <- 1 - exp(- r.S1D)       # probability to die when sick
    p.S2D   <- 1 - exp(- r.S2D)       # probability to die when sicker
    rp.S1S2 <- input$rp.S1S2          # increase of the mortality rate with every additional year being sick
    
    # Cost and utility inputs 
    c.H     <- input$c.H              # cost of remaining one cycle healthy
    c.S1    <- input$c.S1             # cost of remaining one cycle sick
    c.S2    <- input$c.S2             # cost of remaining one cycle sicker
    c.Trt   <- input$c.Trt            # cost of treatment (per cycle)
    
    u.H     <- input$u.H              # utility when healthy 
    u.S1    <- input$u.S1             # utility when sick 
    u.S2    <- input$u.S2             # utility when sicker 
    u.Trt   <- input$u.Trt            # utility when sick(er) and being treated
    ru.S1S2 <- input$ru.S1S2          # decrease in utility of treated sick individuals with every additional year being sick/sicker
    x.lower <- input$x.lower          # lower bound for the individuals' effect modifier at baseline
    x.upper <- input$x.upper          # upper bound for the individuals' effect modifier at baseline
    v.x     <- runif(n.i, x.lower, x.upper) # vector capturing individuals' effect modifier at baseline
    
    ##################################### Functions ###########################################
    
    MicroSim <- function(v.M_1, n.i, n.t, v.n, X = NULL, d.c, d.e, c.H, c.S1, c.S2, c.Trt,
                         u.H, u.S1, u.S2, u.Trt, ru.S1S2, p.HD, p.HS1, p.S1H, p.S1S2, 
                         rr.S1, rr.S2, rp.S1S2, x.lower, x.upper, TS.out = TRUE, TR.out = TRUE, Trt = FALSE, seed = 1) {
      
      v.dwc <- 1 / ((1 + d.c) ^ (0:n.t))   # calculate the cost discount weight based on the discount rate d.c
      v.dwe <- 1 / ((1 + d.e) ^ (0:n.t))   # calculate the QALY discount weight based on the discount rate d.e
      
      # create the matrix capturing the state name/costs/health outcomes for all individuals at each time point 
      m.M <- m.C <- m.E <- matrix(nrow = n.i, ncol = n.t + 1,
                                  dimnames = list(paste("ind",   1:n.i, sep =" "),
                                                  paste("cycle", 0:n.t, sep =" "))) 
      
      m.M[, 1] <- v.M_1             # indicate the initial health state 
      
      for (i in 1:n.i) {
        set.seed(seed + i)          # set the seed for every individual for the random number generator
        
        # create the dur variable that stores the number of consecutive cycles the individual occupies either when sick or sicker
        dur <- 0                            # the individual start without history        
        m.C[i, 1] <- Costs(m.M[i, 1], Trt)  # estimate costs per individual for the initial health state conditional on treatment
        m.E[i, 1] <- Effs(m.M[i, 1], dur, Trt, X = X[i])  # estimate QALYs per individual for the initial health state conditional on treatment, duration of being sick/sicker and individual characteristics
        
        for (t in 1:n.t) {
          v.p <- Probs(m.M[i, t], dur)         # calculate the transition probabilities at cycle t conditional on the duration of being sick/sicker
          
          m.M[i, t + 1] <- sample(v.n, prob = v.p, size = 1)  # sample the new health state and store that state in matrix m.M 
          m.C[i, t + 1] <- Costs(m.M[i, t + 1], Trt)     # estimate the cost per individual during cycle t + 1 conditional on treatment
          m.E[i, t + 1] <-  Effs(m.M[i, t + 1], dur, Trt, X = X[i])    # estimate the utility per individual during cycle t + 1 conditional on treatment, duration of being sick/sicker and individual characteristics
          
          if (m.M[i, t + 1] == "S1" | m.M[i, t + 1] == "S2") {  # expression to identify sick/sicker individuals
            dur <- dur + 1   # update the duration of being sick/sicker
          } else {
            dur <- 0}        # reset duration variable 
          
        } # close the loop for the time points 
      } # close the loop for the individuals
      
      tc <- m.C %*% v.dwc       # total (discounted) cost per individual
      te <- m.E %*% v.dwe       # total (discounted) QALYs per individual 
      
      tc_hat <- mean(tc)        # average (discounted) cost 
      te_hat <- mean(te)        # average (discounted) QALYs
      
      if (TS.out == TRUE) {  # create a  matrix of transitions across states
        TS <- paste(m.M, cbind(m.M[, -1], NA), sep = "->")  # transitions from one state to the other ###
        TS <- matrix(TS, nrow = n.i)
        rownames(TS) <- paste("Cycle", 0:n.t, sep = " ")    # name the rows of the matrix
        colnames(TS) <- paste("Ind",   1:n.s, sep = " ")    # name the columns of the matrix
      } else {
        TS <- NULL
      }
      
      if (TR.out == TRUE) {  # create a trace from the individual trajectories
        TR <- t(apply(m.M, 2, function(x) table(factor(x, levels = v.n, ordered = TRUE))))
        TR <- TR / n.i                                    # create a distribution trace
        rownames(TR) <- paste("Cycle", 0:n.t, sep = " ")  # name the rows of the matrix
        colnames(TR) <- v.n                               # name the columns of the matrix
      } else {
        TR <- NULL
      }
      
      results <- list(m.M = m.M, m.C = m.C, m.E = m.E, tc = tc, te = te, tc_hat = tc_hat, te_hat = te_hat, TS = TS, TR = TR)   # store the results from the simulation in a list  
      return(results)   # return the results
    } # end of the MicroSim function
    
    #### Probability function
    Probs <- function(M_it, dur) { 
      # M_it:   health state occupied by individual i at cycle t (character variable)
      # dur:    the duration of being sick (sick/sicker)
      
      v.p.it <- rep(NA, n.s)     # create vector of state transition probabilities
      names(v.p.it) <- v.n       # name the vector
      
      # update probabilities of death after first converting them to rates and applying the rate ratio
      r.S1D <-  - log(1 - p.S1D)
      r.S2D <-  - log(1 - p.S2D)
      p.S1D <- 1 - exp(- r.S1D * (1 + dur * rp.S1S2)) # calculate p.S1D conditional on duration of being sick/sicker
      p.S2D <- 1 - exp(- r.S2D * (1 + dur * rp.S1S2)) # calculate p.S2D conditional on duration of being sick/sicker
      
      # update v.p.it with the appropriate probabilities   
      v.p.it[M_it ==  "H"] <- c(1 - p.HS1 - p.HD, p.HS1, 0, p.HD)                    # transition probabilities when healthy
      v.p.it[M_it == "S1"] <- c(p.S1H, 1- p.S1H - p.S1S2 - p.S1D, p.S1S2, p.S1D)     # transition probabilities when sick
      v.p.it[M_it == "S2"] <- c(0, 0, 1 - p.S2D, p.S2D)                              # transition probabilities when sicker
      v.p.it[M_it ==  "D"] <- c(0, 0, 0, 1)                                          # transition probabilities when dead
      ifelse(sum(v.p.it) == 1, return(v.p.it), print("Probabilities do not sum to 1")) # return the transition probabilities or produce an error
    }    
    
    ### Costs function
    Costs <- function (M_it, Trt = FALSE) {  
      # M_it: health state occupied by individual i at cycle t (character variable)
      # Trt:  is the individual being treated? (default is FALSE)
      
      c.it <- 0                                   # by default the cost for everyone is zero
      c.it[M_it == "H"]  <- c.H                   # update the cost if healthy
      c.it[M_it == "S1"] <- c.S1 + c.Trt * Trt    # update the cost if sick conditional on treatment
      c.it[M_it == "S2"] <- c.S2 + c.Trt * Trt    # update the cost if sicker conditional on treatment
      return(c.it)                                # return the costs
    }
    
    ### Health outcome function 
    Effs <- function (M_it, dur, Trt = FALSE, cl = 1, X = NULL) { 
      # M_it: health state occupied by individual i at cycle t (character variable)
      # dur:  the duration of being sick/sicker
      # Trt:  is the individual being treated? (default is FALSE)
      # cl:   the cycle length (default = 1 )
      # X:    the vector or matrix of individual characteristics (optional)
      
      u.it               <- 0        # by default the utility for everyone is zero
      u.it[M_it == "H"]  <- u.H      # update the utility if healthy 
      u.it[M_it == "S1"] <- X * Trt * (u.Trt - dur * ru.S1S2) + (1 - Trt) * u.S1 # update the utility if sick conditional on treatment and duration of being sick/sicker
      u.it[M_it == "S2"] <- u.S2     # update the utility if sicker
      QALYs <- u.it * cl             # calculate the QALYs during cycle t
      return(QALYs)                  # return the results
    }
    
    ##################################### Run the simulation ##################################
    sim_no_trt <- MicroSim(v.M_1, n.i, n.t, v.n, X = v.x, d.c, d.e, TS.out = FALSE, TR.out = TRUE, Trt = FALSE) # run for no treatment
    sim_trt    <- MicroSim(v.M_1, n.i, n.t, v.n, X = v.x, d.c, d.e, TS.out = FALSE, TR.out = TRUE, Trt = TRUE) # run for treatment
    
    # Generate proportion plot
    df_tr <- as.data.frame(sim_no_trt$TR)
    df_tr$Cycle <- 0:input$n.t
    df_tr_melt <- melt(df_tr, id.vars = "Cycle")
    
    # Update health state names in the legend
    df_tr_melt$variable <- factor(df_tr_melt$variable, levels = c("H", "S1", "S2", "D"),
                                  labels = c("Healthy", "Sick", "Sicker", "Dead"))
    
    output$proportions_plot <- renderPlot({
      ggplot(df_tr_melt, aes(x = Cycle, y = value, fill = variable)) +
        geom_area(alpha = 0.6) +
        scale_fill_brewer(palette = "Set1") +
        labs(title = "Proportion of Individuals in Each Health State Over Time",
             x = "Cycle", y = "Proportion of Individuals", fill = "Health State") +
        theme_minimal()
    })
    
    # Generate histograms
    output$hist_trt <- renderPlot({
      par(mfrow = c(1, 2))
      hist(sim_trt$tc, breaks = 50, col = "skyblue", main = "Treatment: Healthcare Costs",
           xlab = "Total Costs in €", ylab = "Frequency", border = "black", xaxt = "n", labels = FALSE)
      axis(1, at = axTicks(1), labels = format(axTicks(1), scientific = FALSE))  # Remove scientific notation
      hist(sim_trt$te, breaks = 50, col = "lightgreen", main = "Treatment: QALYs",
           xlab = "Total QALYs", ylab = "Frequency", border = "black", labels = FALSE)
    })
    
    output$hist_no_trt <- renderPlot({
      par(mfrow = c(1, 2))
      hist(sim_no_trt$tc, breaks = 50, col = "tomato", main = "No Treatment: Healthcare Costs",
           xlab = "Total Costs in €", ylab = "Frequency", border = "black", xaxt = "n", labels = FALSE)
      axis(1, at = axTicks(1), labels = format(axTicks(1), scientific = FALSE))  # Remove scientific notation
      hist(sim_no_trt$te, breaks = 50, col = "gold", main = "No Treatment: QALYs",
           xlab = "Total QALYs", ylab = "Frequency", border = "black", labels = FALSE)
    })
    
    ################################# Cost-effectiveness analysis #############################
    
    # store the mean costs (and the MCSE)of each strategy in a new variable C (vector costs)
    v.C <- c(sim_no_trt$tc_hat, sim_trt$tc_hat) 
    se.C<- c(sd(sim_no_trt$tc), sd(sim_trt$tc)) / sqrt(n.i)
    # store the mean QALYs (and the MCSE) of each strategy in a new variable E (vector health outcomes)
    v.E <- c(sim_no_trt$te_hat, sim_trt$te_hat)
    se.E<- c(sd(sim_no_trt$te), sd(sim_trt$te)) / sqrt(n.i)
    
    delta.C <- v.C[2] - v.C[1]                   # calculate incremental costs
    delta.E <- v.E[2] - v.E[1]                   # calculate incremental QALYs
    se.delta.E <- sd(sim_trt$te - sim_no_trt$te) / sqrt(n.i) # Monte Carlo squared error (MCSE) of incremental QALYS
    se.delta.C <- sd(sim_trt$tc - sim_no_trt$tc) / sqrt(n.i) # Monte Carlo squared error (MCSE) of incremental costs
    ICER <- delta.C / delta.E                    # calculate the ICER
    results <- c(delta.C, delta.E, ICER)         # store the values in a new variable
    
    # Create full incremental cost-effectiveness analysis table
    table_micro <- data.frame(
      c(round(v.C, 0),  ""),           # costs per arm
      c(round(se.C, 0), ""),           # MCSE for costs
      c(round(v.E, 3),  ""),           # health outcomes per arm
      c(round(se.E, 3), ""),           # MCSE for health outcomes
      c("", round(delta.C, 0),   ""),  # incremental costs
      c("", round(se.delta.C, 0),""),  # MCSE for incremental costs
      c("", round(delta.E, 3),   ""),  # incremental QALYs 
      c("", round(se.delta.E, 3),""),  # MCSE for health outcomes (QALYs) gained
      c("", round(ICER, 0),      "")   # ICER
    )
    
    rownames(table_micro) = c("No Treatment", "Treatment", "* are MCSE values")  # name the rows
    colnames(table_micro) = c("Costs", "*",  "QALYs", "*", "Incremental Costs", "*", "QALYs Gained", "*", "ICER") # name the columns 
    
    output$table_micro <- renderTable({ table_micro }, rownames = TRUE, colnames = TRUE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

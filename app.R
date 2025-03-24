# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))
print(getwd())

# Load required libraries
library(rsconnect)
library(shiny)
library(ggplot2)
library(reshape2)


# Definition der Benutzeroberfläche (UI) für die Anwendung
ui <- fluidPage(
  titlePanel("Mikrosimulation eines Gesundheitsmodells"),
  sidebarLayout(
    sidebarPanel(
      h4("Einführung"),
      p("Dieses Tool führt eine Mikrosimulation basierend auf dem Sick-Sicker-Modell durch, bei der Individuen zwischen den Zuständen gesund, krank, kränker und tot wechseln. Für die gegebenen Eingaben generiert das Tool eine inkrementelle Kosten-Nutzen-Analyse sowie drei hilfreiche Diagramme zu den Anteilen der Individuen in verschiedenen Gesundheitszuständen und der Verteilung der Kosten und qualitätsadjustierten Lebensjahre (QALYs) über die Individuen."),
      tabsetPanel(
        tabPanel("Allgemein", 
                 numericInput("n.i", "Anzahl der Individuen:", 100000, min = 1000, step = 1000),
                 numericInput("n.t", "Anzahl der Zyklen:", 30, min = 10, step = 1),
                 numericInput("d.c", "Diskontsatz für Kosten:", 0.03, min = 0, step = 0.01),
                 numericInput("d.e", "Diskontsatz für QALYs:", 0.03, min = 0, step = 0.01)
        ),
        tabPanel("Übergangswahrscheinlichkeiten", 
                 numericInput("p.HD", "Sterbewahrscheinlichkeit im gesunden Zustand:", 0.005, min = 0, max = 1, step = 0.001),
                 numericInput("p.HS1", "Wahrscheinlichkeit, im gesunden Zustand krank zu werden:", 0.15, min = 0, max = 1, step = 0.01),
                 numericInput("p.S1H", "Wahrscheinlichkeit, im kranken Zustand gesund zu werden:", 0.5, min = 0, max = 1, step = 0.01),
                 numericInput("p.S1S2", "Wahrscheinlichkeit, im kranken Zustand kränker zu werden:", 0.105, min = 0, max = 1, step = 0.01),
                 numericInput("rr.S1", "Sterberate im kranken Zustand im Vergleich zu gesund:", 3.0, min = 0, step = 0.1),
                 numericInput("rr.S2", "Sterberate im kränkeren Zustand im Vergleich zu gesund:", 10.0, min = 0, step = 0.1),
                 numericInput("rp.S1S2", "Sterberatenanstieg mit jedem zusätzlichen Jahr im kranken Zustand:", 0.2, min = 0, step = 0.01)
        ),
        tabPanel("Kosten & Nutzen", 
                 numericInput("c.H", "Kosten in € für den gesunden Zustand pro Zyklus:", 2000, min = 0, step = 100),
                 numericInput("c.S1", "Kosten in € für den kranken Zustand pro Zyklus:", 4000, min = 0, step = 100),
                 numericInput("c.S2", "Kosten in € für den kränkeren Zustand pro Zyklus:", 15000, min = 0, step = 500),
                 numericInput("c.Trt", "Kosten in € für die Behandlung pro Zyklus:", 12000, min = 0, step = 500),
                 numericInput("u.H", "Nutzen im gesunden Zustand:", 1, min = 0, max = 1, step = 0.01),
                 numericInput("u.S1", "Nutzen im kranken Zustand:", 0.75, min = 0, max = 1, step = 0.01),
                 numericInput("u.S2", "Nutzen im kränkeren Zustand:", 0.5, min = 0, max = 1, step = 0.01),
                 numericInput("u.Trt", "Nutzen im behandelten Zustand:", 0.95, min = 0, max = 1, step = 0.01)
        ),
        tabPanel("Erweiterte Einstellungen", 
                 numericInput("ru.S1S2", "Nutzenabnahme pro Jahr im kranken Zustand:", 0.03, min = 0, step = 0.001),
                 numericInput("x.lower", "Untere Grenze für individuellen Effektmodifikator:", 0.95, min = 0, max = 1, step = 0.01),
                 numericInput("x.upper", "Obere Grenze für individuellen Effektmodifikator:", 1.05, min = 1, max = 10, step = 0.01)
        )
      ),
      actionButton("run", "Simulation starten")
    ),
    mainPanel(
      h3("Inkrementelle Kosten-Nutzen-Analyse"),
      tableOutput("table_micro"),
      p("QALYs = Qualitätsadjustierte Lebensjahre"),
      p("ICER = Inkrementelle Kosten-Nutzen-Verhältnis"),
      p("MCSE = Monte-Carlo-Standardfehler"),
      h3("Diagramme"),
      plotOutput("proportions_plot"),
      plotOutput("hist_trt"),
      plotOutput("hist_no_trt")
    )
  )
)

# Definition der Serverlogik
server <- function(input, output) {
  observeEvent(input$run, {
    
    ##################################### Modelleingaben #######################################
    n.i   <- input$n.i                # Anzahl der simulierten Individuen
    n.t   <- input$n.t                # Zeithorizont, 30 Zyklen
    v.n   <- c("H","S1","S2","D")     # die Modellzustände: Gesund (H), Krank (S1), Kränker (S2), Tot (D)
    n.s   <- length(v.n)              # die Anzahl der Gesundheitszustände
    v.M_1 <- rep("H", n.i)            # jeder beginnt im gesunden Zustand 
    d.c   <- input$d.c                # Diskontierung der Kosten
    d.e   <- input$d.e                # Diskontierung der QALYs
    v.Trt <- c("Keine Behandlung", "Behandlung") # Speichern der Strategienamen
    
    # Übergangswahrscheinlichkeiten (pro Zyklus)
    p.HD    <- input$p.HD             # Sterbewahrscheinlichkeit im gesunden Zustand
    p.HS1   <- input$p.HS1            # Wahrscheinlichkeit, im gesunden Zustand krank zu werden
    p.S1H   <- input$p.S1H            # Wahrscheinlichkeit, im kranken Zustand gesund zu werden
    p.S1S2  <- input$p.S1S2           # Wahrscheinlichkeit, im kranken Zustand kränker zu werden
    rr.S1   <- input$rr.S1            # Sterberate im kranken Zustand im Vergleich zu gesund
    rr.S2   <- input$rr.S2            # Sterberate im kränkeren Zustand im Vergleich zu gesund 
    r.HD    <- -log(1 - p.HD)         # Sterberate im gesunden Zustand 
    r.S1D   <- rr.S1 * r.HD           # Sterberate im kranken Zustand
    r.S2D   <- rr.S2 * r.HD           # Sterberate im kränkeren Zustand
    p.S1D   <- 1 - exp(- r.S1D)       # Sterbewahrscheinlichkeit im kranken Zustand
    p.S2D   <- 1 - exp(- r.S2D)       # Sterbewahrscheinlichkeit im kränkeren Zustand
    rp.S1S2 <- input$rp.S1S2          # Anstieg der Sterberate mit jedem zusätzlichen Jahr im kranken Zustand
    
    # Kosten- und Nutzeneingaben 
    c.H     <- input$c.H              # Kosten für einen Zyklus im gesunden Zustand
    c.S1    <- input$c.S1             # Kosten für einen Zyklus im kranken Zustand
    c.S2    <- input$c.S2             # Kosten für einen Zyklus im kränkeren Zustand
    c.Trt   <- input$c.Trt            # Kosten für die Behandlung (pro Zyklus)
    
    u.H     <- input$u.H              # Nutzen im gesunden Zustand 
    u.S1    <- input$u.S1             # Nutzen im kranken Zustand 
    u.S2    <- input$u.S2             # Nutzen im kränkeren Zustand 
    u.Trt   <- input$u.Trt            # Nutzen im kranken/kränkeren Zustand bei Behandlung
    ru.S1S2 <- input$ru.S1S2          # Nutzenabnahme bei behandelten kranken Individuen mit jedem zusätzlichen Jahr im kranken/kränkeren Zustand
    x.lower <- input$x.lower          # Untere Grenze für den individuellen Effektmodifikator zu Beginn
    x.upper <- input$x.upper          # Obere Grenze für den individuellen Effektmodifikator zu Beginn
    v.x     <- runif(n.i, x.lower, x.upper) # Vektor, der den individuellen Effektmodifikator zu Beginn erfasst
    
    ##################################### Funktionen ###########################################
    
    MicroSim <- function(v.M_1, n.i, n.t, v.n, X = NULL, d.c, d.e, c.H, c.S1, c.S2, c.Trt,
                         u.H, u.S1, u.S2, u.Trt, ru.S1S2, p.HD, p.HS1, p.S1H, p.S1S2, 
                         rr.S1, rr.S2, rp.S1S2, x.lower, x.upper, TS.out = TRUE, TR.out = TRUE, Trt = FALSE, seed = 1) {
      
      v.dwc <- 1 / ((1 + d.c) ^ (0:n.t))   # Berechnung der Kostendiskontierungsgewichte basierend auf dem Diskontsatz d.c
      v.dwe <- 1 / ((1 + d.e) ^ (0:n.t))   # Berechnung der QALY-Diskontierungsgewichte basierend auf dem Diskontsatz d.e
      
      # Erstellen der Matrix, die den Zustandsnamen/Kosten/Gesundheitsergebnisse für alle Individuen zu jedem Zeitpunkt erfasst
      m.M <- m.C <- m.E <- matrix(nrow = n.i, ncol = n.t + 1,
                                  dimnames = list(paste("ind",   1:n.i, sep =" "),
                                                  paste("cycle", 0:n.t, sep =" "))) 
      
      m.M[, 1] <- v.M_1             # Angabe des initialen Gesundheitszustands 
      
      for (i in 1:n.i) {
        set.seed(seed + i)          # Setzen des Seeds für jedes Individuum für den Zufallszahlengenerator
        
        # Erstellen der dur-Variable, die die Anzahl der aufeinanderfolgenden Zyklen speichert, die das Individuum entweder im kranken oder kränkeren Zustand verbringt
        dur <- 0                            # Das Individuum beginnt ohne Vorgeschichte        
        m.C[i, 1] <- Costs(m.M[i, 1], Trt)  # Schätzung der Kosten pro Individuum für den initialen Gesundheitszustand abhängig von der Behandlung
        m.E[i, 1] <- Effs(m.M[i, 1], dur, Trt, X = X[i])  # Schätzung der QALYs pro Individuum für den initialen Gesundheitszustand abhängig von der Behandlung, der Dauer des kranken/kränkeren Zustands und den individuellen Merkmalen
        
        for (t in 1:n.t) {
          v.p <- Probs(m.M[i, t], dur)         # Berechnung der Übergangswahrscheinlichkeiten im Zyklus t abhängig von der Dauer des kranken/kränkeren Zustands
          
          m.M[i, t + 1] <- sample(v.n, prob = v.p, size = 1)  # Ziehen des neuen Gesundheitszustands und Speichern dieses Zustands in der Matrix m.M 
          m.C[i, t + 1] <- Costs(m.M[i, t + 1], Trt)     # Schätzung der Kosten pro Individuum während des Zyklus t + 1 abhängig von der Behandlung
          m.E[i, t + 1] <-  Effs(m.M[i, t + 1], dur, Trt, X = X[i])    # Schätzung des Nutzens pro Individuum während des Zyklus t + 1 abhängig von der Behandlung, der Dauer des kranken/kränkeren Zustands und den individuellen Merkmalen
          
          if (m.M[i, t + 1] == "S1" | m.M[i, t + 1] == "S2") {  # Ausdruck zur Identifizierung von kranken/kränkeren Individuen
            dur <- dur + 1   # Aktualisierung der Dauer des kranken/kränkeren Zustands
          } else {
            dur <- 0}        # Zurücksetzen der dur-Variable 
          
        } # Schließen der Schleife für die Zeitpunkte 
      } # Schließen der Schleife für die Individuen
      
      tc <- m.C %*% v.dwc       # Gesamtkosten (diskontiert) pro Individuum
      te <- m.E %*% v.dwe       # Gesamt-QALYs (diskontiert) pro Individuum 
      
      tc_hat <- mean(tc)        # Durchschnittliche (diskontierte) Kosten 
      te_hat <- mean(te)        # Durchschnittliche (diskontierte) QALYs
      
      if (TS.out == TRUE) {  # Erstellen einer Matrix der Zustandsübergänge
        TS <- paste(m.M, cbind(m.M[, -1], NA), sep = "->")  # Übergänge von einem Zustand zum anderen ###
        TS <- matrix(TS, nrow = n.i)
        rownames(TS) <- paste("Zyklus", 0:n.t, sep = " ")    # Benennen der Zeilen der Matrix
        colnames(TS) <- paste("Ind",   1:n.s, sep = " ")    # Benennen der Spalten der Matrix
      } else {
        TS <- NULL
      }
      
      if (TR.out == TRUE) {  # Erstellen einer Spur der individuellen Trajektorien
        TR <- t(apply(m.M, 2, function(x) table(factor(x, levels = v.n, ordered = TRUE))))
        TR <- TR / n.i                                    # Erstellen einer Verteilungsspur
        rownames(TR) <- paste("Zyklus", 0:n.t, sep = " ")  # Benennen der Zeilen der Matrix
        colnames(TR) <- v.n                               # Benennen der Spalten der Matrix
      } else {
        TR <- NULL
      }
      
      results <- list(m.M = m.M, m.C = m.C, m.E = m.E, tc = tc, te = te, tc_hat = tc_hat, te_hat = te_hat, TS = TS, TR = TR)   # Speichern der Ergebnisse der Simulation in einer Liste  
      return(results)   # Rückgabe der Ergebnisse
    } # Ende der MicroSim-Funktion
    
    #### Wahrscheinlichkeitsfunktion
    Probs <- function(M_it, dur) { 
      # M_it:   Gesundheitszustand, den das Individuum i im Zyklus t einnimmt (Zeichenvariable)
      # dur:    die Dauer des kranken Zustands (krank/kränker)
      
      v.p.it <- rep(NA, n.s)     # Erstellen eines Vektors der Zustandsübergangswahrscheinlichkeiten
      names(v.p.it) <- v.n       # Benennen des Vektors
      
      # Aktualisieren der Sterbewahrscheinlichkeiten nach der Umwandlung in Raten und Anwendung der Ratenverhältnisse
      r.S1D <-  - log(1 - p.S1D)
      r.S2D <-  - log(1 - p.S2D)
      p.S1D <- 1 - exp(- r.S1D * (1 + dur * rp.S1S2)) # Berechnung von p.S1D abhängig von der Dauer des kranken/kränkeren Zustands
      p.S2D <- 1 - exp(- r.S2D * (1 + dur * rp.S1S2)) # Berechnung von p.S2D abhängig von der Dauer des kranken/kränkeren Zustands
      
      # Aktualisieren von v.p.it mit den entsprechenden Wahrscheinlichkeiten   
      v.p.it[M_it ==  "H"] <- c(1 - p.HS1 - p.HD, p.HS1, 0, p.HD)                    # Übergangswahrscheinlichkeiten im gesunden Zustand
      v.p.it[M_it == "S1"] <- c(p.S1H, 1- p.S1H - p.S1S2 - p.S1D, p.S1S2, p.S1D)     # Übergangswahrscheinlichkeiten im kranken Zustand
      v.p.it[M_it == "S2"] <- c(0, 0, 1 - p.S2D, p.S2D)                              # Übergangswahrscheinlichkeiten im kränkeren Zustand
      v.p.it[M_it ==  "D"] <- c(0, 0, 0, 1)                                          # Übergangswahrscheinlichkeiten im toten Zustand
      ifelse(sum(v.p.it) == 1, return(v.p.it), print("Wahrscheinlichkeiten summieren sich nicht zu 1")) # Rückgabe der Übergangswahrscheinlichkeiten oder Erzeugen eines Fehlers
    }    
    
    ### Kostenfunktion
    Costs <- function (M_it, Trt = FALSE) {  
      # M_it: Gesundheitszustand, den das Individuum i im Zyklus t einnimmt (Zeichenvariable)
      # Trt:  wird das Individuum behandelt? (Standard ist FALSE)
      
      c.it <- 0                                   # Standardmäßig sind die Kosten für alle null
      c.it[M_it == "H"]  <- c.H                   # Aktualisieren der Kosten im gesunden Zustand
      c.it[M_it == "S1"] <- c.S1 + c.Trt * Trt    # Aktualisieren der Kosten im kranken Zustand abhängig von der Behandlung
      c.it[M_it == "S2"] <- c.S2 + c.Trt * Trt    # Aktualisieren der Kosten im kränkeren Zustand abhängig von der Behandlung
      return(c.it)                                # Rückgabe der Kosten
    }
    
    ### Gesundheitsergebnis-Funktion 
    Effs <- function (M_it, dur, Trt = FALSE, cl = 1, X = NULL) { 
      # M_it: Gesundheitszustand, den das Individuum i im Zyklus t einnimmt (Zeichenvariable)
      # dur:  die Dauer des kranken/kränkeren Zustands
      # Trt:  wird das Individuum behandelt? (Standard ist FALSE)
      # cl:   die Zykluslänge (Standard = 1 )
      # X:    der Vektor oder die Matrix der individuellen Merkmale (optional)
      
      u.it               <- 0        # Standardmäßig ist der Nutzen für alle null
      u.it[M_it == "H"]  <- u.H      # Aktualisieren des Nutzens im gesunden Zustand 
      u.it[M_it == "S1"] <- X * Trt * (u.Trt - dur * ru.S1S2) + (1 - Trt) * u.S1 # Aktualisieren des Nutzens im kranken Zustand abhängig von der Behandlung und der Dauer des kranken/kränkeren Zustands
      u.it[M_it == "S2"] <- u.S2     # Aktualisieren des Nutzens im kränkeren Zustand
      QALYs <- u.it * cl             # Berechnung der QALYs während des Zyklus t
      return(QALYs)                  # Rückgabe der Ergebnisse
    }
    
    ##################################### Simulation ausführen ##################################
    withProgress(message = 'Simulation läuft...', value = 0, {
      # Increment the progress bar
      incProgress(0.1, detail = "Initialisierung...")
      
    sim_no_trt <- MicroSim(v.M_1, n.i, n.t, v.n, X = v.x, d.c, d.e, TS.out = FALSE, TR.out = TRUE, Trt = FALSE) # Ausführung ohne Behandlung
    incProgress(0.4, detail = "Simulation ohne Behandlung abgeschlossen")
    
    sim_trt    <- MicroSim(v.M_1, n.i, n.t, v.n, X = v.x, d.c, d.e, TS.out = FALSE, TR.out = TRUE, Trt = TRUE) # Ausführung mit Behandlung
    incProgress(0.4, detail = "Simulation mit Behandlung abgeschlossen")
    
    # Finalize the progress bar
    incProgress(0.1, detail = "Fertig!")
    })
    
    # Erstellen des Anteilsdiagramms
    df_tr <- as.data.frame(sim_no_trt$TR)
    df_tr$Cycle <- 0:input$n.t
    df_tr_melt <- melt(df_tr, id.vars = "Cycle")
    
    # Aktualisieren der Gesundheitszustandsnamen in der Legende
    df_tr_melt$variable <- factor(df_tr_melt$variable, levels = c("H", "S1", "S2", "D"),
                                  labels = c("Gesund", "Krank", "Kränker", "Tot"))
    
    output$proportions_plot <- renderPlot({
      ggplot(df_tr_melt, aes(x = Cycle, y = value, fill = variable)) +
        geom_area(alpha = 0.6) +
        scale_fill_brewer(palette = "Set1") +
        labs(title = "Anteil der Individuen in jedem Gesundheitszustand über die Zeit",
             x = "Zyklus", y = "Anteil der Individuen", fill = "Gesundheitszustand") +
        theme_minimal()
    })
    
    # Erstellen der Histogramme
    output$hist_trt <- renderPlot({
      par(mfrow = c(1, 2))
      hist(sim_trt$tc, breaks = 50, col = "skyblue", main = "Behandlung: Gesundheitskosten",
           xlab = "Gesamtkosten in €", ylab = "Häufigkeit", border = "black", xaxt = "n", labels = FALSE)
      axis(1, at = axTicks(1), labels = format(axTicks(1), scientific = FALSE))  # Entfernen der wissenschaftlichen Notation
      hist(sim_trt$te, breaks = 50, col = "lightgreen", main = "Behandlung: QALYs",
           xlab = "Gesamt-QALYs", ylab = "Häufigkeit", border = "black", labels = FALSE)
    })
    
    output$hist_no_trt <- renderPlot({
      par(mfrow = c(1, 2))
      hist(sim_no_trt$tc, breaks = 50, col = "tomato", main = "Keine Behandlung: Gesundheitskosten",
           xlab = "Gesamtkosten in €", ylab = "Häufigkeit", border = "black", xaxt = "n", labels = FALSE)
      axis(1, at = axTicks(1), labels = format(axTicks(1), scientific = FALSE))  # Entfernen der wissenschaftlichen Notation
      hist(sim_no_trt$te, breaks = 50, col = "gold", main = "Keine Behandlung: QALYs",
           xlab = "Gesamt-QALYs", ylab = "Häufigkeit", border = "black", labels = FALSE)
    })
    
    ################################# Kosten-Nutzen-Analyse #############################
    
    # Speichern der durchschnittlichen Kosten (und des MCSE) jeder Strategie in einer neuen Variable C (Vektor der Kosten)
    v.C <- c(sim_no_trt$tc_hat, sim_trt$tc_hat) 
    se.C<- c(sd(sim_no_trt$tc), sd(sim_trt$tc)) / sqrt(n.i)
    # Speichern der durchschnittlichen QALYs (und des MCSE) jeder Strategie in einer neuen Variable E (Vektor der Gesundheitsergebnisse)
    v.E <- c(sim_no_trt$te_hat, sim_trt$te_hat)
    se.E<- c(sd(sim_no_trt$te), sd(sim_trt$te)) / sqrt(n.i)
    
    delta.C <- v.C[2] - v.C[1]                   # Berechnung der inkrementellen Kosten
    delta.E <- v.E[2] - v.E[1]                   # Berechnung der inkrementellen QALYs
    se.delta.E <- sd(sim_trt$te - sim_no_trt$te) / sqrt(n.i) # Monte-Carlo-Quadratfehler (MCSE) der inkrementellen QALYs
    se.delta.C <- sd(sim_trt$tc - sim_no_trt$tc) / sqrt(n.i) # Monte-Carlo-Quadratfehler (MCSE) der inkrementellen Kosten
    ICER <- delta.C / delta.E                    # Berechnung des ICER
    results <- c(delta.C, delta.E, ICER)         # Speichern der Werte in einer neuen Variable
    
    # Erstellen der vollständigen inkrementellen Kosten-Nutzen-Analyse-Tabelle
    table_micro <- data.frame(
      c(round(v.C, 0),  ""),           # Kosten pro Arm
      c(round(se.C, 0), ""),           # MCSE für Kosten
      c(round(v.E, 3),  ""),           # Gesundheitsergebnisse pro Arm
      c(round(se.E, 3), ""),           # MCSE für Gesundheitsergebnisse
      c("", round(delta.C, 0),   ""),  # Inkrementelle Kosten
      c("", round(se.delta.C, 0),""),  # MCSE für inkrementelle Kosten
      c("", round(delta.E, 3),   ""),  # Inkrementelle QALYs 
      c("", round(se.delta.E, 3),""),  # MCSE für Gesundheitsergebnisse (QALYs) gewonnen
      c("", round(ICER, 0),      "")   # ICER
    )
    
    rownames(table_micro) = c("Keine Behandlung", "Behandlung", "* sind MCSE-Werte")  # Benennen der Zeilen
    colnames(table_micro) = c("Kosten", "*",  "QALYs", "*", "Inkrementelle Kosten", "*", "QALYs gewonnen", "*", "ICER") # Benennen der Spalten 
    
    output$table_micro <- renderTable({ table_micro }, rownames = TRUE, colnames = TRUE)
  })
}

# Ausführung der Anwendung
shinyApp(ui = ui, server = server)

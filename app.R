# -------------------------------------------------------------------------------
# --------------- Shiny App Comparative Efficiency of SBF Designs ---------------
# -------------------------------------------------------------------------------

library(shiny)
library(shinycssloaders)
library(dplyr)
library(scales)
library(ggplot2)
library(grid)
library(TeachingDemos)
library(sfsmisc)
library(markdown)

source("3 Sequential Plots.R")
source("helperforapp.R")
source("3a-2 BFDA Analysis.R")
source("3a-1 SequentialSummary Plot.R")
source("2 FixedN Plots.R")
source("2 FixedN Functions.R")

e = new.env()

load_data <- function(){
  efficiencylist <- load("efficiencylist.RData", envir = e)
  ndistslist     <- load("ndistslist.RData",     envir = e)
  seq.simresults <- load("seq.simresults.RData", envir = e)
  fixed.simresults <- load("fixed.simresults.RData", envir = e)
  analysis.expBF.list <-    load("analysis.expBF.list.RData",    envir = e)
}

# Load data
load_data()

defaultstring <- HTML("Default: Cauchy(0, &radic;<span style=text-decoration:overline;>&nbsp;2&nbsp;</span> / 2)")
informedstring <- HTML("Informed: t(&mu; = 0.35, df = 3, r = 0.102)")
defaultstring.1 <- HTML("For default prior: Cauchy(0, &radic;<span style=text-decoration:overline;>&nbsp;2&nbsp;</span> / 2)")
informedstring.1 <- HTML("For informed prior: t(&mu; = 0.35, df = 3, r = 0.102)")

################################################################################
############################# USER INTERFACE ###################################
################################################################################


# Define UI for application

ui <- shinyUI(
  navbarPage(HTML("<span style='font-size:30px'>Bayes Factor Design Analysis</span>"),

# --------------------- UI: Instructions Panel ---------------------------------

             tabPanel("Instructions",
                      includeHTML("Instructions.html")),

# -------------------------- UI: Fixed-N ---------------------------------------
             
             tabPanel("Fixed N Design",
                      fluidPage(
                        
                        fluidRow(h3("General Settings for the Fixed N Design"),
                                 h2("")),
                        fluidRow(column(4,
                                 sliderInput(inputId = "true.ES.fixed",
                                             label = "If your hypothesis is true: Which effect size do you expect?",
                                             value = 0.5,
                                             min = 0.2,
                                             max = 1.2,
                                             step = 0.05,
                                             width = "500px")
                                 ),
                                 column(4,
                                        checkboxGroupInput(inputId = "method.fixed",
                                                           label = "Prior distribtion for effect sizes under the alternative hypothesis",
                                                           choiceNames = list(defaultstring, informedstring),
                                                           choiceValues = c("Default", "Informed"),
                                                           selected = c("Default", "Informed"),
                                                           width = "500px")
                                 ),
                                 column(4,
                                        downloadButton("D1", "Download Report for Fixed-N Design"))
                      ),
                      
                      fluidRow(h3("Part 1: What Bayes factors can I expect?")),
                      
                      fluidRow(sidebarLayout(sidebarPanel(id = "sidebar",
                                                          width = 4,
                                                          HTML("<span style='font-size:20px'><p>Select Settings for Details</p></span>"),
                                                          sliderInput(inputId = "samplesize.fixed",
                                                                      label = "Select a Sample Size",
                                                                      value = 50,
                                                                      min = 10,
                                                                      max = 200,
                                                                      step = 1,
                                                                      width = "500px"),
                                                          sliderInput(inputId = "decisionboundary.fixed",
                                                                      label = "Select a Decision Boundary",
                                                                      value = 10,
                                                                      min = 3,
                                                                      max = 30,
                                                                      step = 1,
                                                                      width = "500px"),
                                                          HTML("<span style='font-size:20px'><p>Select Information to Display</p></span>"),
                                                          checkboxGroupInput(inputId = "choiceBFdetails.fixed",
                                                                             label = NULL,
                                                                             choiceNames = c("Distribution of Bayes Factors", "Median Bayes Factors", "5%, 25%, 75%, and 95% Quantiles", "Rates of Misleading Evidence"),
                                                                             choiceValues = c("BFdist", "MedBF", "BFquantiles", "errorrates.fixedn"),
                                                                             select = "BFdist")
                                                          ),
                                             mainPanel(id = "MainPanel",
                                                       width = 8,
                                                       htmlOutput("nothingselected.error.fixed"),
                                                       tableOutput("medBF.table.default"),
                                                       tableOutput("medBF.table.informed"),
                                                       tableOutput("BF.quantiles.table.def"),
                                                       tableOutput("BF.quantiles.table.inf"),
                                                       tableOutput("errorrates.fixedn.table"),
                                                       plotOutput("BFdist.plot"))
                                             )),
                      fluidRow(h3("Part 2: What sample size do I need to obtain true positive or true negative evidence with a certain probability?")),
                      
                      fluidRow(sidebarLayout(sidebarPanel(id = "sidebar",
                                                          width = 4,
                                                          HTML("<span style='font-size:20px'><p>Select Settings for Details</p></span>"),
                                                          sliderInput(inputId = "probability.fixed",
                                                                      label = "Select a Probability",
                                                                      value = 0.8,
                                                                      min = 0.5,
                                                                      max = 0.99,
                                                                      step = 0.01,
                                                                      width = "500px"),
                                                          sliderInput(inputId = "evidencestrength.fixed",
                                                                      label = "Select a Strength of Evidence",
                                                                      value = 10,
                                                                      min = 3,
                                                                      max = 30,
                                                                      step = 1,
                                                                      width = "500px"),
                                                          checkboxInput("samplesizeH0.fixed",
                                                                        "Show Results Also For ES = 0 as Data Generating Process",
                                                                        FALSE,
                                                                        width = "500px")),
                                             mainPanel(id = "MainPanel",
                                                       width = 8,
                                                       htmlOutput("samplesizeH1.default.text"),
                                                       htmlOutput("samplesizeH1.informed.text"),
                                                       htmlOutput("samplesizeH0.default.text"),
                                                       htmlOutput("samplesizeH0.informed.text"))
                                             )))
             ),
             
# ---------------------- UI: Sequential Design --------------------------------- 

             tabPanel("Sequential Design",
                    fluidPage(
                                  
  
  # Define sidebar layout (color) 
  tags$head(tags$style(
    HTML('
         #sidebar {
         background-color: #eeeeee;
         }
         
         body, label, input, button, select { 
         font-family: "Arial";
         }')
  )),
  
  # Sequential Design UI: Step 1 (Overview)
  
  fluidRow(h3("Step 1: Expected N Given Different Boundaries")
           ),
  
  fluidRow(sidebarLayout(sidebarPanel(id = "sidebar",
                                      width = 4,
                  sliderInput(inputId = "true.ES",
                              label = "If your hypothesis is true: Which effect size do you expect?",
                              value = 0.5,
                              min = 0.2,
                              max = 1.2,
                              step = 0.05),
                  h4("Customize the Plot"),
                  checkboxGroupInput(inputId = "medians",
                                     label = "Choose a Prior Distribution on Effect Size",
                                     choiceNames = list(defaultstring.1, informedstring.1),
                                     choiceValues = c("Default", "Informed"),
                                     selected = c("Default", "Informed")
                                    ),
                  uiOutput("hingesreactive"),
                  uiOutput("whiskersreactive"),
                  htmlOutput("hingeswhiskers"),
                  HTML("<p><p><a href=efficiencyplot.explanation.jpeg/ target=_blank > Click here to see an explanation of the plot on the right</a></p>")
                  ),
           mainPanel(
                  plotOutput("efficiency")
                  ))
           ),
  
  # Sequential Design UI: Step 2 (Selected Boundaries and rates of misleading evidence)
  
  fluidRow(h3("Step 2: Selected boundaries and rates of misleading evidence")),
  
  fluidRow(column(2, style = "background-color:#eeeeee;",
                  h4("Select boundary and prior(s)"),
                  selectInput(inputId = "bound",
                              label = "Select a Boundary",
                              choices = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30),
                              selected = 6),
                  checkboxGroupInput(inputId = "method",
                                     label = "Select the Prior on Effect Size",
                                     choiceNames = list(defaultstring, informedstring),
                                     choiceValues = c("Default", "Informed"),
                                     selected = c("Default", "Informed")),
                  downloadButton("D2", HTML("Download Report for <br> Sequential Design"))
                  ),
           column(2, style = "background-color:#eeeeee;",
                  h4("Select information to display"),
                  checkboxGroupInput(inputId = "SummaryPlot",
                                     label = "Summary Plot",
                                     choiceNames = list("Display Summary (takes ~ 5 seconds)"),
                                     choiceValues = list("BFDAplot")),
                  checkboxGroupInput(inputId = "stats",
                                     label = "Statistics of the Distribution of N",
                                     choiceNames = list("Medians", "Upper and Lower Hinges", "Upper and Lower Whiskers", "5%, 25%, 75%, and 95% Quantile"),
                                     choiceValues = list("medians", "hinges", "whiskers", "quartiles")),
                  checkboxGroupInput(inputId = "FPFN",
                                     label = "Rates of Misleading Evidence",
                                     choiceNames = list("Under H0: False Positive Evidence", "Under H1: False Negative Evidence"),
                                     choiceValues = list("H0", "H1")),
                  checkboxGroupInput(inputId = "vioplot",
                                     label = "Violinplot of the Distribution of N",
                                     choiceNames = list("For ES = 0 [H0 is Correct]", "For Your Selected ES [H1 is Correct]"),
                                     choiceValues = list("H0", "H1")),
                  checkboxGroupInput(inputId = "bp",
                                     label = "Boxplot of the Distribution of N",
                                     choiceNames = list("For ES = 0 [H0 is Correct]", "For Your Selected ES [H1 is Correct]"),
                                     choiceValues = list("H0", "H1")),
                  checkboxGroupInput(inputId = "histogram",
                                     label = "Histograms of the Distribution of N",
                                     choiceNames = list("For ES = 0 [H0 is Correct]", "For Your Selected ES [H1 is Correct]"),
                                     choiceValues = list("H0", "H1"))
                  ),
           column(8,
                  tableOutput("mediantable.0"),
                  tableOutput("mediantable"),
                  tableOutput("hingestable"),
                  tableOutput("hingestable.0"),
                  tableOutput("whiskertable"),
                  tableOutput("whiskertable.0"),
                  tableOutput("quartilestable.default"),
                  tableOutput("quartilestable.informed"),
                  tableOutput("quartilestable.0.default"),
                  tableOutput("quartilestable.0.informed"),
                  tableOutput("FNerror"),
                  tableOutput("FPerror"),
                  plotOutput("FelixPlot.Default"),
                  plotOutput("FelixPlot.Informed"),
                  plotOutput("Cleanvioplot"),
                  plotOutput("Cleanboxplot"),
                  plotOutput("RUGhist")
                  ))
           ))))

################################################################################
################################# SERVER #######################################
################################################################################

# Define server logic 
server <- function(input, output) {
  
# ---- Create UI Choice Options for Hinges / Whiskers in Sequential Design -----
  
  choicenames.hinges <- reactive(methodselection(input$medians)[[1]])
  choicenames.whisker <- reactive(methodselection(input$medians)[[2]])
  choicevalues <- reactive(methodselection(input$medians)[[3]])
  
  output$hingesreactive <- renderUI({
    radioButtons(inputId = "hinges",
                 label = "Upper and lower hinge of N",
                 choiceNames = choicenames.hinges(),
                 choiceValues = choicevalues())
  })
  
  output$whiskersreactive <- renderUI({
    radioButtons(inputId = "whiskers",
                 label = "Upper and lower whiskers of N",
                 choiceNames = choicenames.whisker(),
                 choiceValues = choicevalues())
    
  })
  
  output$hingeswhiskers <- renderUI({
    if((input$whiskers == "default" & input$hinges == "informed") | (input$whiskers == "informed" & input$hinges == "default")){
      HTML("<p style=font-size:15px ><span style=color:red><strong>Please select the same prior distribution for hinges and whiskers!</strong></span></p>")}})

  
  # Get data for specified ES
  efficiency <- reactive({paste0("efficiency.", input$true.ES)})
  analysis.expBF <- reactive({paste0("analysis.expBF.", input$true.ES.fixed)})
  res <- reactive({paste0("res.", input$true.ES.fixed)})
  
  
# ---------------------- Server: Fixed-N Design --------------------------------
  
  # Create Bayes factor distribution plot
  
  output$nothingselected.error.fixed <- renderUI({if(length(input$method.fixed) == 0){
    HTML("<p style=font-size:20px >Please select&nbsp;a prior distribution on effect sizes (see above: Default: Cauchy(0,&radic;<span style=text-decoration: overline;>&nbsp;2&nbsp;</span> / 2) or Informed: t(&mu; = 0.35, df = 3, r = 0.102))</p>")
  }})
  
  
  
  output$BFdist.plot <- renderPlot({if ("BFdist" %in% input$choiceBFdetails.fixed){
    tryCatch({combine.plot.exp.BF.dist(method = input$method.fixed,
                                       res.H1 = e[["fixed.simresults"]][[res()]],
                                       res.H0 = e[["fixed.simresults"]][["res.0"]],
                                       true.ES.H1 = input$true.ES.fixed,
                                       samplesize = input$samplesize.fixed,
                                       boundary = c(1/input$decisionboundary.fixed, input$decisionboundary.fixed))},
             error = function(cond){return("")})
  }})
  
  # Create Table of median Bayes factors
  
  output$medBF.table.default <- renderTable({
    
    if("MedBF" %in% input$choiceBFdetails.fixed){
    
    medBF.1.table.default <- e[["analysis.expBF.list"]][[analysis.expBF()]]$df.default %>% filter(n == input$samplesize.fixed) %>% select(quant.50) %>% exp(.)
    medBF.0.table.default <- e[["analysis.expBF.list"]][["analysis.expBF.0"]]$df.default %>% filter(n == input$samplesize.fixed) %>% select(quant.50) %>% exp(.)
    medBF.table.default <- cbind(medBF.1.table.default, medBF.0.table.default)
    colnames(medBF.table.default) <- c("Median Bayes Factor (Default)", "Median Bayes Factor (Default, H0)")
    
    if("Default" %in% input$method.fixed){
      return(medBF.table.default)
    }}
  })
  
  output$medBF.table.informed <- renderTable({
    
    if ("MedBF" %in% input$choiceBFdetails.fixed){
    medBF.1.table.informed <- e[["analysis.expBF.list"]][[analysis.expBF()]]$df.informed %>% filter(n == input$samplesize.fixed) %>% select(quant.50) %>% exp(.)
    medBF.0.table.informed <- e[["analysis.expBF.list"]][["analysis.expBF.0"]]$df.informed %>% filter(n == input$samplesize.fixed) %>% select(quant.50) %>% exp(.)
    medBF.table.informed <- cbind(medBF.1.table.informed, medBF.0.table.informed)
    colnames(medBF.table.informed) <- c("Median Bayes Factor (Informed)", "Median Bayes Factor (Informed, H0)")
    
    if("Informed" %in% input$method.fixed){
      return(medBF.table.informed)
    }} 
    
  })
  
  # Create Table of Bayes Factor quantiles
  
  output$BF.quantiles.table.def <- renderTable({
    
    if ("BFquantiles" %in% input$choiceBFdetails.fixed){
    
    BF.quantiles.1.table.def <- e[["analysis.expBF.list"]][[analysis.expBF()]]$df.default %>% filter(n == input$samplesize.fixed) %>% select(quant.05, quant.25, quant.75, quant.95) %>% exp(.)
    BF.quantiles.0.table.def <- e[["analysis.expBF.list"]][["analysis.expBF.0"]]$df.default %>% filter(n == input$samplesize.fixed) %>% select(quant.05, quant.25, quant.75, quant.95) %>% exp(.)
    BF.quantiles.table.def <- cbind(c(paste0("H1: ES = ", input$true.ES.fixed), "H0: ES = 0"),rbind(BF.quantiles.1.table.def, BF.quantiles.0.table.def))
    colnames(BF.quantiles.table.def) <- c("DGP", "5%", "25%", "75%", "95%")
    
    if("Default" %in% input$method.fixed){
      return(BF.quantiles.table.def)
    }}},
    
    caption = "Quantiles of the Bayes Factor Distribution (Default Prior on Effect Size)",
    caption.placement = "top"
    
    )
  
  output$BF.quantiles.table.inf <- renderTable({
    
    if ("BFquantiles" %in% input$choiceBFdetails.fixed){
    
    BF.quantiles.1.table.inf <- e[["analysis.expBF.list"]][[analysis.expBF()]]$df.informed %>% filter(n == input$samplesize.fixed) %>% select(quant.05, quant.25, quant.75, quant.95) %>% exp(.)
    BF.quantiles.0.table.inf <- e[["analysis.expBF.list"]][["analysis.expBF.0"]]$df.informed %>% filter(n == input$samplesize.fixed) %>% select(quant.05, quant.25, quant.75, quant.95) %>% exp(.)
    BF.quantiles.table.inf <- cbind(c(paste0("H1: ES = ", input$true.ES.fixed), "H0: ES = 0"),rbind(BF.quantiles.1.table.inf, BF.quantiles.0.table.inf))
    colnames(BF.quantiles.table.inf) <- c("DGP", "5%", "25%", "75%", "95%")
    
    if("Informed" %in% input$method.fixed){
      return(BF.quantiles.table.inf)
    }}},
    
    caption = "Quantiles of the Bayes Factor Distribution (Informed Prior on Effect Size)",
    caption.placement = "top"
  
    )
  
  # Create Table for rates of misleading evidence 
  
  output$errorrates.fixedn.table <- renderTable({
    
      type1error.table <- FPFN.fixed(e[["fixed.simresults"]][["res.0"]], input$method.fixed, input$decisionboundary.fixed, input$samplesize.fixed, 1)
      type2error.table <- FPFN.fixed(e[["fixed.simresults"]][[res()]], input$method.fixed, input$decisionboundary.fixed, input$samplesize.fixed, 2)
      errors <- rbind(type1error.table, type2error.table)
      rownames(errors) <- c("False Positive Evidence Rates", "False Negative Evidence Rates")
      if(ncol(errors) == 1 & "Default" %in% input$method.fixed){colnames(errors) <- "Default Prior on ES"}
      if(ncol(errors) == 1 & "Informed" %in% input$method.fixed){colnames(errors) <- "Informed Prior on ES"}
    
    if("errorrates.fixedn" %in% input$choiceBFdetails.fixed & length(input$method.fixed) != 0){  
      return(errors)  
    
      }}, rownames = TRUE, digits = 3, caption = "Rates of Misleading Evidence", caption.placement = "top", colnames = TRUE)
  
  # Create Text Output for Part II: N for evidence strength with certain probability
  
  NforBF.H0 <- reactive({NforBF(e[["fixed.simresults"]][["res.0"]], input$evidencestrength.fixed, input$probability.fixed, H0 = TRUE)})
  NforBF.H1 <- reactive({NforBF(e[["fixed.simresults"]][[res()]], input$evidencestrength.fixed, input$probability.fixed, H0 = FALSE)})
  
  output$samplesizeH0.default.text <- renderUI({

    text.default.H0 <- NforBF.H0()[1]
    
    if("Default" %in% input$method.fixed & input$samplesizeH0.fixed == TRUE){
      HTML("<p style=font-size:15px >", paste0("If H0 is true and the default prior on effect size is used for analyses, ", text.default.H0, "</p>"))
    }
  })
  
  output$samplesizeH0.informed.text <- renderUI({
    
    text.informed.H0 <- NforBF.H0()[2]
    
    if("Informed" %in% input$method.fixed & input$samplesizeH0.fixed == TRUE){
      HTML("<p style=font-size:15px >", paste0("If H0 is true and the informed prior on effect size is used for analyses, ", text.informed.H0, "</p>"))
    }
  })
  
  output$samplesizeH1.default.text <- renderUI({
    
    text.default.H1 <- NforBF.H1()[1]
    
    if("Default" %in% input$method.fixed){
      HTML("<p style=font-size:15px >", paste0("If the effect size is ", input$true.ES.fixed, " and the default prior on effect size is used for analyses, ", text.default.H1, "</p>"))
    }
  })
  
  output$samplesizeH1.informed.text <- renderUI({
    
    text.informed.H1 <- NforBF.H1()[2]
    
    if("Informed" %in% input$method.fixed){
      HTML("<p style=font-size:15px >", paste0("If the effect size is ", input$true.ES.fixed, " and the informed prior on effect size is used for analyses, ", text.informed.H1, "</p>"))
    }
  })
  
  # Create download button for fixed-N design
  
  fixed.sim <- reactive(e[["fixed.simresults"]])
  analysis.expBF.list <- reactive(e[["analysis.expBF.list"]])
  
  output$D1 <- downloadHandler(
    
    filename = "report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(choiceBFdetails.fixed = input$choiceBFdetails.fixed,
                     method.fixed = input$method.fixed,
                     true.ES.fixed = input$true.ES.fixed,
                     samplesize.fixed = input$samplesize.fixed,
                     decisionboundary.fixed = input$decisionboundary.fixed,
                     res = res(),
                     fixed.sim = fixed.sim(),
                     analysis.expBF.list = analysis.expBF.list(),
                     analysis.expBF = analysis.expBF(),
                     probability.fixed = input$probability.fixed,
                     evidencestrength.fixed = input$evidencestrength.fixed,
                     samplesizeH0.fixed = input$samplesizeH0.fixed,
                     NforBF.H0 = NforBF.H0(),
                     NforBF.H1 = NforBF.H1())
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
# ---------------------- Server: Sequential Design -----------------------------
  
  # Create efficiency plot
  
  defaultmedian <- reactive(is.element("Default", input$medians))
  informedmedian <- reactive(is.element("Informed", input$medians))
  
  output$efficiency <- renderPlot({efficiencyplot(e[["efficiencylist"]][[efficiency()]],
                                                           input$true.ES,
                                                           defaultmedian(),
                                                           informedmedian(),
                                                           input$hinges,
                                                           input$whiskers)})
  
  
  
  # Create Histograms

  n.default.H1 <- reactive({as.vector(e[["ndistslist"]][[paste0("ndists.", input$true.ES)]][paste0("default.", input$bound)])})
  n.default.H0 <- reactive({as.vector(e[["ndistslist"]][["ndists.0"]][paste0("default.", input$bound)])})
  n.informed.H1 <- reactive({as.vector(e[["ndistslist"]][[paste0("ndists.", input$true.ES)]][paste0("informed.", input$bound)])})
  n.informed.H0 <- reactive({as.vector(e[["ndistslist"]][["ndists.0"]][paste0("informed.", input$bound)])})
  
  output$RUGhist <- renderPlot({
    if(length(input$histogram) > 0){
      histogram <- cleanhist.combine(true.ES = input$true.ES,
                                                  n.default.H1 = n.default.H1()[,1],
                                                  n.default.H0 = n.default.H0()[,1],
                                                  n.informed.H1 = n.informed.H1()[,1],
                                                  n.informed.H0 = n.informed.H0()[,1],
                                                  default = is.element("Default", input$method),
                                                  informed = is.element("Informed", input$method),
                                                  H0 = is.element("H0", input$histogram),
                                                  H1 = is.element("H1", input$histogram)
                                                  )
      return(histogram)
    } else {
      return()
    }
      })
  
  # Create Violinplot
  output$Cleanvioplot <- renderPlot({
    if(length(input$vioplot) > 0){
      vioplot <- cleanviolinplot(e[["ndistslist"]],
                                 true.ES = input$true.ES,
                                 bound = input$bound,
                                 default = is.element("Default", input$method),
                                 informed = is.element("Informed", input$method),
                                 H0 = is.element("H0", input$vioplot),
                                 H1 = is.element("H1", input$vioplot))
      return(vioplot)
    }
  })
  
  # Create Boxplotplot
  output$Cleanboxplot <- renderPlot({
    if(length(input$bp) > 0){
      boxplot <- cleanboxplot(e[["ndistslist"]],
                                 true.ES = input$true.ES,
                                 bound = input$bound,
                                 default = is.element("Default", input$method),
                                 informed = is.element("Informed", input$method),
                                 H0 = is.element("H0", input$bp),
                                 H1 = is.element("H1", input$bp))
      return(boxplot)
    }
  })
  
  # Create Tables
  
  tabdata <- reactive({subset.method(e[["efficiencylist"]][[efficiency()]],
                                     default = is.element("Default", input$method),
                                     informed = is.element("Informed", input$method))
                             })
  
  tabdata.0 <- reactive({subset.method(e[["efficiencylist"]][["efficiency.0"]],
                                      default = is.element("Default", input$method),
                                      informed = is.element("Informed", input$method),
                                      H0 = TRUE)})
  
  output$mediantable <- renderTable({
    if(is.element("medians", input$stats)){
    tab <- select(tabdata(), starts_with("median"), boundary2) %>% filter(boundary2 == input$bound) %>% select(-boundary2)
    colnames(tab) <- input$method
    return(tab)
    } else {
    return() 
    }
  }, align = "l", caption = "Median N if H1 is true", caption.placement = "top", digits = 0)
  
  output$mediantable.0 <- renderTable({
    if(is.element("medians", input$stats)){
      tab <- select(tabdata.0(), starts_with("median"), boundary2.H0) %>% filter(boundary2.H0 == input$bound) %>% select(-boundary2.H0)
      colnames(tab) <- input$method
      return(tab)
    } else {
      return() 
    }
  }, align = "l", caption = "Median N if H0 is true", caption.placement = "top", digits = 0)
  
  output$hingestable <- renderTable({
    if(is.element("hinges", input$stats)){
      tab <- select(tabdata(), starts_with("lowhinge"), starts_with("uphinge"), boundary2) %>% filter(boundary2 == input$bound) %>% select(-boundary2)
      if(ncol(tab) == 4){
        colnames(tab) <- c("Lower Hinge Default", "Lower Hinge Informed", "Upper Hinge Default", "Upper Hinge Informed")
      } else if (is.element("lowhinge.default", colnames(tab))){
        colnames(tab) <- c("Lower Hinge Default", "Upper Hinge Default")
      } else {
        colnames(tab) <- c("Lower Hinge Informed", "Upper Hinge Informed")
      }
      return(tab)
    } else {
      return()} 
    }, align = "l", caption = "Hinges of N if H1 is true", caption.placement = "top", digits = 0)
  
  output$hingestable.0 <- renderTable({
    if(is.element("hinges", input$stats)){
      tab <- select(tabdata.0(), starts_with("lowhinge"), starts_with("uphinge"), boundary2.H0) %>% filter(boundary2.H0 == input$bound) %>% select(-boundary2.H0)
      if(ncol(tab) == 4){
        colnames(tab) <- c("Lower Hinge Default", "Lower Hinge Informed", "Upper Hinge Default", "Upper Hinge Informed")
      } else if (is.element("lowhinge.default", colnames(tab))){
        colnames(tab) <- c("Lower Hinge Default", "Upper Hinge Default")
      } else {
        colnames(tab) <- c("Lower Hinge Informed", "Upper Hinge Informed")
      }
      return(tab)
    } else {
      return()}
  }, align = "l", caption = "Hinges of N if H0 is true", caption.placement = "top", digits = 0)
  
  output$whiskertable <- renderTable({
    if(is.element("whiskers", input$stats)){
      tab <- select(tabdata(), starts_with("lowwhisk"), starts_with("upwhisk"), boundary2) %>% filter(boundary2 == input$bound) %>% select(-boundary2)
      if(ncol(tab) == 4){
        colnames(tab) <- c("Lower Whisker Default", "Lower Whisker Informed", "Upper Whisker Default", "Upper Whisker Informed")
      } else if (is.element("lowhinge.default", colnames(tab))){
        colnames(tab) <- c("Lower Whisker Default", "Upper Whisker Default")
      } else {
        colnames(tab) <- c("Lower Whisker Informed", "Upper Whisker Informed")
      }
      return(tab)
    } else {
      return()}
  }, align = "l", caption = "Whiskers of N if H1 is true", caption.placement = "top", digits = 0)
  
  output$whiskertable.0 <- renderTable({
    if(is.element("whiskers", input$stats)){
      tab <- select(tabdata.0(), starts_with("lowwhisk"), starts_with("upwhisk"), boundary2.H0) %>% filter(boundary2.H0 == input$bound) %>% select(-boundary2.H0)
      if(ncol(tab) == 4){
        colnames(tab) <- c("Lower Whisker Default", "Lower Whisker Informed", "Upper Whisker Default", "Upper Whisker Informed")
      } else if (is.element("lowhinge.default", colnames(tab))){
        colnames(tab) <- c("Lower Whisker Default", "Upper Whisker Default")
      } else {
        colnames(tab) <- c("Lower Whisker Informed", "Upper Whisker Informed")
      }
      return(tab)
    } else {
      return()}
  }, align = "l", caption = "Whiskers of N if H0 is true", caption.placement = "top", digits = 0)
  
  output$quartilestable.default <- renderTable({
    if(is.element("quartiles", input$stats) & is.element("Default", input$method)){
      tab.default <- select(tabdata(), starts_with("quant"), boundary2) %>% filter(boundary2 == input$bound) %>% select(-boundary2) %>% select(ends_with("default"))
      if(ncol(tab.default) > 0){colnames(tab.default) <- c("5%", "25%", "75%", "95%")}
      
      return(tab.default)
      
    } else {
      return()}
    
  }, align = "l", caption = "Relevant Quantiles of the Distribution of N (DGP: H1, Default Prior on Effect Size)", caption.placement = "top", digits = 0)
  
  output$quartilestable.informed <- renderTable({
    if(is.element("quartiles", input$stats) & is.element("Informed", input$method)){
      tab.informed <- select(tabdata(), starts_with("quant"), boundary2) %>% filter(boundary2 == input$bound) %>% select(-boundary2) %>% select(ends_with("informed"))
      if(ncol(tab.informed) > 0){colnames(tab.informed) <- c("5%", "25%", "75%", "95%")}

      return(tab.informed)
      
    } else {
      return()}
  }, align = "l", caption = "Relevant Quantiles of the Distribution of N (DGP: H1, Informed Prior on Effect Size)", caption.placement = "top", digits = 0)
  
  
  output$quartilestable.0.default <- renderTable({
    if(is.element("quartiles", input$stats) & is.element("Default", input$method)){
      tab.default <- select(tabdata.0(), starts_with("quant"), boundary2.H0) %>% filter(boundary2.H0 == input$bound) %>% select(-boundary2.H0) %>% select(ends_with("default.H0"))
      if(ncol(tab.default) > 0){colnames(tab.default) <- c("5%", "25%", "75%", "95%")}
      return(tab.default)
    } else {
      return()
      }
  }, align = "l", caption = "Relevant Quantiles of the Distribution of N (DGP: H0, Default Prior on Effect Size)", caption.placement = "top", digits = 0)
  
  
  output$quartilestable.0.informed <- renderTable({
    if(is.element("quartiles", input$stats) & is.element("Informed", input$method)){
      tab.informed <- select(tabdata.0(), starts_with("quant"), boundary2.H0) %>% filter(boundary2.H0 == input$bound) %>% select(-boundary2.H0) %>% select(ends_with("informed.H0"))
      if(ncol(tab.informed) > 0){colnames(tab.informed) <- c("5%", "25%", "75%", "95%")}
      return(tab.informed)
    } else {
      return()
    }
  }, align = "l", caption = "Relevant Quantiles of the Distribution of N (DGP: H0, Informed Prior on Effect Size)", caption.placement = "top", digits = 0)
  

  # Create False Evidence Table
  
  output$FNerror <- renderTable({
    if (is.element("H1", input$FPFN)){
      errors <- select(tabdata(), starts_with("lower.hit"), boundary2) %>% filter(boundary2 == input$bound) %>% select(-boundary2)
      if(ncol(errors) == 2){
        colnames(errors) <- c("Default Prior on Effect Size", "Informed Prior on Effect Size")
      } else if (is.element("lower.hit.default", colnames(errors))) {
        colnames(errors) <- "Default Prior on Effect Size"
      } else {
        colnames(errors) <- "Informed Prior on Effect Size"
      }
      return(errors)
    } else {
      return()
    }
  }, align = "l", caption = "False Negative Error Rates", caption.placement = "top"
  )
   
  output$FPerror <- renderTable({
    if (is.element("H0", input$FPFN)){
      errors <- select(tabdata.0(), starts_with("upper.hit"), boundary2.H0) %>% filter(boundary2.H0 == input$bound) %>% select(-boundary2.H0)
      if(ncol(errors) == 2){
        colnames(errors) <- c("Default Prior on Effect Size", "Informed Prior on Effect Size")
      } else if (is.element("lower.hit.default", colnames(errors))) {
        colnames(errors) <- "Default Prior on Effect Size"
      } else {
        colnames(errors) <- "Informed Prior on Effect Size"
      }
      return(errors)
    } else {
      return()
    }
  }, align = "l", caption = "False Positive Error Rates", caption.placement = "top"
  )
  
  # Create Plot a la Felix
  
  BFDA.result <- reactive({paste0("SequentialBF.obsub.", input$true.ES)})
  
  output$FelixPlot.Default <- renderPlot({
    if (length(input$SummaryPlot) > 0 & is.element("Default", input$method)){
      summaryplot <- summaryplot.sequentialBFDA(SequentialBF.obsub = e[["seq.simresults"]][[BFDA.result()]], 
                                                boundary = as.numeric(input$bound),
                                                method = "default")
      return(summaryplot)
    } else {
      return
    }
  }, height = 600, width = 800)
  
  output$FelixPlot.Informed <- renderPlot({
    if (length(input$SummaryPlot) > 0 & is.element("Informed", input$method)){
      summaryplot <- summaryplot.sequentialBFDA(SequentialBF.obsub = e[["seq.simresults"]][[BFDA.result()]],
                              boundary = as.numeric(input$bound),
                              method = "informed")
      return(summaryplot)
    } else {
      return
    }
  }, height = 600, width = 800)
  
  # Create download button for fixed-N design
  
  ndistslist <- reactive(e[["ndistslist"]])
  efficiencylist <- reactive(e[["efficiencylist"]])
  seq.simresults <- reactive(e[["seq.simresults"]])
  
  output$D2 <- downloadHandler(
    
    filename = "reportSeqBFDA.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "reportSeqBFDA.Rmd")
      file.copy("reportSeqBFDA.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(true.ES = input$true.ES,
                     medians = input$medians,
                     bound = input$bound,
                     method = input$method,
                     SummaryPlot = input$SummaryPlot,
                     stats = input$stats,
                     FPFN = input$FPFN,
                     vioplot = input$vioplot,
                     bp = input$bp,
                     histogram = input$histogram,
                     choicenames.hinges = choicenames.hinges(),
                     choicenames.whisker = choicenames.whisker(),
                     choicevalues = choicevalues(),
                     hinges = input$hinges,
                     whiskers = input$whiskers,
                     efficiency = efficiency(),
                     defaultmedian = defaultmedian(),
                     informedmedian = informedmedian(),
                     n.default.H1 = n.default.H1(),
                     n.default.H0 = n.default.H0(),
                     n.informed.H1 = n.informed.H1(),
                     n.informed.H0 = n.informed.H0(),
                     ndistslist = ndistslist(),
                     efficiencylist = efficiencylist(),
                     seq.simresults = seq.simresults(),
                     tabdata = tabdata(),
                     tabdata.0 = tabdata.0(),
                     BFDA.result = BFDA.result()) 
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


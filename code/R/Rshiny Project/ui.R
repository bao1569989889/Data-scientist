library(shiny) 


fluidPage(    
  
  
  titlePanel("NYC"),
  
  
  sidebarLayout(      
    
    
    sidebarPanel(
      selectInput("region", "Region:", 
                  choices=colnames(r)),
      hr(),
      helpText("NYC tree condition by diffrent region "),
    
      br(),
      sliderInput("n",
                  "Number of observations:",
                  value = 30,
                  min = 1,
                  max = 66),
      hr(),
      helpText("66 bins total"),
      
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 30),
      
      hr(),
      helpText("132 rows total")
      
      
      
      
      
      
      
      ),
    
    
    mainPanel(
      tabsetPanel(
      tabPanel("Summary", verbatimTextOutput("summary")), 
      tabPanel("tree condition", plotOutput("Plot")),
      tabPanel("tree status", plotOutput("plot2")),
      tabPanel("mean diameter for species 1- 66", plotOutput("plot3")),
      tabPanel("mean diameter for species 67- 132",plotOutput("plot4")),
      tabPanel("table for mean diametr each species",tableOutput("outTable"))
      )    
    )
    
  )
)

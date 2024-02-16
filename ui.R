fluidPage(
  
  # Application title
  titlePanel("Segmentation"),
  
  # Sidebar with a slider input for number of bins
  fluidRow(
    
    column(2, 
           wellPanel(
             
             # File input
             fileInput("input_file", "Choose CSV File", accept = ".csv"),
             
             # DTREE Engine
             selectInput("dtree_engine", "Select D-Tree Engine", c("RPART", "CTREE"), "RPART"),
             
             # Model inputs
             selectInput("covariates", "X-Vars", choices = NULL, selected = NULL, multiple = T, selectize = T),
             selectInput("dependent", "Y-Var", choices = NULL, selected = NULL, multiple = F, selectize = T),
             
             # Parameters
             tags$b("Model Parameters:"),
             tags$hr(),
             
             numericInput("par_minbucket", "Minbucket", value = 100),
             numericInput("par_maxdepth", "Maxdepth", value = 3),
             numericInput("par_cp", "Cp (RPART)", value = -1),
             numericInput("par_mincriterion", "Mincriterion (CTREE)", value = 0.95),
             
             # Model run
             actionButton("run_model", "Fit Model")
           )
    ),
    
    # Plot output
    column(10, 
           verbatimTextOutput("data_stats"),
           plotOutput("tree_plot"))
  )
)

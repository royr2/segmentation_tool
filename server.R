options(shiny.maxRequestSize=300*1024^2) 

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Read file and update inputs 
  observeEvent(input$input_file, {
    model_data <<- read.csv(input$input_file$datapath)
    model_data <<- as.data.frame(model_data)
    
    # Update input section
    updateSelectInput(session, inputId = "covariates", choices = colnames(model_data), selected = NULL)
    updateSelectInput(session, inputId = "dependent", choices = colnames(model_data), selected = NULL)
  })
  
  # Reactive expressions
  engine <<- reactive({input$dtree_engine})
  
  # Fit Model
  observeEvent(input$run_model, {
    
    form <<- paste0(input$dependent, "~", paste0(input$covariates, collapse = "+"))
    
    if(engine() == "RPART"){
      mdl <<- rpart(as.formula(form), 
                    data = model_data, 
                    control = rpart.control(minbucket = input$par_minbucket, 
                                            cp = input$par_cp, 
                                            maxdepth = input$par_maxdepth))
      
      output$tree_plot <- renderPlot({
        rpart.plot(mdl, main = paste("Segmentation via", engine()))
      })
    }
    
    if(engine() == "CTREE"){
      rec <- recipe(model_data, formula = form) %>% 
        step_string2factor(all_string()) %>%
        step_dummy(all_factor()) %>% 
        prep()
      
      model_data_ctree <- bake(rec, new_data = NULL) %>% 
        mutate(!! sym(input$dependent) := as.factor(!! sym(input$dependent)))
      
      form <<- paste0(input$dependent, "~ .")
      
      mdl <<- ctree(as.formula(form), 
                    data = model_data_ctree, 
                    control = ctree_control(minbucket = input$par_minbucket, 
                                            maxdepth = input$par_maxdepth, 
                                            mincriterion = input$par_mincriterion))
      
      output$tree_plot <- renderPlot({
        plot.party(mdl, main = paste("Segmentation via", engine()))
      })
    }
    
  })
  
  
  
  
}

library(shiny)
library(shinyjs)
library(taxize)

# Define UI for app that draws a histogram ----

kingdom_list <- c("Archaea","Bacteria","Eukaryota","Fungi","Metazoa","Plant"="Viridiplantae","Viruses")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  tabPanel(title = "Library Generation",
           mainPanel(# the following lines could be uncommented when the download ref seq can
             # work on the rest of the kingdoms
             # radioButtons("kingdom", "Choose a kingdom:",
             #              c("Archaea" = "archaea",
             #                "Bacteria" = "bacteria",
             #                "Fungi" = "fungi",
             #                "Invertebrate" = "invertebrate",
             #                "Plant" = "plant",
             #                "Protozoa" = "protozoa",
             #                "Vertebrate" = "vertibrate",
             #                "Vertebrate other" = "vertibrate_other",
             #                "Virus" = "viral")
             #              ),
             
             # create checkbox input for representative library and reference library
             checkboxInput("representative", "representative", value = TRUE, width = NULL),
             checkboxInput("reference", "reference", value = FALSE, width = NULL),
             
             
             actionButton("downloadref","Download Ref_Seq")
           )
  ),
  mainPanel(
    tabsetPanel(
      type = "tabs",
      
      tabPanel("Kingdom",
               radioButtons("kingdomGroup", label = "Choose a kingdom:", 
                                  choices = kingdom_list),
               actionButton("kingdom_update","Update")
      ),
      
      tabPanel("Phylum",
               checkboxGroupInput("phylumGroup", label = "Choose a phylum:"),
               actionButton("phylum_update","Update")
      ),
      
      tabPanel("Class",
               checkboxGroupInput("classGroup", label = "Choose a class:"),
               actionButton("class_update","Update")
      ),
      tabPanel("Order",
               checkboxGroupInput("orderGroup", label = "Choose an order:"),
               actionButton("order_update","Update")
      ),
      tabPanel("Family",
               checkboxGroupInput("familyGroup", label = "Choose a family:"),
               actionButton("family_update","Update")
      ),
      tabPanel("Genus",
               checkboxGroupInput("genusGroup", label = "Choose a genus:"),
               actionButton("genus_update","Update")
      ),
      tabPanel("Species",
               checkboxGroupInput("speciesGroup", label = "Choose a species:"),
               actionButton("species_update","Update")
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  output$value <- renderPrint({ input$kingdomGroup })
  
  #output$phylumPanel <- renderUI({
  #  if(!is.null(input$kingdomGroup)){
  #    checkboxGroupInput("phylumGroup", label = "Choose a phylum:", 
  #                       choices = c("A","B"))
  #  }else{
  #    h3("No kingdom selected")
  #  }
   # 
  #})
  
  observeEvent(input$kingdom_update,{
    k_input <- input$kingdomGroup
    
    p_list <- character(0)
    c_list <- character(0)
    o_list <- character(0)
    f_list <- character(0)
    g_list <- character(0)
    s_list <- character(0)
    
    for(k in k_input){
      children_list <- NULL
      children_list <- children(k, db = 'ncbi')
      if(!is.null(children_list)){
        children_list <- children_list[[1]]
        
        children_list.p <- children_list[which(children_list$childtaxa_rank == 'phylum'),]
        children_list.c <- children_list[which(children_list$childtaxa_rank == 'class'),]
        children_list.o <- children_list[which(children_list$childtaxa_rank == 'order'),]
        children_list.f <- children_list[which(children_list$childtaxa_rank == 'family'),]
        children_list.g <- children_list[which(children_list$childtaxa_rank == 'genus'),]
        children_list.s <- children_list[which(children_list$childtaxa_rank == 'species'),]
        
        p_list <- append(p_list, children_list.p$childtaxa_name)
        c_list <- append(c_list, children_list.c$childtaxa_name)
        o_list <- append(o_list, children_list.o$childtaxa_name)
        f_list <- append(f_list, children_list.f$childtaxa_name)
        g_list <- append(g_list, children_list.g$childtaxa_name)
        s_list <- append(s_list, children_list.s$childtaxa_name)
      }
    }
    
    updateCheckboxGroupInput(session, "phylumGroup", choices = p_list)
    updateCheckboxGroupInput(session, "classGroup", choices = c_list)
    updateCheckboxGroupInput(session, "orderGroup", choices = o_list)
    updateCheckboxGroupInput(session, "familyGroup", choices = f_list)
    updateCheckboxGroupInput(session, "genusGroup", choices = g_list)
    updateCheckboxGroupInput(session, "speciesGroup", choices = s_list)
  })
  
  observeEvent(input$phylum_update,{
    p_input <- input$phylumGroup
    
    c_list <- character(0)
    o_list <- character(0)
    f_list <- character(0)
    g_list <- character(0)
    s_list <- character(0)
    
    for(p in p_input){
      children_list <- NULL
      Sys.sleep(1)
      children_list <- children(p, db = 'ncbi')
      if(!is.null(children_list)){
        children_list <- children_list[[1]]
        
        children_list.c <- children_list[which(children_list$childtaxa_rank == 'class'),]
        children_list.o <- children_list[which(children_list$childtaxa_rank == 'order'),]
        children_list.f <- children_list[which(children_list$childtaxa_rank == 'family'),]
        children_list.g <- children_list[which(children_list$childtaxa_rank == 'genus'),]
        children_list.s <- children_list[which(children_list$childtaxa_rank == 'species'),]
        
        c_list <- append(c_list, children_list.c$childtaxa_name)
        o_list <- append(o_list, children_list.o$childtaxa_name)
        f_list <- append(f_list, children_list.f$childtaxa_name)
        g_list <- append(g_list, children_list.g$childtaxa_name)
        s_list <- append(s_list, children_list.s$childtaxa_name)
      }
    }
    
    updateCheckboxGroupInput(session, "classGroup", choices = c_list)
    updateCheckboxGroupInput(session, "orderGroup", choices = o_list)
    updateCheckboxGroupInput(session, "familyGroup", choices = f_list)
    updateCheckboxGroupInput(session, "genusGroup", choices = g_list)
    updateCheckboxGroupInput(session, "speciesGroup", choices = s_list)
  })
  
  observeEvent(input$class_update,{
    c_input <- input$classGroup
    
    o_list <- character(0)
    f_list <- character(0)
    g_list <- character(0)
    s_list <- character(0)
    
    for(c in c_input){
      children_list <- NULL
      children_list <- children(c, db = 'ncbi')
      if(!is.null(children_list)){
        children_list <- children_list[[1]]
        
        children_list.o <- children_list[which(children_list$childtaxa_rank == 'order'),]
        children_list.f <- children_list[which(children_list$childtaxa_rank == 'family'),]
        children_list.g <- children_list[which(children_list$childtaxa_rank == 'genus'),]
        children_list.s <- children_list[which(children_list$childtaxa_rank == 'species'),]
        
        o_list <- append(o_list, children_list.o$childtaxa_name)
        f_list <- append(f_list, children_list.f$childtaxa_name)
        g_list <- append(g_list, children_list.g$childtaxa_name)
        s_list <- append(s_list, children_list.s$childtaxa_name)
      }
    }
    
    updateCheckboxGroupInput(session, "orderGroup", choices = o_list)
    updateCheckboxGroupInput(session, "familyGroup", choices = f_list)
    updateCheckboxGroupInput(session, "genusGroup", choices = g_list)
    updateCheckboxGroupInput(session, "speciesGroup", choices = s_list)
  })
  
  observeEvent(input$order_update,{
    o_input <- input$orderGroup

    f_list <- character(0)
    g_list <- character(0)
    s_list <- character(0)
    
    for(o in o_input){
      children_list <- NULL
      delay
      children_list <- children(o, db = 'ncbi')
      if(!is.null(children_list)){
        children_list <- children_list[[1]]
        
        children_list.f <- children_list[which(children_list$childtaxa_rank == 'family'),]
        children_list.g <- children_list[which(children_list$childtaxa_rank == 'genus'),]
        children_list.s <- children_list[which(children_list$childtaxa_rank == 'species'),]
        
        f_list <- append(f_list, children_list.f$childtaxa_name)
        g_list <- append(g_list, children_list.g$childtaxa_name)
        s_list <- append(s_list, children_list.s$childtaxa_name)
      }
    }
    
    updateCheckboxGroupInput(session, "familyGroup", choices = f_list)
    updateCheckboxGroupInput(session, "genusGroup", choices = g_list)
    updateCheckboxGroupInput(session, "speciesGroup", choices = s_list)
  })
  
  observeEvent(input$family_update,{
    f_input <- input$familyGroup
    
    g_list <- character(0)
    s_list <- character(0)
    
    for(f in f_input){
      children_list <- NULL
      children_list <- children(f, db = 'ncbi')
      if(!is.null(children_list)){
        children_list <- children_list[[1]]
        
        children_list.g <- children_list[which(children_list$childtaxa_rank == 'genus'),]
        children_list.s <- children_list[which(children_list$childtaxa_rank == 'species'),]

        g_list <- append(g_list, children_list.g$childtaxa_name)
        s_list <- append(s_list, children_list.s$childtaxa_name)
      }
    }
    
    updateCheckboxGroupInput(session, "genusGroup", choices = g_list)
    updateCheckboxGroupInput(session, "speciesGroup", choices = s_list)
  })
  
  observeEvent(input$genus_update,{
    g_input <- input$genusGroup
    
    s_list <- character(0)
    
    for(g in g_input){
      children_list <- NULL
      children_list <- children(g, db = 'ncbi')
      if(!is.null(children_list)){
        children_list <- children_list[[1]]
        
        children_list.s <- children_list[which(children_list$childtaxa_rank == 'species'),]
        
        s_list <- append(s_list, children_list.s$childtaxa_name)
      }
    }

    updateCheckboxGroupInput(session, "speciesGroup", choices = s_list)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

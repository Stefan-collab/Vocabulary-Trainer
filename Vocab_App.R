library(shiny); library(shinythemes); library(googlesheets4); library(googledrive); library(readr)
library(dplyr); library(purrr); library(tidyr); library(ggplot2); library(DT); library(readxl)


# setting encoding and locale is important for the language-specific letters
# e.g. I choose locale = 'Turkish' for my Turkish vocabulary trainer
options(encoding="UTF-8")
Sys.setlocale(category = "LC_ALL", locale = "Turkish")

options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "your@email.adress")
gs4_auth(token = drive_token())
GoogleSheetID = "xxxxx"

###### --------------------- UI ------------------------------
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  tags$style(HTML('table.dataTable tr:nth-child(even) {background-color: dimgrey !important;}')),
  tags$style(HTML('table.dataTable tr:nth-child(odd) {background-color: grey !important;}')),  
  tags$style(HTML('table.dataTable th {background-color: dimgrey !important;}')),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("Lektion","Lektion", 
                     choices = c(0:13), 
                     selected = c(0:12), 
                     multiple = T),
      conditionalPanel(condition="input.tabselected == 1 | input.tabselected == 2",
                       checkboxGroupInput("Box","Box", 
                                          choices = 1:5,
                                          selected = 4:5)
                       ),
      conditionalPanel(condition="input.tabselected == 1",
                      hr(),
                      actionButton("neuesWort","Neues Wort"),
                      actionButton("DEzeigen","Deutsch zeigen"),
                      hr(),
                      actionButton("richtig", "richtig", class = "btn-success"),
                      actionButton("falsch", "falsch", class = "btn-danger")
                    
                      
      ),
      conditionalPanel(condition="input.tabselected == 2",
                       actionButton("Filteranwenden", "Filter anwenden"),
                       hr(),
                       downloadButton("download_data", "Komplettabzug herunterladen")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Test", value=1,
                 tableOutput("test_table"),
                 tableOutput("rfTable")
        ),
        tabPanel("Tabelle", value=2,
                 dataTableOutput("Tabelle")
        ),
        tabPanel("Top5", value=3,
                 tableOutput("Top5")
        ),
        tabPanel("Grafiken", value=4,
                 plotOutput("plot1"), hr(),
                 plotOutput("plot2")
        ),
        id = "tabselected"
        )

    )
  )
)


###### --------------------- Server ------------------------------
server <- function(input, output){
  
  rawdata = eventReactive({
    input$neuesWort
    input$Filteranwenden},
    {
    rawdata = read_sheet(GoogleSheetID)
          
    rawdata %>%
      mutate(Lektion = as.integer(Lektion),
             Index = as.integer(Index),
             Box = as.integer(Box),
             Count = as.integer(Count),
             CountR = as.integer(CountR))
    }, ignoreNULL = FALSE)
  
  
  data = eventReactive(eventExpr = {
    input$neuesWort
    input$Filteranwenden},
    valueExpr = {
      rawdata() %>%
        filter(Lektion %in% input$Lektion,
               Box %in% input$Box) %>%
        select(`Türkisch`, Deutsch, Box, Count, CountR, Lektion, Index)
    }, ignoreNULL = FALSE)
  
  
  output$Tabelle = DT::renderDataTable({
    data() %>% 
      mutate(Fails = Count - CountR) %>%
      select(1:5, Fails, Lektion, Index)
  }, rownames=F, options=list(pageLength=10))
  
  rnum = eventReactive(eventExpr = input$neuesWort, 
                       valueExpr = {round(runif(1, 1, nrow(data())),0)},
                       ignoreNULL = FALSE)
  
  DEflag = reactiveValues(data1 = 0)
  observeEvent(input$neuesWort, {DEflag$data1 <- 0})
  observeEvent(input$DEzeigen,  {DEflag$data1 <- 1})
  
  observeEvent(input$richtig,
               {temp_rawdata = rawdata()
               rowindex = which(temp_rawdata$Index == pull(data()[rnum(),"Index"]) )
               
               temp_rawdata[rowindex, "Box"] <-  
                 max(1, pull(temp_rawdata[ rowindex, "Box"]) - 1)
               
               temp_rawdata[rowindex, "Count"] <-
                 pull(temp_rawdata[rowindex, "Count"]) + 1
               
               temp_rawdata[rowindex, "CountR"] <-
                 pull(temp_rawdata[rowindex, "CountR"]) + 1
               range_write(temp_rawdata, ss = GoogleSheetID)
               
               rm(temp_rawdata, rowindex)})
  
  observeEvent(input$falsch,
               {temp_rawdata = rawdata()
               rowindex = which(temp_rawdata$Index == pull(data()[rnum(),"Index"]) )
               
               temp_rawdata[rowindex, "Box"] <- 5

               
               temp_rawdata[rowindex, "Count"] <-
                 pull(temp_rawdata[rowindex, "Count"]) + 1

               range_write(temp_rawdata, ss = GoogleSheetID)
               
               rm(temp_rawdata)})
  
  output$test_table = renderTable({

    if(DEflag$data1 == 0){
      data()[rnum(), "Türkisch"]
    }else{
      data()[rnum(), ]
    }
      
  })
  

   output$plot1 = renderPlot(
     ggplot(
       rawdata() %>%
         filter(Lektion %in% input$Lektion) %>%
         group_by(Box) %>%
         summarise(n = n(), a="Box"),
       aes(a, n, fill=as.factor(Box))) + 
       geom_bar(stat="identity") +
       coord_flip() +
       labs(x=NULL, y=NULL, fill=NULL)
     )
   
   output$plot2 = renderPlot(
     ggplot(
       rawdata() %>%
         filter(Lektion %in% input$Lektion) %>%
         group_by(Lektion, Box) %>%
         summarise(n = n()),
       aes(Lektion, n, fill=as.factor(Box))) +
       geom_bar(stat = "identity") +
       labs(x=NULL, y=NULL, fill=NULL)
   )
   
   
   output$download_data <- downloadHandler(
     filename = function(){paste0("TurkishApp.csv")},
     content = function(file){write.csv2(rawdata(), file, row.names = F)}
   )
  
   
   output$Top5 = renderTable({
     data() %>%
       mutate(Fails = Count - CountR) %>%
       filter(Box == 5) %>%
       arrange(desc(Fails)) %>%
       select(1:5, Fails, Lektion, Index) %>%
       head(5)
   }, rownames=F, options=list(pageLength=10))
   
}


shinyApp(ui, server)

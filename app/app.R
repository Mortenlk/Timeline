#App.R


rm(list=ls()) # Fjerner alle matriser

# Pakker ------------------------------------------------------------------


library(shiny)
library(shinydashboard)
library(RMariaDB)
library(RMariaDB)
library(tidyverse)
library(plotly)
library(DT)
library(DBI)
library(ggthemes)
library(png)
library(corrplot)
library(rmarkdown)
library(timevis)
library(rhandsontable)
library(jsonlite)



# keys --------------------------------------------------------------------


id = fromJSON("keys.json")$id  

pw = fromJSON("keys.json")$pw



# Første tabell -----------------------------------------------------------


tidsl <- data.frame(
  id      = 1:3,
  content = c("A",
              "B",
              "C"
              ),
  start   = c("2019-11-06 08:05:00",
              "2019-11-06 10:25:00",
              "2019-11-06 11:35:00"
              ),
  end     = c("2019-11-06 10:00:00",
              "2019-11-06 11:35:00",
              "2019-11-08 08:38:00"
              )
)


#df <- tidsl

ui <- dashboardPage(

#1  Header ------------------------------------------------------------------

  dashboardHeader( disable = FALSE,
    
    title = "dashboard",
    titleWidth = 450


  )# Header slutt
,


# Sidebar -----------------------------------------------------------------

  dashboardSidebar(  disable = FALSE,
                     width = 250,

    
# Sidebarmenu -------------------------------------------------------------

        
    sidebarMenu(


# Sidebarmenu 1 Dashboard -------------------------------------------------

      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")
               )# menuItem slutt



    ) #Menu slutt

    
  ) # Sidebar slutt
,


# 2 Body --------------------------------------------------------------------

  dashboardBody(
    
    tabItems(

# Body 1 Dashboard --------------------------------------------------------

      tabItem(tabName = "dashboard",
              
              
              fluidRow(
                
                tabBox(
                  height = 300,
                  width = 12,
                  side = "right",
                  title = " ",
                  tabPanel("Tidsfrister",timevisOutput("timeline"))
                  
                )
                
                
              ),
              
              fluidPage(
                mainPanel(
                  rHandsontableOutput("hot"),
                  actionButton("save","Lagre")
                )
              )
              
              
      ) 

        
) 



# Neste side kommer her (lim inn over. -----------------------------------------------


    ) #  dashboardBody slutt
    
    
  ) #Body slutt





# 3 Server ------------------------------------------------------------------


server <- function(input, output) { 


  #Tidslinje  
  
  output$timeline <- renderTimevis({
    timevis(df)
  })

  
  

# Reactive ----------------------------------------------------------------

  
  
  con <- dbConnect(RMariaDB::MariaDB(),
                   user= id, password= pw,
                   dbname="livetabell", host="localhost")
  
  
  
  #dbWriteTable(con, "df", values$data, overwrite=T)
  
  df <- dbReadTable(con, "df", df, overwrite=TRUE)
  
  
  dbDisconnect(con)
  rm(con)
  
  
  values <- reactiveValues(data = df)
  
  observe({
    if(!is.null(input$hot)){
      values$data <- as.data.frame(hot_to_r(input$hot))
     # isolate(values$data[,'conten'] <- ifelse(values$data[,'id'], values$data[,'start']-values$data[,'end'] ,0))
      print(values$data)
      output$hot <- renderRHandsontable({
        rhandsontable(values$data)
      })
    }
  })    
  
  output$hot <- renderRHandsontable({
    rhandsontable(values$data)
  })
  

  # When the Save button is clicked, last opp
  observeEvent(input$save, {
    
    
    con <- dbConnect(RMariaDB::MariaDB(),
                     user= id , password= pw,
                     dbname="livetabell", host="localhost")

#    dbWriteTable(con, "df", df, overwrite=T)
    
    
    
    dbWriteTable(con, "df", values$data, overwrite=T)
    
   # df <- dbReadTable(con, "df", alt, overwrite=TRUE)
    
    
    dbDisconnect(con)
    rm(con)
    
    
  })
  
  
  
  
  
  } # Server slutt


# App ---------------------------------------------------------------------


shinyApp(ui, server)
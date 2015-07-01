require(shiny)
require(DT)
require(ggplot2)
require(scales)
runApp(
  list(
    ui = fluidPage(
      sidebarPanel(fileInput('file1', 'Choose CSV File',
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain', 
                                      '.csv')),
                   htmlOutput("contents2"),
                   htmlOutput("contents3"),
                   htmlOutput("contents5")
      ),
      mainPanel(
        plotOutput("plot1"),
        dataTableOutput("contents4")
      )
    ),
    
    server = function(input, output, session) {
      
      contents1 <- reactive({
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        dat <<- read.csv(inFile$datapath)
        dat[,2:21] <<- lapply(dat[,2:21],function(x){as.numeric(gsub(",", "", x))})
        names(dat)
      })
      
      output$contents2 <- renderUI({
        if (is.null(input$file1))
          return(NULL)
        selectInput('columnsx', 'Columns X', contents1()[3:21])
      })
      
      output$contents3 <- renderUI({
        if (is.null(input$file1))
          return(NULL)
        selectInput('columnsy', 'Columns Y', contents1()[3:21])
      })
      
      output$contents5 <- renderUI({
        checkboxInput('rank', 'Show ranks rather than data?', FALSE)
      })
      
      output$contents4 <- renderDataTable({
        if (is.null(input$file1))
          return(NULL)
        
        if(input$rank == FALSE) {
          dat[,c(names(dat)[1:3],input$columnsx,input$columnsy)]
        } else {
          datr <- dat[,c(names(dat)[1:3],input$columnsx,input$columnsy)]
          datr[3:5] <- apply(datr[3:5], 2, rank)
          datr
        }
      }, options = list(paging = FALSE, searching = FALSE))
      
      output$plot1 <- renderPlot({
        if (is.null(input$file1))
          return(NULL)
        p <- ggplot(dat, aes_string(x=dat[input$columnsx], y=dat[input$columnsy])) +
          labs(x=input$columnsx, y=input$columnsy) +
          geom_point() + scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma)
        print(p)
      })
    }
  ))
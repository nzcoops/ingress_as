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
                   htmlOutput("contents5"),
                   htmlOutput("contents6"),
                   htmlOutput("contents7")
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
        if (is.null(input$file1))
          return(NULL)
        checkboxInput('rank', 'Show ranks rather than data?', FALSE)
      })

      output$contents6 <- renderUI({
        if (is.null(input$file1))
          return(NULL)
        checkboxInput('newvar', 'Calculate a new variable from x and y?', FALSE)
      })
      
      output$contents7 <- renderUI({
        if (!is.null(input$file1) && input$newvar == TRUE){
          radioButtons('math', 'Mathematical Operator',
                       c(Addition = '+',
                         Division = '/',
                         Multiplication = '*',
                         Subtraction = '\t'
                       ),
                       '/')  
        } else {
          return(NULL)
        }
      })
            
#       output$contents8 <- renderText({
#         dat$newvariable <- round(sapply(paste(dat$x, m, dat$y), function(x) eval(parse(text=x))), 3)
#       }
#       )
      
      output$contents4 <- renderDataTable({
        if (is.null(input$file1))
          return(NULL)
        if(input$rank == FALSE) {
          dat[,c(names(dat)[1:3],input$columnsx,input$columnsy)]
        } else {
          datr <- dat[,c(names(dat)[1:3],input$columnsx,input$columnsy)]
          datr$newvariable <- round(sapply(paste(dat[,input$columnsx], input$math, dat[,input$columnsy]), function(x) eval(parse(text=x))), 3)
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
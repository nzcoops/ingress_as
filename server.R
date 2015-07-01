require(shiny)
require(ggplot2)
require(scales)
theme_set(theme_gray(base_size = 18))

shinyServer (function(input, output, session) {
  
  contents1 <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dat <<- read.csv(inFile$datapath)
    dat[,2:21] <<- lapply(dat[,2:21],function(x){as.numeric(gsub(",", "", x))})
    names(dat)
  })
  
  output$helptext <- renderUI({
    if (!is.null(input$file1)){
      return(NULL) 
    } else {
      includeHTML("intro.html")
    }
  }
  )
  
  output$contents2 <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    selectInput('columnsx', 'Columns X', contents1()[3:21], contents1()[4])
  })
  
  output$contents3 <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    selectInput('columnsy', 'Columns Y', contents1()[3:21], contents1()[5])
  })
  
  output$contents5 <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    checkboxInput('rank', 'Show data as ranks', FALSE)
  })
  
  output$contents6 <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    checkboxInput('newvar', 'Calculate a new variable from x and y', FALSE)
  })
  
  output$contents9 <- renderUI({
    if (!is.null(input$file1) && input$newvar == TRUE)
      checkboxInput('newvarY', 'Plot "MyVar" on the Y Axis', FALSE)
  })
  
  output$contents7 <- renderUI({
    if (!is.null(input$file1) && input$newvar == TRUE){
      radioButtons('math', 'Mathematical Operator (eg X/Y)',
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
  
  # Sort the data frame out with the new variable
  dat_dt <- reactive({
    if(!is.null(input$file1) & input$newvar == TRUE & input$rank == FALSE) {
      dat <- dat[,c(names(dat)[1:3],input$columnsx,input$columnsy)]
      dat$MyVar <- round(sapply(paste(dat[,input$columnsx], input$math, dat[,input$columnsy]), function(x) eval(parse(text=x))), 3)
      return(dat)
    } else if(!is.null(input$file1) & input$newvar == TRUE & input$rank == TRUE){
      dat <- dat[,c(names(dat)[1:3],input$columnsx,input$columnsy)]
      dat$MyVar <- round(sapply(paste(dat[,input$columnsx], input$math, dat[,input$columnsy]), function(x) eval(parse(text=x))), 3)
      dat[,4:6] <- apply(dat[,4:6], 2, function(x) rank(-x))
      return(dat)
    } else if(!is.null(input$file1) & input$newvar == FALSE & input$rank == TRUE){
      dat <- dat[,c(names(dat)[1:3],input$columnsx,input$columnsy)]
      dat[,4:5] <- apply(dat[,4:5], 2, function(x) rank(-x))
      return(dat)
    } else {
      return(dat <- dat[,c(names(dat)[1:3],input$columnsx,input$columnsy)])
    }
  })
  
  
  output$contents4 <- renderDataTable({
    if (is.null(input$file1))
      return(NULL)
    dat_dt()
  }, options = list(paging = FALSE, searching = FALSE))
  
  output$plot1 <- renderPlot({
    if (is.null(input$file1))
      return(NULL)
    if(input$newvar == T && input$newvarY == T) {
      p <- ggplot(dat_dt(), aes_string(x=dat_dt()[input$columnsx], y=dat_dt()["MyVar"])) +
        labs(x=input$columnsx, y="MyVar") +
        geom_point() + scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma)
      print(p)
    } else {
      p <- ggplot(dat, aes_string(x=dat[input$columnsx], y=dat[input$columnsy])) +
        labs(x=input$columnsx, y=input$columnsy) +
        geom_point() + scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma)
      print(p)
    }
  })
}
)
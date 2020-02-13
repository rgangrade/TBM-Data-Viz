rm(list=ls())
gc()

library(shiny)
library(ggplot2)
library(matrixStats)
library(cowplot)
library(xlsx)
library(data.table)
library(shinythemes)
library(scales)
library(zoo)
library(plotly)

ui <- fluidPage(theme = shinytheme("slate"),
                sidebarLayout(
                  sidebarPanel(width = 3,
                  fileInput("file","Upload File"),
                  # radioButtons("sep","Seperator",choices = c(Comma=",")),
                  # checkboxInput("header","Use Headers"),
                  numericInput("obs1","Enter Start Ring No.",min = 0,max = 2000,value = 1),
                  numericInput("obs2","Enter End Ring No.",min = 0,max = 2000,value = 1),
                  selectInput("columnid1","Choose Timestamp",choices=NULL),
                  selectInput("columnid2","Choose Chamber Pr. 1",choices=NULL),
                  selectInput("columnid3","Choose Chamber Pr. 2",choices=NULL),
                  selectInput("columnid4","Choose Advance Speed",choices=NULL),
                  selectInput("columnid5","Choose Thrust",choices=NULL),
                  selectInput("columnid6","Choose Shield Pr. 1",choices=NULL),
                  selectInput("columnid7","Choose Shield Pr. 2",choices=NULL),
                  downloadButton("DownloadGraphs")
                  ),

      mainPanel(
        tableOutput("input_file"),
        plotlyOutput("Chamber",width = 1400,height = 400),
        br(),
        plotlyOutput("AR",width = 1400,height = 400),
        br(),
        plotlyOutput("Thrust",width = 1400,height = 400),
        br(),
        plotlyOutput("Shield",width = 1400,height = 400)
               )
))

options(shiny.maxRequestSize=1000*1024^2) #Maximum size of the data file to be uploaded is kept to 1000 MB
server <- function(input, output, session) {
  
  piezo <- read.csv('C:/Users/rajatgangrade/Desktop/Shiny-Directory/TBM+1-2300+Cleaned.csv',stringsAsFactors = FALSE)
  piezo <- as.data.frame(piezo)
  piezo$TimeStamp <-as.POSIXct(paste(piezo$TimeStamp),format="%Y-%m-%d %H:%M:%S")
  
  selectedData <- reactive({
    subset(piezo,piezo$Ringnum>=input$obs1 & piezo$Ringnum <= input$obs2)
      # piezo[,c(input$columnid1,input$columnid2,input$columnid3)]
  })

  output$input_file <- renderTable({
    file_to_read = input$file
    req(file_to_read)
    f <- read.csv(file_to_read$datapath,sep=",",header=TRUE,nrows = 5)
    vars <- names(f)
    # Update select input immediately after clicking on the action button. 
    updateSelectInput(session, "columnid1","Choose Timestamp", choices = vars,selected = vars[sapply(f,is.numeric)])
    updateSelectInput(session, "columnid2","Choose Chamber Pr. 1", choices = vars,selected = vars[sapply(f,is.numeric)])
    updateSelectInput(session, "columnid3","Choose Chamber Pr. 2", choices = vars,selected = vars[sapply(f,is.numeric)])
    updateSelectInput(session, "columnid4","Choose Advance Speed", choices = vars,selected = vars[sapply(f,is.numeric)])
    updateSelectInput(session, "columnid5","Choose Thrust", choices = vars,selected = vars[sapply(f,is.numeric)])
    updateSelectInput(session, "columnid6","Choose Shield Pr. 1", choices = vars,selected = vars[sapply(f,is.numeric)])
    updateSelectInput(session, "columnid7","Choose Shield Pr. 2", choices = vars,selected = vars[sapply(f,is.numeric)])
    f
  })
  
  
  date_format_tz <- function(format = "%Y-%m-%d \n %H:%M", tz = "America/Denver") {
    function(x) format(x, format, tz=tz)
  }
  
  output$Chamber <- renderPlotly({
      req(input$columnid1,input$columnid2,input$columnid3)
      g1 <- ggplot()+
      geom_line(selectedData(),mapping=aes_string(x=input$columnid1,y=input$columnid2),color="gold")+
      geom_line(selectedData(),mapping=aes_string(x=input$columnid1,y=input$columnid3),color="purple")+
      theme_bw(base_size = 18)+
      theme(legend.position="right",axis.text = element_text(color="white"),
            axis.title = element_text(color="white"),
            legend.margin = margin(0,0,0,0), plot.margin = margin(0.25,1,0.25,0.25, unit = "cm"),
            legend.box.margin = margin(0,0,-0.5,0, unit = "cm"),
            plot.background = element_rect(fill="#2a3439",colour = 'white'),
            panel.background = element_rect(fill="#2a3439",color="white"),
            panel.grid.major = element_line(color = "gray80",size=0.5),
            panel.grid.minor = element_line(color = "gray80"))+
      labs(x="Time (hours)",y="Chamber Pressure (bar)")+
      scale_x_datetime(labels = date_format_tz())+
      # scale_x_continuous(limits = c(0,2000),breaks = seq(0,2000,100))+
      scale_y_continuous(expand = c(0,0),minor_breaks = seq(0,5,0.2))
    plot(g1)
  })
  
  output$AR <- renderPlotly({
    req(input$columnid1,input$columnid4)
    g2 <- ggplot()+
      geom_line(selectedData(),mapping=aes_string(x=input$columnid1,y=input$columnid4),color="#5920af")+
      theme_bw(base_size = 18)+
      theme(legend.position="right",axis.text = element_text(color="white"),
            axis.title = element_text(color="white"),
            legend.margin = margin(0,0,0,0), plot.margin = margin(0.25,1,0.25,0.25, unit = "cm"),
            legend.box.margin = margin(0,0,-0.5,0, unit = "cm"),
            plot.background = element_rect(fill="#2a3439",colour = 'white'),
            panel.background = element_rect(fill="#2a3439",color="white"),
            panel.grid.major = element_line(color = "gray80",size=0.5),
            panel.grid.minor = element_line(color = "gray80"))+
      labs(x="Time (hours)",y="Advance (mm/min)")+
      scale_x_datetime(labels = date_format_tz())
      # scale_y_continuous(expand = c(0,0),minor_breaks = seq(0,5,0.2))
    plot(g2)
  })
  
  output$Thrust <- renderPlotly({
    req(input$columnid1,input$columnid5)
    g3 <- ggplot()+
      geom_line(selectedData(),mapping=aes_string(x=input$columnid1,y=input$columnid5),color="orange")+
      theme_bw(base_size = 18)+
      theme(legend.position="right",axis.text = element_text(color="white"),
            axis.title = element_text(color="white"),
            legend.margin = margin(0,0,0,0), plot.margin = margin(0.25,1,0.25,0.25, unit = "cm"),
            legend.box.margin = margin(0,0,-0.5,0, unit = "cm"),
            plot.background = element_rect(fill="#2a3439",colour = 'white'),
            panel.background = element_rect(fill="#2a3439",color="white"),
            panel.grid.major = element_line(color = "gray80",size=0.5),
            panel.grid.minor = element_line(color = "gray80"))+
      labs(x="Time (hours)",y="Thrust (MN)")+
      scale_x_datetime(labels = date_format_tz())
    # scale_y_continuous(expand = c(0,0),minor_breaks = seq(0,5,0.2))
    plot(g3)
  })
  
  output$Shield <- renderPlotly({
    req(input$columnid1,input$columnid6,input$columnid7)
    g4 <- ggplot()+
      geom_line(selectedData(),mapping=aes_string(x=input$columnid1,y=input$columnid6),color="#9e211d")+
      geom_line(selectedData(),mapping=aes_string(x=input$columnid1,y=input$columnid7),color="#56ebd3")+
      theme_bw(base_size = 18)+
      theme(legend.position="right",axis.text = element_text(color="white"),
            axis.title = element_text(color="white"),
            legend.margin = margin(0,0,0,0), plot.margin = margin(0.25,1,0.25,0.25, unit = "cm"),
            legend.box.margin = margin(0,0,-0.5,0, unit = "cm"),
            plot.background = element_rect(fill="#2a3439",colour = 'white'),
            panel.background = element_rect(fill="#2a3439",color="white"),
            panel.grid.major = element_line(color = "gray80",size=0.5),
            panel.grid.minor = element_line(color = "gray80"))+
      labs(x="Time (hours)",y="Shield Pressure (bar)")+
      scale_x_datetime(labels = date_format_tz())+
      # scale_x_continuous(limits = c(0,2000),breaks = seq(0,2000,100))+
      scale_y_continuous(expand = c(0,0),minor_breaks = seq(0,5,0.2))
    plot(g4)
  })
  
  output$DownloadGraphs = downloadHandler(
    filename = 'graphs.pdf',
    content = function(file) {
      pdf(file)
      
      arrangeGrob(print(plotinput()[['g1']]),
                  print(plotinput()[['g2']]),
                  print(plotinput()[['g3']]),
                  print(plotinput()[['g4']]), ncol = 4)  
      dev.off()
    })
}
shinyApp(ui, server)
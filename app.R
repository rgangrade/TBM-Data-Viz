rm(list=ls())
gc()

library(shiny)
library(ggplot2)
library(matrixStats)
library(cowplot)
library(xlsx)
library(data.table)
library(shinythemes)

#************************************
#Reading the CSV / XLSX File in Shiny
#************************************

ui <- navbarPage("Navbar",
      tabPanel("Overview",
               fluidPage(theme = shinytheme("slate"),
                         headerPanel(title = "TBM Data Visualization Web App"),
                         sidebarLayout(
                           sidebarPanel(width = 3,
                                        fileInput("file","Upload File"),
                                        radioButtons("sep","Seperator",choices = c(Comma=",",Period=".",Tilde="~",minus="-")),
                                        checkboxInput("header","Use Headers"),
                                        selectInput("columnid1","Variable X-Axis:",choices=NULL),
                                        selectInput("columnid2","Variable Y1 (Gold):",choices=NULL),
                                        selectInput("columnid3","Variable Y2 (Purple):",choices=NULL),
                                        sliderInput("slider1","Start Ring No.",min=0,max = 2000,value = 200),
                                        sliderInput("slider2","End Ring No.",min=0,max = 2000,value = 400)
                           ),
                           
                           mainPanel(
                             tableOutput("input_file"),
                             plotOutput("plot",width = 1400,height = 300),
                             br(),
                             plotOutput("zoom",width = 800,height = 250),
                             br(),
                             fluidRow(
                               column(4,plotOutput("hist1",width = 350,height = 350),align="center"),
                               column(4,plotOutput("hist2",width = 350,height = 350),align="center")
                               
                             )
                             
                           )
                         )
               )),
      tabPanel("Summary",verbatimTextOutput("summary")))

options(shiny.maxRequestSize=1000*1024^2) #Maximum size of the data file to be uploaded is kept to 1000 MB
server <- function(input, output, session) {
  
  # tbm <- fread('G:/My Drive/CSM_Research/NEBT/I&M_NEBT/Modeling2020/NEBT-Settlement-01.19.2020-Plot.csv')
  # tbm <- as.data.frame(tbm)
  # selectedData <- reactive({
  #   tbm[,c(input$columnid1,input$columnid2)]
  # })
  
   piezo <- read.csv('C:/Users/rajatgangrade/Desktop/Shiny-Directory/TBM-Data-Ring.csv')
  selectedData <- reactive({
    piezo[,c(input$columnid1,input$columnid2,input$columnid3)]
  })
  
  output$input_file <- renderTable({
  file_to_read = input$file
  req(file_to_read)
    # if(is.null(file_to_read)){return()}
  f <- read.csv(file_to_read$datapath,sep=input$sep,header=input$header,nrows = 5)
  vars <- names(f)
    # Update select input immediately after clicking on the action button. 
  updateSelectInput(session, "columnid1","Variable X-Axis:", choices = vars,selected = vars[sapply(f,is.numeric)])
  updateSelectInput(session, "columnid2","Variable Y1 (Gold):", choices = vars,selected = vars[sapply(f,is.numeric)])
  updateSelectInput(session, "columnid3","Variable Y2 (Purple):", choices = vars,selected = vars[sapply(f,is.numeric)])
    # read.csv(file_to_read$datapath,sep=input$sep,header=input$header)
    # read.xlsx(file_to_read$datapath,sep=input$sep,header=input$header,sheetIndex = 1)
  f
  })

 # ranges <- reactiveValues(x = NULL, y = NULL)
 output$plot <- renderPlot({
   g <- ggplot()+geom_line(selectedData(),mapping=aes_string(x=input$columnid1,y=input$columnid2),color="gold")+
     geom_line(selectedData(),mapping=aes_string(x=input$columnid1,y=input$columnid3),color="purple")+
     geom_vline(aes(xintercept=input$slider1),size=1.5,linetype=2,color="red")+
     geom_vline(aes(xintercept=input$slider2),size=1.5,linetype=2,color="red")+
     # coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
     theme_bw(base_size = 20)+
     theme(legend.position="right",axis.text = element_text(color="white"),
           axis.title = element_text(color="white"),
           legend.margin = margin(0,0,0,0), plot.margin = margin(0.25,1,0.25,0.25, unit = "cm"),
           legend.box.margin = margin(0,0,-0.5,0, unit = "cm"),
           plot.background = element_rect(fill="#2a3439",colour = '#2a3439'),
           panel.background = element_rect(fill="#2a3439",color="white"),
           panel.grid.major = element_line(color = "gray80",size=0.5),
           panel.grid.minor = element_line(color = "gray80"))+
     labs(x="Ring No.",y="Variable Y1,Y2")+
     scale_x_continuous(limits = c(0,2000),breaks = seq(0,2000,100))+
     scale_y_continuous(expand = c(0,0))
   plot(g)
 })
 
  output$zoom <- renderPlot({
   g.zoom <- ggplot()+geom_line(selectedData(),mapping=aes_string(x=input$columnid1,y=input$columnid2),color="gold")+
     geom_line(selectedData(),mapping=aes_string(x=input$columnid1,y=input$columnid3),color="purple")+
     theme_bw(base_size = 20)+
     theme(legend.position="right",axis.text = element_text(color="white"),
           axis.title = element_text(color="white"),
           legend.margin = margin(0,0,0,0), plot.margin = margin(0.25,1,0.25,0.25, unit = "cm"),
           legend.box.margin = margin(0,0,-0.5,0, unit = "cm"),
           plot.background = element_rect(fill="#2a3439",colour = '#2a3439'),
           panel.background = element_rect(fill="#2a3439",color="white"),
           panel.grid.major = element_line(color = "gray80",size=0.5),
           panel.grid.minor = element_line(color = "gray80"))+
     labs(x="Ring No.",y="Variable Y1,Y2")+
     scale_x_continuous(limits = c(input$slider1,input$slider2))+
     scale_y_continuous(expand = c(0,0))
   plot(g.zoom)
 })
 
 output$hist1 <- renderPlot({
   hist1 <- ggplot()+geom_histogram(selectedData(),mapping=aes_string(x=input$columnid2),fill="gold",alpha=0.7)+
     theme_bw(base_size = 20)+
     theme(legend.position="right",axis.text = element_text(color="white"),
           axis.title = element_text(color="white"),
           plot.margin = margin(0.25,1,0.25,0.25, unit = "cm"),
           plot.background = element_rect(fill="#2a3439",colour = '#2a3439'),
           panel.background = element_rect(fill="#2a3439",color="white"),
           panel.grid.major = element_line(color = "gray80"),
           panel.grid.minor = element_line(color = "gray80"))+
     labs(x="Variable Y1",y="Count")
     # geom_vline(aes(xintercept=mean(input$columnid2)),color="red")+
     # geom_vline(aes(xintercept=median(input$columnid2)),color="blue")
   plot(hist1)
 })
 
 output$hist2 <- renderPlot({
   hist2 <- ggplot()+geom_histogram(selectedData(),mapping=aes_string(x=input$columnid3),fill="purple",alpha=0.7)+
     theme_bw(base_size = 20)+
     theme(legend.position="right",axis.text = element_text(color="white"),
           axis.title = element_text(color="white"),
           plot.margin = margin(0.25,1,0.25,0.25, unit = "cm"),
           plot.background = element_rect(fill="#2a3439",colour = '#2a3439'),
           panel.background = element_rect(fill="#2a3439",color="white"),
           panel.grid.major = element_line(color = "gray80"),
           panel.grid.minor = element_line(color = "gray80"))+
     labs(x="Variable Y2",y="Count")
   # geom_vline(aes(xintercept=mean(input$columnid2)),color="red")+
   # geom_vline(aes(xintercept=median(input$columnid2)),color="blue")
   plot(hist2)
 })
 
}
shinyApp(ui, server)

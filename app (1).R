library(shiny)

ui <- fluidPage(
  headerPanel("Total Daily Usage Per Bed Per Block"),
  sidebarLayout(
    sidebarPanel(
      hr(),
      fluidRow(column(3, 
                      checkboxGroupInput("B1", label = h3("Brecon"), 
                                         choices = list("1" = 1)),
                      checkboxGroupInput("B2", label = "", 
                                         choices = list("2" = 1)),
                      checkboxGroupInput("B3", label = "", 
                                         choices = list("3" = 1)),
                      checkboxGroupInput("B4", label = "", 
                                         choices = list("4" = 1)),                                
                      checkboxGroupInput("B5", label = "", 
                                         choices = list("5" = 1)),
                      checkboxGroupInput("B6", label = "", 
                                         choices = list("6" = 1)),
                      checkboxGroupInput("B7", label = "", 
                                         choices = list("7" = 1))),
               column(3,
                      checkboxGroupInput("C1", label = h3("Cotswold"), 
                                         choices = list("1" = 1)),
                      checkboxGroupInput("C2", label ="", 
                                         choices = list("2" = 1)),
                      checkboxGroupInput("C3", label ="", 
                                         choices = list("3" = 1)),
                      checkboxGroupInput("C4", label ="", 
                                         choices = list("4" = 1)),
                      checkboxGroupInput("C5", label ="", 
                                         choices = list("5" = 1)),
                      checkboxGroupInput("C6", label ="", 
                                         choices = list("6" = 1))
                      ),
                      
               column(3,
                      checkboxGroupInput("M1", label =h3("Mendip"), 
                                         choices = list("1" = 1)),
                      checkboxGroupInput("M2", label ="", 
                                         choices = list("2" = 1)),
                      checkboxGroupInput("M3", label ="", 
                                         choices = list("3" = 1)),
                      checkboxGroupInput("M4", label ="", 
                                         choices = list("4" = 1)),
                      checkboxGroupInput("M5", label ="", 
                                         choices = list("5" = 1))
                      ),
               column(3,
                      checkboxGroupInput("Q1", label =h3("Quantock"), 
                                         choices = list("1" = 1)),
                      checkboxGroupInput("Q2", label ="", 
                                         choices = list("2" = 1)),
                      checkboxGroupInput("Q3", label ="", 
                                         choices = list("3" = 1)),
                      checkboxGroupInput("Q4", label ="", 
                                         choices = list("4" = 1)),
                      checkboxGroupInput("Q5", label ="", 
                                         choices = list("5" = 1)),
                      checkboxGroupInput("Q6", label ="", 
                                         choices = list("6" = 1))
                      )
                
      )),
    mainPanel(
      plotOutput("plots")
    )
  ))


server <- function(input, output) {
  
  library(readxl)
  Data_Means<-read_excel("D:/Report/Anomalies results.xlsx",sheet="Normallity")
  Beds<-c(72,72,84,84,84,84,84,82,84,84,82,84,84,80,84,68,84,84,84,84,66,84,84,66)
  RawDayTotals<-Data_Means[1:242,3:26]
  DayPerBedTotal<-t(apply(RawDayTotals,1,function(x){x/Beds}))
  TempPerBed<-DayPerBedTotal
  TempPerBed[TempPerBed==0]<-NA
  median.line<-apply(TempPerBed,1,function(x){quantile(x,probs=.5,na.rm=TRUE)})
  
  output$plots <- renderPlot({
    
    p <- plot(median.line,ylab="Total usage per day per bed (Cubic Metres)", xlab="", type="l",lty=6,main="Blocks daily per bed water usage",ylim=c(0,(max(DayPerBedTotal))))
    legend("topleft",c("median","Brecon","Cotswold","Mendip","Quantock"),col = c("black","green","blue","red","brown"),lty = 1,bty="n")
    
    abline(v=(39),col="blue",lty=3)
    abline(v=(48),col="blue",lty=3)
    abline(v=(100),col="blue",lty=3)
    abline(v=(124),col="blue",lty=3)
    abline(v=(137),col="blue",lty=3)
    abline(v=(219),col="blue",lty=3)
    abline(v=(235),col="blue",lty=3)
    abline(h=0,col="black",lty=3)
    
    text(x=65,y=1,labels="TB1",adj=.5,cex=.75)
    text(x=110,y=1,labels="Xmas",adj=.5,cex=.75)
    text(x=180,y=1,labels="TB2",adj=.5,cex=.75)
    text(x=227,y=1,labels="Easter",adj=.5,cex=.75)
    
    if(!is.null(input$B1)) {
      p <- p 
      lines( DayPerBedTotal[,1],col="green")
    }
    if(!is.null(input$B2)) {
      p <- p 
      lines( DayPerBedTotal[,2],col="green",lty=2)
    }
    if(!is.null(input$B3)) {
      p <- p 
      lines( DayPerBedTotal[,3],col="green",lty=3)
    }
    if(!is.null(input$B4)) {
      p <- p 
      lines( DayPerBedTotal[,4],col="green",lty=4)
    }
    if(!is.null(input$B5)) {
      p <- p 
      lines( DayPerBedTotal[,5],col="green",lty=1,lwd=1.5)
    }
    if(!is.null(input$B6)) {
      p <- p 
      lines( DayPerBedTotal[,6],col="green",lty=2,lwd=1.5)
    }
    if(!is.null(input$B7)) {
      p <- p 
      lines( DayPerBedTotal[,7],col="green",lty=3,lwd=1.5)
    }
    if(!is.null(input$C1)) {
      p <- p 
      lines( DayPerBedTotal[,8],col="blue",lty=1,lwd=1)
    }
    if(!is.null(input$C2)) {
      p <- p 
      lines( DayPerBedTotal[,9],col="blue",lty=2,lwd=1)
    }
    if(!is.null(input$C3)) {
      p <- p 
      lines( DayPerBedTotal[,10],col="blue",lty=3,lwd=1)
    }
    if(!is.null(input$C4)) {
      p <- p 
      lines( DayPerBedTotal[,11],col="blue",lty=1,lwd=1.5)
    }
    if(!is.null(input$C5)) {
      p <- p 
      lines( DayPerBedTotal[,12],col="blue",lty=2,lwd=1.5)
    }
    if(!is.null(input$C6)) {
      p <- p 
      lines( DayPerBedTotal[,13],col="blue",lty=3,lwd=1.5)
    }
    if(!is.null(input$M1)) {
      p <- p 
      lines( DayPerBedTotal[,14],col="red",lty=1,lwd=1)
    }
    if(!is.null(input$M2)) {
      p <- p 
      lines( DayPerBedTotal[,15],col="red",lty=2,lwd=1)
    }
    if(!is.null(input$M3)) {
      p <- p 
      lines( DayPerBedTotal[,16],col="red",lty=3,lwd=1)
    }
    if(!is.null(input$M4)) {
      p <- p 
      lines( DayPerBedTotal[,17],col="red",lty=1,lwd=1.5)
    }
    if(!is.null(input$M5)) {
      p <- p 
      lines( DayPerBedTotal[,18],col="red",lty=2,lwd=1.5)
    }
    if(!is.null(input$Q1)) {
      p <- p 
      lines( DayPerBedTotal[,19],col="brown",lty=1,lwd=1)
    }
    if(!is.null(input$Q2)) {
      p <- p 
      lines( DayPerBedTotal[,20],col="brown",lty=2,lwd=1)
    }
    if(!is.null(input$Q3)) {
      p <- p 
      lines( DayPerBedTotal[,21],col="brown",lty=3,lwd=1)
    }
    if(!is.null(input$Q4)) {
      p <- p 
      lines( DayPerBedTotal[,22],col="brown",lty=1,lwd=1.5)
    }
    if(!is.null(input$Q5)) {
      p <- p 
      lines( DayPerBedTotal[,23],col="brown",lty=2,lwd=1.5)
    }
    if(!is.null(input$Q6)) {
      p <- p 
      lines( DayPerBedTotal[,24],col="brown",lty=3,lwd=1.5)
    }
    return(p)
    
  })
  
  
}

shinyApp(ui = ui, server = server)
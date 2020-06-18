library(shiny)

ui <- fluidPage(
  headerPanel("Poffley's Professional Plots"),
  sidebarLayout(
    sidebarPanel("Customise your Poffley Plot to suit your needs",
                 hr(),
                 fluidRow(column(10, 
                                 fileInput("file1", "Choose CSV File"),
                                 sliderInput(inputId ="LB",label="% of Excellent" ,min=0,max=50,value=10),
                                 sliderInput(inputId ="UB",label="% of Excessive" ,min=0,max=50,value=25),
                                 dateInput("Date", "Date selector (for text):", format = "yyyy-mm-dd", value="2018-09-19"),
                                 textInput("texty","Title for Poffley plot","Enter Title Here"),
                                 checkboxGroupInput("Term_lines", label =("Teaching Blocks"), 
                                                    choices = list("Teaching block lines" = 1)),
                                 checkboxGroupInput("Median", label =("Add lines"), 
                                                    choices = list("Median" = 1)),
                                 checkboxGroupInput("Mean", label =(""), 
                                                    choices = list("Mean" = 1)),
                                 checkboxGroupInput("Median_2", label =(""), 
                                                    choices = list("Median (top 2 removed)" = 1))
                                 
                 )
                 )
    )               
    
    ,
    mainPanel(
      plotOutput("plot"),
      textOutput("text_calc1"),
      textOutput("text_calc2"),
      textOutput("text_calc3"),
      textOutput("text_calc4"),
      textOutput("text_calc5"),
      textOutput("text_calc6"),
      h4(textOutput("Savings")),
      h4(textOutput("Savings2"))
    )
  )
)

server <- function(input, output) {
  
  library(readxl)
  
  
  output$plot<-renderPlot({
    Data_Means<-read_excel(input$file1)
    Beds<-c(72,72,84,84,84,84,84,82,84,84,82,84,84,80,84,68,84,84,84,84,66,84,84,66)
    RawDayTotals<-Data_Means[,3:26]
    DayPerBedTotal<-t(apply(RawDayTotals,1,function(x){x/Beds}))
    Mean.Data<-rowSums(DayPerBedTotal)/rowSums(DayPerBedTotal>0)
    TempData<-Data_Means[,3:26]
    TempData[TempData==0]<-NA
    TempPerBed<-DayPerBedTotal
    TempPerBed[TempPerBed==0]<-NA 
    
    colnames(DayPerBedTotal)<-c(paste0("Brecon ",1:7),paste0("Cotswold ",1:6),paste0("Mendip ",1:5),paste0("Quantock ",1:6))
    
    LB.daily<-apply(TempPerBed,1,function(x){quantile(x,probs=(input$LB/100),na.rm=TRUE)})
    UB.daily<-apply(TempPerBed,1,function(x){quantile(x,probs=(1-(input$UB/100)),na.rm=TRUE)})
    max.daily<-apply(TempPerBed,1,function(x){quantile(x,probs=1,na.rm=TRUE)})
    min.daily<-apply(TempPerBed,1,function(x){quantile(x,probs=0,na.rm=TRUE)})
    median.line<-apply(TempPerBed,1,function(x){quantile(x,probs=.5,na.rm=TRUE)})
    median.2.line<-apply(TempPerBed,1,function(x){quantile(x,probs=.46,na.rm=TRUE)})
    xvals<-seq(1:length(LB.daily))[!is.na(LB.daily)]
    
    library(RColorBrewer)
    p <-plot(Mean.Data,type="l",ylab="Cubic Metres per bed per day",main=input$texty,cex.main=1.75,ylim=c(0,max(DayPerBedTotal)),xlab="",xaxt="n",lwd=1.5)
    mtext("Time",side=1,line=.5)
    
    mypalette<-brewer.pal(5,"RdYlBu")
    polygon(c(xvals, rev(xvals)),               
            c(max.daily[xvals],rev(UB.daily[xvals])),                
            col = mypalette[1],border=mypalette[1])
    polygon(c(xvals, rev(xvals)),                
            c(LB.daily[xvals], rev(min.daily[xvals])),
            col = mypalette[5],border=mypalette[5])
    polygon(c(xvals, rev(xvals)),           
            c(UB.daily[xvals], rev(LB.daily[xvals])),   
            col = mypalette[3])
    
    abline(v=(which(as.Date(Data_Means$Date)==input$Date)),col="black",lty=2,lwd=1.5)
    
    abline(h=0,col="black",lty=3)
    
    text(x=15,y=1,labels="Summer holidays",adj=.5,cex=.75)
    text(x=65,y=1,labels="TB1",adj=.5,cex=.75)
    text(x=110,y=1,labels="Xmas",adj=.5,cex=.75)
    text(x=180,y=1,labels="TB2",adj=.5,cex=.75)
    text(x=227,y=1,labels="Easter",adj=.5,cex=.75)
    
    if(!is.null(input$Median)) {
      p <- p 
      lines( median.line,col="coral2")
      
    }
    if(!is.null(input$Mean)) {
      p <- p 
      lines( Mean.Data,col="magenta")
      
    } 
    if(!is.null(input$Median_2)) {
      p <- p 
      lines( median.2.line,col="chartreuse")
      
    } 
    if(!is.null(input$Term_lines)) {
      p <- p 
      abline(v=(18),col="blue",lty=3)
      abline(v=(109),col="blue",lty=3)
      abline(v=(144),col="blue",lty=3)
      abline(v=(228),col="blue",lty=3)
    }
    return(p)
    
  })
  
  
  
  output$text_calc1 <- renderText({
    paste0("The maximum per bed total water consumption on ", input$Date, " is ", formula1(),".")})
  output$text_calc2 <- renderText({
    paste0("The block which used the maximum amount of water on ", input$Date, " is ", formula2(),".")})
  output$text_calc3 <- renderText({
    paste0("The minimum per bed total water consumption on ", input$Date, " is ", formula3(),".")})
  output$text_calc4 <- renderText({
    paste0("The block which used the minimum amount of water on ", input$Date, " is ", formula4(),".")})
  output$text_calc5 <- renderText({
    paste0("The median per bed total water consumption on ", input$Date, " is ", formula5(),".")})
  output$text_calc6 <- renderText({
    paste0("The median (minus the highest two blocks) per bed total water consumption on ", input$Date, " is ", formula6(),".")})
  output$Savings <- renderText({
    paste0("This would save ", round(formula7,3), " cubic metres of water per bed per acidemic year, leading to a total saving of ", round((formula7*1932),3), " cubic metres.")})
  output$Savings2 <- renderText({
    paste0("This would save the university ", round(formula8,2), " pounds per acidemic year.")})
}

shinyApp(ui = ui, server = server)
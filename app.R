library(shiny)
library(plotrix)
library(tidyverse)
library(RColorBrewer)
my_colors = brewer.pal(8,"Set2")

#first dataset
names(divorcestats)[1] <- ""
divorcestats<-data.matrix(divorcestats)

dimnames(divorcestats) = list(
  c("2013","2014","2015","2016","2017"),
  c("Johor" ,"Kedah","Kelantan","Melaka","Negeri  Sembilan","Pahang","Perak",
    "Perlis","Pulau Pinang","Sabah","Sarawak","Selangor","Terengganu","Kuala Lumpur"))

#second dataset
names(divorcestats1)[1] <- ""
divorcestats1<-data.matrix(divorcestats1)

dimnames(divorcestats1) = list(
  c("Johor" ,"Kedah","Kelantan","Melaka","Negeri  Sembilan","Pahang","Perak",
    "Perlis","Pulau Pinang","Sabah","Sarawak","Selangor","Terengganu","Kuala Lumpur"),
  c("2013","2014","2015","2016","2017"))

#third dataset
names(sebabdivorce)[1] <- ""
sebabdivorce<-data.matrix(sebabdivorce)

dimnames(sebabdivorce) = list(
  c("Women","Men"),
  c("A","B","C","D","E","F","G"))

ui <- fluidPage(
  
  
  titlePanel("Malaysia Divorce Data"),
  
 
  sidebarLayout(
    
   
    sidebarPanel(
      helpText("Choose either region or year to dislay the data"),
      
      selectInput("region", "Region:",
                  choices=colnames(divorcestats)),
      helpText("The data display will be based on region selected"),
      hr(),
      
      selectInput("year", "Year:",
                  choices=colnames(divorcestats1)),
      helpText("The data display will be based on year selected"),
      hr(),
      helpText("Statistik Pendaftaran Perceraian Mahkamah Syariah Seluruh Malaysia")
    ),
    
   
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Plot", plotOutput("regionPlot"),plotOutput("yearPlot")),
                  
                  tabPanel("Summary",h4("Summary for selected Region (based on year)"),
                           h5("Maximum no of divorce:"), verbatimTextOutput("maxsummary"),
                           h5("Minimum no of divorce:"),verbatimTextOutput("minsummary"),
                           h4("Summary for selected Year (based on region)"),
                           h5("Maximum no of divorce:"),verbatimTextOutput("summarymax"),
                           h5("Minimum no of divorce:"),verbatimTextOutput("summarymin")),
                  
                  tabPanel("Table",h4("Table for Region"), tableOutput("table"),h4("Table for Year"),tableOutput("table1")),
                  tabPanel("Overview",h4("General review for Malaysia divorce rate"), plotOutput("Malaysia"),plotOutput("reasonPlot"),
                           h5("A = Lack of understanding between spouse"),
                           h5("B = Cheating on the spouse"),
                           h5("C = Meddling by family members and in laws"),
                           h5("D = Irresponsible spouse"),
                           h5("E = Financial woes"),
                           h5("F = Couples addicted to banned substances"),
                           h5("G = Others"),
                           h6("Findings of the Malaysian population and family survey on the causes of divorce and separation"))
               
      )
      
      
    )
  )
)


server <- function(input, output) {
  
  #plot output
  output$regionPlot <- renderPlot({
    Year<-c("2013","2014","2015","2016","2017")
    Number_of_divorce <- divorcestats[,input$region] 
    
    clplot(Year,Number_of_divorce, main =input$region,lwd=3, col =my_colors, showcuts=T , bty="n" )
   
    
  })
  
  output$yearPlot <- renderPlot({
    midpts<-barplot(divorcestats1[,input$year], 
                    main=input$year,
                    ylab="Number of Divorce",
                    cex.axis = 1.0, cex.names = 0.6,col ="#800000", border = "grey")
    text(divorcestats1[,input$city],
         x = midpts,
         offset = 1.0,
         cex = 0.5,
         srt = 60,
         xpd = TRUE,
         pos = 2 )
  })
  
  
  
  
  #summary output
  output$maxsummary <- renderText({
    max(divorcestats[,input$region])
    })
  output$minsummary <- renderText({
    min(divorcestats[,input$region])
    })
  
  output$summarymax <- renderText({
    max( divorcestats1[,input$year])
  })
  output$summarymin <- renderText({
    min( divorcestats1[,input$year])
  })
  
  
  #table output
  output$table <- renderTable({
    divorcestats[,input$region]
  })
  output$table1 <- renderTable({
    divorcestats1[,input$year]
  })
  
  #overview output
  output$Malaysia <-renderPlot({
    
    # Create data
    value =c(49718,54113,58764,61084,60088)
    names(value) =c("2013","2014","2015","2016","2017")
    
    
    statistik=data.frame(x=names(value),y =value)
    
    ggplot(statistik, aes(x=x, y=y)) +
      geom_segment( aes(x=x, xend=x, y=0, yend=y), color="grey",linetype="solid",size=1) +
      geom_point( color="orange", size=4) +
      theme_light() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      xlab("Year") +
      ylab("Number of divorce")
  })
  
  output$reasonPlot <- renderPlot({
    barplot(sebabdivorce, main = "Why Marriages Fail", xlab = "Reasons ", col = c("#ff8080","#00bfff"), border = "white",
            legend= row.names(sebabdivorce),beside = TRUE)
  })
}

 
shinyApp(ui = ui, server = server)

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer(function(input, output) {
  
  output$selected_var<-renderText({
    paste('Viewing water quality data at',input$site,'between',input$dates[1],'and',input$dates[2])
    
    })
  
  output$plot1<-renderPlot({
    
    plotdata <- subset(newbuoydata,SiteName==input$site & DateTime >= input$dates[1] & DateTime<= input$dates[2])

    ggplot(data=plotdata,aes(x=plotdata$Chlorophyll,y=plotdata[,input$param]),color='green')+ geom_point() +
      geom_smooth() + xlab("Chlorophyll") + ylab(input$param)+ ggtitle("Input Parameter and Chlorophyll Comparison")

    })

  output$plot2<-renderPlot({ 
    
    plotdata <- subset(newbuoydata,SiteName==input$site & DateTime >= input$dates[1] & 
                         DateTime<= input$dates[2])
    
    ggplot(data=plotdata,aes(x=plotdata$DateTime,y=plotdata[,input$param]),color='red')+ 
      geom_point() + geom_smooth() + xlab("Time") + ylab(input$param) + ggtitle("Time Series Comparison")
    })
  
chlmod <- reactive({
  plotdata <- subset(newbuoydata,SiteName==input$site &
                       DateTime >= input$dates[1] &
                       DateTime<= input$dates[2])
  mod <- lm(plotdata$Chlorophyll~plotdata[,input$param])
  modsummary <- summary(mod)
  return(modsummary)
})
output$modelresults <- renderText({
  paste("R-Squared between Chlorophyll and",input$param," during this timeframe:",chlmod()$r.squared)
})

url<-a("R shiny QC Application", href="https://www.google.com/")
output$tab<-renderUI({
  tagList("URL link:",url)
})

output$mymap<-renderLeaflet({
 leafdata<-subset(newbuoydata, SiteName==input$site) 
 leaflet()%>% addTiles()%>% setView(lng=unique(leafdata$Longitud), lat=unique(leafdata$Latitude),zoom = 11) %>% 
   addMarkers(lng=unique(leafdata$Longitud), lat=unique(leafdata$Latitude))
})

plotmod <- reactive({
  plotdata1 <- subset(newbuoydata,SiteName == input$site &
                        DateTime >= input$dates[1] & DateTime <= input$dates[2])
  
  mod1 <- rbind("Site_Location" = input$site,
                "Para"= input$param,
                "5%" = round(quantile(plotdata1[,input$param],(0.05),na.rm =TRUE),2),
                "95%"= round(quantile(plotdata1[,input$param],na.rm = TRUE,(0.95)),2),
                "Mean" = round(mean(plotdata1[,input$param],na.rm=TRUE),2), 
                "Std Dev" = round(sd(plotdata1[,input$param],na.rm = TRUE),2))
  
  modtable <- data.frame(col1 = c("Site Location","Parameter","5% Quartile","95% Quartile","Mean","Std Dev"),
                         col2 = mod1)
  `colnames<-`(modtable,NULL)
  
})

output$table <- renderTable(plotmod())

#Download PDF

output$downloadReport <- downloadHandler(
  filename = function() {
    paste('my-report', sep = '.', switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },
  
  content = function(file) {
    src <- normalizePath('report.Rmd')
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd', overwrite = TRUE)
    
    library(rmarkdown)
    out <- render('report.Rmd', switch(
      input$format,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
)


})
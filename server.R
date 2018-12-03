
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer(function(input, output) {
  
  output$selected_var<-renderText({
    paste('Viewing water quality data at',input$site,'between',input$dates[1],'and',input$dates[2])
    
    })
  
  #Plot 1 
  
  output$plot1<-renderPlot({
    
    plotdata <- subset(newbuoydata,SiteName==input$site & DateTime >= input$dates[1] & DateTime<= input$dates[2])

    ggplot(data=plotdata,aes(x=plotdata$Chlorophyll,y=plotdata[,input$param]),color='green')+ geom_point() +
      geom_smooth() + xlab("Chlorophyll") + ylab(input$param)+ ggtitle("Input Parameter and Chlorophyll Comparison")

    })
  
  #Plot 2

  output$plot2<-renderPlot({ 
    
    plotdata <- subset(newbuoydata,SiteName==input$site & DateTime >= input$dates[1] & 
                         DateTime<= input$dates[2])
    
    ggplot(data=plotdata,aes(x=plotdata$DateTime,y=plotdata[,input$param]),color='red')+ 
      geom_point() + geom_smooth() + xlab("Time") + ylab(input$param) + ggtitle("Time Series Comparison")
    })
  
  #Plot 3
  
  output$plot3<-renderPlot({ 
    
    plotdata <- subset(newbuoydata,SiteName==input$site & DateTime >= input$dates[1] & 
                         DateTime<= input$dates[2])
    
    # Compute Linear Regression
    
    z <- plotdata$Chlorophyll
    x <- plotdata[,input$param]
    y <- plotdata[, input$param2]  
    
    fit <- lm(z ~ x + y)
    
    fitpoints <- predict(fit)
    
    # scatter plot with regression plane
    
    grid.lines = 26
    x.pred <- seq(min(x,na.rm = TRUE), max(x,na.rm = TRUE), length.out = grid.lines)
    y.pred <- seq(min(y,na.rm = TRUE), max(y,na.rm = TRUE), length.out = grid.lines)
    xy <- expand.grid( x = x.pred, y = y.pred)
    z.pred <- matrix(predict(fit, newdata = xy), 
                     nrow = grid.lines, ncol = grid.lines)
    
    fitpoints <- predict(fit)
    
    plot3 <- scatter3D(x, y, z, phi = 0, ticktype = "detailed", par(xpd=TRUE),
              zlab = "Chlorophyll", xlab = input$param, ylab = input$param2,  
              surf = list(x = x.pred, y = y.pred, z = z.pred,facets = NA), main = "Multiple Regression Model")
  })
  
  #Mulitple Regression Fit
  
  fitmod <- reactive({
    
    plotdata <- subset(newbuoydata,SiteName==input$site &
                         DateTime >= input$dates[1] &
                         DateTime<= input$dates[2])
    
    x <- plotdata$Chlorophyll
    y <- plotdata[,input$param]
    z <- plotdata[, input$param2]  
    
    fitline <- lm(x ~ z + y)
    
    fitsummary <- summary(fitline)
    
    return(fitsummary)
  })
  
  output$multresults <- renderText({
    paste("R-Squared between Chlorophyll, ",input$param,"and ", input$param2, "during this timeframe:",fitmod()$r.squared)
  })
  
#Single Regression Text
  
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

#URL

url<-a("R shiny QC Application", href="https://bagmati.shinyapps.io/WaterQCApp/")
output$tab<-renderUI({
  tagList("URL link:",url)
})

#Map

output$mymap<-renderLeaflet({
 leafdata<-subset(newbuoydata, SiteName==input$site) 
 leaflet() %>% 
   addTiles() %>% 
   addMarkers(lng=unique(leafdata$Longitude), 
              lat=unique(leafdata$Latitude)
              ) %>% 
 setView(lng=unique(leafdata$Longitude), lat=unique(leafdata$Latitude),zoom = 11)
})

#Table 
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

output$downloadReport.pdf <- downloadHandler(
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
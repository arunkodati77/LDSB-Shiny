library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(dplyr)
library(shinyWidgets)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggplotify)
library(RSQLite)
library(shinyalert)
library(berryFunctions)
library(rms)
library(zoo)
library(weathercan)
library(plotly)
library(lessR)
library(shinycustomloader)
library(sendmailR)



source("ReqFunctions.R",local=TRUE)$value


shinyServer(function(input, output,session) { 
  
  # session$onSessionEnded(function() {
  #   stopApp()
  # })
  # onSessionEnded = function(callback) {
  #   "Registers the given callback to be invoked when the session is closed
  #     (i.e. the connection to the client has been severed). The return value
  #     is a function which unregisters the callback. If multiple callbacks are
  #     registered, the order in which they are invoked is not guaranteed."
  #   return(.closedCallbacks$register(callback))
  # }
  
  
      #Reactive Values-----
  downloadedDataRvalues <- reactiveVal(NULL) 
  sitesDataRValues <- reactiveVal(NULL)
  reactdf <- reactiveVal(NULL)
  rValues <- reactiveValues(p1=NULL,p2=NULL,p3=NULL,p4=NULL)
  snRValues <- reactiveValues(snr1=NULL,snr2=NULL,snr3=NULL,snr4=NULL)
  spRValues <- reactiveValues(spr1=NULL,spr2=NULL,spr3=NULL,spr4=NULL)
  manageReactiveValues <- reactiveVal(NULL)
  normaliZedReactiveValues <- reactiveVal(NULL)
  SN_SelectedReactiveValues <- reactiveVal(NULL)
  SP_SelectedReactiveValues <- reactiveVal(NULL)
  HDDReactiveValues <- reactiveVal(NULL)
  
      #Observe---------
 observe({sitesData()})
  observe({downloadedData()})
  observe({HDDData()})
  observe({getNormalizedData()})
  observe({SN_selectedData()})
  observe({SP_selectedData()})
  # observe({})
  
  
      #Database Connection-----
  dbcnt <- reactive({
    d <- dbConnect(SQLite(),dbname="LDSB Database.db")
    d
  })
  
  
      #Database Get Query-----
  
  
  downloadedData <- reactive({
    data <- dbGetQuery(dbcnt(),"select * from downloadData")
    downloadedDataRvalues(data)
    data
  })
  

  
  sitesData <- reactive({
    sd <- dbGetQuery(dbcnt(),"select * from Sites")
    sitesDataRValues(sd)
    sd
  })
  
  HDDData <- reactive({
    hd <- dbGetQuery(dbcnt(),"select * from HDD_Table")
    HDDReactiveValues(hd)
    hd
  })

  
 
  
  
      #Observe Events-----
  
  
  observeEvent(input$sendMail,{
    browser()
    sendmail("arunkodati77@gmail.com","arunkodati0418@gmail.com","test","Hi")
    
  })
  
  observeEvent(input$refresh, {
    js$refresh();
  })
  
  observeEvent(input$reset_input_PDF,{
    
    updateTextInput(session,"deliveredByTB", value = "")
    updateTextInput(session,"invoiceDateTB", value = "")
    updateTextInput(session,"invoiceTotalTB", value = "")
    updateTextInput(session,"schoolNameTB", value = "")
    updateTextInput(session,"orderQuantityTB", value = "")
    updateTextInput(session,"unitPriceTB", value = "")
    updateTextInput(session,"kwhEquivalentTB", value = "")
  })
  observeEvent(input$reset_input_Sites,{
    
    updateTextInput(session,"siteIdentifierTB", value = "")
    updateTextInput(session,"siteNameTB", value = "")
    updateTextInput(session,"addressTB", value = "")
    updateTextInput(session,"cityTB", value = "")
    updateTextInput(session,"zipCodeTB", value = "")
    updateTextInput(session,"areaTB", value = "")
    updateTextInput(session,"openingHoursTB", value = "")
  })
  
  cnvrtPDF_Excel <- observeEvent(input$convrtPDF,{
   
      inFiles <- input$files
      sites<- sitesData()
      
      mydf <- getPdfOutput(inFiles,length(input$files[, 1]),sites)
      
      reactdf(mydf)
      
      return(mydf)
  })
 updatedData <- observeEvent(input$cnvrtPDFToExcel_cell_edit, {
    
    dat <- reactdf()
    cell <- input$cnvrtPDFToExcel_cell_edit
    
    dat[cell$row, cell$col] <- cell$value
   
    reactdf(dat)
    
  })
 
      #Reactive Functions------ 
 
 
 getNormalizedData <- reactive(
   {

     sd <- downloadedDataRvalues()
     hd <- HDDReactiveValues()
     schoolsData <- sitesDataRValues()
     s <-weather_dl(station_ids  = 47267 , start = "2009-01-01", end = "2019-12-31",interval = "day")
     
     
     
     sd$invoiceDate <- as.Date(sd$invoiceDate,"%d/%m/%Y")
     s$date <- as.Date(s$date,"%Y-%m-%d")
     
     b <- sd%>% filter(!is.na(invoiceDate))
     
     d94 <- b %>%
       mutate(mon_year = as.yearmon(invoiceDate,"%Y-%m-%d"),month = format(invoiceDate, "%m"),year= format(invoiceDate, "%Y"))
    
     
     d561 <- d94 %>% group_by(schoolName,mon_year,month,year,deliveredBy)%>%
       summarise(sum_KE = sum(as.numeric(kwhEquivalent),na.rm=TRUE),
                  quantity=sum(as.numeric(orderQuantity),na.rm = TRUE),
                  amount=sum(as.numeric(invoiceTotal),na.rm = TRUE), 
                  unitPrice=mean(as.numeric(unitPrice),na.rm = TRUE))
     
     s$mon_year <- as.yearmon(s$date,"%Y-%m-%d")
     
     hdd_period <- s  %>% group_by(mon_year)%>% 
       summarise(sum_hdd = sum(heat_deg_days,na.rm=TRUE))
     
     d <- merge(x = d561, y = hdd_period, by = "mon_year", all.x = TRUE)
     
     
     
     dff <- d %>% mutate(KwhPerHDD=as.numeric(sum_KE )/as.numeric(sum_hdd))
     
     row_sub <- dff %>% filter(KwhPerHDD!=0) 
     
     options(scipen = 999)
     row_sub$Avg_HDD <- hd$AverageHDD_Month[match(row_sub$month,hd$Month)]
     row_sub$Area_Sq_feet <- schoolsData$Area_SQFT[match(row_sub$schoolName,schoolsData$Site_Name)]
     row_sub$op_hours_week <- schoolsData$Operating_hours[match(row_sub$schoolName,schoolsData$Site_Name)]
     
     finalData <- row_sub%>% filter(!is.na(Area_Sq_feet)&!is.na(op_hours_week))
     finalData$Area_Sq_feet <- as.numeric(gsub(",", "", as.character(finalData$Area_Sq_feet)))
     gh <- finalData %>% mutate(NormalizedKWH2= Avg_HDD*KwhPerHDD,
                              KWhperSqFeet=round(as.numeric(NormalizedKWH2)/as.numeric(Area_Sq_feet),4),
                              KWHperSqFeetPerHour=round(as.numeric(NormalizedKWH2)/(as.numeric(Area_Sq_feet)*((as.numeric(op_hours_week)*30)/7)),4))
    

     normaliZedReactiveValues(gh)
     gh
   }
 )
  
 dataValidation <- reactive({ 
   
   if(input$deliveredByTB!=""&input$invoiceDateTB!=""&input$invoiceTotalTB !=""
      &input$schoolNameTB != ""&input$orderQuantityTB != ""&input$unitPriceTB != ""&input$kwhEquivalentTB != "")
   {
     return(TRUE)
   }
   else
   {
     return(FALSE)
   }
   
   
 })
 
 
 siteDataValidation <- reactive({ 
   
   if(input$siteIdentifierTB!=""&input$siteNameTB!=""&input$addressTB !=""
      &input$cityTB != ""&input$zipCodeTB != ""&input$areaTB != ""&input$openingHoursTB != "")
   {
     return(TRUE)
   }
   else
   {
     return(FALSE)
   }
   
   
 })
 
 
 
 
 stationNames <-  reactive({
   stations_search(input$stationSearch, interval =input$intervalBtn)
 })
 
 xx_SNchange <- reactive({
  
   paste(input$SN_schoolNameVar,input$SN_daterange)
 })
 
 xx_SPchange <- reactive({
   
   paste(input$SP_schoolNameVar,input$SP_daterange)
 })
 
 # selectedVarData <-  reactive({
 #   
 #   
 #   sidata <- selectedData()
 #   
 #   return(sidata)
 # })
 getHDDData <- reactive({

   station <- input$finalStationName
   stationData <- getStations()
   stationID <- stationData %>% filter(station_name==station)%>%select(station_id)
   weatherData <-  weather_dl(station_ids  = stationID[1] , start = input$weatherDaterange[1], end = input$weatherDaterange[2],interval =input$intervalBtn)
   weatherOutPut <- data.frame(Station_Name = weatherData$station_name, Date =  weatherData$date, HDD_18celsius= weatherData$heat_deg_days, CDD=weatherData$cool_deg_days)
   weatherOutPut
 })
 
 
 formatedData <- reactive({
   data <- downloadData()
   data$invoiceDate <- as.Date(data$invoiceDate,format="%d/%m/%Y")
   
   finalDF<- data %>%
     mutate(month = format(invoiceDate, "%m"), year = format(invoiceDate, "%Y"))
   finalDF
 })
  
 
  

 
 
 
      #Event Reactive Functions------
  getStations <- eventReactive(input$stationSearchBtn, {
    
    if(input$stationSearch!="")
    {
      snData <-stationNames()
      startYear <- as.numeric(str_split(input$weatherDaterange[1],"-")[[1]][1])
      endYear <- as.numeric(str_split(input$weatherDaterange[2],"-")[[1]][1])
      
      filteredData <- snData %>% filter((start-startYear<=0 & end -endYear>= 0)&(prov==input$provinceBtn))
      filteredData
    }
    else
    {
      shinyalert("Oops!", "Enter a station name", type = "error")
    }
    
    
  })
  selectedDateData <- eventReactive(input$daterange,{
    sitesData <- downloadedData()
    abis<-strptime(sitesData$invoiceDate,format="%d/%m/%Y")
    sitesData$invoiceDate <-  as.Date(abis, format = "%Y-%m-%d")
    selData <- sitesData %>% filter(invoiceDate >= input$daterange[1] & invoiceDate <= input$daterange[2])
    
    return(selData)
    
  })
  
  # observeEvent(xxchange(),{
  #   browser()
  #   selectedSchoolsdata <- input$SN_schoolNameVar
  #   sitesData <- SN_SelectedReactiveValues()
  #   sitesData$schoolName <- as.factor(sitesData$schoolName)
  #   if(length(selectedSchoolsdata)==0)
  #   {
  #     selectedSchoolsdata <- unique(sitesData$schoolName)
  #   }
  #   if(length(input$SN_daterange)==0)
  #   {
  #     x<-as.Date("2015-01-01","%Y-%m-%d")
  #     startRange <- as.yearmon(x)
  #     endRange <- as.yearmon(Sys.Date())
  #   }
  #   else
  #   {
  #     startRange <- as.yearmon(input$SN_daterange[1])
  #     endRange <- as.yearmon(input$SN_daterange[2])
  #   }
  #   
  #   sitesData$mon_year2 <- as.yearmon(sitesData$mon_year,"%m/%Y")
  #   
  #   
  #   # schData <- sitesData%>% filter(schoolName==selectedSchoolsdata)%>%filter(invoiceDate >= input$daterange[1] & invoiceDate <= input$daterange[2])
  #   
  #   schData<- filter(sitesData,(deliveredBy=="Suncor")& (schoolName %in%selectedSchoolsdata)& 
  #                      (mon_year2 >= startRange & mon_year2 <= endRange) )
  #   
  #   # schData<- subset(sitesData, (schoolName==selectedSchoolsdata) & 
  #   #                    (mon_year2 >= startRange & mon_year2 <= endRange))
  #   SN_SelectedReactiveValues(schData)
  #   return(schData)
  #   
  # })

  
  
  SN_selectedData <-eventReactive(xx_SNchange(),{

    selectedSchoolsdata <- input$SN_schoolNameVar
    NRvalues <- normaliZedReactiveValues()
    sitesData <- filter(NRvalues,deliveredBy=="Suncor")
    sitesData$schoolName <- as.factor(sitesData$schoolName)
    if(length(selectedSchoolsdata)==0)
    {
      selectedSchoolsdata <- unique(sitesData$schoolName)
    }
    if(length(input$SN_daterange)==0)
    {
      x<-as.Date("2015-01-01","%Y-%m-%d")
      startRange <- as.yearmon(x)
      endRange <- as.yearmon(Sys.Date())
    }
    else
    {
      startRange <- as.yearmon(input$SN_daterange[1])
      endRange <- as.yearmon(input$SN_daterange[2])
    }
   
    # sitesData$mon_year2 <- as.yearmon(sitesData$mon_year,"%m/%Y")
   
    
    # schData <- sitesData%>% filter(schoolName==selectedSchoolsdata)%>%filter(invoiceDate >= input$daterange[1] & invoiceDate <= input$daterange[2])
    
    schData<- filter(sitesData, (schoolName %in%selectedSchoolsdata)& 
                                          (mon_year >= startRange & mon_year <= endRange) )
              
    # schData<- subset(sitesData, (schoolName==selectedSchoolsdata) & 
    #                    (mon_year2 >= startRange & mon_year2 <= endRange))
 
    SN_SelectedReactiveValues(schData)
    return(schData)
    
  })
  
  
  SP_selectedData <-eventReactive(xx_SPchange(),{
    
    selectedSchoolsdata <- input$SP_schoolNameVar
    NRvalues <- normaliZedReactiveValues()
    sitesData <- filter(NRvalues,deliveredBy=="Superior Propane")
    sitesData$schoolName <- as.factor(sitesData$schoolName)
    if(length(selectedSchoolsdata)==0)
    {
      selectedSchoolsdata <- unique(sitesData$schoolName)
    }
    if(length(input$SP_daterange)==0)
    {
      x<-as.Date("2015-01-01","%Y-%m-%d")
      startRange <- as.yearmon(x)
      endRange <- as.yearmon(Sys.Date())
    }
    else
    {
      startRange <- as.yearmon(input$SP_daterange[1])
      endRange <- as.yearmon(input$SP_daterange[2])
    }
    
    # sitesData$mon_year2 <- as.yearmon(sitesData$mon_year,"%m/%Y")
    
    
    # schData <- sitesData%>% filter(schoolName==selectedSchoolsdata)%>%filter(invoiceDate >= input$daterange[1] & invoiceDate <= input$daterange[2])
    
    schData<- filter(sitesData, (schoolName %in%selectedSchoolsdata)& 
                       (mon_year >= startRange & mon_year <= endRange) )
    
    # schData<- subset(sitesData, (schoolName==selectedSchoolsdata) & 
    #                    (mon_year2 >= startRange & mon_year2 <= endRange))
    
    SP_SelectedReactiveValues(schData)
    return(schData)
    
  })
  
  
  
  
      #Download Handlers-----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("downloadData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactdf(), file, row.names = FALSE)
    }
  )
  output$SN_DownloadData <- downloadHandler(
    filename = function() {
      paste("Selected Suncor Data ", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(SN_selectedData(), file, row.names = FALSE)
    }
  )
  output$SP_DownloadData <- downloadHandler(
    filename = function() {
      paste("Selected Superior Propane Data ", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(SP_selectedData(), file, row.names = FALSE)
    }
  )
  output$SN_generateReport <- downloadHandler(
    filename = function() {
      "Suncor Report.pdf"
    },
    content = function(file) {
      pdf(file,onefile=TRUE)
      grid.arrange(snRValues$snr1,snRValues$snr2)
      dev.off()
    }
  )
  
  output$SP_generateReport <- downloadHandler(
    filename = function() {
      "Superior Propane Report.pdf"
    },
    content = function(file) {
      pdf(file,onefile=TRUE)
      grid.arrange(spRValues$spr1,spRValues$spr2)
      dev.off()
    }
  )
  
  output$generateWeatherReport <- downloadHandler(
    filename = function() {
      "weatherReport.csv"
    },
    
    content = function(file) {
      write.csv(getHDDData(), file, row.names = FALSE)
      
    }
  )
  
   
  
  
      #RenderUI Functions------ 
 
  output$dashBoardBoxes <- renderUI({

    data <- normaliZedReactiveValues()
    sites <- sitesDataRValues()
    totalSchools <- sort(unique(data$schoolName))
# sd <- data%>% mutate(yeCar=)
    currentYear <- as.integer(format(Sys.Date(), "%Y"))
   
    lapply(1:length(totalSchools), function(i) {
      fData_SN_P1Y <- data %>% filter((schoolName==totalSchools[i])&(deliveredBy=="Suncor")&(year==(currentYear-1)))
      fData_SN_P2Y <- data %>% filter((schoolName==totalSchools[i])&(deliveredBy=="Suncor")&(year==(currentYear-2)))
      
      fData_SP_P1Y <- data %>% filter((schoolName==totalSchools[i])&(deliveredBy=="Superior Propane")& (year==(currentYear-1)))
      fData_SP_P2Y <- data %>% filter((schoolName==totalSchools[i])&(deliveredBy=="Superior Propane")& (year==(currentYear-2)))
      
      if(dim(fData_SN_P1Y)[1]>0&dim(fData_SN_P2Y)[1]>0)
      {
        Per_SN <- (sum(as.numeric(fData_SN_P1Y$quantity))-sum(as.numeric(fData_SN_P2Y$quantity)))/(sum(as.numeric(fData_SN_P2Y$quantity)))
        restext_SN <- getPercentResult(Per_SN)
        Per_SN2 <- (sum(as.numeric(fData_SN_P1Y$KWhperSqFeet))-sum(as.numeric(fData_SN_P2Y$KWhperSqFeet)))/(sum(as.numeric(fData_SN_P2Y$KWhperSqFeet)))
        restext_SN2 <- getPercentResult(Per_SN2)
      }
      else if(dim(fData_SN_P1Y)[1]>0&dim(fData_SN_P2Y)[1]==0)
      {
        Per_SN <- sum(as.numeric(fData_SN_P1Y$quantity))
        resText <-reactive((paste0("<i>","<b>","<font color='red'>","increased by ", round(Per_SN,2),"</font>","</b>","</i>")))
        restext_SN <- resText()
        Per_SN2 <- sum(as.numeric(fData_SN_P1Y$KWhperSqFeet))
        resText2 <-reactive((paste0("<i>","<b>","<font color='red'>","increased by ", round(Per_SN2,2),"</font>","</b>","</i>")))
        restext_SN2 <- resText2()
      }
      else if(dim(fData_SN_P1Y)[1]==0&dim(fData_SN_P2Y)[1]>0)
      {
        Per_SN <- sum(as.numeric(fData_SN_P2Y$quantity))
        resText <-reactive((paste0("<i>","<b>","<font color='red'>","Decreased by ", round(Per_SN,2),"</font>","</b>","</i>")))
        restext_SN <- resText()
        Per_SN2 <- sum(as.numeric(fData_SN_P2Y$KWhperSqFeet))
        resText2 <-reactive((paste0("<i>","<b>","<font color='red'>","Decreased by ", round(Per_SN2,2),"</font>","</b>","</i>")))
        restext_SN2 <- resText2()
      }
      else
      {
        resText <-reactive((paste0("<i>","<b>","<font color='#2fb534'>","increased by ", "0%","</font>","</b>","</i>")))
        restext_SN <- resText()
        restext_SN2 <- resText()
      }
      
      if(dim(fData_SP_P1Y)[1]>0&dim(fData_SP_P2Y)[1]>0)
      {
        Per_SP <- (sum(as.numeric(fData_SP_P1Y$quantity))-sum(as.numeric(fData_SP_P2Y$quantity)))/(sum(as.numeric(fData_SP_P2Y$quantity)))
        restext_SP <- getPercentResult(Per_SP)
        Per_SP2 <- (sum(as.numeric(fData_SP_P1Y$KWhperSqFeet))-sum(as.numeric(fData_SP_P2Y$KWhperSqFeet)))/(sum(as.numeric(fData_SP_P2Y$KWhperSqFeet)))
        restext_SP2 <- getPercentResult(Per_SP2)
      }
      else if(dim(fData_SP_P1Y)[1]>0&dim(fData_SP_P2Y)[1]==0)
      {
        Per_SP <- sum(as.numeric(fData_SP_P1Y$quantity))
        resText <-reactive((paste0("<i>","<b>","<font color='red'>","increased by ", round(Per_SP,2),"</font>","</b>","</i>")))
        restext_SP <- resText()
        Per_SP2 <- sum(as.numeric(fData_SP_P1Y$KWhperSqFeet))
        resText2 <-reactive((paste0("<i>","<b>","<font color='red'>","increased by ", round(Per_SP2,2),"</font>","</b>","</i>")))
        restext_SP2 <- resText2()
      }
      else if(dim(fData_SP_P1Y)[1]==0&dim(fData_SP_P2Y)[1]>0)
      {
        Per_SP <- sum(as.numeric(fData_SP_P2Y$quantity))
        resText <-reactive((paste0("<i>","<b>","<font color='red'>","Decreased by ", round(Per_SP,2),"</font>","</b>","</i>")))
        restext_SP <- resText()
        Per_SP2 <- sum(as.numeric(fData_SP_P2Y$KWhperSqFeet))
        resText2 <-reactive((paste0("<i>","<b>","<font color='red'>","Decreased by ", round(Per_SP2,2),"</font>","</b>","</i>")))
        restext_SP2 <- resText2()
      }
      else
      {
        resText <-reactive((paste0("<i>","<b>","<font color='#2fb534'>","increased by ", "0%","</font>","</b>","</i>")))
        restext_SP <- resText()
        restext_SP2 <- resText()
      }
      
     
      
      # Per_SP <- (sum(as.numeric(fData_SN_P1Y$quantity))-sum(as.numeric(fData_SN_P2Y$quantity)))/(sum(as.numeric(fData_SN_P2Y$quantity)))
      # restext_SP <- getPercentResult(Per_SP)
      
     
      
      TotalConsp_SN <- sum(as.numeric(fData_SN_P1Y$quantity),na.rm = TRUE)
      KWHPerfeet_SN <- sum(as.numeric(fData_SN_P1Y$KWhperSqFeet),na.rm = TRUE)
      TotalConsp_SP <- sum(as.numeric(fData_SP_P1Y$quantity),na.rm=TRUE)
      KWHPerfeet_SP <- sum(as.numeric(fData_SP_P1Y$KWhperSqFeet),na.rm = TRUE)
      
      siteDetails <- sites%>% filter(Site_Name==totalSchools[i])
      
      if(is.nan(TotalConsp_SN))
      {
        TotalConsp_SN<-0
      }
      if(is.nan(TotalConsp_SP))
      {
        TotalConsp_SP<-0
      }
      if(is.nan(KWHPerfeet_SN))
      {
        KWHPerfeet_SN<-0
      }
      if(is.nan(KWHPerfeet_SP))
      {
        KWHPerfeet_SP<-0
      }


      box(title = paste0(totalSchools[i]),status = "primary",solidHeader = TRUE,


         # render({fData_SN %>% ggplot(aes(mon_year,NormalizedKWH2))+geom_line(aes(color=schoolName))})
         # renderPlot({fData_SP %>% ggplot(aes(mon_year,NormalizedKWH2))+geom_line(aes(color=schoolName))})
        
      
         
        
         HTML(paste("<b>","Address: ","</b>",siteDetails$Address,",",siteDetails$City,",",siteDetails$ZipCode,"<br>")),
         
         HTML(paste("<b>","Area: ","</b>",siteDetails$Area_SQFT," SQFT","<br>")),
        
         HTML(paste("<b>","Operating Hours: ","</b>",siteDetails$Operating_hours," Hrs/Week","<br>")),
         
         HTML(paste("<b>","Suncor:","<br>","</b>")),
         HTML(paste("<b>","a) ","</b>","Oil consumption for ",currentYear-1," is " ,TotalConsp_SN,"Litres, ",restext_SN," than ",currentYear-2,"<br>",sep="")),
         HTML(paste("<b>","b) ","</b>","Normalized KWH per square feet for the year ", currentYear-1," is ","<b>",KWHPerfeet_SN," KWH/sq-feet,","</b>","<br>",restext_SN2," than ",currentYear-2,"<br>",sep="")),
         HTML(paste("<b>","Superior Propane:","<br>","</b>")),
         HTML(paste("<b>","a) ","</b>","Propane consumption for ",currentYear-1," is " ,TotalConsp_SP,"Litres, ",restext_SP," than ",currentYear-2,"<br>",sep="")),
         HTML(paste("<b>","b) ","</b>","Normalized KWH per square feet for the year ", currentYear-1," is ","<b>",KWHPerfeet_SP," KWH/sq-feet,","</b>","<br>",restext_SP2," than ",currentYear-2,"<br>",sep="")),
         

         actionButton(inputId="a", label="Visit School", 
                      icon = icon("firefox"), 
                      onclick =paste("window.open(","'",siteDetails$URL,"'",",","'_blank')"),
                      style="color: #fff; background-color: #0c4294;align:right")


          )
    })

  })

  
  output$stationNames <- renderUI({
    swd <- getStations()
    pickerInput("finalStationName", "select a station", choices = swd$station_name, selected = NULL,multiple = FALSE)
  })
  
  
  output$SN_datePicker <- renderUI({
    dateRangeInput("SN_daterange", "Date range:",
                   start = "2017-01-01",
                   end   = Sys.Date())
  })
  
  output$SP_datePicker <- renderUI({
    dateRangeInput("SP_daterange", "Date range:",
                   start = "2017-01-01",
                   end   = Sys.Date())
  })
  output$weatherDatePicker <- renderUI({
    dateRangeInput("weatherDaterange", "Date range:",
                   start = "2017-01-01",
                   end   = Sys.Date())
  })
  
  
  output$SN_schoolVar <- renderUI({
    sdata <- normaliZedReactiveValues()
    schoolNames <- filter(sdata,deliveredBy=="Suncor")
    pickerInput(
      "SN_schoolNameVar",
      label = "Select School",
      choices =as.vector(unique(schoolNames$schoolName)),
      options = list(
        `actions-box` = TRUE,
        `none-selected-text` = "All Schools Selected",
        size = 25
      ),
      multiple = TRUE
    )})
  
  output$SP_schoolVar <- renderUI({
    sdata <- normaliZedReactiveValues()
    schoolNames <- filter(sdata,deliveredBy=="Superior Propane")
    pickerInput(
      "SP_schoolNameVar",
      label = "Select School",
      choices =as.vector(unique(schoolNames$schoolName)),
      options = list(
        `actions-box` = TRUE,
        `none-selected-text` = "All Schools Selected",
        size = 25
      ),
      multiple = TRUE
    )})
   
   
  
  
  
      #Render Plots----- 
  
  
  # output$plot1 <- renderImage({
  #   # A temp file to save the output.
  #   # This file will be removed later by renderImage
  #   
  #   siteData <- normaliZedReactiveValues()
  #   siteData <- transform(siteData, Date3 = as.Date(mon_year, frac = 1))
  #   outfile <- tempfile(fileext='.gif')
  #   
  #   # now maktransform(df, Date3 = as.Date(Date2, frac = 1))e the animation
  #   p = ggplot(siteData, aes(mon_year,NormalizedKWH2, 
  #                             color = schoolName)) + geom_point() + transition_time(Date3)
  #  
  #   
  #   anim_save("outfile.gif", animate(p)) # New
  #   
  #   # Return a list containing the filename
  #   list(src = "outfile.gif",
  #        contentType = 'image/gif'
  #        # width = 400,
  #        # height = 300,
  #        # alt = "This is alternate text"
  #   )}, deleteFile = TRUE)
  # 
   
   output$snLinePlot <- renderPlotly({
     siteData <- SN_SelectedReactiveValues()

     snRValues$snr1 <- siteData %>% ggplot(aes(mon_year,NormalizedKWH2))+geom_line(aes(color=schoolName))+
                              geom_hline(yintercept=mean(as.numeric(siteData$NormalizedKWH2)), 
                                         linetype="dashed", color = "black", size=1)+
                                        ggtitle("Plot of Monthly Consumption(Normalised Consumption)") + xlab("Month, Year") + ylab("Normalized KWH")
     ggplotly( snRValues$snr1,tooltip=c("x","y","schoolName"))
     # rValues$p3
   })
  
  # output$snBarPlot <- renderPlotly({
  #   siteData <- SN_SelectedReactiveValues()
  #   
  #   snRValues$snr2 <- siteData %>% ggplot(aes(mon_year,KWhperSqFeet))+geom_bar(stat="identity",aes(fill=schoolName))+
  #     geom_hline(yintercept=mean(as.numeric(siteData$KWhperSqFeet)), 
  #                linetype="dashed", color = "black", size=1)+
  #     ggtitle("Energy consumption per Square feet per hour vs School") + xlab("School") + ylab("KWH/Sq-feet/hour")
  #   ggplotly( snRValues$snr2,tooltip=c("x","y","schoolName"))
  #   # rValues$p3
  # })
  output$snBarPlot2 <- renderPlotly({
    siteData <- SN_SelectedReactiveValues()
    
    newData <- siteData %>% group_by(schoolName,year)%>% summarise(sum_NormalizedKWH2=sum(NormalizedKWH2))
    snRValues$snr2 <- newData %>%ggplot(aes(x=reorder(schoolName,-sum_NormalizedKWH2),y=sum_NormalizedKWH2,,fill=year))+geom_bar(stat="identity",position = 'dodge')+
      ggtitle("Energy consumption per Square feet vs School") + xlab("School") + ylab("KWH/Sq-feet")
    ggplotly( snRValues$snr2,tooltip=c("x","y","year","schoolName"))
    # rValues$p3
  })
  
  output$snBarPlot <- renderPlotly({
    siteData <- SN_SelectedReactiveValues()
    
    newData <- siteData %>% group_by(schoolName)%>% summarise(sum_KWHperfeet=sum(KWhperSqFeet))
    snRValues$snr3 <- newData %>%ggplot(aes(x=reorder(schoolName,-sum_KWHperfeet),y=sum_KWHperfeet))+geom_bar(stat="identity",aes(fill=schoolName))+
      ggtitle("Energy consumption per Square feet vs School") + xlab("School") + ylab("KWH/Sq-feet")
    ggplotly( snRValues$snr3,tooltip=c("x","y","mon_year"))
    # rValues$p3
  })
  
  
  
  output$spLinePlot <- renderPlotly({
    siteData <- SP_SelectedReactiveValues()
    
    spRValues$spr1 <- siteData %>% ggplot(aes(mon_year,NormalizedKWH2))+geom_line(aes(color=schoolName))+
      geom_hline(yintercept=mean(as.numeric(siteData$NormalizedKWH2)), 
                 linetype="dashed", color = "black", size=1)+
      ggtitle("Plot of Monthly Consumption(Normalised Consumption)") + xlab("Month, Year") + ylab("Normalized KWH")
    ggplotly(spRValues$spr1,tooltip=c("x","y","schoolName"))
    # rValues$p3
  })
  
  # output$spBarPlot <- renderPlotly({
  #   siteData <- SP_SelectedReactiveValues()
  #   
  #   spRValues$spr2 <- siteData %>% ggplot(aes(mon_year,KWhperSqFeet))+geom_bar(stat="identity",aes(fill=schoolName))+
  #     geom_hline(yintercept=mean(as.numeric(siteData$KWhperSqFeet)), 
  #                linetype="dashed", color = "black", size=1)+
  #     ggtitle("Energy consumption per Square feet per hour vs School") + xlab("School") + ylab("KWH/Sq-feet/hour")
  #   ggplotly(spRValues$spr2,tooltip=c("x","y","schoolName"))
  #   # rValues$p3
  # })
  output$spBarPlot <- renderPlotly({
    siteData <- SP_SelectedReactiveValues()
    
    newData <- siteData %>% group_by(schoolName)%>% summarise(sum_KWHperfeet=sum(KWhperSqFeet))
    
    spRValues$spr2 <- newData %>% ggplot(aes(x=reorder(schoolName,-sum_KWHperfeet),y=sum_KWHperfeet))+geom_bar(stat="identity",aes(fill=schoolName))+
      ggtitle("Energy consumption per Square feet vs School") + xlab("School") + ylab("KWH/Sq-feet")
    ggplotly(spRValues$spr2,tooltip=c("x","y","mon_year"))
    # rValues$p3
  })
  # })
  output$spBarPlot2 <- renderPlotly({
    siteData <- SP_SelectedReactiveValues()
    
    newData <- siteData %>% group_by(schoolName,year)%>% summarise(sum_NormalizedKWH2=sum(NormalizedKWH2))
    
    spRValues$spr3 <- newData %>% ggplot(aes(x=reorder(schoolName,-sum_NormalizedKWH2),y=sum_NormalizedKWH2,fill=year))+geom_bar(stat="identity",position = 'dodge')+
      ggtitle("Energy consumption per Square feet vs School") + xlab("School") + ylab("KWH/Sq-feet")
    ggplotly(spRValues$spr3,tooltip=c("x","y","mon_year"))
    # rValues$p3
  })
  
  
   # 
   # output$snBarPlot <- renderPlot({
   #   siteData <- selectedVarData()
   #   
   #   rValues$p4<- siteData %>% ggplot(aes(fill=deliveredBy, y=kwhEquivalent, x=schoolName)) +  geom_bar(position="dodge", stat="identity")
   #   rValues$p4
   # })
   
   
  
      #Render Text------
   
  output$SN_AnalysisHeading <- renderText({
    "Suncor Data Analysis"
  })
  output$SP_AnalysisHeading <- renderText({
    "Superior Propane Data Analysis"
  })
  output$pdfHeading <- renderText({
  "Extract Data from PDF to Excel/CSV"
  })
  output$manageData <- renderText({
    "Manage Data"
  })
  output$manageSiteData <- renderText({
    "Manage Sites"
  })
  
  output$weatherReportHeading <- renderText({
    "Download Heating Degrees Days Report"
  })
  
  output$DashboardHeading <- renderText({
    "Welcome to L.D.S.B"
  })
  output$overviewTitle <- renderText({
    "Data Overview:"
  })
  output$SN_Summary1 <- renderText({
    siteData <- normaliZedReactiveValues()
    currentYear <- as.integer(format(Sys.Date(), "%Y"))
    fData_SN_P1Y <- siteData %>% filter(deliveredBy=="Suncor"& year==currentYear-1)
    fData_SN_P2Y <- siteData %>% filter(deliveredBy=="Suncor"& year==currentYear-2)
    
    Per <- (sum(as.numeric(fData_SN_P1Y$quantity))-sum(as.numeric(fData_SN_P2Y$quantity)))/(sum(as.numeric(fData_SN_P2Y$quantity)))
    restext <- getPercentResult(Per)

    res <- reactive(HTML(paste("Total Oil consumption for the year 2019 has","<br>", restext ," compared to 2018",sep="")))
    res()
  })
  output$SN_Summary2 <- renderText({
    siteData <- normaliZedReactiveValues()
    currentYear <- as.integer(format(Sys.Date(), "%Y"))
    fData_SN_P1Y <- siteData %>% filter(deliveredBy=="Suncor"& year==currentYear-1)
    fData_SN_P2Y <- siteData %>% filter(deliveredBy=="Suncor"& year==currentYear-2)
    
    Per <- (sum(as.numeric(fData_SN_P1Y$NormalizedKWH2))-sum(as.numeric(fData_SN_P2Y$NormalizedKWH2)))/(sum(as.numeric(fData_SN_P2Y$NormalizedKWH2)))
    resText <- getPercentResult(Per)
    res <- reactive(HTML(paste("<br>","However, Normalized KWH ", resText ,"<br>compared to 2018",sep="")))
    res()
  })
  output$SP_Summary1 <- renderText({
    siteData <- normaliZedReactiveValues()
    currentYear <- as.integer(format(Sys.Date(), "%Y"))
    fData_SP_P1Y <- siteData %>% filter(deliveredBy=="Superior Propane"& year==currentYear-1)
    fData_SP_P2Y <- siteData %>% filter(deliveredBy=="Superior Propane"& year==currentYear-2)
    
    Per <- (sum(as.numeric(fData_SP_P1Y$quantity))-sum(as.numeric(fData_SP_P2Y$quantity)))/(sum(as.numeric(fData_SP_P2Y$quantity)))
    restext <- getPercentResult(Per)
    
    res <- reactive(HTML(paste("Total Propane consumption for the year 2019 has","<br>", restext ," compared to 2018",sep="")))
    res()
  })
  output$SP_Summary2 <- renderText({
    siteData <- normaliZedReactiveValues()
    currentYear <- as.integer(format(Sys.Date(), "%Y"))
    fData_SP_P1Y <- siteData %>% filter(deliveredBy=="Superior Propane"& year==currentYear-1)
    fData_SP_P2Y <- siteData %>% filter(deliveredBy=="Superior Propane"& year==currentYear-2)
    
    Per <- (sum(as.numeric(fData_SP_P1Y$NormalizedKWH2))-sum(as.numeric(fData_SP_P2Y$NormalizedKWH2)))/(sum(as.numeric(fData_SP_P2Y$NormalizedKWH2)))
    resText <- getPercentResult(Per)
    res <- reactive(HTML(paste("<br>","However, Normalized KWH ", resText ,"<br>compared to 2018",sep="")))
    res()
  })
  output$ourSchoolsTitle <- renderUI({
    HTML(paste("<h2>","<b>","<u>","Our schools at a glance","</u>","</b>","</h3>"))
  })
  
  
      #Infobox-----
  
  
  output$DB_AvgConsp_SN <- renderValueBox({
    
    siteData <- normaliZedReactiveValues()
    currentYear <- as.integer(format(Sys.Date(), "%Y"))
    fData_SN <- siteData %>% filter(deliveredBy=="Suncor"& year==currentYear-1)
   browser
    avgConsp_SN <- round(sum(as.numeric(fData_SN$quantity),na.rm = TRUE),2)
    val<-tags$p(paste(avgConsp_SN,"Litres"), style = "font-size: 45%;")
    vb1 <-valueBox(val, paste("Total Oil Consumption for year",currentYear-1),icon = icon("oil-can"),color = "green")
    vb1
  })
  output$DB_AvgConsp_SP <- renderValueBox({
    
    siteData <- normaliZedReactiveValues()
    currentYear <- as.integer(format(Sys.Date(), "%Y"))
    fData_SP <- siteData %>% filter(deliveredBy=="Superior Propane"&year==currentYear-1)
    
    avgConsp_SP <- round(sum(as.numeric(fData_SP$quantity),na.rm = TRUE),2)
    val<-tags$p(paste(avgConsp_SP,"Litres"), style = "font-size: 45%;")
    vb1 <-valueBox(val, paste("Total Propane Consumption for the year",currentYear-1),icon = icon("gripfire"),color = "yellow")
    vb1
  })
  
  
 output$SN_numberOfSchools <- renderValueBox({

     siteData <- SN_SelectedReactiveValues()
     val<-tags$p(paste(length(unique(siteData$school)),"Schools"), style = "font-size: 58%;")
  rValues$p1 <-valueBox(val, "Data being displayed",icon = icon("university"),color = "yellow")
  rValues$p1
 })
  output$SN_TotalQuantity <- renderValueBox({

    siteData <- SN_SelectedReactiveValues()
    val<-tags$p(paste(sum(siteData$quantity),"Litres"), style = "font-size: 58%;")
    rValues$p2 <-valueBox(val, "Total ordered Quantity",icon = icon("balance-scale"),color = "blue")
    rValues$p2
  })
  output$SN_TotalAmount <- renderValueBox({
    
    siteData <- SN_SelectedReactiveValues()
    val<-tags$p(paste("CDN$ ",sum(siteData$amount)), style = "font-size: 58%;")
    p5 <-valueBox(val, "Total Amount Spent",icon = icon("dollar-sign"),color = "green")
    p5
  })
  output$SN_AveragePrice <- renderValueBox({
    
    siteData <- SN_SelectedReactiveValues()
    val<-tags$p(paste(round(mean(siteData$unitPrice),3),"/Litre"), style = "font-size: 58%;")
    p6 <-valueBox(val, "Average Price ",icon = icon("coins"),color = "purple")
    p6
  })
  output$SN_kwhEquivalent <- renderValueBox({
    
    siteData <- SN_SelectedReactiveValues()
    val<-tags$p(paste(round(sum(siteData$NormalizedKWH2),2),"KWH"), style = "font-size: 58%;")
    p7 <-valueBox(val, "Electric Equivalent ",icon = icon("bolt"),color = "red")
    p7
  })
  
  
  
  output$SP_numberOfSchools <- renderValueBox({
    
    siteData <- SP_SelectedReactiveValues()
    val<-tags$p(paste(length(unique(siteData$school)),"Schools"), style = "font-size: 58%;")
    p13 <-valueBox(val, "Data being displayed",icon = icon("university"),color = "yellow")
    p13
  })
  output$SP_TotalQuantity <- renderValueBox({
    
    siteData <- SP_SelectedReactiveValues()
    val<-tags$p(paste(sum(siteData$quantity),"Litres"), style = "font-size: 58%;")
    p14 <-valueBox(val, "Total ordered Quantity",icon = icon("balance-scale"),color = "blue")
    p14
  })
  output$SP_TotalAmount <- renderValueBox({
    
    siteData <- SP_SelectedReactiveValues()
    val<-tags$p(paste("CDN$ ",sum(siteData$amount)), style = "font-size: 58%;")
    p15 <-valueBox(val, "Total Amount Spent",icon = icon("dollar-sign"),color = "green")
    p15
  })
  output$SP_AveragePrice <- renderValueBox({
    
    siteData <- SP_SelectedReactiveValues()
    val<-tags$p(paste(round(mean(siteData$unitPrice),3),"/Litre"), style = "font-size: 58%;")
    p16 <-valueBox(val, "Average Price ",icon = icon("coins"),color = "purple")
    p16
  })
  output$SP_kwhEquivalent <- renderValueBox({
    
    siteData <- SP_SelectedReactiveValues()
    val<-tags$p(paste(round(sum(siteData$NormalizedKWH2),2),"KWH"), style = "font-size: 58%;")
    p17 <-valueBox(val, "Electric Equivalent ",icon = icon("bolt"),color = "red")
    p17
  })
 # output$numOfconf <- renderInfoBox({
 #   
 #   siteData <- selectedVarData()
 #   spConsump <-  getTotalConsumption(siteData,"Superior Propane")
 #   
 #   rValues$p2 <-infoBox("Total Propane Consumption",, icon = icon("fire"),color = "red")
 #   rValues$p2
 # })
 # 
 
 
  
      #Data Tables------
 
 
 output$cnvrtPDFToExcel <- DT::renderDT({
   myData <- reactdf()
   DT::datatable(myData, editable = TRUE)
 })
 
 output$displayData <- DT::renderDT({
  
   myData <- downloadedDataRvalues()
   manageReactiveValues(myData)
   DT::datatable(filter="top",myData, editable = TRUE, selection = 'multiple',rownames = FALSE)
 })
 output$displaySiteData <- DT::renderDT({
  
   myData <- sitesDataRValues()
   manageReactiveValues(myData)
   DT::datatable(filter="top",myData, editable = TRUE, selection = 'multiple',rownames = FALSE)
 })
 
 
 
 
      #DB modify Queries--------
 
 addSiteDataDB <- observeEvent(input$addSiteData, {
   
   if(siteDataValidation())
   {
     query <- paste("INSERT INTO Sites (Site_Identifier,Site_Name,Address,City,ZipCode,Area_SQFT,Operating_hours) VALUES(","'",input$siteIdentifierTB,"'",",","'",input$siteNameTB,"'",",","'",input$addressTB,"'",',',
                    "'",input$cityTB,"'",",","'",input$zipCodeTB,"'",",","'",input$areaTB,"'",",","'",input$openingHoursTB,"'",")",sep="" )
     dbSendQuery(dbcnt(),query)
     sitesDataRValues(dbGetQuery(dbcnt(),"select * from Sites"))
     shinyalert("It's Done", "Successfully added the Data", type = "success")
   }
   else
   {
     shinyalert("Oops!", "Enter all Values", type = "error")
   }
 })
 
 addDataDB <- observeEvent(input$addData, {
 
   if(dataValidation())
   {
     query <- paste("INSERT INTO downloadData (deliveredBy, invoiceDate,invoiceTotal,schoolName,orderQuantity,unitPrice,kwhEquivalent) VALUES(","'",input$deliveredByTB,"'",",","'",input$invoiceDateTB,"'",",","'",input$invoiceTotalTB,"'",',',
                    "'",input$schoolNameTB,"'",",","'",input$orderQuantityTB,"'",",","'",input$unitPriceTB,"'",",","'",input$kwhEquivalentTB,"'",")",sep="" )
     dbSendQuery(dbcnt(),query)
     downloadedDataRvalues(dbGetQuery(dbcnt(),"select * from downloadData"))
     shinyalert("It's Done", "Successfully Deleted the Data", type = "success")
   }
   else
   {
     shinyalert("Oops!", "Enter all Values", type = "error")
   }
   
   
 })
 
 deleteSelectedData <- observeEvent(input$deleteData, {

   dat <- downloadedDataRvalues()
   cell <- input$displayData_rows_selected
   if(length(cell)>0)
   {
     for(i in 1:length(cell)){
       query <- paste("DELETE from downloadData"," Where ID=",dat[cell[i],1],sep="" )
       dbSendQuery(dbcnt(),query)
     }
     downloadedDataRvalues(dbGetQuery(dbcnt(),"select * from downloadData"))
     shinyalert("It's Done", "Successfully Deleted the Data", type = "success")
   }
   else
   {
     shinyalert("Opps!", "Select rows to remove", type = "error")
   }
   
 })
 
 deleteSelectedSiteData <- observeEvent(input$deleteSiteData, {
   dat <- sitesDataRValues()
   cell <- input$displaySiteData_rows_selected
   if(length(cell)>0)
   {
     for(i in 1:length(cell)){
       query <- paste("DELETE from Sites"," Where SID=",dat[cell[i],1],sep="" )
       dbSendQuery(dbcnt(),query)
     }
     sitesDataRValues(dbGetQuery(dbcnt(),"select * from Sites"))
     shinyalert("It's Done", "Successfully Deleted the Data", type = "success")
   }
   else
   {
     shinyalert("Opps!", "Select rows to remove", type = "error")
   }
 })
 
 savepdfData <- observeEvent(input$saveData,{
   
   extracteddata <- reactdf()
   
   for(i in 1:dim(extracteddata)[1])
   {
     query <- paste("INSERT INTO downloadData (deliveredBy, invoiceDate,invoiceTotal,schoolName,orderQuantity,unitPrice,kwhEquivalent) VALUES(","'",extracteddata[i,1],"'",",","'",extracteddata[i,2],"'",",","'",extracteddata[i,3],"'",',',
                    "'",extracteddata[i,4],"'",",","'",extracteddata[i,5],"'",",","'",extracteddata[i,6],"'",",","'",extracteddata[i,7],"'",")",sep="" )
     dbSendQuery(dbcnt(),query)
     
   }
  
   shinyalert("It's Done", "Successfully saved the Data", type = "success")
 })
 
 
 updatedWholeData <- observeEvent(input$displayData_cell_edit, {
   
   dat <- downloadedDataRvalues()
   cell <- input$displayData_cell_edit
  
   colName <- switch(cell$col,"deliveredBy","invoiceDate","invoiceTotal","schoolName","orderQuantity","unitPrice","kwhEquivalent")
   query <- paste("UPDATE downloadData SET ",colName, "=","'",cell$value,"'"," Where ID=",dat[cell$row,1],sep="" )
   dbSendQuery(dbcnt(),query)
   downloadedDataRvalues(dbGetQuery(dbcnt(),"select * from downloadData"))
   shinyalert("It's Done", "Successfully saved the Data", type = "success")
 })
 
 updatedSitesData <- observeEvent(input$displaySiteData_cell_edit, {
   dat <- sitesDataRValues()
   cell <- input$displaySiteData_cell_edit
   
   colName <- switch(cell$col,"Site_Identifier","Site_Name","Address","City","ZipCode","Area_SQFT","Operating_hours")
   query <- paste("UPDATE Sites SET ",colName, "=","'",cell$value,"'"," Where SID=",dat[cell$row,1],sep="" )
   dbSendQuery(dbcnt(),query)
   sitesDataRValues(dbGetQuery(dbcnt(),"select * from Sites"))
   shinyalert("It's Done", "Successfully saved the Data", type = "success")
 })
 
  
})



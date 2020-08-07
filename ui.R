library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(shinyWidgets)
library(RSQLite)
library(shinyalert)
library(plotly)
library(dplyr)
library(shinycustomloader)
library(V8)


jscode <- "shinyjs.refresh = function() { history.go(0); }"
title <-  tags$a(tags$img(src="https://is3-ssl.mzstatic.com/image/thumb/Purple118/v4/21/dd/55/21dd551c-ec03-d318-d6d5-ca640ec2f314/source/512x512bb.jpg",height=50,width=50),"Limestone District School Board")
source("ReqFunctions.R",local=TRUE)$value


shinyUI(fluidPage(
 
  dashboardPage(skin="green",title="Limestone District School Board",
  dashboardHeader(title=title, titleWidth = 400),
  dashboardSidebar(width=400,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Analysis", icon = icon("wrench"), tabName = "analysis",
              startExpanded = FALSE,menuSubItem("Suncor", tabName = "SN_Analysis",icon = icon("oil-can")),
              menuSubItem("Superior Propane", tabName = "SP_Analysis",icon = icon("gripfire"))),
      menuItem("Manage Data", icon = icon("database"), tabName = "manageData",
                       startExpanded = FALSE,menuSubItem("PDF Data", tabName = "pdfData",icon = icon("file-pdf")),
                       menuSubItem("Sites Data", tabName = "sitesData",icon = icon("university"))),
      
    
      menuItem("LDSB Tools", icon = icon("wrench"), tabName = "tools",
              startExpanded = FALSE,menuSubItem("Extract PDF Data", tabName = "pdfExtract",icon = icon("file-pdf")),
              menuSubItem("Weather Report", tabName = "weatherReport",icon = icon("cloud")))
    
      # menuItem("Workshops", icon = icon("th"), tabName = "Workshops",
      #          badgeLabel = "new", badgeColor = "red")
    )
  ),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode),
    useShinyalert(),
    tags$head(tags$style(HTML('
    .main-header a {color:#e6eaf5; font-size:auto;font-weight:bold}
                            '))),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                textOutput("DashboardHeading"),
                
                tags$style(type="text/css", "#DashboardHeading { height: 100px;
                           width: 100%; text-align:center; font-size: 60px;font-weight:bold;color:#111161}")
              ), 
              fluidRow(
                # textOutput("overviewTitle"),
                
                tags$style(type="text/css", "#overviewTitle { height: 50px;
                           width: 100%; text-align:left; font-size: 30px;font-weight:bold;color:#111161}")
              ), 
              fluidRow(
                
                
                box(width=6, title = span( icon("oil-can"), "Suncor"),status = "danger",solidHeader = TRUE,
                   fluidRow(valueBoxOutput("DB_AvgConsp_SN")%>% withLoader(type="html",loader="loader4"),
                    htmlOutput("SN_Summary1"),
                    htmlOutput("SN_Summary2"))
                    
                    
                    ),
                box(width=6, title = span( icon("gripfire"), "Superior Propane"),status = "danger",solidHeader = TRUE,
                   fluidRow(valueBoxOutput("DB_AvgConsp_SP")%>% withLoader(type="html",loader="loader4"),
                            htmlOutput("SP_Summary1"),
                            htmlOutput("SP_Summary2"))
                   )
                    
                ),
              fluidRow(
                htmlOutput("ourSchoolsTitle"),
                
                tags$style(type="text/css", "#ourSchoolsTitle { height: 70px;
                           width: 100%; text-align:center; font-size: 30px;font-weight:bold;color:#111161}")
              ), 
              fluidRow(uiOutput("dashBoardBoxes")%>%withLoader(type="html",loader="loader4"))
              
              
              
      ),
      tabItem(tabName = "SN_Analysis",
              fluidRow(
                textOutput("SN_AnalysisHeading"),
                
                tags$style(type="text/css", "#SN_AnalysisHeading { height: 100px;
                           width: 100%; text-align:center; font-size: 40px;font-weight:bold;}")
                ),
              
              fluidRow(
              
               
                box(width=7, title = "Summary",status = "danger",solidHeader = TRUE,
                    
                    valueBoxOutput("SN_numberOfSchools")%>% withLoader(type="html",loader="loader4"),
                    valueBoxOutput("SN_TotalQuantity"),
                    valueBoxOutput("SN_TotalAmount"),
                    valueBoxOutput("SN_AveragePrice"),
                    valueBoxOutput("SN_kwhEquivalent")
                    
                ),
                
              
                box(width=5,title = "Filter",status = "primary",solidHeader = TRUE, 
                    uiOutput("SN_schoolVar")%>% withLoader(type="html",loader="loader4"),
                    uiOutput("SN_datePicker")
                ),
                 
                downloadButton('SN_generateReport', 'Generate Report',style="color: #fff; background-color: #082863;align:right"),
              downloadButton('SN_DownloadData', 'Download Selected Data',style="color: #fff; background-color: #105741;align:right")),
              actionButton('sendMail','send Mail'),
              fluidRow(plotlyOutput("snLinePlot")%>% withLoader(type="html",loader="loader4")),
              fluidRow(plotlyOutput("snBarPlot")),
              fluidRow(plotlyOutput("snBarPlot2"))
              
      ),
      tabItem(tabName = "SP_Analysis",
              fluidRow(
                textOutput("SP_AnalysisHeading"),
                
                tags$style(type="text/css", "#SP_AnalysisHeading { height: 100px;
                           width: 100%; text-align:center; font-size: 40px;font-weight:bold;}")
              ),
              
              fluidRow(
                
                
                box(width=7, title = "Summary",status = "danger",solidHeader = TRUE,
                    
                    valueBoxOutput("SP_numberOfSchools")%>% withLoader(type="html",loader="loader4"),
                    valueBoxOutput("SP_TotalQuantity"),
                    valueBoxOutput("SP_TotalAmount"),
                    valueBoxOutput("SP_AveragePrice"),
                    valueBoxOutput("SP_kwhEquivalent")
                    
                ),
                
                
                box(width=5,title = "Filter",status = "primary",solidHeader = TRUE, 
                    uiOutput("SP_schoolVar")%>% withLoader(type="html",loader="loader4"),
                    uiOutput("SP_datePicker")
                ),
                
                downloadButton('SP_generateReport', 'Generate Report',style="color: #fff; background-color: #082863;align:right"),
                downloadButton('SP_DownloadData', 'Download Selected Data',style="color: #fff; background-color: #105741;align:right")),
              
              fluidRow(plotlyOutput("spLinePlot")%>% withLoader(type="html",loader="loader4")),
              fluidRow(plotlyOutput("spBarPlot")),
              fluidRow(plotlyOutput("spBarPlot2"))
              
      ),
     
      tabItem(tabName = "weatherReport",
              fluidRow(
                textOutput("weatherReportHeading"),
                tags$style(type="text/css", "#weatherReportHeading { height: 100px;
                           width: 100%; text-align:center; font-size: 40px;font-weight:bold;}")),
              fluidRow( column(8, align="center", offset = 2,
                textInput("stationSearch", "Find weather Station"),
                        radioButtons("intervalBtn", "Interval", choices = c("hour","day","month"), selected = "day",inline = TRUE),
                        radioButtons("provinceBtn", "Province", choices = c("ON","QC","NS","NB","MB","BC","PE","SK","AB","NL"), selected = "ON",inline = TRUE),
                        uiOutput("weatherDatePicker"),
                        actionButton("stationSearchBtn","Search Station"))),
              fluidRow(column(8, align="center", offset = 2, uiOutput("stationNames"))),
              fluidRow(column(8, align="center", offset = 2,downloadButton('generateWeatherReport', 'Download Data',style="color: #fff; background-color: #18191a")))
      ),
     
      
      tabItem(tabName = "pdfExtract",
              
              fluidRow(
                textOutput("pdfHeading"),
                tags$style(type="text/css", "#pdfHeading { height: 100px;
                           width: 100%; text-align:center; font-size: 40px;font-weight:bold;}")),
             
              
              fluidRow(
                column(8, align="center", offset = 2,
              fileInput(
                inputId = "files", 
                label = "Choose PDF Files", 
                multiple = TRUE,
                accept = c('.pdf')
              ))),
               
              fluidRow(
                column(8, align="center", offset = 2,
                actionButton("convrtPDF", "Convert",icon("exchange-alt"),style="color: #fff; background-color: #0c5285"),
                downloadButton('downloadData', 'Download Data',style="color: #fff; background-color: #18191a"),
                # actionButton('saveData', "Save"),
                actionButton("saveData", "Save", icon("paper-plane"), 
                             style="color: #fff; background-color: #048a29"))),
              dataTableOutput('cnvrtPDFToExcel')%>% withLoader(type="html",loader="loader4")
             
      ),
      tabItem(tabName = "pdfData",
              fluidRow(
                textOutput("manageData"),
                tags$style(type="text/css", "#manageData { height: 100px;
                           width: 100%; text-align:center; font-size: 40px;font-weight:bold;}")),
              actionButton("refresh", "Refresh", icon("sync"), 
                           style="color: #fff; background-color: #45a8a2"),
              fluidRow(
                box(width = 12, 
                    splitLayout(
                           textInput("deliveredByTB", "Delivered By"),
                           textInput("invoiceDateTB", "Invoice Date"),
                           textInput("invoiceTotalTB", "Invoice Total"),
                           textInput("schoolNameTB", "School Name"),
                           textInput("orderQuantityTB", "Order Quantity"),
                           textInput("unitPriceTB", "Unit Price"),
                           textInput("kwhEquivalentTB", "KWH Equivalent")
                             )
                      )
                  ),
              fluidRow(
                actionButton('addData', "Add Row",icon("plus-circle"),style="color: #fff; background-color: #048a29"),
                actionButton('deleteData', "Remove Row(s)",icon("minus-circle"),style="color: #fff; background-color: #96090e"),
                actionButton("reset_input_PDF", "Clear",icon("undo"),style="color: #fff; background-color: #0e0163"),
                hr()
              ),
              fluidRow(dataTableOutput('displayData')%>% withLoader(type="html",loader="loader4"))
      ),
      tabItem(tabName = "sitesData",
              fluidRow(
                textOutput("manageSiteData"),
                tags$style(type="text/css", "#manageSiteData { height: 100px;
                           width: 100%; text-align:center; font-size: 40px;font-weight:bold;}")),
              
              fluidRow(
                box(width = 12, 
                    splitLayout(
                      textInput("siteIdentifierTB", "Site Identifier"),
                      textInput("siteNameTB", "Site Name"),
                      textInput("addressTB", "Address"),
                      textInput("cityTB", "City"),
                      textInput("zipCodeTB", "Zip Code"),
                      textInput("areaTB", "Area(in sqft)"),
                      textInput("openingHoursTB", "Hours of Operation")
                    )
                )
              ),
              fluidRow(
                actionButton('addSiteData', "Add Row",icon("plus-circle"),style="color: #fff; background-color: #048a29"),
                actionButton('deleteSiteData', "Remove Row(s)",icon("minus-circle"),style="color: #fff; background-color: #96090e"),
                actionButton("reset_input_Sites", "Clear",icon("undo"),style="color: #fff; background-color: #0e0163"),
                hr()
              ),
              fluidRow(dataTableOutput('displaySiteData')%>% withLoader(type="html",loader="loader4"))
              
              
      )
    )
    
  )
)))
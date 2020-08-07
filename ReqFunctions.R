library(pdftools)
# install.packages("magick")

library(tesseract)
library(stringr)
library(readxl)
library(lubridate)
library(weathercan)


getPercentResult <- function(Per)
{
  if(Per>0)
  {
    resText <-reactive((paste0("<i>","<b>","<font color='#2fb534' weight=bold>","Increased by ", round(abs(Per)*100,2),"%", "</font>","</b>","</i>")))
  }
  else
  {
    resText <-reactive((paste0("<i>","<b>","<font color='red'>","Decreased by ", round(abs(Per)*100,2),"%","</font>","</b>","</i>")))
  }
  
  return(resText())
}





getTotalConsumption <-  function(data,type)
{
  data$orderQuantity<-as.numeric(gsub(",", "", as.character(data$orderQuantity)))
  # val  <-data%>% filter(deliveredBy==type) %>% summarise(B=sum(orderQuantity,na.rm = TRUE))
  val  <-data%>% subset(deliveredBy==type) %>% summarise(B=sum(orderQuantity,na.rm = TRUE))
  return(val)
}

getSchoolName <- function(sites,fileName)
{
  
  siteIdentifier <- str_trim(str_split(fileName,"_")[[1]][2])
  schoolName <- sites[which(sites$Site_Identifier == siteIdentifier),3 ]
  if(length(schoolName)==0)
  {
    return("error in file name")
  }
  else
  {
   
    return(schoolName)
  }
}

getKwhEqui <- function(quantity,type)
{
  if(type=="SN"&length(quantity)>0)
  {
    return(10*quantity)
  }
 else if(type=="SP"&length(quantity)>0)
  {
   #1Gallon of propane = 3.79Lit, 1Gallon of propane = 27kwh
    return(7.124*quantity)
 }
  else
  {
    return(0)
  }
}

getPdfOutput <- function(inFiles,len,sites)
{
  
  
  convert = list()
  fileName = list()
  
  for(nr in 1:len){
    convert[[nr]] <- pdf_ocr_text(inFiles[[nr, 'datapath']],pages = 1,dpi=500)
    fileName[[nr]] <- inFiles[[nr, 'name']]
  }
  
  pdfdata <- unlist(convert)
  fileNamedata <- unlist(fileName)
  finalX <- NULL
  mydf <- data.frame()
  
  
  for(i in 1:length(pdfdata))
  {

    finalX <- strsplit(pdfdata[i],"\n")[[1]]
    docType <- str_trim(str_split(fileNamedata[i],"_")[[1]][1])
    
    schoolName <- getSchoolName(sites,fileNamedata[i])
    
    if(docType=="SP")
    {
      mydf[i,1] <- "Superior Propane"
      
      x <- str_extract(finalX,'\\d{1,2}\\/\\d{1,2}\\/\\d{4}')
      mydf[i,2] <- format(mdy(x[!is.na(x)][1]),"%d/%m/%Y")
      
      
      orderIndex <-  which(grepl(" BULK PROPANE|Tank Rental|LATE FEE",finalX))
      
      
      orderDetails <- str_extract_all(finalX[orderIndex],'\\d{1,2}\\,\\d{3}\\.\\d{2}|\\d+\\.\\d{1,2}|\\d{1}\\.\\d{4}')
      
      r <-  grepl("SERVICE CONTRACT",finalX[1:5])
      
    
      if(is.na(match(TRUE,r)))
      {
        mydf[i,3] <-  as.numeric(gsub(",", "", as.character(orderDetails[[1]][3])))
        
        mydf[i,4] <-  schoolName
        
        mydf[i,5] <- as.numeric(orderDetails[[1]][1])
        
        mydf[i,6] <- as.numeric(orderDetails[[1]][2])
        
        mydf[i,7] <- getKwhEqui(as.numeric(orderDetails[[1]][1]),"SP")
      }
      else
      {
        yds <- str_extract(finalX,'\\d{1,2}\\,\\d{3}\\.\\d{2}|\\d+\\.\\d{2}')
        mydf[i,3] <- as.numeric(yds[!is.na(yds)][1])
        
        mydf[i,4] <-  schoolName
        
        
        mydf[i,5] <- 0
        
        mydf[i,6] <- 0
        
        mydf[i,7] <- 0
      }
    
      if(sum(is.na(mydf[i,]))>=4)
      {
        mydf[i,1]<- "please check"
        mydf[i,2]<- "file format of"
        
        mydf[i,4]<- fileNamedata[i]
        
      }
      
      
    
    }
    else if(docType=="SN")
    {
      
      mydf[i,1] <- "Suncor"
      
      oQ  <- str_extract(finalX,'\\d{1,2}\\,\\d{3}\\.\\d{3}|\\d+\\.\\d{3}')
      orderQuantity <-  oQ[!is.na(oQ)][1]
      orderIndex <- match(orderQuantity,oQ)
      
      invDate <- str_extract(finalX,'\\w+\\s\\d{2}\\,\\s\\d{4}')
      mydf[i,2] <-  format(mdy(invDate[!is.na(invDate)][1]),"%d/%m/%Y")
      
     
        invTotal <- str_extract_all(finalX[orderIndex],'\\d{1,2}\\,\\d{3}\\.\\d{2}|\\d+\\.\\d{2}\\"')[[1]][2]
        
      mydf[i,3] <- as.numeric(gsub(",", "", as.character(invTotal)))
        
        # addressIndex <- match("Sold to: Shipped to:",finalX)
      mydf[i,4] <-  schoolName
        
      mydf[i,5] <-   as.numeric(gsub(",", "", as.character(orderQuantity)))
     
        
        unitPriceElement <- str_extract_all(finalX[orderIndex],'\\d{1}\\.\\d{4}')[[1]]
        if(length(unitPriceElement)>0)
        {
          mydf[i,6] <- as.numeric(unitPriceElement[length(unitPriceElement)])
        }
        else
        {
          mydf[i,6] <- NA
        }
       
      
     
        mydf[i,7] <- getKwhEqui( as.numeric(gsub(",", "", as.character(orderQuantity))),"SN")
      
        if(sum(is.na(mydf[i,]))>=4)
        {
          mydf[i,1]<- "please check"
          mydf[i,2]<- "file format of"
        
          mydf[i,4]<- fileNamedata[i]
          
          
          
        }

    }

  }
  
  colnames(mydf)<- c("deliveredBy","invoiceDate","invoiceTotal","schoolName","orderQuantity","unitPrice","kwhEquivalent")
  fileNamedata[1]
  return(mydf)
}

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(forecast)
library(tidyr)
library(shinyWidgets)

data<-read.csv("Final-50-stocks.csv",stringsAsFactors=FALSE)
data$DATE<-as.Date(data$DATE,format="%Y-%m-%d")

findFirstNumeric<-function(column)
{
  for(value in column) 
  {
    if(is.numeric(value) && !is.na(value)) 
    {
      return(value)
    }
  }
}

growthRate<-function(data) 
{
  result<-data.frame(Column=character(),GrowthRate=numeric(),stringsAsFactors=FALSE)
  
  for(colName in colnames(data)) 
  {
    if(colName!="DATE") 
    {
      firstNumericValue<-findFirstNumeric(data[[colName]])
      maxVal<-(data[[nrow(data),colName]])
      growth_rate<-((maxVal/firstNumericValue)*100)-100
      result<-rbind(result, data.frame(Column=colName,GrowthRate_in_percent=growth_rate))
    }
  }
  
  return(result)
}

getTop10<-function(df) {
  df%>% 
  select(where(is.numeric))%>%
  summarise(across(everything(),~sort(.x,decreasing=TRUE)[1:10]))
}



ui<-dashboardPage(
  dashboardHeader(title="Stock Price Analysis & Prediction"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",tabName="dashboard",icon=icon("dashboard")),
      menuItem("Growth Rate",tabName="growthRates",icon=icon("chart-bar")),
      menuItem("Top 10 stocks\n(based on growth rate)",tabName="top10Stocks",icon=icon("chart-line")),
      menuItem("Statistics",tabName="statistics",icon=icon("calculator")),
      menuItem("Tech Sector",tabName="tech",icon=icon("laptop")),
      menuItem("Banking Sector",tabName="banking",icon=icon("university")),
      menuItem("Petroleum Sector",tabName="petroleum",icon=icon("gas-pump")),
      menuItem("Automobile Sector",tabName="automobile",icon=icon("car")),
      menuItem("Financial Sector",tabName="financial",icon=icon("coins")),
      menuItem("Steel Sector",tabName="steel",icon=icon("industry")),
      menuItem("Cement Sector",tabName="cement",icon=icon("truck")),
      menuItem("Large Scale Company Sector",tabName="largeScale",icon=icon("boxes")),
      menuItem("Pharma Sector",tabName="pharma",icon=icon("medkit")),
      menuItem("Food Sector",tabName="food",icon=icon("utensils")),
      menuItem("Government Sector",tabName="government",icon=icon("university")),
      menuItem("Chemical Sector",tabName="chemical",icon=icon("flask")),
      menuItem("Power Sector",tabName="power",icon=icon("bolt")),
      menuItem("Industry Sector",tabName="industry",icon=icon("industry")),
      menuItem("Predictions",tabName="predictions",icon=icon("chart-line")),
      menuItem("Download Data",tabName="download",icon=icon("download"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName="dashboard",
              fluidRow(
                box(title="Select Date Range",width=6,dateRangeInput("dateRange","Date Range:",start=min(data$DATE,na.rm=TRUE),end=max(data$DATE,na.rm=TRUE))),
                
                box(title="Select Company",width=6,pickerInput("companyPicker","Select Companies:",choices=colnames(data)[-1],selected=c("TCS","WIPRO","RELIANCE"),multiple=TRUE))),
              
              fluidRow(
                box(title="Stock Prices",width=12,plotOutput("pricePlot"))
              )
      ),
      
      tabItem(tabName="growthRates",
              fluidRow(
                box(title="Growth Rates by Companies",width=12,plotOutput("growthPlot")),
                box(title="Growth Rate Data",width=12,tableOutput("growthTable"))
              )
      ),
      
      tabItem(tabName="top10Stocks",
              fluidRow(
                box(title="Top 10 Stocks based on growth rate",width=12,tableOutput("top10Table"))
              )
      ),
      
      tabItem(tabName="tech",
              fluidRow(
                box(title="Tech Sector Stock Prices",width=12,plotOutput("techPlot"))
              )
      ),
      
      
      tabItem(tabName="banking",
              fluidRow(
                box(title="Banking Sector Stock Prices",width=12,plotOutput("bankPlot"))
              )
      ),
      
      
      tabItem(tabName="petroleum",
              fluidRow(
                box(title="petroleum Sector Stock Prices",width=12,plotOutput("petrolPlot"))
              )
      ),
      
      tabItem(tabName="automobile",
              fluidRow(
                box(title="Automobile Sector Stock Prices",width=12,plotOutput("autoPlot"))
              )
      ),
      
      tabItem(tabName="financial",
              fluidRow(
                box(title="Financial Sector Stock Prices",width=12,plotOutput("financePlot"))
              )
      ),

      tabItem(tabName="steel",
              fluidRow(
                box(title="Steel Sector Stock Prices",width=12,plotOutput("steelPlot"))
              )
      ),
      
      
      tabItem(tabName="cement",
              fluidRow(
                box(title="cement Sector Stock Prices",width=12,plotOutput("cementPlot"))
              )
      ),
      

      tabItem(tabName="pharma",
              fluidRow(
                box(title="Pharma Sector Stock Prices",width=12,plotOutput("pharmaPlot"))
              )
      ),
      
      tabItem(tabName="food",
              fluidRow(
                box(title="Food Sector Stock Prices",width=12,plotOutput("foodPlot"))
              )
      ),
      
      tabItem(tabName="government",
              fluidRow(
                box(title="Government Sector Stock Prices",width=12,plotOutput("governmentPlot"))
              )
      ),
      
      tabItem(tabName="chemical",
              fluidRow(
                box(title="Chemical Sector Stock Prices",width=12,plotOutput("chemicalPlot"))
              )
      ),
      
      tabItem(tabName="power",
              fluidRow(
                box(title="Power Sector Stock Prices",width=12,plotOutput("powerPlot"))
              )
      ),
      
      tabItem(tabName="largeScale",
              fluidRow(
                box(title="Large Scale Company Sector Stock Prices",width=12,plotOutput("largeScalePlot"))
              )
      ),
      
      tabItem(tabName="industry",
              fluidRow(
                box(title="Industrial Sector Stock Prices",width=12,plotOutput("industryPlot"))
              )
      ),
      

      tabItem(tabName="predictions",
              fluidRow(
                box(title="Predicted Stock Prices",width=12,selectInput("predictionCompany","Select Company:",choices=colnames(data)[-1]),plotOutput("predictionPlot")),
                
                box(title="Winning/Losing Stocks",width=12,verbatimTextOutput("winnerLoserOutput"))
              )
      ),
      
      tabItem(tabName="statistics",
              fluidRow(
                box(title="Select Company",width=6,pickerInput("statCompanyPicker","Select Company:",choices=colnames(data)[-1],selected=colnames(data)[2])),
                box(title="Statistics",width=6,tableOutput("statTable"))
              )
      ),
      
      tabItem(tabName="download",
              fluidRow(
                box(title="Download Data",width=6,
                    downloadButton("downloadCSV","Download Filtered Data")
                ),
                box(title="Download Single Company Analysis",width=6,
                    pickerInput("analysisCompanyPicker","Select Company: ",choices=colnames(data)[-1]),
                    downloadButton("downloadAnalysis","Download Analysis")
                ),
                box(title="Download All Companies Analysis",width=6,
                    downloadButton("downloadAllAnalysis","Download All Companies Analysis")
                )
              )
      )
    )
  )
)


server<-function(input,output){
  
  filteredData<-reactive({
    req(input$dateRange)
    data %>%
      filter(DATE>=input$dateRange[1] & DATE<=input$dateRange[2])%>%
      select(DATE,all_of(input$companyPicker))%>%
      drop_na()
  })
  
  
  output$pricePlot<-renderPlot({
    req(input$companyPicker)
    df<-filteredData()%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")
    
    ggplot(df,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Stock Price Trends",x="Date",y="Price")+theme_minimal()+theme(legend.position = "bottom")
  })
  
  output$growthPlot<-renderPlot({
    growthData<-growthRate(data)
    vec<-vector()
    i=1
    for(c in growthData$Column)
    {
      vec[i]<-paste(c,"(",round(growthData$GrowthRate_in_percent[i],digits=2),"%)",sep=" ")
      i=i+1
    }
    ggplot(growthData,aes(x=Column,y=GrowthRate_in_percent))+geom_bar(stat="identity",fill="lightgreen")+geom_text(aes(label=vec),hjust=-0.1,angle=90,size=3)+labs(title="Growth Rate by Companies",x="Companies",y="Growth Rate (%)")+theme_minimal()+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
  })
  
  output$top10Table<-renderTable({
    top10Stocks<-growthRate(data)%>%
    arrange(desc(GrowthRate_in_percent))%>%
    head(10)
    top10Stocks
  })

  output$techPlot<-renderPlot({
    dataTech<-data%>%
      select(DATE,HCLTECH,INFY,TCS,WIPRO,TECHM)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataTech,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Tech Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  
  
  output$bankPlot<-renderPlot({
    dataBank<-data%>%
      select(DATE,AXISBANK,SBIN,HDFCBANK,ICICIBANK,INDUSBANK,KOTAKBANK)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataBank,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Banking Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  
  
  output$petrolPlot<-renderPlot({
    dataPetrol<-data%>%
      select(DATE,BPCL,IOC)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataPetrol,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Petroleum Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  
  output$autoPlot<-renderPlot({
    dataAuto<-data%>%
      select(DATE,EICHERMOTOR,HEROMOTOCO,MARUTI,TATAMOTORS,BAJAJ.AUTO)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataAuto,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Automobile Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  
  output$financePlot<-renderPlot({
    dataFinance<-data%>%
      select(DATE,BAJAJFINSERV,BAJAJFINANCE)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataFinance,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Financial Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  
  output$largeScalePlot<-renderPlot({
    datalargeScale<-data%>%
      select(DATE,ITC,HINDUNILVR)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(datalargeScale,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Large Scale Company Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  
  
  output$steelPlot<-renderPlot({
    dataSteel<-data%>%
      select(DATE,TATASTEEL,JSWSTEEL)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataSteel,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Steel Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  
  
  output$cementPlot<-renderPlot({
    dataCement<-data%>%
      select(DATE,SHREECEM,ULTRACEMO,GRASIM)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataCement,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Cement Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  

  output$pharmaPlot<-renderPlot({
    dataPharma<-data%>%
      select(DATE,DRREDDYS,CIPLA,SUNPHARMA)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataPharma,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Pharma Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  
  output$foodPlot<-renderPlot({
    dataFood<-data%>%
      select(DATE,BRITANNIA,NESTLEIND)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataFood,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Food Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  
  output$governmentPlot<-renderPlot({
    dataGovernment<-data%>%
      select(DATE,COALINDIA,NTPC,ONGC)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataGovernment,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Government Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  
  output$chemicalPlot<-renderPlot({
    dataChemical<-data%>%
      select(DATE,UPL,M.M)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataChemical,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Chemical Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  
  output$powerPlot<-renderPlot({
    dataPower<-data%>%
      select(DATE,ADANIPORTS,HINDALCO,POWERGRID)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataPower,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Power Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  
  output$industryPlot<-renderPlot({
    dataIndustry<-data%>%
      select(DATE,RELIANCE,LT,TITAN,ADANIPORTS,BHARTIARTL,ASIANPAINT,M.M)%>%
      pivot_longer(cols=-DATE,names_to="Company",values_to="Value")%>%
      filter(!is.na(Value))
    
    ggplot(dataIndustry,aes(x=DATE,y=Value,color=Company,group=Company))+geom_line()+labs(title="Industrial Sector Stock Prices",x="Date",y="Price")+theme_minimal()
  })
  

  output$predictionPlot<-renderPlot({
    req(input$predictionCompany)
    company<-input$predictionCompany
    companyData<-data%>%
      select(DATE,!!sym(company))%>%
      filter(!is.na(!!sym(company)))
    
    if (nrow(companyData)<2){
      return(NULL)
    }
    
    etsModel<-ets(companyData[[2]])
    forecastedData<-forecast(etsModel,h=30)
    
    forecastDF<-data.frame(DATE=seq(max(companyData$DATE)+1,by="day",length.out=30),Value=forecastedData$mean)
    
    ggplot()+geom_line(data=companyData,aes(x=DATE,y=!!sym(company)),color='blue',size=1)+geom_line(data=forecastDF,aes(x=DATE,y=Value),color='red',linetype="dashed")+labs(title=paste(company,"Stock Price Forecast"),x="Date",y="Price")+theme_minimal()
  })
  

  output$winnerLoserOutput<-renderPrint({
    recentData<-data%>%
      filter(DATE==max(DATE))
    
    if(nrow(recentData)==0){
      return("No recent data available.")
    }
    
    predictedReturns<-sapply(colnames(recentData)[-1],function(company){
      companyData<-data%>%
        select(DATE,!!sym(company))%>%
        filter(!is.na(!!sym(company)))
      
      if (nrow(companyData)<2){
        return(NA)
      }
      
      etsModel<-ets(companyData[[2]])
      nextDayForecast<-forecast(etsModel,h=1)
      return(as.numeric(nextDayForecast$mean))
    })
    
    predictedReturns<-sort(predictedReturns,decreasing=TRUE,na.last=NA)
    
    winner<-names(predictedReturns[1])
    loser<-names(predictedReturns[length(predictedReturns)])
    
    cat(paste("Predicted Winner: ",winner,"with the highest predicted return."),"\n",paste("Predicted Loser: ",loser,"with the lowest predicted return."))
  })
  
  output$statTable<-renderTable({
    req(input$statCompanyPicker)
    selected_company<-input$statCompanyPicker
    stats_data<-data%>%
      filter(DATE>=input$dateRange[1] & DATE<=input$dateRange[2])%>%
      select(DATE,!!sym(selected_company))%>%
      drop_na()
    
    stats_summary<-data.frame(
      Statistic=c("Mean","Median","Variance","Standard Deviation"),
      Value=c(
        mean(stats_data[[2]],na.rm=TRUE),
        median(stats_data[[2]],na.rm=TRUE),
        var(stats_data[[2]],na.rm=TRUE),
        sd(stats_data[[2]],na.rm=TRUE)
      )
    )
    
    stats_summary
  })
  
  
  output$downloadCSV<-downloadHandler(
    filename=function(){
      paste("filtered_data_",Sys.Date(),".csv",sep="")
    },
    content=function(file){
      write.csv(filteredData(),file,row.names=FALSE)
    }
  )
  
  output$downloadAnalysis <- downloadHandler(
    filename=function() {
      paste(input$analysisCompanyPicker,"_analysis_",Sys.Date(),".txt",sep="")
    },
    content=function(file){
      req(input$analysisCompanyPicker)
      selected_company<-input$analysisCompanyPicker
      
      company_data<-data %>%
        filter(DATE>=input$dateRange[1] & DATE<=input$dateRange[2])%>%
        select(DATE,!!sym(selected_company)) %>%
        drop_na()
      
      analysis_text<-paste(
        "Company: ",selected_company,"\n",
        "Mean: ",mean(company_data[[2]],na.rm=TRUE),"\n",
        "Median: ",median(company_data[[2]],na.rm=TRUE),"\n",
        "Variance: ",var(company_data[[2]],na.rm=TRUE),"\n",
        "Standard Deviation: ",sd(company_data[[2]],na.rm=TRUE),"\n",
        sep=""
      )
      
      writeLines(analysis_text,file)
    }
  )
  
  output$downloadAllAnalysis<-downloadHandler(
    filename=function(){
      paste("all_companies_analysis_",Sys.Date(),".txt",sep="")
    },
    content=function(file){
      all_analysis<-""
      
      for (company in colnames(data)[-1])
      {
        company_data<-data%>%
          filter(DATE>=input$dateRange[1] & DATE<=input$dateRange[2])%>%
          select(DATE,!!sym(company))%>%
          drop_na()
        
        analysis_text<-paste(
          "Company:",company,"\n",
          "Mean:",mean(company_data[[2]],na.rm=TRUE), "\n",
          "Median:",median(company_data[[2]],na.rm=TRUE), "\n",
          "Variance:",var(company_data[[2]],na.rm=TRUE), "\n",
          "Standard Deviation:",sd(company_data[[2]],na.rm=TRUE), "\n\n",
          sep=""
        )
        
        all_analysis<-paste(all_analysis,analysis_text,sep="")
      }
      
      writeLines(all_analysis, file)
    }
  )

}

shinyApp(ui=ui,server=server)

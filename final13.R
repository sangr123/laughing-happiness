library(leaflet)
library(tidyverse)
library(shiny)


 # Define UI for application that draws a histogram
ui <- fluidPage(
  
  "청년 친화 강소기업", 
  
  
  # Application title
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId="업종","업종:",
                  choices = c("전체", "정보통신업","도매 및 소매업","전문, 과학 및 기술 서비스업","제조업","기타")
      ),
      selectInput(inputId="BEST", "BEST 선정 분야:",
                  choices = c("전체","임금","일생활균형","고용안정","임금&일생활균형","임금&고용안정","일생활균형&고용안정","임금&일생활균형&고용안정")
      ),
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("2021",leafletOutput("map1", height=700) , tableOutput("list21") ),
        
        tabPanel("2022",leafletOutput("map2", height=700) , tableOutput("list22") ),
        
        tabPanel("21-22 연속 선정 기업",leafletOutput("map3", height=700), tableOutput("list2")),
        
        tabPanel("일자리카페",leafletOutput("map4", height=700), tableOutput("list4")),        
        
        tabPanel("의류 대여",leafletOutput("map5", height=700), tableOutput("list5")),
        
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  sb21 <- read.csv('C:/temp/list21.csv',fileEncoding = "euc-kr")
  sb21a <- read.csv('C:/temp/list212.csv',fileEncoding = "euc-kr")
  sb22 <- read.csv('C:/temp/list22.csv',fileEncoding = "euc-kr")
  sb22a <- read.csv('C:/temp/list222.csv',fileEncoding = "euc-kr")
  sb2 <- read.csv('C:/temp/0000.csv',fileEncoding = "euc-kr")
  sb2a <- read.csv('C:/temp/rank.list.csv',fileEncoding = "euc-kr")
  sb4 <- read.csv('C:/temp/cafe.csv',fileEncoding = "euc-kr")
  sb4a <- read.csv('C:/temp/cafe2.csv',fileEncoding = "euc-kr")
  sb5 <- read.csv('C:/temp/clothes.csv',fileEncoding = "euc-kr")
  sb5a <- read.csv('C:/temp/clothes2.csv',fileEncoding = "euc-kr")
  dfsb21<-as.data.frame(sb21)
  dfsb22<-as.data.frame(sb22)
  
  
  
  
  output$map1 <- renderLeaflet({
    
    
    #정보통신업
    if(input$업종=="정보통신업"&input$BEST=="임금"){
      leaflet(subset(sb21, 업종 == '정보통신업'& 임금 == 1 )) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '정보통신업'& 임금 == 1 )
                          ,lng=subset(sb21, 업종 == '정보통신업'& 임금 == 1 )$경도
                          , lat=subset(sb21, 업종 == '정보통신업'& 임금 == 1 )$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '정보통신업'& 임금 == 1 )$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '정보통신업'& 임금 == 1 )$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '정보통신업'& 임금 == 1 )$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="일생활균형"){
      leaflet(subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 )
                          ,lng=subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 )$경도
                          , lat=subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 )$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 )$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 )$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 )$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="고용안정"){
      leaflet(subset(sb21, 업종 == '정보통신업'& 고용안정 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '정보통신업'& 고용안정 == 1)
                          ,lng=subset(sb21, 업종 == '정보통신업'& 고용안정 == 1)$경도
                          , lat=subset(sb21, 업종 == '정보통신업'& 고용안정 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '정보통신업'& 고용안정 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '정보통신업'& 고용안정 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '정보통신업'& 고용안정 == 1)$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)
                          ,lng=subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)$경도
                          , lat=subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)$기업규모))
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)
                          ,lng=subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)$경도
                          , lat=subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)
                          ,lng=subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)$경도
                          , lat=subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb21, 업종 == '정보통신업'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '정보통신업'& 비고==3)
                          ,lng=subset(sb21, 업종 == '정보통신업'& 비고==3)$경도
                          , lat=subset(sb21, 업종 == '정보통신업'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '정보통신업'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '정보통신업'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '정보통신업'& 비고==3)$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="전체"){
      leaflet(subset(sb21, 업종 == '정보통신업')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '정보통신업')
                          ,lng=subset(sb21, 업종 == '정보통신업')$경도
                          , lat=subset(sb21, 업종 == '정보통신업')$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '정보통신업')$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '정보통신업')$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '정보통신업')$기업규모))
      
      
      
      #도매 및 소매업
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금"){
      leaflet(subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1)
                          ,lng=subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1)$경도
                          , lat=subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1)$기업규모))
      
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="일생활균형"){
      leaflet(subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1)
                          ,lng=subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1)$경도
                          , lat=subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="고용안정"){
      leaflet(subset(sb21, 업종 == '도매 및 소매업'& 고용안정 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '도매 및 소매업'& 고용안정 == 1)
                          ,lng=subset(sb21, 업종 == '도매 및 소매업'& 고용안정 == 1)$경도
                          , lat=subset(sb21, 업종 == '도매 및 소매업'& 고용안정 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '도매 및 소매업'& 고용안정 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '도매 및 소매업'& 고용안정 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '도매 및 소매업'& 고용안정 == 1)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)
                          ,lng=subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)$경도
                          , lat=subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)
                          ,lng=subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)$경도
                          , lat=subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)
                          ,lng=subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)$경도
                          , lat=subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb21, 업종 == '도매 및 소매업'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '도매 및 소매업'& 비고==3)
                          ,lng=subset(sb21, 업종 == '도매 및 소매업'& 비고==3)$경도
                          , lat=subset(sb21, 업종 == '도매 및 소매업'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '도매 및 소매업'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '도매 및 소매업'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '도매 및 소매업'& 비고==3)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="전체"){
      leaflet(subset(sb21, 업종 == '도매 및 소매업')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '도매 및 소매업')
                          ,lng=subset(sb21, 업종 == '도매 및 소매업')$경도
                          , lat=subset(sb21, 업종 == '도매 및 소매업')$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '도매 및 소매업')$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '도매 및 소매업')$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '도매 및 소매업')$기업규모))
      
      
      #전문, 과학 및 기술 서비스업 
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금"){
      leaflet(subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)
                          ,lng=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)$경도
                          , lat=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="일생활균형"){
      leaflet(subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)
                          ,lng=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)$경도
                          , lat=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="고용안정"){
      leaflet(subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)
                          ,lng=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)$경도
                          , lat=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)
                          ,lng=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)$경도
                          , lat=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)
                          ,lng=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)$경도
                          , lat=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)
                          ,lng=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)$경도
                          , lat=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)
                          ,lng=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$경도
                          , lat=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="전체"){
      leaflet(subset(sb21, 업종 == '전문, 과학 및 기술 서비스업')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '전문, 과학 및 기술 서비스업')
                          ,lng=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업')$경도
                          , lat=subset(sb21, 업종 == '전문, 과학 및 기술 서비스업')$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업')$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업')$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '전문, 과학 및 기술 서비스업')$기업규모))
      
      
      #제조업   
    }else if(input$업종=="제조업"&input$BEST=="임금"){
      leaflet(subset(sb21, 업종 == '제조업'& 임금 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '제조업'& 임금 == 1)
                          ,lng=subset(sb21, 업종 == '제조업'& 임금 == 1)$경도
                          , lat=subset(sb21, 업종 == '제조업'& 임금 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '제조업'& 임금 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '제조업'& 임금 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '제조업'& 임금 == 1)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="일생활균형"){
      leaflet(subset(sb21, 업종 == '제조업'& 일생활균형 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '제조업'& 일생활균형 == 1)
                          ,lng=subset(sb21, 업종 == '제조업'& 일생활균형 == 1)$경도
                          , lat=subset(sb21, 업종 == '제조업'& 일생활균형 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '제조업'& 일생활균형 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '제조업'& 일생활균형 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '제조업'& 일생활균형 == 1)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="고용안정"){
      leaflet(subset(sb21, 업종 == '제조업'& 고용안정 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '제조업'& 고용안정 == 1)
                          ,lng=subset(sb21, 업종 == '제조업'& 고용안정 == 1)$경도
                          , lat=subset(sb21, 업종 == '제조업'& 고용안정 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '제조업'& 고용안정 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '제조업'& 고용안정 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '제조업'& 고용안정 == 1)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb21, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)
                          ,lng=subset(sb21, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)$경도
                          , lat=subset(sb21, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb21, 업종 == '제조업'& 임금 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '제조업'& 임금 == 1 & 고용안정==1)
                          ,lng=subset(sb21, 업종 == '제조업'& 임금 == 1 & 고용안정==1)$경도
                          , lat=subset(sb21, 업종 == '제조업'& 임금 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '제조업'& 임금 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '제조업'& 임금 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '제조업'& 임금 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb21, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)
                          ,lng=subset(sb21, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)$경도
                          , lat=subset(sb21, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb21, 업종 == '제조업'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '제조업'& 비고==3)
                          ,lng=subset(sb21, 업종 == '제조업'& 비고==3)$경도
                          , lat=subset(sb21, 업종 == '제조업'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '제조업'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '제조업'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '제조업'& 비고==3)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="전체"){
      leaflet(subset(sb21, 업종 == '제조업')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '제조업')
                          ,lng=subset(sb21, 업종 == '제조업')$경도
                          , lat=subset(sb21, 업종 == '제조업')$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '제조업')$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '제조업')$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '제조업')$기업규모))
      
      
      #기타    
    }else if(input$업종=="기타"&input$BEST=="임금"){
      leaflet(subset(sb21, 업종 == '기타'& 임금 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '기타'& 임금 == 1)
                          ,lng=subset(sb21, 업종 == '기타'& 임금 == 1)$경도
                          , lat=subset(sb21, 업종 == '기타'& 임금 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '기타'& 임금 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '기타'& 임금 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '기타'& 임금 == 1)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="일생활균형"){
      leaflet(subset(sb21, 업종 == '기타'& 일생활균형 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '기타'& 일생활균형 == 1)
                          ,lng=subset(sb21, 업종 == '기타'& 일생활균형 == 1)$경도
                          , lat=subset(sb21, 업종 == '기타'& 일생활균형 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '기타'& 일생활균형 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '기타'& 일생활균형 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '기타'& 일생활균형 == 1)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="고용안정"){
      leaflet(subset(sb21, 업종 == '기타'& 고용안정 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '기타'& 고용안정 == 1)
                          ,lng=subset(sb21, 업종 == '기타'& 고용안정 == 1)$경도
                          , lat=subset(sb21, 업종 == '기타'& 고용안정 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '기타'& 고용안정 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '기타'& 고용안정 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '기타'& 고용안정 == 1)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb21, 업종 == '기타'& 임금 == 1 & 일생활균형==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '기타'& 임금 == 1 & 일생활균형==1)
                          ,lng=subset(sb21, 업종 == '기타'& 임금 == 1 & 일생활균형==1)$경도
                          , lat=subset(sb21, 업종 == '기타'& 임금 == 1 & 일생활균형==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '기타'& 임금 == 1 & 일생활균형==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '기타'& 임금 == 1 & 일생활균형==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '기타'& 임금 == 1 & 일생활균형==1)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb21, 업종 == '기타'& 임금 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '기타'& 임금 == 1 & 고용안정==1)
                          ,lng=subset(sb21, 업종 == '기타'& 임금 == 1 & 고용안정==1)$경도
                          , lat=subset(sb21, 업종 == '기타'& 임금 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '기타'& 임금 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '기타'& 임금 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '기타'& 임금 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb21, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)
                          ,lng=subset(sb21, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)$경도
                          , lat=subset(sb21, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb21, 업종 == '기타'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '기타'& 비고==3)
                          ,lng=subset(sb21, 업종 == '기타'& 비고==3)$경도
                          , lat=subset(sb21, 업종 == '기타'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '기타'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '기타'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '기타'& 비고==3)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="전체"){
      leaflet(subset(sb21, 업종 == '기타')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21, 업종 == '기타')
                          ,lng=subset(sb21, 업종 == '기타')$경도
                          , lat=subset(sb21, 업종 == '기타')$위도
                          , popup=paste0("사업장명: ", subset(sb21, 업종 == '기타')$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '기타')$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '기타')$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="전체"){
      leaflet(sb21) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = sb21
                          ,lng=sb21$경도
                          , lat=sb21$위도
                          , popup=paste0("사업장명: ", sb21$사업장명
                                         , "<br/>소재지: ", sb21$소재지
                                         , "<br/>기업규모: ", sb21$기업규모))
      
      #전체
    }else if(input$업종=="전체"&input$BEST=="임금"){
      leaflet(subset(sb21,  임금 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21,  임금 == 1)
                          ,lng=subset(sb21,  임금 == 1)$경도
                          , lat=subset(sb21,  임금 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21,  임금 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21,  임금 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21,  임금 == 1)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="일생활균형"){
      leaflet(subset(sb21,  일생활균형 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21,  일생활균형 == 1)
                          ,lng=subset(sb21,  일생활균형 == 1)$경도
                          , lat=subset(sb21,  일생활균형 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21,  일생활균형 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21,  일생활균형 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21,  일생활균형 == 1)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="고용안정"){
      leaflet(subset(sb21,  고용안정 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21,  고용안정 == 1)
                          ,lng=subset(sb21,  고용안정 == 1)$경도
                          , lat=subset(sb21,  고용안정 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb21,  고용안정 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb21,  고용안정 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb21,  고용안정 == 1)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb21,  임금 == 1 & 일생활균형==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21,  임금 == 1 & 일생활균형==1)
                          ,lng=subset(sb21,  임금 == 1 & 일생활균형==1)$경도
                          , lat=subset(sb21,  임금 == 1 & 일생활균형==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21,  임금 == 1 & 일생활균형==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21,  임금 == 1 & 일생활균형==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21,  임금 == 1 & 일생활균형==1)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb21,  임금 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21,  임금 == 1 & 고용안정==1)
                          ,lng=subset(sb21,  임금 == 1 & 고용안정==1)$경도
                          , lat=subset(sb21,  임금 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21,  임금 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21,  임금 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 임금 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb21, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21,  일생활균형 == 1 & 고용안정==1)
                          ,lng=subset(sb21,  일생활균형 == 1 & 고용안정==1)$경도
                          , lat=subset(sb21,  일생활균형 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb21,  일생활균형 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb21,  일생활균형 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb21,  일생활균형 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb21,  비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb21,  비고==3)
                          ,lng=subset(sb21,  비고==3)$경도
                          , lat=subset(sb21,  비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb21,  비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb21, 업종 == '기타'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb21, 업종 == '기타'& 비고==3)$기업규모))
      
      
      
    }
    
  })
  
  
  
  output$map2 <- renderLeaflet({
    
    
    #정보통신업
    if(input$업종=="정보통신업"&input$BEST=="임금"){
      leaflet(subset(sb22, 업종 == '정보통신업'& 임금 == 1 )) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '정보통신업'& 임금 == 1 )
                          ,lng=subset(sb22, 업종 == '정보통신업'& 임금 == 1 )$경도
                          , lat=subset(sb22, 업종 == '정보통신업'& 임금 == 1 )$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '정보통신업'& 임금 == 1 )$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '정보통신업'& 임금 == 1 )$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '정보통신업'& 임금 == 1 )$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="일생활균형"){
      leaflet(subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 )
                          ,lng=subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 )$경도
                          , lat=subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 )$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 )$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 )$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 )$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="고용안정"){
      leaflet(subset(sb22, 업종 == '정보통신업'& 고용안정 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '정보통신업'& 고용안정 == 1)
                          ,lng=subset(sb22, 업종 == '정보통신업'& 고용안정 == 1)$경도
                          , lat=subset(sb22, 업종 == '정보통신업'& 고용안정 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '정보통신업'& 고용안정 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '정보통신업'& 고용안정 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '정보통신업'& 고용안정 == 1)$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)
                          ,lng=subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)$경도
                          , lat=subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)$기업규모))
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)
                          ,lng=subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)$경도
                          , lat=subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1)$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)
                          ,lng=subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)$경도
                          , lat=subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb22, 업종 == '정보통신업'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '정보통신업'& 비고==3)
                          ,lng=subset(sb22, 업종 == '정보통신업'& 비고==3)$경도
                          , lat=subset(sb22, 업종 == '정보통신업'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '정보통신업'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '정보통신업'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '정보통신업'& 비고==3)$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="전체"){
      leaflet(subset(sb22, 업종 == '정보통신업')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '정보통신업')
                          ,lng=subset(sb22, 업종 == '정보통신업')$경도
                          , lat=subset(sb22, 업종 == '정보통신업')$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '정보통신업')$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '정보통신업')$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '정보통신업')$기업규모))
      
      
      
      #도매 및 소매업
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금"){
      leaflet(subset(sb21, 업종 == '도매 및 소매업'& 임금 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1)
                          ,lng=subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1)$경도
                          , lat=subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1)$기업규모))
      
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="일생활균형"){
      leaflet(subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1)
                          ,lng=subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1)$경도
                          , lat=subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="고용안정"){
      leaflet(subset(sb22, 업종 == '도매 및 소매업'& 고용안정 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '도매 및 소매업'& 고용안정 == 1)
                          ,lng=subset(sb22, 업종 == '도매 및 소매업'& 고용안정 == 1)$경도
                          , lat=subset(sb22, 업종 == '도매 및 소매업'& 고용안정 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '도매 및 소매업'& 고용안정 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '도매 및 소매업'& 고용안정 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '도매 및 소매업'& 고용안정 == 1)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)
                          ,lng=subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)$경도
                          , lat=subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)
                          ,lng=subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)$경도
                          , lat=subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)
                          ,lng=subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)$경도
                          , lat=subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb22, 업종 == '도매 및 소매업'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '도매 및 소매업'& 비고==3)
                          ,lng=subset(sb22, 업종 == '도매 및 소매업'& 비고==3)$경도
                          , lat=subset(sb22, 업종 == '도매 및 소매업'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '도매 및 소매업'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '도매 및 소매업'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '도매 및 소매업'& 비고==3)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="전체"){
      leaflet(subset(sb22, 업종 == '도매 및 소매업')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '도매 및 소매업')
                          ,lng=subset(sb22, 업종 == '도매 및 소매업')$경도
                          , lat=subset(sb22, 업종 == '도매 및 소매업')$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '도매 및 소매업')$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '도매 및 소매업')$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '도매 및 소매업')$기업규모))
      
      
      #전문, 과학 및 기술 서비스업 
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금"){
      leaflet(subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)
                          ,lng=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)$경도
                          , lat=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="일생활균형"){
      leaflet(subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)
                          ,lng=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)$경도
                          , lat=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="고용안정"){
      leaflet(subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)
                          ,lng=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)$경도
                          , lat=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)
                          ,lng=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)$경도
                          , lat=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)
                          ,lng=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)$경도
                          , lat=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)
                          ,lng=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)$경도
                          , lat=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)
                          ,lng=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$경도
                          , lat=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="전체"){
      leaflet(subset(sb22, 업종 == '전문, 과학 및 기술 서비스업')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '전문, 과학 및 기술 서비스업')
                          ,lng=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업')$경도
                          , lat=subset(sb22, 업종 == '전문, 과학 및 기술 서비스업')$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업')$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업')$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '전문, 과학 및 기술 서비스업')$기업규모))
      
      
      #제조업   
    }else if(input$업종=="제조업"&input$BEST=="임금"){
      leaflet(subset(sb22, 업종 == '제조업'& 임금 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '제조업'& 임금 == 1)
                          ,lng=subset(sb22, 업종 == '제조업'& 임금 == 1)$경도
                          , lat=subset(sb22, 업종 == '제조업'& 임금 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '제조업'& 임금 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '제조업'& 임금 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '제조업'& 임금 == 1)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="일생활균형"){
      leaflet(subset(sb22, 업종 == '제조업'& 일생활균형 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '제조업'& 일생활균형 == 1)
                          ,lng=subset(sb22, 업종 == '제조업'& 일생활균형 == 1)$경도
                          , lat=subset(sb22, 업종 == '제조업'& 일생활균형 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '제조업'& 일생활균형 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '제조업'& 일생활균형 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '제조업'& 일생활균형 == 1)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="고용안정"){
      leaflet(subset(sb22, 업종 == '제조업'& 고용안정 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '제조업'& 고용안정 == 1)
                          ,lng=subset(sb22, 업종 == '제조업'& 고용안정 == 1)$경도
                          , lat=subset(sb22, 업종 == '제조업'& 고용안정 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '제조업'& 고용안정 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '제조업'& 고용안정 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '제조업'& 고용안정 == 1)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb22, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)
                          ,lng=subset(sb22, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)$경도
                          , lat=subset(sb22, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb22, 업종 == '제조업'& 임금 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '제조업'& 임금 == 1 & 고용안정==1)
                          ,lng=subset(sb22, 업종 == '제조업'& 임금 == 1 & 고용안정==1)$경도
                          , lat=subset(sb22, 업종 == '제조업'& 임금 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '제조업'& 임금 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '제조업'& 임금 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '제조업'& 임금 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb22, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)
                          ,lng=subset(sb22, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)$경도
                          , lat=subset(sb22, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb22, 업종 == '제조업'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '제조업'& 비고==3)
                          ,lng=subset(sb22, 업종 == '제조업'& 비고==3)$경도
                          , lat=subset(sb22, 업종 == '제조업'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '제조업'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '제조업'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '제조업'& 비고==3)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="전체"){
      leaflet(subset(sb22, 업종 == '제조업')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '제조업')
                          ,lng=subset(sb22, 업종 == '제조업')$경도
                          , lat=subset(sb22, 업종 == '제조업')$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '제조업')$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '제조업')$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '제조업')$기업규모))
      
      
      #기타    
    }else if(input$업종=="기타"&input$BEST=="임금"){
      leaflet(subset(sb22, 업종 == '기타'& 임금 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '기타'& 임금 == 1)
                          ,lng=subset(sb22, 업종 == '기타'& 임금 == 1)$경도
                          , lat=subset(sb22, 업종 == '기타'& 임금 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '기타'& 임금 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '기타'& 임금 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '기타'& 임금 == 1)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="일생활균형"){
      leaflet(subset(sb22, 업종 == '기타'& 일생활균형 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '기타'& 일생활균형 == 1)
                          ,lng=subset(sb22, 업종 == '기타'& 일생활균형 == 1)$경도
                          , lat=subset(sb22, 업종 == '기타'& 일생활균형 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '기타'& 일생활균형 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '기타'& 일생활균형 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '기타'& 일생활균형 == 1)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="고용안정"){
      leaflet(subset(sb22, 업종 == '기타'& 고용안정 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '기타'& 고용안정 == 1)
                          ,lng=subset(sb22, 업종 == '기타'& 고용안정 == 1)$경도
                          , lat=subset(sb22, 업종 == '기타'& 고용안정 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '기타'& 고용안정 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '기타'& 고용안정 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '기타'& 고용안정 == 1)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb22, 업종 == '기타'& 임금 == 1 & 일생활균형==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '기타'& 임금 == 1 & 일생활균형==1)
                          ,lng=subset(sb22, 업종 == '기타'& 임금 == 1 & 일생활균형==1)$경도
                          , lat=subset(sb22, 업종 == '기타'& 임금 == 1 & 일생활균형==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '기타'& 임금 == 1 & 일생활균형==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '기타'& 임금 == 1 & 일생활균형==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '기타'& 임금 == 1 & 일생활균형==1)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb22, 업종 == '기타'& 임금 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '기타'& 임금 == 1 & 고용안정==1)
                          ,lng=subset(sb22, 업종 == '기타'& 임금 == 1 & 고용안정==1)$경도
                          , lat=subset(sb22, 업종 == '기타'& 임금 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '기타'& 임금 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '기타'& 임금 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '기타'& 임금 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb22, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)
                          ,lng=subset(sb22, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)$경도
                          , lat=subset(sb22, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb22, 업종 == '기타'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '기타'& 비고==3)
                          ,lng=subset(sb22, 업종 == '기타'& 비고==3)$경도
                          , lat=subset(sb22, 업종 == '기타'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '기타'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '기타'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '기타'& 비고==3)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="전체"){
      leaflet(subset(sb22, 업종 == '기타')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22, 업종 == '기타')
                          ,lng=subset(sb22, 업종 == '기타')$경도
                          , lat=subset(sb22, 업종 == '기타')$위도
                          , popup=paste0("사업장명: ", subset(sb22, 업종 == '기타')$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '기타')$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '기타')$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="전체"){
      leaflet(sb22) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = sb22
                          ,lng=sb22$경도
                          , lat=sb22$위도
                          , popup=paste0("사업장명: ", sb22$사업장명
                                         , "<br/>소재지: ", sb22$소재지
                                         , "<br/>기업규모: ", sb22$기업규모))
      
      #전체
    }else if(input$업종=="전체"&input$BEST=="임금"){
      leaflet(subset(sb22,  임금 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22,  임금 == 1)
                          ,lng=subset(sb22,  임금 == 1)$경도
                          , lat=subset(sb22,  임금 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22,  임금 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22,  임금 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22,  임금 == 1)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="일생활균형"){
      leaflet(subset(sb22,  일생활균형 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22,  일생활균형 == 1)
                          ,lng=subset(sb22,  일생활균형 == 1)$경도
                          , lat=subset(sb22,  일생활균형 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22,  일생활균형 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22,  일생활균형 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22,  일생활균형 == 1)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="고용안정"){
      leaflet(subset(sb22,  고용안정 == 1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22,  고용안정 == 1)
                          ,lng=subset(sb22,  고용안정 == 1)$경도
                          , lat=subset(sb22,  고용안정 == 1)$위도
                          , popup=paste0("사업장명: ", subset(sb22,  고용안정 == 1)$사업장명
                                         , "<br/>소재지: ", subset(sb22,  고용안정 == 1)$소재지
                                         , "<br/>기업규모: ", subset(sb22,  고용안정 == 1)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb22,  임금 == 1 & 일생활균형==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22,  임금 == 1 & 일생활균형==1)
                          ,lng=subset(sb22,  임금 == 1 & 일생활균형==1)$경도
                          , lat=subset(sb22,  임금 == 1 & 일생활균형==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22,  임금 == 1 & 일생활균형==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22,  임금 == 1 & 일생활균형==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22,  임금 == 1 & 일생활균형==1)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb22,  임금 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22,  임금 == 1 & 고용안정==1)
                          ,lng=subset(sb22,  임금 == 1 & 고용안정==1)$경도
                          , lat=subset(sb22,  임금 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22,  임금 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22,  임금 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 임금 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb22, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22,  일생활균형 == 1 & 고용안정==1)
                          ,lng=subset(sb22,  일생활균형 == 1 & 고용안정==1)$경도
                          , lat=subset(sb22,  일생활균형 == 1 & 고용안정==1)$위도
                          , popup=paste0("사업장명: ", subset(sb22,  일생활균형 == 1 & 고용안정==1)$사업장명
                                         , "<br/>소재지: ", subset(sb22,  일생활균형 == 1 & 고용안정==1)$소재지
                                         , "<br/>기업규모: ", subset(sb22,  일생활균형 == 1 & 고용안정==1)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb22,  비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb22,  비고==3)
                          ,lng=subset(sb22,  비고==3)$경도
                          , lat=subset(sb22,  비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb22,  비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb22, 업종 == '기타'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb22, 업종 == '기타'& 비고==3)$기업규모))
      
      
      
    }
    
  })
  
  output$map3 <- renderLeaflet({
    
    
    #정보통신업
    if(input$업종=="정보통신업"&input$BEST=="임금"){
      leaflet(subset(sb2, 업종 == '정보통신업'& 임금 == 2 )) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '정보통신업'& 임금 == 2 )
                          ,lng=subset(sb2, 업종 == '정보통신업'& 임금 == 2 )$경도
                          , lat=subset(sb2, 업종 == '정보통신업'& 임금 == 2 )$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '정보통신업'& 임금 == 2 )$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '정보통신업'& 임금 == 2 )$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '정보통신업'& 임금 == 2 )$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="일생활균형"){
      leaflet(subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 )
                          ,lng=subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 )$경도
                          , lat=subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 )$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 )$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 )$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 )$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="고용안정"){
      leaflet(subset(sb2, 업종 == '정보통신업'& 고용안정 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '정보통신업'& 고용안정 == 2)
                          ,lng=subset(sb2, 업종 == '정보통신업'& 고용안정 == 2)$경도
                          , lat=subset(sb2, 업종 == '정보통신업'& 고용안정 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '정보통신업'& 고용안정 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '정보통신업'& 고용안정 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '정보통신업'& 고용안정 == 2)$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 일생활균형==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 일생활균형==2)
                          ,lng=subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 일생활균형==2)$경도
                          , lat=subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 일생활균형==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 일생활균형==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 일생활균형==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 일생활균형==2)$기업규모))
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 고용안정==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 고용안정==2)
                          ,lng=subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 고용안정==2)$경도
                          , lat=subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 고용안정==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 고용안정==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 고용안정==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '정보통신업'& 임금 == 2 & 고용안정==2)$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 & 고용안정==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 & 고용안정==2)
                          ,lng=subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 & 고용안정==2)$경도
                          , lat=subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 & 고용안정==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 & 고용안정==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 & 고용안정==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '정보통신업'& 일생활균형 == 2 & 고용안정==2)$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb2, 업종 == '정보통신업'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '정보통신업'& 비고==3)
                          ,lng=subset(sb2, 업종 == '정보통신업'& 비고==3)$경도
                          , lat=subset(sb2, 업종 == '정보통신업'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '정보통신업'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '정보통신업'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '정보통신업'& 비고==3)$기업규모))
      
      
    }else if(input$업종=="정보통신업"&input$BEST=="전체"){
      leaflet(subset(sb2, 업종 == '정보통신업')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '정보통신업')
                          ,lng=subset(sb2, 업종 == '정보통신업')$경도
                          , lat=subset(sb2, 업종 == '정보통신업')$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '정보통신업')$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '정보통신업')$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '정보통신업')$기업규모))
      
      
      
      #도매 및 소매업
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금"){
      leaflet(subset(sb21, 업종 == '도매 및 소매업'& 임금 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2)
                          ,lng=subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2)$경도
                          , lat=subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2)$기업규모))
      
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="일생활균형"){
      leaflet(subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2)
                          ,lng=subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2)$경도
                          , lat=subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="고용안정"){
      leaflet(subset(sb2, 업종 == '도매 및 소매업'& 고용안정 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '도매 및 소매업'& 고용안정 == 2)
                          ,lng=subset(sb2, 업종 == '도매 및 소매업'& 고용안정 == 2)$경도
                          , lat=subset(sb2, 업종 == '도매 및 소매업'& 고용안정 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '도매 및 소매업'& 고용안정 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '도매 및 소매업'& 고용안정 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '도매 및 소매업'& 고용안정 == 2)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 일생활균형==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 일생활균형==2)
                          ,lng=subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 일생활균형==2)$경도
                          , lat=subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 일생활균형==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 일생활균형==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 일생활균형==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 일생활균형==2)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 고용안정==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 고용안정==2)
                          ,lng=subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 고용안정==2)$경도
                          , lat=subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 고용안정==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 고용안정==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 고용안정==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '도매 및 소매업'& 임금 == 2 & 고용안정==2)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2 & 고용안정==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2 & 고용안정==2)
                          ,lng=subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2 & 고용안정==2)$경도
                          , lat=subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2 & 고용안정==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2 & 고용안정==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2 & 고용안정==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '도매 및 소매업'& 일생활균형 == 2 & 고용안정==2)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb2, 업종 == '도매 및 소매업'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '도매 및 소매업'& 비고==3)
                          ,lng=subset(sb2, 업종 == '도매 및 소매업'& 비고==3)$경도
                          , lat=subset(sb2, 업종 == '도매 및 소매업'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '도매 및 소매업'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '도매 및 소매업'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '도매 및 소매업'& 비고==3)$기업규모))
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="전체"){
      leaflet(subset(sb2, 업종 == '도매 및 소매업')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '도매 및 소매업')
                          ,lng=subset(sb2, 업종 == '도매 및 소매업')$경도
                          , lat=subset(sb2, 업종 == '도매 및 소매업')$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '도매 및 소매업')$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '도매 및 소매업')$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '도매 및 소매업')$기업규모))
      
      
      #전문, 과학 및 기술 서비스업 
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금"){
      leaflet(subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2)
                          ,lng=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2)$경도
                          , lat=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="일생활균형"){
      leaflet(subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2)
                          ,lng=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2)$경도
                          , lat=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="고용안정"){
      leaflet(subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 2)
                          ,lng=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 2)$경도
                          , lat=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 2)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 일생활균형==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 일생활균형==2)
                          ,lng=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 일생활균형==2)$경도
                          , lat=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 일생활균형==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 일생활균형==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 일생활균형==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 일생활균형==2)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 고용안정==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 고용안정==2)
                          ,lng=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 고용안정==2)$경도
                          , lat=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 고용안정==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 고용안정==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 고용안정==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 2 & 고용안정==2)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2 & 고용안정==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2 & 고용안정==2)
                          ,lng=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2 & 고용안정==2)$경도
                          , lat=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2 & 고용안정==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2 & 고용안정==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2 & 고용안정==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 2 & 고용안정==2)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)
                          ,lng=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$경도
                          , lat=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3)$기업규모))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="전체"){
      leaflet(subset(sb2, 업종 == '전문, 과학 및 기술 서비스업')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '전문, 과학 및 기술 서비스업')
                          ,lng=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업')$경도
                          , lat=subset(sb2, 업종 == '전문, 과학 및 기술 서비스업')$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업')$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업')$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '전문, 과학 및 기술 서비스업')$기업규모))
      
      
      #제조업   
    }else if(input$업종=="제조업"&input$BEST=="임금"){
      leaflet(subset(sb2, 업종 == '제조업'& 임금 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '제조업'& 임금 == 2)
                          ,lng=subset(sb2, 업종 == '제조업'& 임금 == 2)$경도
                          , lat=subset(sb2, 업종 == '제조업'& 임금 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '제조업'& 임금 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '제조업'& 임금 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '제조업'& 임금 == 2)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="일생활균형"){
      leaflet(subset(sb2, 업종 == '제조업'& 일생활균형 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '제조업'& 일생활균형 == 2)
                          ,lng=subset(sb2, 업종 == '제조업'& 일생활균형 == 2)$경도
                          , lat=subset(sb2, 업종 == '제조업'& 일생활균형 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '제조업'& 일생활균형 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '제조업'& 일생활균형 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '제조업'& 일생활균형 == 2)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="고용안정"){
      leaflet(subset(sb2, 업종 == '제조업'& 고용안정 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '제조업'& 고용안정 == 2)
                          ,lng=subset(sb2, 업종 == '제조업'& 고용안정 == 2)$경도
                          , lat=subset(sb2, 업종 == '제조업'& 고용안정 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '제조업'& 고용안정 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '제조업'& 고용안정 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '제조업'& 고용안정 == 2)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb2, 업종 == '제조업'& 임금 == 2 & 일생활균형==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '제조업'& 임금 == 2 & 일생활균형==2)
                          ,lng=subset(sb2, 업종 == '제조업'& 임금 == 2 & 일생활균형==2)$경도
                          , lat=subset(sb2, 업종 == '제조업'& 임금 == 2 & 일생활균형==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '제조업'& 임금 == 2 & 일생활균형==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '제조업'& 임금 == 2 & 일생활균형==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '제조업'& 임금 == 2 & 일생활균형==2)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb2, 업종 == '제조업'& 임금 == 2 & 고용안정==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '제조업'& 임금 == 2 & 고용안정==2)
                          ,lng=subset(sb2, 업종 == '제조업'& 임금 == 2 & 고용안정==2)$경도
                          , lat=subset(sb2, 업종 == '제조업'& 임금 == 2 & 고용안정==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '제조업'& 임금 == 2 & 고용안정==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '제조업'& 임금 == 2 & 고용안정==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '제조업'& 임금 == 2 & 고용안정==2)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb2, 업종 == '제조업'& 일생활균형 == 2 & 고용안정==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '제조업'& 일생활균형 == 2 & 고용안정==2)
                          ,lng=subset(sb2, 업종 == '제조업'& 일생활균형 == 2 & 고용안정==2)$경도
                          , lat=subset(sb2, 업종 == '제조업'& 일생활균형 == 2 & 고용안정==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '제조업'& 일생활균형 == 2 & 고용안정==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '제조업'& 일생활균형 == 2 & 고용안정==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '제조업'& 일생활균형 == 2 & 고용안정==2)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb2, 업종 == '제조업'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '제조업'& 비고==3)
                          ,lng=subset(sb2, 업종 == '제조업'& 비고==3)$경도
                          , lat=subset(sb2, 업종 == '제조업'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '제조업'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '제조업'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '제조업'& 비고==3)$기업규모))
      
    }else if(input$업종=="제조업"&input$BEST=="전체"){
      leaflet(subset(sb2, 업종 == '제조업')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '제조업')
                          ,lng=subset(sb2, 업종 == '제조업')$경도
                          , lat=subset(sb2, 업종 == '제조업')$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '제조업')$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '제조업')$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '제조업')$기업규모))
      
      
      #기타    
    }else if(input$업종=="기타"&input$BEST=="임금"){
      leaflet(subset(sb2, 업종 == '기타'& 임금 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '기타'& 임금 == 2)
                          ,lng=subset(sb2, 업종 == '기타'& 임금 == 2)$경도
                          , lat=subset(sb2, 업종 == '기타'& 임금 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '기타'& 임금 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '기타'& 임금 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '기타'& 임금 == 2)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="일생활균형"){
      leaflet(subset(sb2, 업종 == '기타'& 일생활균형 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '기타'& 일생활균형 == 2)
                          ,lng=subset(sb2, 업종 == '기타'& 일생활균형 == 2)$경도
                          , lat=subset(sb2, 업종 == '기타'& 일생활균형 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '기타'& 일생활균형 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '기타'& 일생활균형 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '기타'& 일생활균형 == 2)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="고용안정"){
      leaflet(subset(sb2, 업종 == '기타'& 고용안정 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '기타'& 고용안정 == 2)
                          ,lng=subset(sb2, 업종 == '기타'& 고용안정 == 2)$경도
                          , lat=subset(sb2, 업종 == '기타'& 고용안정 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '기타'& 고용안정 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '기타'& 고용안정 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '기타'& 고용안정 == 2)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb2, 업종 == '기타'& 임금 == 2 & 일생활균형==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '기타'& 임금 == 2 & 일생활균형==2)
                          ,lng=subset(sb2, 업종 == '기타'& 임금 == 2 & 일생활균형==2)$경도
                          , lat=subset(sb2, 업종 == '기타'& 임금 == 2 & 일생활균형==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '기타'& 임금 == 2 & 일생활균형==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '기타'& 임금 == 2 & 일생활균형==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '기타'& 임금 == 2 & 일생활균형==2)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb2, 업종 == '기타'& 임금 == 2 & 고용안정==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '기타'& 임금 == 2 & 고용안정==2)
                          ,lng=subset(sb2, 업종 == '기타'& 임금 == 2 & 고용안정==2)$경도
                          , lat=subset(sb2, 업종 == '기타'& 임금 == 2 & 고용안정==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '기타'& 임금 == 2 & 고용안정==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '기타'& 임금 == 2 & 고용안정==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '기타'& 임금 == 2 & 고용안정==2)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb2, 업종 == '기타'& 일생활균형 == 2 & 고용안정==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '기타'& 일생활균형 == 2 & 고용안정==2)
                          ,lng=subset(sb2, 업종 == '기타'& 일생활균형 == 2 & 고용안정==2)$경도
                          , lat=subset(sb2, 업종 == '기타'& 일생활균형 == 2 & 고용안정==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '기타'& 일생활균형 == 2 & 고용안정==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '기타'& 일생활균형 == 2 & 고용안정==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '기타'& 일생활균형 == 2 & 고용안정==2)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb2, 업종 == '기타'& 비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '기타'& 비고==3)
                          ,lng=subset(sb2, 업종 == '기타'& 비고==3)$경도
                          , lat=subset(sb2, 업종 == '기타'& 비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '기타'& 비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '기타'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '기타'& 비고==3)$기업규모))
      
    }else if(input$업종=="기타"&input$BEST=="전체"){
      leaflet(subset(sb2, 업종 == '기타')) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2, 업종 == '기타')
                          ,lng=subset(sb2, 업종 == '기타')$경도
                          , lat=subset(sb2, 업종 == '기타')$위도
                          , popup=paste0("사업장명: ", subset(sb2, 업종 == '기타')$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '기타')$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '기타')$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="전체"){
      leaflet(sb2) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = sb2
                          ,lng=sb2$경도
                          , lat=sb2$위도
                          , popup=paste0("사업장명: ", sb2$사업장명
                                         , "<br/>소재지: ", sb2$소재지
                                         , "<br/>기업규모: ", sb2$기업규모))
      
      #전체
    }else if(input$업종=="전체"&input$BEST=="임금"){
      leaflet(subset(sb2,  임금 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2,  임금 == 2)
                          ,lng=subset(sb2,  임금 == 2)$경도
                          , lat=subset(sb2,  임금 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2,  임금 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2,  임금 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2,  임금 == 2)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="일생활균형"){
      leaflet(subset(sb2,  일생활균형 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2,  일생활균형 == 2)
                          ,lng=subset(sb2,  일생활균형 == 2)$경도
                          , lat=subset(sb2,  일생활균형 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2,  일생활균형 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2,  일생활균형 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2,  일생활균형 == 2)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="고용안정"){
      leaflet(subset(sb2,  고용안정 == 2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2,  고용안정 == 2)
                          ,lng=subset(sb2,  고용안정 == 2)$경도
                          , lat=subset(sb2,  고용안정 == 2)$위도
                          , popup=paste0("사업장명: ", subset(sb2,  고용안정 == 2)$사업장명
                                         , "<br/>소재지: ", subset(sb2,  고용안정 == 2)$소재지
                                         , "<br/>기업규모: ", subset(sb2,  고용안정 == 2)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="임금&일생활균형"){
      leaflet(subset(sb2,  임금 == 2 & 일생활균형==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2,  임금 == 2 & 일생활균형==2)
                          ,lng=subset(sb2,  임금 == 2 & 일생활균형==2)$경도
                          , lat=subset(sb2,  임금 == 2 & 일생활균형==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2,  임금 == 2 & 일생활균형==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2,  임금 == 2 & 일생활균형==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2,  임금 == 2 & 일생활균형==2)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="임금&고용안정"){
      leaflet(subset(sb2,  임금 == 2 & 고용안정==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2,  임금 == 2 & 고용안정==2)
                          ,lng=subset(sb2,  임금 == 2 & 고용안정==2)$경도
                          , lat=subset(sb2,  임금 == 2 & 고용안정==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2,  임금 == 2 & 고용안정==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2,  임금 == 2 & 고용안정==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 임금 == 2 & 고용안정==2)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="일생활균형&고용안정"){
      leaflet(subset(sb2, 업종 == '기타'& 일생활균형 == 2 & 고용안정==2)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2,  일생활균형 == 2 & 고용안정==2)
                          ,lng=subset(sb2,  일생활균형 == 2 & 고용안정==2)$경도
                          , lat=subset(sb2,  일생활균형 == 2 & 고용안정==2)$위도
                          , popup=paste0("사업장명: ", subset(sb2,  일생활균형 == 2 & 고용안정==2)$사업장명
                                         , "<br/>소재지: ", subset(sb2,  일생활균형 == 2 & 고용안정==2)$소재지
                                         , "<br/>기업규모: ", subset(sb2,  일생활균형 == 2 & 고용안정==2)$기업규모))
      
    }else if(input$업종=="전체"&input$BEST=="임금&일생활균형&고용안정"){
      leaflet(subset(sb2,  비고==3)) %>%
        setView(lng=126.9784, lat=37.55, zoom=12) %>%
        addTiles() %>%
        addAwesomeMarkers(data = subset(sb2,  비고==3)
                          ,lng=subset(sb2,  비고==3)$경도
                          , lat=subset(sb2,  비고==3)$위도
                          , popup=paste0("사업장명: ", subset(sb2,  비고==3)$사업장명
                                         , "<br/>소재지: ", subset(sb2, 업종 == '기타'& 비고==3)$소재지
                                         , "<br/>기업규모: ", subset(sb2, 업종 == '기타'& 비고==3)$기업규모))
      
      
      
    }
    
  })
  
  
  output$map4 <- renderLeaflet({
    leaflet(sb4) %>%
      setView(lng=126.98, lat=37.55, zoom=12) %>%
      addTiles() %>%
      addAwesomeMarkers(data = sb4,lng=sb4$경도, 
                        lat=sb4$위도, popup=paste0("카페명: ", 
                                                 sb4$카페명, "<br/>상세주소: ",
                                                 sb4$상세주소, "<br/>이용시간: ", 
                                                 sb4$이용시간))
  })
  
  output$map5 <- renderLeaflet({
    leaflet(sb5) %>%
      setView(lng=126.99, lat=37.52, zoom=13) %>%
      addTiles() %>%
      addAwesomeMarkers(data = sb5,lng=sb5$경도, 
                        lat=sb5$위도, popup=paste0("업체명: ", 
                                                 sb5$업체명, "<br/>주소: ", 
                                                 sb5$주소, "<br/>전화번호: ", 
                                                 sb5$전화번호))
  })
  
  
  output$list21 <- renderTable({
    
    
    #정보통신업
    if(input$업종=="정보통신업"&input$BEST=="임금"){
      (subset(sb21a, 업종 == '정보통신업'& 임금 == 1 ))
      
    }else if(input$업종=="정보통신업"&input$BEST=="일생활균형"){
      (subset(sb21a, 업종 == '정보통신업'& 일생활균형 == 1))
      
    }else if(input$업종=="정보통신업"&input$BEST=="고용안정"){
      (subset(sb21a, 업종 == '정보통신업'& 고용안정 == 1))
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&일생활균형"){
      (subset(sb21a, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)) 
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&고용안정"){
      (subset(sb21a, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1))   
      
    }else if(input$업종=="정보통신업"&input$BEST=="일생활균형&고용안정"){
      (subset(sb21a, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb21a, 업종 == '정보통신업'& 비고==3)) 
      
    }else if(input$업종=="정보통신업"&input$BEST=="전체"){
      (subset(sb21a, 업종 == '정보통신업'))  
      
      
      #도매 및 소매업
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금"){
      (subset(sb21a, 업종 == '도매 및 소매업'& 임금 == 1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="일생활균형"){
      (subset(sb21a, 업종 == '도매 및 소매업'& 일생활균형 == 1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="고용안정"){
      (subset(sb21a, 업종 == '도매 및 소매업'& 고용안정 == 1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&일생활균형"){
      (subset(sb21a, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&고용안정"){
      (subset(sb21a, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1))    
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="일생활균형&고용안정"){
      (subset(sb21a, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb21a, 업종 == '도매 및 소매업'& 비고==3)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="전체"){
      (subset(sb21a, 업종 == '도매 및 소매업')) 
      
      
      #전문, 과학 및 기술 서비스업 
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금"){
      (subset(sb21a, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="일생활균형"){
      (subset(sb21a, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="고용안정"){
      (subset(sb21a, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&일생활균형"){
      (subset(sb21a, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&고용안정"){
      (subset(sb21a, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="일생활균형&고용안정"){
      (subset(sb21a, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb21a, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3))  
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="전체"){
      (subset(sb21a, 업종 == '전문, 과학 및 기술 서비스업'))   
      
      
      #제조업   
    }else if(input$업종=="제조업"&input$BEST=="임금"){
      (subset(sb21a, 업종 == '제조업'& 임금 == 1))
      
    }else if(input$업종=="제조업"&input$BEST=="일생활균형"){
      (subset(sb21a, 업종 == '제조업'& 일생활균형 == 1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="고용안정"){
      (subset(sb21a, 업종 == '제조업'& 고용안정 == 1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="임금&일생활균형"){
      (subset(sb21a, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="임금&고용안정"){
      (subset(sb21a, 업종 == '제조업'& 임금 == 1 & 고용안정==1))    
      
    }else if(input$업종=="제조업"&input$BEST=="일생활균형&고용안정"){
      (subset(sb21a, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb21a, 업종 == '제조업'& 비고==3))   
      
    }else if(input$업종=="제조업"&input$BEST=="전체"){
      (subset(sb21a, 업종 == '제조업'))  
      
      
      #기타    
    }else if(input$업종=="기타"&input$BEST=="임금"){
      (subset(sb21a, 업종 == '기타'& 임금 == 1)) 
      
    }else if(input$업종=="기타"&input$BEST=="일생활균형"){
      (subset(sb21a, 업종 == '기타'& 임금 == 1)) 
      
    }else if(input$업종=="기타"&input$BEST=="고용안정"){
      (subset(sb21a, 업종 == '기타'& 임금 == 1)) 
      
    }else if(input$업종=="기타"&input$BEST=="임금&일생활균형"){
      (subset(sb21a, 업종 == '기타'& 임금 == 1 & 일생활균형==1)) 
      
    }else if(input$업종=="기타"&input$BEST=="임금&고용안정"){
      (subset(sb21a, 업종 == '기타'& 임금 == 1 & 고용안정==1))    
      
    }else if(input$업종=="기타"&input$BEST=="일생활균형&고용안정"){
      (subset(sb21a, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="기타"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb21a, 업종 == '기타'& 비고==3))
      
    }else if(input$업종=="기타"&input$BEST=="전체"){
      (subset(sb21a, 업종 == '기타')) 
      
    }else if(input$업종=="전체"&input$BEST=="전체"){
      (sb21a)}
    
    
  })
  
  output$list22 <- renderTable({
    
    
    #정보통신업
    if(input$업종=="정보통신업"&input$BEST=="임금"){
      (subset(sb22a, 업종 == '정보통신업'& 임금 == 1 ))
      
    }else if(input$업종=="정보통신업"&input$BEST=="일생활균형"){
      (subset(sb22a, 업종 == '정보통신업'& 일생활균형 == 1))
      
    }else if(input$업종=="정보통신업"&input$BEST=="고용안정"){
      (subset(sb22a, 업종 == '정보통신업'& 고용안정 == 1))
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&일생활균형"){
      (subset(sb22a, 업종 == '정보통신업'& 임금 == 1 & 일생활균형==1)) 
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&고용안정"){
      (subset(sb22a, 업종 == '정보통신업'& 임금 == 1 & 고용안정==1))   
      
    }else if(input$업종=="정보통신업"&input$BEST=="일생활균형&고용안정"){
      (subset(sb22a, 업종 == '정보통신업'& 일생활균형 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb22a, 업종 == '정보통신업'& 비고==3)) 
      
    }else if(input$업종=="정보통신업"&input$BEST=="전체"){
      (subset(sb22a, 업종 == '정보통신업'))  
      
      
      #도매 및 소매업
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금"){
      (subset(sb22a, 업종 == '도매 및 소매업'& 임금 == 1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="일생활균형"){
      (subset(sb22a, 업종 == '도매 및 소매업'& 일생활균형 == 1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="고용안정"){
      (subset(sb22a, 업종 == '도매 및 소매업'& 고용안정 == 1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&일생활균형"){
      (subset(sb22a, 업종 == '도매 및 소매업'& 임금 == 1 & 일생활균형==1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&고용안정"){
      (subset(sb22a, 업종 == '도매 및 소매업'& 임금 == 1 & 고용안정==1))    
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="일생활균형&고용안정"){
      (subset(sb22a, 업종 == '도매 및 소매업'& 일생활균형 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb22a, 업종 == '도매 및 소매업'& 비고==3)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="전체"){
      (subset(sb22a, 업종 == '도매 및 소매업')) 
      
      
      #전문, 과학 및 기술 서비스업 
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금"){
      (subset(sb22a, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="일생활균형"){
      (subset(sb22a, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="고용안정"){
      (subset(sb22a, 업종 == '전문, 과학 및 기술 서비스업'& 고용안정 == 1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&일생활균형"){
      (subset(sb22a, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 일생활균형==1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&고용안정"){
      (subset(sb22a, 업종 == '전문, 과학 및 기술 서비스업'& 임금 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="일생활균형&고용안정"){
      (subset(sb22a, 업종 == '전문, 과학 및 기술 서비스업'& 일생활균형 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb22a, 업종 == '전문, 과학 및 기술 서비스업'& 비고==3))  
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="전체"){
      (subset(sb22a, 업종 == '전문, 과학 및 기술 서비스업'))   
      
      
      #제조업   
    }else if(input$업종=="제조업"&input$BEST=="임금"){
      (subset(sb22a, 업종 == '제조업'& 임금 == 1))
      
    }else if(input$업종=="제조업"&input$BEST=="일생활균형"){
      (subset(sb22a, 업종 == '제조업'& 일생활균형 == 1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="고용안정"){
      (subset(sb22a, 업종 == '제조업'& 고용안정 == 1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="임금&일생활균형"){
      (subset(sb22a, 업종 == '제조업'& 임금 == 1 & 일생활균형==1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="임금&고용안정"){
      (subset(sb22a, 업종 == '제조업'& 임금 == 1 & 고용안정==1))    
      
    }else if(input$업종=="제조업"&input$BEST=="일생활균형&고용안정"){
      (subset(sb22a, 업종 == '제조업'& 일생활균형 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb22a, 업종 == '제조업'& 비고==3))   
      
    }else if(input$업종=="제조업"&input$BEST=="전체"){
      (subset(sb22a, 업종 == '제조업'))  
      
      
      #기타    
    }else if(input$업종=="기타"&input$BEST=="임금"){
      (subset(sb22a, 업종 == '기타'& 임금 == 1)) 
      
    }else if(input$업종=="기타"&input$BEST=="일생활균형"){
      (subset(sb22a, 업종 == '기타'& 임금 == 1)) 
      
    }else if(input$업종=="기타"&input$BEST=="고용안정"){
      (subset(sb22a, 업종 == '기타'& 임금 == 1)) 
      
    }else if(input$업종=="기타"&input$BEST=="임금&일생활균형"){
      (subset(sb22a, 업종 == '기타'& 임금 == 1 & 일생활균형==1)) 
      
    }else if(input$업종=="기타"&input$BEST=="임금&고용안정"){
      (subset(sb22a, 업종 == '기타'& 임금 == 1 & 고용안정==1))    
      
    }else if(input$업종=="기타"&input$BEST=="일생활균형&고용안정"){
      (subset(sb22a, 업종 == '기타'& 일생활균형 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="기타"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb22a, 업종 == '기타'& 비고==3))
      
    }else if(input$업종=="기타"&input$BEST=="전체"){
      (subset(sb22a, 업종 == '기타')) 
      
    }else if(input$업종=="전체"&input$BEST=="전체"){
      (sb22a) 
      
      #전체
    }else if(input$업종=="전체"&input$BEST=="임금"){
      (subset(sb22a,  임금 == 1)) 
      
    }else if(input$업종=="전체"&input$BEST=="일생활균형"){
      (subset(sb22a,  일생활균형 == 1)) 
      
    }else if(input$업종=="전체"&input$BEST=="고용안정"){
      (subset(sb22a,  고용안정 == 1)) 
      
    }else if(input$업종=="전체"&input$BEST=="임금&일생활균형"){
      (subset(sb22a,  임금 == 1 & 일생활균형==1)) 
      
    }else if(input$업종=="전체"&input$BEST=="임금&고용안정"){
      (subset(sb22a,  임금 == 1 & 고용안정==1))    
      
    }else if(input$업종=="전체"&input$BEST=="일생활균형&고용안정"){
      (subset(sb22a,  일생활균형 == 1 & 고용안정==1)) 
      
    }else if(input$업종=="전체"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb22a,  비고==3))  
      
      
      
    }
    
    
  }) 
  
  output$list2 <- renderTable({
    
    
    #정보통신업
    if(input$업종=="정보통신업"&input$BEST=="임금"){
      (subset(sb2a, 업종22 == '정보통신업'& 임금22 == 1 & 임금21==1)) 
      
    }else if(input$업종=="정보통신업"&input$BEST=="일생활균형"){
      (subset(sb2a, 업종22 == '정보통신업'& 일생활균형22 == 1 & 일생활균형21== 1))
      
    }else if(input$업종=="정보통신업"&input$BEST=="고용안정"){
      (subset(sb2a, 업종22 == '정보통신업'& 고용안정22 == 1 & 고용안정21== 1))
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&일생활균형"){
      (subset(sb2a, 업종22 == '정보통신업'& 임금22 == 1 & 일생활균형22==1 & 임금21 == 1 & 일생활균형21 == 1)) 
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&고용안정"){
      (subset(sb2a, 업종22 == '정보통신업'& 임금22 == 1 & 고용안정22==1 & 임금21 == 1 & 고용안정21 == 1))   
      
    }else if(input$업종=="정보통신업"&input$BEST=="일생활균형&고용안정"){
      (subset(sb2a, 업종22 == '정보통신업'& 일생활균형22 == 1 & 고용안정22==1 & 일생활균형21 == 1 & 고용안정21 == 1)) 
      
    }else if(input$업종=="정보통신업"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb2a, 업종22 == '정보통신업'& 랭킹 == 1)) 
      
    }else if(input$업종=="정보통신업"&input$BEST=="전체"){
      (subset(sb2a, 업종22 == '정보통신업'))    
      
      
      #도매 및 소매업
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금"){
      (subset(sb2a, 업종22 == '도매 및 소매업'& 임금22 == 1 & 임금21==1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="일생활균형"){
      (subset(sb2a, 업종22 == '도매 및 소매업'& 일생활균형22 == 1 & 일생활균형21== 1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="고용안정"){
      (subset(sb2a, 업종22 == '도매 및 소매업'& 고용안정22 == 1 & 고용안정21== 1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&일생활균형"){
      (subset(sb2a, 업종22 == '도매 및 소매업'& 임금22 == 1 & 일생활균형22==1 & 임금21 == 1 & 일생활균형21 == 1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&고용안정"){
      (subset(sb2a, 업종22 == '도매 및 소매업'& 임금22 == 1 & 고용안정22==1 & 임금21 == 1 & 고용안정21 == 1))    
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="일생활균형&고용안정"){
      (subset(sb2a, 업종22 == '도매 및 소매업'& 일생활균형22 == 1 & 고용안정22==1 & 일생활균형21 == 1 & 고용안정21 == 1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb2a, 업종22 == '도매 및 소매업'& 랭킹 == 1)) 
      
    }else if(input$업종=="도매 및 소매업"&input$BEST=="전체"){
      (subset(sb2a, 업종22 == '도매 및 소매업'))  
      
      
      #전문, 과학 및 기술 서비스업 
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금"){
      (subset(sb2a, 업종22 == '전문, 과학 및 기술 서비스업'& 임금22 == 1 & 임금21==1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="일생활균형"){
      (subset(sb2a, 업종22 == '전문, 과학 및 기술 서비스업'& 일생활균형22 == 1 & 일생활균형21== 1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="고용안정"){
      (subset(sb2a, 업종22 == '전문, 과학 및 기술 서비스업'& 고용안정22 == 1 & 고용안정21== 1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&일생활균형"){
      (subset(sb2a, 업종22 == '전문, 과학 및 기술 서비스업'& 임금22 == 1 & 일생활균형22==1 & 임금21 == 1 & 일생활균형21 == 1))
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&고용안정"){
      (subset(sb2a, 업종22 == '전문, 과학 및 기술 서비스업'& 임금22 == 1 & 고용안정22==1 & 임금21 == 1 & 고용안정21 == 1))    
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="일생활균형&고용안정"){
      (subset(sb2a, 업종22 == '전문, 과학 및 기술 서비스업'& 일생활균형22 == 1 & 고용안정22==1 & 일생활균형21 == 1 & 고용안정21 == 1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb2a, 업종22 == '전문, 과학 및 기술 서비스업'& 랭킹 == 1)) 
      
    }else if(input$업종=="전문, 과학 및 기술 서비스업"&input$BEST=="전체"){
      (subset(sb2a, 업종22 == '전문, 과학 및 기술 서비스업'))   
      
      
      #제조업   
    }else if(input$업종=="제조업"&input$BEST=="임금"){
      (subset(sb2a, 업종22 == '제조업'& 임금22 == 1 & 임금21==1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="일생활균형"){
      (subset(sb2a, 업종22 == '제조업'& 일생활균형22 == 1 & 일생활균형21== 1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="고용안정"){
      (subset(sb2a, 업종22 == '제조업'& 고용안정22 == 1 & 고용안정21== 1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="임금&일생활균형"){
      (subset(sb2a, 업종22 == '제조업'& 임금22 == 1 & 일생활균형22==1 & 임금21 == 1 & 일생활균형21 == 1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="임금&고용안정"){
      (subset(sb2a, 업종22 == '제조업'& 임금22 == 1 & 고용안정22==1 & 임금21 == 1 & 고용안정21 == 1))    
      
    }else if(input$업종=="제조업"&input$BEST=="일생활균형&고용안정"){
      (subset(sb2a, 업종22 == '제조업'& 일생활균형22 == 1 & 고용안정22==1 & 일생활균형21 == 1 & 고용안정21 == 1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb2a, 업종22 == '제조업'& 랭킹 == 1)) 
      
    }else if(input$업종=="제조업"&input$BEST=="전체"){
      (subset(sb2a, 업종22 == '제조업'))   
      
      
      #기타    
    }else if(input$업종=="기타"&input$BEST=="임금"){
      (subset(sb2a, 업종22 == '기타'& 임금22 == 1 & 임금21==1)) 
      
    }else if(input$업종=="기타"&input$BEST=="일생활균형"){
      (subset(sb2a, 업종22 == '기타'& 일생활균형22 == 1 & 일생활균형21== 1)) 
      
    }else if(input$업종=="기타"&input$BEST=="고용안정"){
      (subset(sb2a, 업종22 == '기타'& 고용안정22 == 1 & 고용안정21== 1)) 
      
    }else if(input$업종=="기타"&input$BEST=="임금&일생활균형"){
      (subset(sb2a, 업종22 == '기타'& 임금22 == 1 & 일생활균형22==1 & 임금21 == 1 & 일생활균형21 == 1)) 
      
    }else if(input$업종=="기타"&input$BEST=="임금&고용안정"){
      (subset(sb2a, 업종22 == '기타'& 임금22 == 1 & 고용안정22==1 & 임금21 == 1 & 고용안정21 == 1))    
      
    }else if(input$업종=="기타"&input$BEST=="일생활균형&고용안정"){
      (subset(sb2a, 업종22 == '기타'& 일생활균형22 == 1 & 고용안정22==1 & 일생활균형21 == 1 & 고용안정21 == 1)) 
      
    }else if(input$업종=="기타"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb2a, 업종22 == '기타'& 랭킹 == 1)) 
      
    }else if(input$업종=="기타"&input$BEST=="전체"){
      (subset(sb2a, 업종22 == '기타'))
      
    }else if(input$업종=="전체"&input$BEST=="전체"){
      (sb2a)
      
      #전체 
    }else if(input$업종=="전체"&input$BEST=="임금"){
      (subset(sb2a,  임금22 == 1 & 임금21==1)) 
      
    }else if(input$업종=="전체"&input$BEST=="일생활균형"){
      (subset(sb2a,  일생활균형22 == 1 & 일생활균형21== 1)) 
      
    }else if(input$업종=="전체"&input$BEST=="고용안정"){
      (subset(sb2a,  고용안정22 == 1 & 고용안정21== 1)) 
      
    }else if(input$업종=="전체"&input$BEST=="임금&일생활균형"){
      (subset(sb2a,  임금22 == 1 & 일생활균형22==1 & 임금21 == 1 & 일생활균형21 == 1)) 
      
    }else if(input$업종=="전체"&input$BEST=="임금&고용안정"){
      (subset(sb2a,  임금22 == 1 & 고용안정22==1 & 임금21 == 1 & 고용안정21 == 1))    
      
    }else if(input$업종=="전체"&input$BEST=="일생활균형&고용안정"){
      (subset(sb2a,  일생활균형22 == 1 & 고용안정22==1 & 일생활균형21 == 1 & 고용안정21 == 1)) 
      
    }else if(input$업종=="전체"&input$BEST=="임금&일생활균형&고용안정"){
      (subset(sb2a,  랭킹 == 1))
      
      
    } 
    
    
  })
  
  output$list4 <- renderTable(sb4a)
  
  output$list5 <- renderTable(sb5a)
  
 }

# Run the application 
shinyApp(ui = ui, server = server)

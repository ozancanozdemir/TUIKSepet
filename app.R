library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)
library(lubridate)
data<-read.csv("https://users.metu.edu.tr/ozancan/sepet.csv",sep=";")
convert_fnc<-function(x){
  x<-as.numeric(gsub(",",".",x))
}
data[,3:ncol(data)]<-sapply(data[,3:ncol(data)],convert_fnc)
colnames(data)<-gsub("X.","",colnames(data))
colnames(data)[1]<-"Madde.adlari"
# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("yeti"),
      titlePanel("TUIK Sepet"),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("urun", "Urun Seciniz",
                                  as.vector(data$Madde.adlari))
                    ),

                    mainPanel(
                      
                      
                      h3(textOutput("caption")),
                      
                      
                      plotlyOutput("enfPlot"),
                      
                      tableOutput("table1"),
                      tableOutput("table2"),
                      plotlyOutput("karsilastirma"),
                      plotlyOutput("karsilastirma1"),
                      br(),
                      div(h3(strong("Hakkinda"),style="color:darkred")),
                      p("Bu uygulama TUIK tarafindan Tuketici Fiyat Endeksi (TUFE) hesaplamak icin her ay olusturulan madde sepeti ve 
                    maddelerin birim olcumlerinin ortalama fiyatlarini iceren, TUIK tarafindan paylasilan acik veri kullanilarak yapilmistir.
                    Bu uygulama ile madde sepetinde yer alan urunlerin Ocak 2005 tarihinden itibaren olan aylik ortalama fiyatlarini inceleyebilir, 
                    bir ay onceki, bir yil onceki ve icinde bulunulan yilin Ocak ayina gore fiyatlarin yuzdelik degisimlerini gorebilir ve 
                    son olarak secili maddenin son 1 yil icindeki aylara gore fiyatlarini goruntuleyebilirsiniz.",align="Justify"),
                      div(h5(strong("Gelistiren"),style="color:darkred")),
                      a(h5("Ozancan Ozdemir"),href="https://users.metu.edu.tr/ozancan",target="_blank"),
                      p("Orta Dogu Teknik Universitesi Istatistik Bolumu Arastirma Gorevlisi & Doktora ogrencisi"),
                      div(h5(strong("Soru ve Gorusleriniz Icin")),style="color:darkred"),
                      p("ozancan@metu.edu.tr"),
                      h6("Powered by : "),
                      HTML(paste('<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/RStudio_logo_flat.svg/1280px-RStudio_logo_flat.svg.png" style="width:128px;height:50px;">')))
      
    )
  )
#)

# Define server 
server <- function(input, output) {
  

  output$enfPlot <- renderPlotly({
    index <- which(data$Madde.adlari==input$urun)
    t <- as.character(data$Tarih)[1:(ncol(data)-2)]
    Tarih<-as.Date(t,format = "%d.%m.%Y")
    urun<-as.numeric(data[index,-c(1,2)])
    df<-data.frame(Tarih,urun)
    library(ggplot2)
    library(plotly)
    p<-ggplot(df,aes(Tarih,urun,group=1))+geom_line()+geom_point(col="darkred",size =0.5)+
      labs(title = paste(input$urun,"Urunun Ortalama Birim Fiyatinin Yillara Gore Degisimi"),
           y = "Urun Birim Fiyati (TL)")+annotate("text", x = df$Tarih[15], y = max(df$urun),
                                                  label = paste("Son Tarih:",colnames(data)[ncol(data)])) +theme_bw()
    ggplotly(p)
    
  })

  output$table1 <- renderTable({
    index <- which(data$Madde.adlari==input$urun)
    urun<-as.numeric(data[index,-c(1,2)])
    t <- as.character(data$Tarih)[1:(ncol(data)-2)]
    Tarih<-as.Date(t,format = "%d.%m.%Y")
    bironcekiay<-paste("%",round(((urun[length(urun)]-urun[length(urun)-1])*100)/urun[length(urun)-1],2))
    bironcekiyil<-paste("%",round(((urun[length(urun)]-urun[length(urun)-12])*100)/urun[length(urun)-12],2))
    yilbasi<-paste("%",round(((urun[length(urun)]-urun[length(urun)+1-month(Tarih[length(Tarih)])])*100)/urun[length(urun)+1-month(Tarih[length(Tarih)])],2))
    Degisim<-c("Bir Onceki Aya Gore Degisim","Bir Onceki Yila Gore Degisim","Yilbasina Gore Degisim")
    Oran<-c(bironcekiay,bironcekiyil,yilbasi)
    df1<-data.frame(Degisim,Oran)
    df1
  })
  
  
  output$table2<-renderTable({
    index <- which(data$Madde.adlari==input$urun)
    urun<-data[index,-c(1,2)]
    urun[seq(length(urun)-12,length(urun),1)]
  })
  
  
  output$karsilastirma <- renderPlotly({
    Madde = data$Madde.adlari
    Yuzde = round(((data[,ncol(data)]- data[,ncol(data)-1])*100)/ data[,ncol(data)-1],2)
    Fark = data.frame(Madde,Yuzde)
    Azalan = head(Fark[order(Yuzde),],10)
    Artan =  head(Fark[order(Yuzde,decreasing = T),],10)
    Azalan_plot<-ggplot(Azalan,aes(x=Madde,y=Yuzde))+geom_bar(stat = "identity",fill ="darkred")+labs(title="Bir Önceki Aya Göre Fiyatı En Fazla Düşen Maddeler")+theme_bw()+
      theme(axis.text.x = element_text(angle= 90))+coord_flip()
    ggplotly(Azalan_plot)
  })
  
  output$karsilastirma1 <- renderPlotly({
    Madde = data$Madde.adlari
    Yuzde = round(((data[,ncol(data)]- data[,ncol(data)-1])*100)/ data[,ncol(data)-1],2)
    Fark = data.frame(Madde,Yuzde)
    Artan =  head(Fark[order(Yuzde,decreasing = T),],10)
    Artan_plot<-ggplot(Artan,aes(x=Madde,y=Yuzde))+geom_bar(stat = "identity",fill ="darkred")+labs(title="Bir Önceki Aya Göre Fiyatı En Fazla Artan Maddeler")+theme_bw()+
      theme(axis.text.x = element_text(angle = 90))+coord_flip()
    ggplotly(Artan_plot)
  })
  
}

shinyApp(ui = ui, server = server)

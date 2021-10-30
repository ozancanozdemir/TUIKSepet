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

# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("yeti"),
                # App title ----
                titlePanel(h1("TÜİK Sepet")),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("urun", "Ürün Seçiniz",
                                as.vector(data$Madde.adları))
                  ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                  
                  # Output: Formatted text for caption ----
                  h3(textOutput("caption")),
                  
                  # Output: Plot of the requested variable against mpg ----
                  plotlyOutput("enfPlot"),
                  
                  tableOutput("table1"),
                  tableOutput("table2"),
                  br(),
                  div(h3(strong("Hakkında"),style="color:darkred")),
                  p("Bu uygulama TÜİK'in, Tüketici Fiyat Endeksi(TÜFE) hesaplamak için her ay oluşturduğu madde sepeti ve 
                 maddelerin birim ölçümlerinin ortalama fiyatların içeren, TÜİK'in paylaştığı açık veri kullanılarak yapılmıştır.
                 Bu uygulama ile madde sepetinde yer alan ürünlerin Ocak 2005'ten itibaren olan fiyatlarını inceleyebilir, 
                 bir ay önceki, bir yıl önceki ve bulunulan yılın Ocak ayına göre fiyatların yüzdelik değişimlerini görebilir ve 
                 son olarak seçili maddenin son 1 yıl içindeki aylara göre fiyatlarını görüntüleyebilirsiniz.",align="Justify"),
                  div(h5(strong("Geliştiren"),style="color:darkred")),
                  a(h5("Ozancan Özdemir"),href="https://users.metu.edu.tr/ozancan",target="_blank"),
                  p("Orta Doğu Teknik Üniversitesi İstatistik Bölümü Araştırma Görevlisi & Doktora Öğrencisi"),
                  a(h5("Twitter:"),href="https://twitter.com/OzancanOzdemir",target="_blank"),
                  a(h5("Linkedin"),href="https://www.linkedin.com/in/ozancan-özdemir-40b94768/",target="_blank"),
                  div(h5(strong("Soru ve Görüşleriniz İçin")),style="color:darkred"),
                  p("ozancan@metu.edu.tr"),
                  h6("Powered by : "),
                  HTML(paste('<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/RStudio_logo_flat.svg/1280px-RStudio_logo_flat.svg.png" style="width:128px;height:50px;">'))
                )
                )
                
)


# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  
  output$enfPlot <- renderPlotly({
    index <- which(data$Madde.adları==input$urun)
    t <- as.character(data$Tarih)[1:(ncol(data)-2)]
    Tarih<-as.Date(t,format = "%d.%m.%Y")
    Ürün<-as.numeric(data[index,-c(1,2)])
    df<-data.frame(Tarih,Ürün)
    p<-ggplot(df,aes(Tarih,Ürün,group=1))+geom_line()+geom_point(col="darkred",size =0.5)+
      labs(title = paste(input$urun,"Ürününün Ortalama Birim Fiyatının Yıllara Göre Değişimi"),
           y = "Ürün Birim Fiyatı (TL)")+annotate("text", x = df$Tarih[15], y = max(df$Ürün),
                                                 label = paste("Son Tarih:",colnames(data)[ncol(data)])) +theme_bw()
    ggplotly(p, tooltip = "text")
  })
  
  output$table1 <- renderTable({
    index <- which(data$Madde.adları==input$urun)
    t <- as.character(data$Tarih)[1:(ncol(data)-2)]
    Tarih<-as.Date(t,format = "%d.%m.%Y")
    Ürün<-as.numeric(data[index,-c(1,2)])
    biröncekiay<-paste("%",round(((Ürün[length(Ürün)]-Ürün[length(Ürün)-1])*100)/Ürün[length(Ürün)-1],2))
    biröncekiyıl<-paste("%",round(((Ürün[length(Ürün)]-Ürün[length(Ürün)-12])*100)/Ürün[length(Ürün)-12],2))
    yılbası<-paste("%",round(((Ürün[length(Ürün)]-Ürün[length(Ürün)+1-month(Tarih[length(Tarih)])])*100)/Ürün[length(Ürün)+1-month(Tarih[length(Tarih)])],2))
    Değişim<-c("Bir Önceki Aya Göre Değişim","Bir Önceki Yıla Göre Değişim","Yılbaşına Göre Değişim")
    Oran<-c(biröncekiay,biröncekiyıl,yılbası)
    df1<-data.frame(Değişim,Oran)
    df1
  })
  
  output$table2<-renderTable({
    index <- which(data$Madde.adları==input$urun)
    Ürün<-data[index,-c(1,2)]
    Ürün[seq(length(Ürün)-12,length(Ürün),1)]
  })
  
}
shinyApp(ui = ui, server = server)

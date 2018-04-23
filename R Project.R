#Link do Projektu
#https://mateuszprojekt.shinyapps.io/PROJEKT/

#Poczatek Programu:

# sprawdzamy katalog roboczy

getwd()

# ustawiamy nasz katalog roboczy

setwd("./")
getwd()

# sr - suicide rates (bardzo intuicyjne)
# mhg - mental health governance

sr <- read.table("./xmart.csv", header=TRUE, sep=",", skip=2)
mhg <- read.table("./data.csv", header=TRUE, sep=",", skip=1)

names(sr)

# modyfikujemy nazwy "kolumn", tak aby byÅ‚y bardziej czytelne

names(sr) <- c("Country","Both","Female","Male")
head(sr)

# sprawdzamy typ danych

class(sr)

# sprawdzamy typy poszczegÃ³lnych "kolumn"

str(sr)

# modyfikujemy typy poszczegÃ³lnych danych
# konwrsja danych

sr$Country <- as.character(sr$Country)
sr$Male <- as.numeric(as.character(sr$Male))
sr$Female <- as.numeric(as.character(sr$Female))
sr$Both <- as.numeric(as.character(sr$Both))

# sprawdzamy, czy mamy jakieÅ› brakujÄ…ce dane

sum(!complete.cases(sr))

#sr <- sr[complete.cases(sr),]

sr_mean <- mean(sr$Both)
sr_mean
sr[sr$Both>sr_mean,]
sr[sr$Both<sr_mean,]

#Liczba wierszy z wartosciami wiekszymi/mniejszymi od sredniej
#Liczba Kolumn

dim(sr[sr$Both<sr_mean,])
dim(sr[sr$Both>sr_mean,])

# kwantyle, min, mac, mediana, srednia

max(sr$Both)
quantile(sr$Both,na.rm=T, probs=seq(0,1,0.05))
min(sr$Both)
max(sr$Both)
median(sr$Both)
mean(sr$Both)

#####################################
head(mhg)
mhg<- mhg[-2]
mhg<- mhg[-2]
head(mhg)
head(mhg)
names(mhg)
names(mhg) <- c("Country","legislation","plan","policy")
names(mhg)

#zamiana wartosci, dolaczenie kolumn, przypisanie wagi do wprowadzanych dzialan

legislation2 <- as.numeric(mhg$legislation =="Yes")
plan2 <- as.numeric(mhg$plan =="Yes")
policy2 <- as.numeric(mhg$policy =="Yes")

#sum-suma podjetych dzialan (wazona)

sum <- 3*plan2+2*legislation2+4*policy2

#tworzenie nowej tabeli z dodatkowymi kolumnami

MHG <- cbind(mhg, legislation2, plan2, policy2, sum)
head(MHG)

#przypisanie ilosci krajów do zmiennych

LEGISLATION <- sum(MHG$legislation2)
PLAN <- sum(MHG$plan2)
POLICY <- sum(MHG$policy2)

#obliczenie liczby krajów maj¹cych zatwierdzone dane ustawodawstwo 
#null - brak/zero, o - pierwsza kol, t - druga, th - trzecia, - all ....

null <- sum(MHG$sum == "0")
o8t <- sum(MHG$sum =="5")
t8th<- sum(MHG$sum =="7")
th8o <- sum(MHG$sum =="6")
all <- sum(MHG$sum == "9")


danekolowe<-data.frame(Choices=NA,
                       SUM=NA
                       
)[0,]
danekolowe


danekolowe[1,1] <- "Legislation"
danekolowe[2,1] <- "Policy"
danekolowe[3,1] <- "Plan"
danekolowe[1,2] <- sum(legislation2)
danekolowe[2,2] <- sum(policy2)
danekolowe[3,2] <- sum(plan2)

str(danekolowe)

#Ladowanie rozszerzonych danych do mapy

Dane_mapa <- read.table("./test.csv", header=TRUE, sep=",", skip=1)

infomap<-data.frame(Panstwo=NA,
                    Srednia=NA,
                    ID=NA
)[0,]

infomap

Dane_mapa[13]
Dane_mapa[19]
Dane_mapa[14]
head(Dane_mapa)
str(head(Dane_mapa))

# Import do infomap danych o Panstwach

for( i in 1:nrow(Dane_mapa)) {
  
  if(i*3 > 512) break
  
  df <- data.frame(
    Panstwo=Dane_mapa[i*3,"Afghanistan"],
    Srednia=Dane_mapa[i*3,"X5.7"],
    ID=Dane_mapa[i*3,"AFG"]
  )
  infomap<- rbind(infomap, df)
}

#Zmienne zwiazane z charakterystyka legendy

ZmiennaHist <- list(
  family = "sans-serif",
  size = 12,
  color = "#000"
)
LegendaHist <- list(
  font = ZmiennaHist,
  x = 1.3,
  y = 1.0,
  bgcolor = "#E2E2E2",
  bordercolor = "#FFFFFF",
  borderwidth = 2
)

# Gestosc dla danych (Liczba samobojstw obojga plci)

fit_density <- density(sr$Both)

Kobiety <- sr$Female
Mezczyzni <- sr$Male

sr2 <- sr
names(sr2) <- c("Panstwo","Razem","Kobiety","Mezczyzni")
sr2 <- sr2[-5]
str(sr2)

SredniaK <- mean(sr2$Kobiety)
SredniaM <- mean(sr2$Mezczyzni)

model <- lm(Mezczyzni ~ Kobiety)

LiniaRegresji <- paste("Rownanie Prostej Linii Regresji: ", "y = ",round(model$coefficients[2],2)," * x + ", round(model$coefficients[1],2))
#linia wokol ktorej ustawiaja sie dane
LiniaRegresji
Korelacja <- paste("Wspolczynnik korelacji: ", round(cor(Mezczyzni, Kobiety),2))
#Jak bardzo dane sa od siebie liniowo zalezne
#>Korelacja dodatnia oznacza, ¿e wraz z wzrostem wartoœci jednej cechy 
#>nastêpuje wzrost wartoœci drugiej, przy czym wspó³czynnik korelacji przyjmuj¹cy wartoœæ 1 oznacza 
#>najsilniejsz¹ korelacjê dodatni¹. Natomiast korelacjê ujemn¹ mo¿emy interpretowaæ, ¿e wraz z 
#>wzrostem wartoœci jednej cechy nastêpuje spadek wartoœci drugiej. Wspó³czynnik korelacji równy -1 
#>oznacza najsilniejsz¹ korelacjê ujemn¹. Wartoœæ wspó³czynnika równa 0 oznacza, ¿e zmienne nie s¹ ze sob¹ w ¿aden sposób powi¹zane. 
Korelacja

#Implementacja Interfejsu Shiny

library(shiny)
library(plotly)

ui <- fluidPage(
  
  h1("Zdrowie psychiczne na Swiecie", align = "center",style = " color: grey"),
  h2("1> Samobojstwa na Swiecie", align = "center",style = " color: grey"),
  #Formatowanie:
  #style = " color: grey; font-family: 'Lobster', cursive"
  
  fluidRow(
    column(12,"",
           
           br(),
           br(),
           
           fluidRow(column(width = 8, plotlyOutput("Mapa", width = "150%"))),
           
           br(),
           br(),
           
           fluidRow(column(width = 8, plotlyOutput("Histogramy", width = "150%", height = "500px"))),
           br(),
           br(),
           br(),
           
           fluidRow(column(width = 8, plotlyOutput("Wykres", width = "150%"))),
           br(),
           h4(LiniaRegresji, align = "center",style = "color: red"),
           h4(Korelacja, align = "center",style = "color: red"),
           #Formatowanie:
           #style = " font-family: 'times'; font-style: italic; color: red"
           br(),
           br(),
           
           fluidRow(column(width = 8, plotlyOutput("Slupki", width = "150%"))),
           
           br(),
           br()
    )
  ),
  
  h2("2> Dzialania Rzadowe dotyczace zdrowia psychicznego", align = "center", style = " color: grey; font-family: 'Lobster', cursive"),
  
  br(),
  br(),
  selectInput(inputId = 'Choices', 
              
              width = "50%",
              
              label='Wybor Podjetych Dzialan', 
              
              choices = sort(names(table(danekolowe$Choices))),
              
              selected = "Plan"
  ),
  
  br(),
  
  fluidRow(column(width = 8, plotlyOutput("Ustawy", width = "150%", height = "700px"))),
  
  fluidRow(column(width = 8, plotlyOutput("Podzial", width = "150%", height = "500px"))),
  
  fluidRow(column(width = 8, plotlyOutput("UstawyTrzy", width = "150%", height = "500px")))
)

server <- function(input, output){
  
  output$Histogramy <- renderPlotly({
    
    plot_ly(x = sr$Female,  type = "histogram", name = "Histogram P(x) Kobiet", showlegend = TRUE) %>%
      
      add_trace(x = sr$Male, opacity = 0.6, type = "histogram",name = "Histogram P(x) Mezczyzn")  %>%
      
      add_trace(x = fit_density$x, y = fit_density$y,opacity = 2, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Gestosc Rozkladu") %>% 
      
      layout(title = "Histogram liczby samobojstw", xaxis = list(title = "Liczba samobojstw na 100 000"),
             yaxis = list(title = "Liczba krajow"),barmode="overlay", yaxis2 = list(overlaying = "y", side = "right"),
             legend = LegendaHist)
  })
  output$Ustawy <- renderPlotly({
    
    plot_ly(labels = c("TAK","NIE"), values = c(danekolowe[["SUM"]][danekolowe$Choices==input$Choices],182- danekolowe[["SUM"]][danekolowe$Choices==input$Choices]), type = "pie", domain = list(x = c(0.08, 1), y = c(0.2, 0.8)), 
            name = "Prawo", showlegend = T)
    
    layout(title = "Wykres przedstawiajacy liczbe krajow, w ktorych zostaly podjete wybrane dzialania rzadowe")
  })
  
  output$Podzial <- renderPlotly({
    
    ds <- data.frame(labels = c("Liczba krajow nieposiadajach zadnego ustawodawstwa", "Liczba krajow nieposiadajacych zatwierdzonej polityki", "Liczba krajow nieposiadajacych zatwierdzonego ustawodawstwa", "Liczba krajow nieposiadajacych zatwierdzonego planu", "Liczba krajow majaca wszystkie dzialania zatwierdzone"),
                     values = c(null, o8t, t8th, th8o, all))
    
    plot_ly(ds, labels = labels, values = values, type = "pie",domain = list(x = c(0.08, 1), y = c(0.1, 0.9))) %>% 
      layout(title = "Podzial wg. wprowadzonego ustawodawstwa dot. zdrowia psychicznego")
  })
  
  output$UstawyTrzy <- renderPlotly({
    
    ds2 <- data.frame(labels = c("Prawo zdrowia psychicznego", "Wdrozenie prawa zdrowia psychicznego", "Obowiazujace przepisy dot. zdrowia psychicznego"),
                      values = c(LEGISLATION,PLAN,POLICY))
    
    plot_ly(ds2, labels = labels, values = values, type = "pie",domain = list(x = c(0.08, 1), y = c(0.1, 0.9))) %>% 
      layout(title = "Ustawodawstwo zdrowia psychicznego wsrod badanych krajow")
  })
  
  output$Wykres <- renderPlotly({
    
    ggplot(sr2, aes(x = Kobiety, y = Mezczyzni, fill = Panstwo)) +
      scale_colour_continuous(guide = FALSE) +
      
      geom_point(shape=1, color = "red", size = 2) +
      
      labs(title = "Wykres zaleznosci samobojstw pomiedzy Kobietami a Mezczyznami", x = "Liczba Samobojstw Kobiet", y = "Liczba Samobojstw Mezczyzn") +
      
      theme(axis.title.y = element_text(face="bold", colour="#990000", size=15),
            axis.title.x = element_text(face="bold", colour="#990000", size=15),
            axis.text.x = element_text(colour = "#ff6666", size = 10), 
            axis.text.y = element_text(colour = "#668cff", size = 10)) +
      
      theme(legend.position="none") +
      
      theme(panel.background = element_rect(fill = 'grey75')) +
      
      geom_smooth(method=lm,fill = "blue" ,size=0.5)   # Implementacja linii regresji
    
    library(ggplot2)
    ggplotly()
  })
  
  output$Slupki <- renderPlotly({
    
    plot_ly(
      
      x = c("Srednia Kobiet", "Srednia Mezczyzn"),
      y = c(SredniaK, SredniaM),
      showlegend = FALSE,
      type = "bar",
      marker = list(color = toRGB("green"))
    )
    layout(title = "Sredni odsetek samobojstw", xaxis = list(title = "Podzial na plec"),
           yaxis = list(title = "Liczba zgonow"))
  })
  
  output$Mapa <- renderPlotly({
    
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # Ustaiwenia dla renderu mapy
    g <- list(
      showframe = TRUE,
      showcoastlines = TRUE,
      projection = list(type = 'Mercator')
    )
    
    plot_ly(infomap, z = Srednia, text = Panstwo, locations = ID, type = 'choropleth',
            colors = 'Reds',marker = list(line = l),colorbar = list(tickprefix = '>', title = 'Natezenie Samobojstw 1:100 000')) %>%
      
      layout(title = "Mapa przedstawiajaca Swiat w obliczu popelnianych samobojstw przez ludzi",
             geo = g)
  })
}
shinyApp(ui=ui, server=server)
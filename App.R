library(shiny)
library(shinythemes)
library(data.table)
library(ggthemes)
library(dplyr)
library(lubridate)
library(plotly)
data<- fread("https://raw.github.com/datasets/covid-19/master/data/countries-aggregated.csv",
  header = FALSE, stringsAsFactors = FALSE)
data_day <- tail(data$V1,1)
month <- as.character(month(data_day,label = TRUE))
day_lub <- as.character(day(data_day))
day_lub <- paste(day_lub,",",sep="")
dow <- as.character(wday(data_day,label = TRUE))
year <- as.character(year(data_day))

pays <- c("Benin","Burkina Faso",
          "Cabo Verde","Cote d'Ivoire",
          "Gambia","Ghana","Guinea",
          "Guinea-Bissau","Liberia",
          "Mali","Mauritania",
          "Niger","Nigeria","Senegal",
          "Sierra Leone","Togo")
lang <- c(rep("Francophone",2),"Lusophone","Francophone",rep("Anglophone",2),
          "Francophone","Lusophone",
          "Anglophone",rep("Francophone",3),
          "Anglophone","Francophone",
          "Anglophone","Francophone")
pop <- read.csv("Tidy_pop.csv")
WA <- data.frame()
for (i in 1:16){
  choice <- filter(data,V2 ==paste(pays[i]))
  choice <- filter(choice, V3 >0)
  choice[3:5] <- lapply(choice[3:5],as.integer)
  names(choice) <- c("Date","Country","Cases",
                     "Recovered","Deaths")
  choice$Pop2020 <- pop$Pop2020[i]
  choice$dpc_percent <- round((choice$Deaths/choice$Cases)*100,1)
  choice$cpht <- round(choice$Cases/(choice$Pop2020/1e2),2)
  choice$dpht <- round(choice$Deaths/(choice$Pop2020/1e2),2)
  choice$Region <- "West Africa"
  choice$Language <- lang[i]
  choice$Day <- seq(1,nrow(choice),1)
  WA <- rbind(WA,choice)
}  
WA <- WA[,c(1,12,10,2,11,6,3:5,7:9)]
all_regions <- data.frame(unique(WA$Language))
colnames(all_regions) <- "region"
all_regions <- arrange(all_regions,region)
all_regions <- as.vector(t(all_regions))

ui <- navbarPage(
  
  titlePanel(title=div(img(src="virus.png",
      width="40px",height="40px"),
      " COVID-19 in West Africa ")
      ),
  theme = shinytheme("darkly"),
  
  navbarMenu(
    "Videos",
    tabPanel(title = "COVID-19 in Africa",
             htmlOutput("video_1a")),
    tabPanel(title = "Corona Virus: Country Policies",
             htmlOutput("video_1b")),
    tabPanel(title = "Ventilators",
             htmlOutput("video_1c")),
    tabPanel(title = "Essais de Vaccin en Afrique",
             htmlOutput("video_1d"))
  ),
  
  navbarMenu(
    "Plots",
    tabPanel(
      title = "Barplots",
      uiOutput("Choose_region"),
      plotlyOutput("plot1", height = 500)),
    
    tabPanel(
      title = "Boxplots",
      plotlyOutput("plot2", height = 500)),
    
    tabPanel(
      title = "CountryTrends",
      plotOutput("plot3"))
    
) )

server = function(input, output, session) {
  output$Choose_region <- renderUI({
    selectInput("select",
        "Select a Region",
        choices = all_regions,
        selected = all_regions[1])
  })

  get_data <- reactive({
    region_selected = input$select
    region_data = subset(WA,
    Date == tail(Date,1) &
      Language == region_selected)
    return(region_data)
  })
  
get_data2 <- reactive({
    region_selected = input$select
    region_data2 = subset(WA,
    Language == region_selected)
    return(region_data2)
  })


output$plot1 <- renderPlotly({
    region_data = get_data()

  p <- ggplot(region_data,
        aes(x=reorder(Country,Cases),
        y=Cases,
        cases=Cases,deaths=Deaths,
        cases_pht= cpht,
        d_pc = dpc_percent)) +
      labs(title = paste("West Africa Confirmed Cases --",dow,month,day_lub,year,
         subtitle = "\ncpht=Cases per 100,000 -- dpc_percent = Deaths per Case (%)")) +
      xlab("") +
      geom_bar(stat='identity', color="black", fill="blue") +
      coord_flip()
    ggplotly(p,tooltip = c("country",
                            "cases",
                           "Deaths",
                           "cpht",
                           "d_pc"))
 })
  
output$plot2 <- renderPlotly({
  region_data = get_data()
  
p <- ggplot(region_data, 
          aes(x="",y=cpht,
          country = Country,
          cases = Cases,
          deaths = Deaths,
          d_pht = dpht)) +
    geom_boxplot()+
    
labs(title = paste("John Tukey's Boxplot --",dow,month,day_lub,year,
subtitle = "\ncpht=Cases per 100,000 -- dpht = Deaths per 100,000")) +
   ylab("Cases per 100,000") +
   xlab(paste(region_data$Language,
       "Countries: Mean Cases per 100,000 = ",round(mean(region_data$cpht)),2)) +
        geom_jitter(shape=16, 
        position=position_jitter(0.01),
        color="blue")
ggplotly(p,tooltip = c("Country",
                       "Cases",
                       "cpht",
                      "Deaths",
                      "dpht"))
})
  
output$plot3 <- renderPlot({
    region_data2 = get_data2()
  p <- ggplot(region_data2, 
      aes(x = Day,y=Cases, 
      group=Country,country=Country,
      cases=Cases,deaths = Deaths)) +
    geom_line(aes(color=Country),size=2) +
    
    labs(title = paste("COVID-19: Propagation in West Africa",dow,month,day_lub,year,
      subtitle = "\nConfirmed Cases")) +
     theme(plot.title = element_text(size = 20))+ 
    xlab("Days Since First Cases Confirmed")+
    ylab("Total Cases") +
    theme_economist() 
p
}) 
  
  output$video_1a <- renderUI({
    tags$iframe(width = "1000",
                height = "500",
                src = "https://www.youtube.com/embed/yDowVv6zVTo")
  })
  
  output$video_1b <- renderUI({
    tags$iframe(width = "1000",
                height = "500",
                src ="https://www.youtube.com/embed/SpTDUy2zceY")
  })
  
  output$video_1c <- renderUI({
    tags$iframe(width = "1000",
                height = "500",
                src = "https://www.youtube.com/embed/K-tH-HbZ7E8")
  })
  
  output$video_1d <- renderUI({
    tags$iframe(width = "1000",
                height = "500",
                # src = "https://www.youtube.com/embed/INmIcOPKo_M")
                src = "https://www.youtube.com/embed/lFFGhPk2S_4")
  })
 

}
shinyApp(ui = ui, server = server)

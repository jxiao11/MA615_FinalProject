library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(dplyr)
library(gtrendsR)
library(reshape2)
library(maps)
library(rvest)
library(scales)
library(stringr)
library(readtext)
library(wordcloud)
library(data.table)
library(rworldmap)
library(ggpubr) ### arrangle mutiple plot into one page
library(RCurl)  ### read online html table 
library(rlist)
library(XML)
library(magick)



#################### Deal with data, get ready for shiny######################


#### Part1 Google Trend Data####
google.trends = gtrends(c("gun control"), gprop = "web", time = "all")[[c(1)]]
google.trends2 = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends2) = google.trends$date

colnames(google.trends2)[colnames(google.trends2)=="gun control_world"] <- "popularity"

google.trends_region <- gtrends(c("gun control"), gprop = "web", time = "all")[[c(2)]]

google.trends_region <- google.trends_region[-c(192,210),]
google.trends_region <- dcast(google.trends_region, location ~ keyword + geo, value.var = "hits")
colnames(google.trends_region)[colnames(google.trends_region)=="gun control_world"] <- "popularity"


### Part1 World heatMap Data ####
world_map <- maps::map("world2", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify()

google.trends_region$location <- recode(google.trends_region$location
                                        ,"United States"= 'usa' 
                                        ,'United Kingdom' = 'uk'
)
google.trends_region$country <- tolower(google.trends_region$location)

##################Load Part 2 Data Gun Distribution#############################
#### Number of guns per 100 capita by country ####

theurl <- getURL("https://en.wikipedia.org/wiki/Estimated_number_of_guns_per_capita_by_country",.opts = list(ssl.verifypeer = FALSE) )
tables <- readHTMLTable(theurl)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)

Gun_per_capital <- tables[[1]]
Gun_per_capital3<-Gun_per_capital %>%
  select(-Rank,-Notes)
colnames(Gun_per_capital3)[2] <- "guns_per_100"
Gun_per_capital3$guns_per_100<-as.vector(Gun_per_capital3$guns_per_100)
x <- strsplit(Gun_per_capital3$guns_per_100, '[', fixed = T)
Gun_per_capital3$guns_per_100 <- sapply(x, function(y){y[1]})
Gun_per_capital3<-Gun_per_capital3[-1,]

Gun_per_capital3$Country<-as.character(Gun_per_capital3$Country)
Gun_per_capital3$guns_per_100<-as.numeric(Gun_per_capital3$guns_per_100)




##### Part3 tidy text data #####
fileText2 <- readtext("Trump_2018.txt")
fileText <- readtext("Obama_2013.txt")

tidy_books_obama <- data_frame(text = fileText$text) %>% unnest_tokens(word, text)
tidy_books_trump <- data_frame(text = fileText2$text) %>% 
  unnest_tokens(word, text)%>% 
  filter(word !="trump")









##### Part 4 Mass Shooting Stastics Data #####
df1 <-fread("Mass Shootings Dataset Ver 5.csv")
data<-fread("Mass Shootings Dataset Ver 5.csv")
##############################################
##### Data for creating database

df2 <- data %>%
  select(Date,Title,Location,Fatalities,Injured)


##############################################
##### with include Las Vegas Shooting
df2$Date <- as.Date(df2$Date,"%m/%d/%Y")
df3 <- df1 %>% 
  select(Date,Fatalities,Injured)
df3$Date <- as.Date(df3$Date,"%m/%d/%Y")

df4 <- df3 %>% 
  gather(key ="Type",value = "Individual",-Date)
##############################################
##### No include Las Vegas Shooting

df5 <- subset(df1,Title != "Las Vegas Strip mass shooting") 

df6 <- df5 %>%
  select(Date,Fatalities,Injured)
df6$Date <- as.Date(df6$Date,"%m/%d/%Y")
df7 <- df6 %>% 
  gather(key ="Type",value = "Individual",-Date)
############################################


ui <- dashboardPage(
  dashboardHeader(title = "Final Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gun Control",icon=icon("heart"),tabName = "Part1"),
      menuItem("Gun Distribution",icon = icon("heart"),tabName = "Part2"),
      menuItem("Text Analysis",icon = icon("heart"),tabName = "Part3"),
      menuItem("Mass Shooting Stastics",icon = icon("heart"),tabName = "Part4",
               menuSubItem("Death/Injured Stastics",tabName = "Part4a"),
               menuSubItem("Database",tabName = "Part4b"))
      
      
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Part1",
              fluidRow(
                box(title = "Google Trend: US Gun Control topic populairty",width = 6,plotOutput("global_popularity")),
                box(title = "Google Trend: World Gun Control topic popularity",plotOutput("Map_plot"),width = 6),
                box(title = "Interpretation","This Time series plot show the US gun control topic popularity in Past 15 years,
                    I was wondering why there are two high peaks in year 2013 and 2018. Then, I found there are two mass shooting happened
                    in these years, and President Obama and Trump gave a speech after these two mass shootings. Futuremore, I did a text anylysis 
                    for their speeches"
                ),
                box(title ="Interpretation" ,"This world heat map represent the Google Trend gun contorl topic popularity in worldwide. As we can see US gun control topic 
                    has the hightes popularity. I think  the Us control is a topic worth to study."
                ))
              
              
              
              
                ),
      tabItem(tabName = "Part2",
              fluidRow(
                box(title = "Map of civilian guns per 100 capita by country",width = 12,HTML('<center><img src="Rplot.png"></center>'),
                    box(title ="Interpretation",status = "success",width = NULL,
                        helpText(a("Small Army Survey",href = "https://en.wikipedia.org/wiki/Small_Arms_Survey"), "provides an estimate of the total number 
                                 of known civillan owned guns in a country per 100 people. As we can see 
                                 United States has most gun per 100 capita, and almost double than the second most"))
                        )
                        )),
      
      tabItem(tabName = "Part3",
              fluidRow(
                tabBox(height = "250px",
                       tabPanel("Obama Talk",plotOutput("PosNeative1"),
                                box(title = "Interpretation",status = "success",width = NULL,
                                    helpText("I dont't think President Obama did a good job about reducing mass shootings or gun control,
                                             Obama have done 14 speeches after 14 mass shootings. In my opinion, he was trying to complete his 
                                             duties rather than solving the most fundamental problem."))),
                       tabPanel("Obama Pos/Nea Analysis",plotOutput("Obama_Talk")),
                       tabPanel("Obama Word freq",plotOutput("frequency1"))),
                
                tabBox(height = "250px",
                       tabPanel("Trump Talk,",plotOutput("PosNeative2"),
                                box(title = "Interpretation",status = "success",width = NULL,
                                    helpText("President Obama did a better job than Trump, As we can see Trump did mention less Guns or Gun control 
                                             in his response after shooting. Basically, all his talks is about blaming-victim. He never address gun problem. "))),
                       tabPanel("Trump POS/Nea Analysis",plotOutput("Trump_Talk")),
                       tabPanel("Trump Word freq",plotOutput("frequency2"))
                                    )
                                )
                       ),
      
      
      
      
      tabItem(tabName = "Part4a",
              fluidRow(
                tabBox(width = 12,
                       tabPanel(title = "Mass Shooting",width = 12,plotOutput("Mass_Shooting_Stastics"),
                                box(title = "Interpretation",status = "success",width = NULL,
                                    helpText("This plot represent the Fatalities and injured in US mass shooting in past 50 years. The first plot include Las Vegas shooting but not 
                                             include in second plot inorder to provide a better statistics view.", br(), "We can see the Mass shooting happens more frequently
                                             after 2007 and more people are injured and dead in shooting after 1982"
                                             
                                             
                                    ))),
                       
                       
                       
                       tabPanel(title = "Histogram",width=12,plotOutput("histogram"),
                                box(title = "Interpretation",status = "success",width = NULL,
                                    helpText("This Histogram provide a stattistics representation of the causes in US mass shooting in past 50 years.
                                             Terroism is one of the main cause in mass shooting, but I do not want to disscuss it because which relate to 
                                             some potilics problems. Other than that, the main causes of mass shooting are psycho and anger. In my opinion, there is 
                                             a clearly link between mass shooting and mental ilness")))
                                    )
                                    )
                                    ),
      tabItem(tabName = "Part4b",
              fluidRow(
                column(4,
                       selectInput("Title",
                                   "US Mass Shooting Database",
                                   c("All",
                                     unique(as.character(df2$Title))))
                )
              ),
              # Create a new row for the table.
              fluidRow(
                DT::dataTableOutput("table")
              )
      )
                )
              )
              )





server <- function(input, output) {
  df1 <-fread("Mass Shootings Dataset Ver 5.csv")
  data<-fread("Mass Shootings Dataset Ver 5.csv")
  
  
  output$global_popularity <- renderPlot({
    ggplot(data = google.trends2, aes(date,popularity,color = "green"))+
      geom_line()+
      ggtitle("gun control google popularity")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(legend.position = "none")+
      scale_x_date(
        limits = as.Date(c("2004-01-01", "2018-05-01")),
        date_labels = "%Y",
        date_breaks = "3 years")
    
    
    
    
  })
  output$Map_plot <- renderPlot({
    
    
    country<- joinCountryData2Map(google.trends_region, joinCode = "NAME", nameJoinColumn = 'country', verbose = T)
    mapCountryData(country, nameColumnToPlot="popularity", mapTitle="Gun_Control Topic World Popularity")
    
    
    
  })
  


    

    


  
  
  output$PosNeative1<- renderPlot({
    
    bing_word_counts_obama <- tidy_books_obama %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    
    bing_word_counts_obama %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()  
    
  })
  
  output$PosNeative2<- renderPlot({
    
    bing_word_counts_trump <- tidy_books_trump %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    
    bing_word_counts_trump %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
    
  })
  output$Obama_Talk<- renderPlot({
    
    
    tidy_books_obama %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 100)
    
    
    
  })
  
  output$Trump_Talk<- renderPlot({
    
    
    
    tidy_books_trump %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 100)
    
    
    
  })
  output$frequency1<- renderPlot({
    tidy_books_obama1 <- tidy_books_obama %>%
      anti_join(stop_words)%>%
      count(word,sort = TRUE)
    wordcloud(words = tidy_books_obama1$word, freq = tidy_books_obama1$n,scale = c(5,0.1), min.freq = 1,
              max.words=70, random.order=FALSE, rot.per=0.2, 
              colors=brewer.pal(8, "Dark2"))
    
    
  })
  output$frequency2<- renderPlot({
    tidy_books_trump1 <- tidy_books_trump %>%
      anti_join(stop_words)%>%
      count(word,sort = TRUE)
    wordcloud(words = tidy_books_trump1$word, freq = tidy_books_trump1$n,scale = c(5,0.1), min.freq = 1,
              max.words=70, random.order=FALSE, rot.per=0.2, 
              colors=brewer.pal(8, "Dark2"))
    
  })
  
  
  output$Mass_Shooting_Stastics <- renderPlot({
    g1 <- ggplot(data = df4, aes(Date, Individual, color = Type),na.rm = T) +
      geom_line()+ 
      ggtitle("Mass Shooting in US ")+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_x_date(
        limits = as.Date(c("1965-01-01", "2018-05-01")),
        date_labels = "%Y",
        date_breaks = "5 years")+
      theme(legend.position=c(.1,.85))
    g2<-ggplot(data = df7, aes(Date, Individual, color = Type),na.rm = T) +
      geom_line()+ 
      ggtitle("Mass Shooting in US/ No include Las_Vegas Shooting ")+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_x_date(
        limits = as.Date(c("1965-01-01", "2018-05-01")),
        date_labels = "%Y",
        date_breaks = "5 years")+
      theme(legend.position=c(.1,.85))	
    ggarrange(g1,g2,ncol = 1,nrow = 2,labels = c("A","B"))
    
    
  })
  
  output$histogram<- renderPlot({
    
    
    
    df_Victim <- aggregate(data.frame(count = df1$Cause), list(value = df1$Cause), length)
    df_victim2 <- df_Victim[-1,]
    colnames(df_victim2)[1] <- "cause"
    
    ggplot(data=df_victim2, aes(x=cause, y= count,fill=cause)) +
      geom_bar(stat="identity")+
      theme(axis.text.x = element_text(angle=65, vjust=0.6,size = 13))+
      ggtitle("Type of Causes in US Mass Shootings")+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    
  })
  output$table <- DT::renderDataTable(DT::datatable({
    
    if (input$Title != "All") {
      df2 <- df2[df2$Title == input$Title,]
    }
    df2
  }))
  
  output$value <- renderText({ input$caption })
  
  
  
  
}

shinyApp(ui, server)




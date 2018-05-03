library(tidyverse)
library(tidytext)
library(gtrendsR)
library(reshape2)
library(maps)
library(rvest)
library(scales)
library(readtext)
library(wordcloud)
library(data.table)
library(rworldmap)
library(ggpubr) ### arrangle mutiple plot into one page
library(RCurl)  ### read online html table 
library(rlist)
library(XML)

### get google trends gun control popularity over time  data 
google.trends = gtrends(c("gun control"), gprop = "web", time = "all")[[c(1)]]

google.trends2 = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends2) = google.trends$date

colnames(google.trends2)[colnames(google.trends2)=="gun control_world"] <- "popularity"

### Timeseries plot of gun control popularity over time 

ggplot(data = google.trends2, aes(date,popularity,color = "green"))+
  geom_line()+
  ggtitle("gun control google popularity")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(
    limits = as.Date(c("2002-01-01", "2018-01-01")),
    date_labels = "%Y",
    date_breaks = "2 years")+
  theme(legend.position = "none")


### get goold trends gun control popularity over region data
google.trends_region <- gtrends(c("gun control"), gprop = "web", time = "all")[[c(2)]]

google.trends_region <- google.trends_region[-c(192,210),]
google.trends_region <- dcast(google.trends_region, location ~ keyword + geo, value.var = "hits")
colnames(google.trends_region)[colnames(google.trends_region)=="gun control_world"] <- "popularity"


### load world map
world_map <- maps::map("world2", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify()

google.trends_region$location <- recode(google.trends_region$location
                                      ,"United States"= 'usa' 
                                      ,'United Kingdom' = 'uk'
)
google.trends_region$country <- tolower(google.trends_region$location) 


#### plot ###

country<- joinCountryData2Map(google.trends_region, joinCode = "NAME", nameJoinColumn = 'country', verbose = T)
mapCountryData(country, nameColumnToPlot="popularity", mapTitle="Gun_Control Topic World Popularity")


#### Number of guns per 100 capita by country ####

#### lOad data online #### 
### this is a small file, no loading issue####
theurl <- getURL("https://en.wikipedia.org/wiki/Estimated_number_of_guns_per_capita_by_country",.opts = list(ssl.verifypeer = FALSE) )
tables <- readHTMLTable(theurl)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)

### clean data ###
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


#### plot world heat map using rworldmap package ####
country2<- joinCountryData2Map(Gun_per_capital3, joinCode = "NAME", nameJoinColumn = 'Country', verbose = T)

colourPalette <- brewer.pal(7,'Set3')
mapParams <- mapCountryData(country2,
                            nameColumnToPlot="guns_per_100",
                            addLegend=FALSE,
                            catMethod = "logFixedWidth",
                            colourPalette= colourPalette,
                            mapTitle = "Guns Per 100 Capita by Country")

do.call(addMapLegend
        ,c(mapParams
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 2))


#### sentiment analysis ####

fileText <- readtext("Obama_2013.txt")
fileText2 <- readtext("Trump_2018.txt")
tidy_books_obama <- data_frame(text = fileText$text) %>% unnest_tokens(word, text)
tidy_books_trump <- data_frame(text = fileText2$text) %>% 
  unnest_tokens(word, text)%>% 
  filter(word != "trump")%>% 
  filter(word != "president")
  

##### Postive and Negative Words Bar plot and Word Cloud Analysis

bing_word_counts_obama <- tidy_books_obama %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


bing_word_counts_trump <- tidy_books_trump %>%
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


#### Resize windown.#####
#### otherwise, next positve negative wordcloud plot wont fit ####
#### you may or may not need to call this function ####
resize.win <- function(Width=6, Height=6)
  
{
  # works for windows
  dev.off(); # dev.new(width=6, height=6)
  windows(record=TRUE, width=Width, height=Height)
}
resize.win(4,4)

##############################


#### world colud anaylis.

tidy_books_obama %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

resize.win(4,4)

tidy_books_trump %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

####################################################
####Word frequency Analysis ##### Wordcloouds

tidy_books_obama1 <- tidy_books_obama %>%
  anti_join(stop_words)%>%
  count(word,sort = TRUE)

tidy_books_trump1 <- tidy_books_trump %>%
  anti_join(stop_words)%>%
  count(word,sort = TRUE)

wordcloud(words = tidy_books_obama1$word, freq = tidy_books_obama1$n,scale = c(4,0.1), min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))


wordcloud(words = tidy_books_trump1$word, freq = tidy_books_trump1$n,scale = c(4,0.1), min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

###########################################################################

######## MASS Shooting in US #######
########
df1<-fread("Mass Shootings Dataset Ver 5.csv")
df2 <- df1 %>%
  select(Date,Fatalities,Injured)
df2$Date <- as.Date(df2$Date,"%m/%d/%Y")
df4 <- df2 %>% 
  gather(key ="Type",value = "Individual",-Date)

g1 <- ggplot(data = df4, aes(Date, Individual, color = Type),na.rm = T) +
  geom_line()+ 
  ggtitle("Mass Shooting in US ")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(
  limits = as.Date(c("1965-01-01", "2018-05-01")),
  date_labels = "%Y",
  date_breaks = "5 years")+
  theme(legend.position=c(.1,.85))	


#### Las Vegas shooting cause a extremly high death and injured, Let's remove this row to see how the result looks like.

df5 <- subset(df1,Title != "Las Vegas Strip mass shooting") 
df6 <- df5 %>%
  select(Date,Fatalities,Injured)
df6$Date <- as.Date(df6$Date,"%m/%d/%Y")

df7 <- df6 %>% 
  gather(key ="Type",value = "Individual",-Date)

g2<-ggplot(data = df7, aes(Date, Individual, color = Type),na.rm = T) +
  geom_line()+ 
  ggtitle("Mass Shooting in US/ No include Las_Vegas Shooting ")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(
    limits = as.Date(c("1965-01-01", "2018-05-01")),
    date_labels = "%Y",
    date_breaks = "5 years")+
  theme(legend.position=c(.1,.85))	

### Bind this two plot to one ###
ggarrange(g1,g2,ncol = 1,nrow = 2,labels = c("A","B"))



### build a histogram of Victom Type###

df_Victim <- aggregate(data.frame(count = df1$Cause), list(value = df1$Cause), length)
df_victim2 <- df_Victim[-1,]
colnames(df_victim2)[1] <- "cause"

ggplot(data=df_victim2, aes(x=cause, y= count,fill=cause)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6,size=12))+
  ggtitle("Type of Causes in US Mass Shootings")+
  theme(plot.title = element_text(hjust = 0.5))






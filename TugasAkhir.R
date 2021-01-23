##R Global
#library
library(SnowballC)
library(twitteR)
library(tm)
library(NLP)
library(rtweet)
library(SentimentAnalysis)
library(dplyr)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(vroom)
library(here)
library(e1071)
library(caret)
library(syuzhet)
library(RTextTools)
library(wordcloud2)
library(shiny)
library(plotly)

#mengambil data twitter
reqURL <- "http://api.twitter.com/oath/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
CUSTOMER_KEY <- "vPoDCU9994ZZUsEHFf5p8QrBV" 
CUSTOMER_SECRET <- "36r659jBumNUURjefZIBLc2Kz1kV3ozd22hOabFgWna8zNkvY8" 
ACCESS_TOKEN <- "313826433-eEv2aWHNeF0krqHkDc2BwWYhvgdLKPd80zI31vTX" 
ACCESS_secret <- "jv45289JTKP0lTkKjyZNShP7SCmtCvkeVVktt0C0Yk5gb" 
setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret)
Datatweets <- searchTwitter('COVID19', n=1000, lang="id",retryOnRateLimit = 10e3)
#save data mentah
saveRDS(Datatweets,file = 'dataMentah.rds')

#load data set
Datatweets<-readRDS('dataMentah.rds')
covid=twListToDF(Datatweets)
View(covid)

#membersihkan hal yang tidak penting
komen <- covid$text
#hanya ambil tweet saja
komenc <- Corpus(VectorSource(komen))
removeURL <-function(x) gsub("http[^[:space:]]*", "", x)
twitclean <-tm_map(komenc,removeURL)

removeRT<-function(y) gsub("RT", "", y)
twitclean<-tm_map(twitclean,removeRT)

removeUN<-function(z) gsub("@\\w+", "", z)
twitclean<-tm_map(twitclean,removeUN)

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)

twitclean<-tm_map(twitclean, removePunctuation)
twitclean<-tm_map(twitclean, tolower)

## save data
dataframe<-data.frame(text=unlist(sapply(twitclean,'[')),stringsAsFactors = F)
View(dataframe)
write.csv(dataframe , "data_bersih.csv")


# R Ui
twitter<- vroom(here("data_bersih.csv"))
tweet<- twitter$text
ui <- fluidPage(
  titlePanel("SENTIMENT ANALISIS CoVid19 menurut netizen"),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Emotion", plotOutput("emotion")), 
                # Plot
                tabPanel("Data Clear", DT::dataTableOutput('tbl')), 
                # Output Data Dalam Tabel
                tabPanel("Wordcloud", plotOutput("Wordcloud"))
    )
  )
)
#Data Tabel
server <- function(input, output) {
  #///Output Data
  output$tbl = DT::renderDataTable({
    DT::datatable(twitter, options = list(lengthChange = FALSE))
  })
## Membuat Analisis Sentimen
  output$emotion <- renderPlot({cleancoment<-read.csv("data_bersih.csv",stringsAsFactors = FALSE)  
review <-as.character(cleancoment$text)
get_nrc_sentiment('happy')
get_nrc_sentiment('excitement')
s<-get_nrc_sentiment(review)
review_combine<-cbind(cleancoment$text,s)
par(mar=rep(2,4))
barplot(colSums(s),col=rainbow(10),ylab='count',main='sentiment analisis')
  }, height=400)
##Membuatt Wordcloud
  output$Wordcloud <- renderPlot({Datatweets<-readRDS('dataMentah.rds')  
covid=twListToDF(Datatweets)
View(covid)
komen<-covid$text
komenc<-Corpus(VectorSource(komen))

#hapus hal yang tidak penting
removeURL <-function(x) gsub("http[^[:space:]]*", "", x)
twitclean <-tm_map(komenc,removeURL)

removeRT<-function(y) gsub("RT", "", y)
twitclean<-tm_map(twitclean,removeRT)

removeUN<-function(z) gsub("@\\w+", "", z)
twitclean<-tm_map(twitclean,removeUN)

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)

twitclean<-tm_map(twitclean, removePunctuation)
twitclean<-tm_map(twitclean, tolower)

removecovid<-function(x) gsub("waris", "", x)
wc_new<-tm_map(twitclean,removecovid)

{
  dtm<-TermDocumentMatrix(wc_new)
  m<-as.matrix(dtm)
  v<-sort(rowSums(m),decreasing = TRUE)
  wc_new<-data.frame(word=names(v),freq=v)
}
head(wc_new,n=10)

set.seed(1234) # for reproducibility 
wordcloud(words = wc_new$word, 
          freq = wc_new$freq, 
          min.freq = 1,           
          max.words =100, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))
  })
}
  shinyApp(ui = ui, server = server)
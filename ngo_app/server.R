library(shiny)
library(pdftools)
library(stringr)
library(stringi)
library(tm)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(plotly)
library(DT)
library(shinydashboard)
library(tidytext)
library(cluster)
library(tidyverse)
shinyServer(function(input, output) {
  options(shiny.maxRequestSize=800*1024^2) 
  mypdf1_list <- reactive({
    withProgress({
      if(nchar(input$link)>0){
        setProgress(message = "Downloading Document...")
        address=unlist(strsplit(input$link,","))
        pdfs=list()
        for(i in 1:length(address)){
          pdfs[[i]]= pdf_text(str_trim(address[i]))
        }
        pdfs
      }else(return(NULL))
    })
  })
  mypdf2_list<-reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress({
        setProgress(message = "Extracting Text...")
        lst=list()
        for(i in 1:length(inFile[,1])){
          lst[[i]] <- pdf_text(inFile[[i, 'datapath']])
        }
        lst
      })
    }
  })

  documents<-reactive({
    inFile <- input$file1
    c(unlist(strsplit(input$link,",")),inFile$name)
  })
  mymatrix<-reactive({
    withProgress({
      setProgress(message = "Processing corpus...")
      txt=c(unlist(mypdf1_list()), unlist(mypdf2_list()))
      if(is.null(txt))
        return(NULL)
      
      # Create corpus
      corpus=Corpus(VectorSource(txt))
      # Convert to lower-case
      corpus =  tm_map(corpus, tolower)
      #corpus = tm_map(corpus, PlainTextDocument)
      corpus = tm_map(corpus, removePunctuation)
      corpus = tm_map(corpus, removeNumbers)
      
      # Remove stopwords
      corpus=tm_map(corpus,function(x) removeWords(x,stopwords("english")))
      
      frequencies = DocumentTermMatrix(corpus)
      # sparse = removeSparseTerms(frequencies,0.9)
      #sparse =as.matrix(sparse)
      
      sparse =as.matrix(frequencies)
      sparse=apply(sparse,2,sum)
      sparse=sparse[order(sparse,decreasing = T)]
      
      Term=names(sparse)
      Frequency=as.vector(sparse)
      
      sparse=as.data.frame(list(Term=Term,Frequency=Frequency))
      sparse$Term = stri_trans_totitle(sparse$Term) 
      sparse
      
    })
  })
  
  output$wordcloud <- renderPlot({
    sparse=mymatrix()
    pal2 <- brewer.pal(8,"Dark2")
    wordcloud(sparse$Term,sparse$Frequency, min.freq=input$freq, max.words=input$max,
              random.order=FALSE,scale=c(4,0.5),
              rot.per=0.35, use.r.layout=FALSE, colors=pal2)
      pdfs=c(mypdf1_list(), mypdf2_list())
      if(length(pdfs)>0){
        for(i in 1:length(pdfs)){
          txt = pdfs[[i]]
          # Create corpus
          corpus=Corpus(VectorSource(txt))
          # Convert to lower-case
          corpus=tm_map(corpus,tolower)
         # corpus = tm_map(corpus, PlainTextDocument)
          corpus = tm_map(corpus, removePunctuation)
          corpus = tm_map(corpus, removeNumbers)
          # Remove stopwords
          corpus=tm_map(corpus,function(x) removeWords(x,stopwords("english")))
          frequencies = DocumentTermMatrix(corpus)
          #sparse = removeSparseTerms(frequencies,1)
          sparse =as.matrix(frequencies)
          sparse=apply(sparse,2,sum)
          sparse=sparse[order(sparse,decreasing = T)]
          Term=names(sparse)
          Frequency=as.vector(sparse)
          sparse=as.data.frame(list(Term=Term,Frequency=Frequency))
          sparse$Term = stri_trans_totitle(sparse$Term) 
          pal2 <- brewer.pal(8,"Dark2")
          documents=documents()
          x11(title = documents[i])
          wordcloud(sparse$Term,sparse$Frequency, min.freq=input$freq, max.words=input$max,
                    random.order=FALSE,scale=c(4,0.5),
                    rot.per=0.35, use.r.layout=FALSE, colors=pal2)
        }
      }
    })
 
  
  output$myplot <- renderPlot({
    txt=c(unlist(mypdf1_list()), unlist(mypdf2_list()))
    if(is.null(txt))
      return(NULL)
    
    # Create corpus
    corpus=Corpus(VectorSource(txt))
    # Convert to lower-case
    corpus =  tm_map(corpus, tolower)
    #corpus = tm_map(corpus, PlainTextDocument)
    corpus = tm_map(corpus, removePunctuation)
    corpus = tm_map(corpus, removeNumbers)
    
    # Remove stopwords
    corpus=tm_map(corpus,function(x) removeWords(x,stopwords("english")))
    tdm<-TermDocumentMatrix(corpus)
    sparse=tidy(tdm)
    td1<- sparse %>% group_by(term)%>% summarise(count=n()) %>% top_n(100)
    afinn <- td1 %>% 
      inner_join(get_sentiments("afinn"), by = c(term= "word"))%>% 
      mutate(sentiment = score) %>% 
      mutate(method = "AFINN")
    
    bing_and_nrc <- bind_rows(td1 %>% 
                                inner_join(get_sentiments("bing"), by = c(term = "word"))%>%
                                mutate(method = "Bing et al."),
                              td1 %>% 
                                inner_join(get_sentiments("nrc"), by = c(term = "word")) %>% 
                                filter(sentiment %in% c("positive", 
                                                        "negative")) %>%
                                mutate(method = "NRC"))%>%
      count(method, term, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)
    
    bind_rows(afinn, 
              bing_and_nrc) %>%
      ggplot(aes(term, sentiment, fill = method)) + 
      geom_col(show.legend = FALSE) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      facet_wrap(~method, nrow = 3, scales = "free_y")
  })
  
  
  output$cluster <- renderPlot({
    txt=c(unlist(mypdf1_list()), unlist(mypdf2_list()))
    if(is.null(txt))
      return(NULL)
    
    # Create corpus
    corpus=Corpus(VectorSource(txt))
    # Convert to lower-case
    corpus =  tm_map(corpus, tolower)
    #corpus = tm_map(corpus, PlainTextDocument)
    corpus = tm_map(corpus, removePunctuation)
    corpus = tm_map(corpus, removeNumbers)
    
    # Remove stopwords
    corpus=tm_map(corpus,function(x) removeWords(x,stopwords("english")))
    dtm<-DocumentTermMatrix(corpus)
    tdm.tfidf <- tm::weightTfIdf(tdm)
    tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
    tfidf.matrix <- as.matrix(tdm.tfidf)
    dist.matrix = proxy::dist(tfidf.matrix, method = "cosine") 
    truth.K<-16
    clustering.kmeans <- kmeans(tfidf.matrix, truth.K) 
    clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
    clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10) 
    points <- cmdscale(dist.matrix, k = 2) 
    palette <- colorspace::diverge_hcl(truth.K) # Creating a color palette 
    previous.par <- par(mfrow=c(2,2), mar = rep(1.5, 4)) 
    master.cluster <- clustering.kmeans$cluster 
    slave.hierarchical <- cutree(clustering.hierarchical, k = truth.K) 
    slave.dbscan <- clustering.dbscan$cluster 
    stacked.clustering <- rep(NA, length(master.cluster))  
    names(stacked.clustering) <- 1:length(master.cluster)
    
    plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
         mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
    
    plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
         mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
    
    plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
         mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  })
  
  
  output$minfreq = renderUI({
    if(is.null(mymatrix()))
      return(NULL)
    sliderInput("freq",
                em("Minimum Frequency:",style="color:black;font-size:100%"),
                min = 1,  max = 50, value = 15)
  })
  output$maxwords = renderUI({
    if(is.null(mymatrix()))
      return(NULL)
    sliderInput("max",
                em("Maximum Number of Words:",style="color:black;font-size:100%"),
                min = 1,  max = 300,  value = 200)
  })
  output$forEach = renderUI({
    if(is.null(mymatrix()))
      return(NULL)
    checkboxInput("for_each", label = p("Create Word Cloud for Each Document",style="color:#ff0000;font-size:120%" ))
  })
})
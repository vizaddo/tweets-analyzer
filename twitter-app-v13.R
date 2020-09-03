library(data.table)
library(igraph)
library(httpuv)
library(rtweet)
library(shiny)
library(tidytext)
library(textdata)
library(tidyverse)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(DT)
library(syuzhet)
library(visNetwork)
library(shinycssloaders)

ui <- fluidPage(
  
  h1(strong("Twitter Analyzer")),
  "Powered by Shiny-App",
  # The tabsetpanel creates all the panels in this UI. 
  tabsetPanel(
    
    # This is the Welcome Panel for the user. The user will be able to access this panel to understand all the other panels.
    tabPanel(title = 'Welcome!',
             
             (h1('Welcome to the Twitter Analyzer!')),
             h4(em('Beofre using this UI, please take your time to read this short instruction. 
                   While using the UI you can always switch back to this tab to get better explanations.')),
             strong(h5('This ShinyApp will extract Social Media Data from Twitter and perform text and network analysis on it!')),
             br(''),
             h5('The following descriptions will explain what can be expected in each of the above tabs.'),
             br(''),
             
             strong(h4('1. Topic and Wordcloud')),
             
             h4(em('User Input')),
             pre('Topic of interest'),
             h5('Feel free to type any Twitter related hashtag that might interest you.
                The app will create an API with Twitter and extract all the tweets that are related to your selected topic. 
                After extracting the tweets, related to your topic, the ShinyApp performs some text analysis to clean up the tweets. 
                It then shows you the most important words in the tweets. 
                The cleaning process eliminates stopwords such as and, him, her etc. but it also erases special characters and transforms all tweets to lowercase.'),
             pre('Sampple Size of the tweets'),
             h5('You can select between 100 and 1000 tweets related to your topic. 
                The tweets that are extracted from Twitter are transformed into a data table with ca. 90 columns.
                The most important columns that are used to perform the analysis of this App are shown below.  
                These are four columns: actual text, the screen name which is the twitter account name, the followers count, 
                and lastly if these twitter users are verified or not.'),
             pre('Number of words in the wordcloud'),
             h5('You can add as many words to the wordcloud as you want. 
                The most important word will be bigger while the least important words
                are smaller. We recommend you to limit the number of words inside the wordcloud to a maximum of 25 words, 
                otherwise it will get too messy.'),
             
             br(''),
             
             strong(h4('2. Exploration')),
             h4(em('User Input')),
             pre('Number of nodes'),
             h5('Nodes in this analysis represent a word, and this app wants to know 
                how many words you want to have displayed in the network.
                The word network is very simple. Each word represents a single node, 
                and these words are connected to each other 
                if they appeared in the same tweet together.
                In fact, if you zoom in to the network and click on one of the words you can see all the linkages to other words.
                This means that this word appreared with all these words in one tweet.'),
             pre('Centrality Measures'),
             h5('You can select between two centrality measures: Degree and Betweenness Centrality.'),
             h5('The', strong('Degree centrality,'), 'helps you find the word
                that is connected to other words through common tweets.'),
             h5('The', strong('Betweenness centrality,'), 'shows the word that is most important.'),
             h5('Based on the centrality measure you click, you will see a data table that ranks the word with the highest degree, 
                for example.'),
             
             
             br(''),
             
             strong(h4('3. Graph Data')),
             h4(em('User Input')),
             pre('Binwidth'),
             h5('You can select the binwidth. The lower the binwidth, the wider the bars in the degree distribution.
                The higher the binwidth, the thinner the bars get'),
             pre('X-axis limit'),
             
             
             br(''),
             
             strong(h4('4. Sentiment Analysis')),
             h4(em('User Input')),
             pre('Number of words to display'),
             h5('How many words do you want to include. 
                This is linked to the Top words option button'),
             pre('Different types of sentiment analysis'),
             h5('The', strong('Overall Sentiment'), 'breaks down the sentiment into 10 different categories. 
                It gives each word a single category, for example, hate with a negative sentiment. The app then counts the 
                sentiment of all the words in the network, 
                and gives you an overview of the overall sentiment of your extracted tweets.
                '),
             h5('The', strong('Top Words'), 'does the same, just now it lists the words that appear in that category'),
             h5('The', strong('Negative Sentiment'), 'shows you another network of the words, but this time the color is negative 
                if the word has a negative sentiment, and it is green if it has a positive sentiment'),
             
             
             br(''),
             
             strong(h4('5. Retweet Network')),
             h4(em('User Input')),
             pre('Min followers'),
             h5('The color in the network will change its color based on the amount of followers. 
                If the users have more followers, then the node will be green, and otherwise the nodes will be red'),
             pre('Number of Users '),
             h5('Specify the number of users you want to display in the directed graph that is shown. 
                If an arrow points to a user, this means this user has retweeted the post. 
                The node with the arrow is the original source, while the node with the flat line is the 
                retweeter.'),
             
             
             br(''),
             br(''),
             br('')
             
    ),
    
    # topic and wordcloud panel
    tabPanel(title = "Topic and Wordcloud",
             sidebarLayout(
               mainPanel(
                 h1('Wordcloud'),
                 withSpinner(plotOutput("wordcloud", width = "600px", height = "400px")),
                 dataTableOutput("table")
               ),
               sidebarPanel(
                 textInput("topic", "Select your topic", "coronavirus"),
                 sliderInput("tweet", "Number of tweets to extract", "100",
                             min = 100, max = 1000),
                 numericInput("max.word", "How many words do you want in the wordcloud?",
                              min = 5, max = 25, value = 10),
                 actionButton("go.wordcloud", "Create Wordcloud", icon = icon("pencil"))
               )
             )),
    
    # network exploration - the plot - panel
    tabPanel(title = "Exploration",
             sidebarLayout(
               mainPanel(
                 h1('Word Network'),
                 h4('colored by Centrality Measures'),
                 pre('The words are connected if they appeared in the same tweet'),
                 withSpinner(visNetworkOutput("int.plot")),
                 textOutput('transitivity'),
                 dataTableOutput('centrality.exploration')
               ),
               sidebarPanel(
                 numericInput("nodes", "Number of nodes (words) in the network",
                              min = 1, max = 300, value = 25),
                 radioButtons('centrality', 
                              'Select the centrality measures',
                              choices = c('Degree Centrality', 'Betweenness Centrality'))
               )
             )
    ),
    
    # The centrality measures
    tabPanel(title = "Graph Data",
             mainPanel(
               h1('The Degree Distribution'),
               withSpinner(plotOutput("distribution")),
               dataTableOutput("centrality")
             ),
             sidebarPanel(
               numericInput("bin", "Choose binwidth", "30", min = 1),
               numericInput("lim", "Choose your x-axis limit", "300", min = 1)
             )
    ),
    
    # The Sentiment Analysis tab panel 
    tabPanel(title = 'Sentiment Analysis', 
             mainPanel(
               h1('Network of words colored by sentiment'),
               h5('The nodes (words) that are in red have a negative sentiment, while the green nodes associate with a positive sentiment'),
               withSpinner(visNetworkOutput('sentiment.net')),
               withSpinner(plotOutput('sentiment'))
             ),
             sidebarPanel(
               numericInput("word", "How many words do you want to display in the barchart: Top Words", "5", min = 1, max = 10),
               sliderInput("nodes.sentiment", "How many words do you want to display in the network?", "30",
                           min = 20, max = 500),
               radioButtons("sentiment.show", "Show graph", 
                            choices = c('Overall Sentiment', "Top Words"))
             )),
    
    # This represents the retweet network tab panel 
    tabPanel(title = 'Retweet Network', 
             mainPanel(
               h1('The Retweet Network colored by number of followers'),
               h5('The lightblue nodes have more followers than the cutoff, while the green nodes have less followers'),
               visNetworkOutput('retweet'), 
               dataTableOutput('retweet.table')), 
             sidebarPanel(
               numericInput('followers', 'What is the cutoff value for followers?', value = 300, min = 5),
               numericInput("nodes.retweet", "Specify the number of users you want to display in the network", min = 1, max = 300, value =25)
             ))
  )
  
)


  
server <- function(input, output){
  
  validation <- reactive({
    validate(
      need(input$nodes.retweet <= 50, "The maximum number of nodes is 50, please lower your number!")
    )  
  })
  
  # We create this reactive function to create the API to Twitter and do some cleaning steps.
  # When the user changes the topic, this reactive function will make sure that all the future analysis are changed!
  
  # The cleaning steps consist of eliminating the special characters inside the tweets text column.
  # It is important to put this function as a reactive function to make sure the information changes. 
  tweets.input <- reactive({
    #load the tweets
    access.token <- "135434674-rb5AV4aT6MMWfifka1ucxoiVuIxteVtlyTEJmxZI"
    access.token.secret <- "of2S67goX17gSTNdOyM8YGTu9G37O6QR7cvsFax0vtx4M"
    
    twitter_token <- create_token(
      app = "rCloudVido",
      consumer_key = "MySh48t3xjkhOw8qibpOQHZtt",
      consumer_secret = "o5QtBi3yrAbQq9VDy7omu0j5EDf98MCCqMwQAAvRr0ZWS9BYl6",
      access_token = access.token,
      access_secret = access.token.secret,
      set_renv = TRUE)
    
    # This function extracts the tweets, and the user can change the topic and the sample size! 
    input$go.wordcloud
    topic <- str_replace(input$topic, pattern = fixed(' '), '')
    tweets <- isolate(
      search_tweets(topic, n = input$tweet, type = "mixed", lang = "en")
    )
    tweets
    
    ## Remove hyperlinks and all other special characters
    tweets$text <-  gsub("https\\S*", "", tweets$text)
    tweets$text <-  gsub("@\\S*", "", tweets$text) 
    tweets$text  <-  gsub("amp", "", tweets$text) 
    tweets$text  <-  gsub("[\r\n]", "", tweets$text)
    tweets$text  <-  gsub("[[:punct:]]", "", tweets$text)
    
    tweets
    # This reactive function is mainly used as input for the data tables that are shown. 
    # The data tables should be dynamic, meaning they should change whenever the user changes stuff.
    
    
  })
  
  graph.input <- reactive({
    
    # We use the half-cleaned text as input from the previous reactive function. 
    # In this part, we are doing some text cleaning, and the reason why we are not including this part 
    # in the previous reactive function is because we need the previous reactive for the data table. 
    tweets <- tweets.input()
    # We do some further text cleaning, and then transforming this text data into a CORPUS!
    tweets$text <- str_replace_all(tweets$text,"[^[:graph:]]", " ")
    tweets$text <- iconv(tweets$text, 'utf-8', 'ascii', sub='')
    tweet.corp <- VCorpus(VectorSource(tweets$text)) # This transforms everything into a corpus
    
    #We create a function that cleans the corpus to remove punctuation, number, lower case, removing stopwords of the english dictionary,
    # and we strip the spaces in between. 
    corp_clean <- function(corp){
      corp <- tm_map(corp, removePunctuation)
      corp <- tm_map(corp, removeNumbers)
      corp <- tm_map(corp, content_transformer(tolower))
      corp <- tm_map(corp, removeWords, stopwords("en"))
      corp <- tm_map(corp, stripWhitespace)
    }
    # We perform the function on our corpus: tweet.corp 
    tweet.corp <- corp_clean(tweet.corp)
    
    # The corpus is then transformed into a TDM, which means that each term (word) is a row, and the 
    # documents (tweets) are columns. We use TDM instead of the DTM because it saves RAM space, as
    # we will have much more words than tweets! 
    tweet.tdm <- TermDocumentMatrix(tweet.corp)
    # We transform the TDM into a matrix
    tweet.m <- as.matrix(tweet.tdm)
    tweet.m[1:5, 1:5]
    # Then the next step is to turn the matrix into data frame.
    dt.tweet.tdm <- as.data.table(tweet.m, keep.rownames = "terms")
    
    # We want all the tweet columns to be in one so we can create our final graph object.
    # We use the gather function to perform this analysis. 
    # The result is a data table with the words in one column, and in the other column we have the tweets.
    
    dt.tweet.gather <- dt.tweet.tdm %>%
      gather(docs, present, -1) %>%
      as.data.table() 
    dt.tweet.gather <- dt.tweet.gather[present > 0] #  If the word is present in the tweet, it will shown in this data table
    # With this we make sure that only words are connected if they actually are present in the same tweet.
    
    
    # We need to create a bipartite graph so we merge the words and the tweets into one column.
    # To distinguish words from tweets we create a column that will be true for terms (words), and false for docs (tweets).
    dt.term <- dt.tweet.gather[, list(name=unique(terms), type=TRUE)]
    dt.doc <- dt.tweet.gather[, list(name=unique(docs), type=FALSE)]
    
    # We put all words and tweets into one single column to use them as vertices 
    dt.vertices <- rbind(dt.term, dt.doc)
    # This creates the bipartite graph!
    g <- graph.data.frame(dt.tweet.gather[, .(terms, docs)], directed = FALSE, 
                          vertices = dt.vertices) 
    
    # We project it to the word space.
    # The projection to the word space means that the words are the nodes, and these are connected to each other 
    # if they appear in the same tweet.
    g.terms <- bipartite.projection(g)$proj2
    # We calculate the two centrality measures on our graph.
    V(g.terms)$degree <- degree(g.terms)
    V(g.terms)$betweennes <- betweenness(g.terms)
    
    #remove the topic word
    #topic <- str_replace(input$topic, pattern = fixed(' '), '')
    #g.terms.new <- V(g.terms)[name != topic]
    g.terms
    
    # This graph object is now a projection to term (word) space, with the centrality measures as the vertex attributes.
    # The graph object is a reactive function, as we want to ensure that all the networks will change 
    # whenever the user changes the topic.
  })
  
  # The first reactive function that is created, cleaned the text data of the tweets.
  # In this renderDataTable function, it is used as an input. 
  # We then select certain rows which we want to show as a data table in the UI.
  # This data table is shown in the tab: Topic and Wordcloud beneath the wordcloud.
  output$table <- DT::renderDataTable({
    tweets <- tweets.input()
    tweets[, c("text", "screen_name", "followers_count", "verified")]
  })
  
  # This render function shows the wordcloud! 
  output$wordcloud <- renderPlot({
    
    tweets <- tweets.input()
    topic <- str_replace(input$topic, pattern = fixed(' '), '')
    ## add query as stopword
    custom_stop_words <- tribble(
      ~word, ~lexicon,
      topic, "CUSTOM"
    ) # This function takes out all the custom words such as the topic word itself.
    # For example, when typing in a topic such as coronavirus, we don't want the word 'coronavirus' 
    # to be displayed in the wordcloud. Obviously, this word will appear a lot!
    # The above function makes sure that these custom words are not shown in the wordcloud. 
    
    stop_words_topic <- stop_words %>%
      rbind(custom_stop_words)
    
    ## Remove stopwords
    tweets.clean <- tweets %>%
      select(text) %>%
      unnest_tokens(word, text)
    tweets.clean <- tweets.clean %>%
      anti_join(stop_words_topic)
    
    ## build wordcloud
    tweets.wordcloud <- tweets.clean %>%
      count(word) %>%
      arrange(desc(n))
    
    # Final function to build the wordcloud. The maximum words to display can be changed in the UI by the user.
    input$go.wordcloud
    wc <- isolate(wordcloud(words = tweets.wordcloud$word, 
                            freq = tweets.wordcloud$n, max.words = input$max.word, 
                            colors = brewer.pal(6, 'Dark2'))
    )
    wc
  })
  # This output is shown in the tab: Exploration. 
  # This is just plain text to give the user information about the transitivity.
  # We also included an if-statement that changes the word at the end of this sentence.
  # The change in the last word will give the uneducated user (in network analytics) an indication if the 
  # transitivity value is high or not. 
  output$transitivity <- renderText({
    g.terms <- graph.input()
    
    cl.coe <-  transitivity(g.terms)
    last.word <-  if(cl.coe > 0.5) {'high'}
    else {'low'}
    
    print(paste('The clustering coefficient of the hashtag you selected is:', round(cl.coe, 2),'. This means that the words are connected to 
                other words in the network through other tweets by a', last.word, 'degree.' )) 
    
    
    
    
    # output$net.explore <- renderPlot({
    
    # use the graph object we created before as the reactive function as an input. 
    
    g.terms <- graph.input()
    
    # We extract the vertex attributes from the graph object and convert it into a data frame
    dt.g.terms <- data.table(get.data.frame(g.terms, "vertices"))
    # We create an extra column that is 1 if the centrality measures are above or equal to the mean, and 0 below.  
    dt.g.terms$degree_check <- ifelse(dt.g.terms$degree >= mean(dt.g.terms$degree), 1, 0)
    dt.g.terms$btw_check <- ifelse(dt.g.terms$betweenness >= mean(dt.g.terms$betweenness), 1, 0)
    
    # We add this 'check' to the graph object, as an additional attribute. This is all responsible for the coloring
    # of the nodes in the end. 
    V(g.terms)$btw_color <- dt.g.terms$btw_check
    V(g.terms)$degree_color <- dt.g.terms$degree_check
    
    # The color will be lightgreen for the nodes that do not have high centrality measures and red if high
    sub.color <- c('lightgreen', 'red')
    
    
    # This is the final graph that includes an if-else statement that changes the degree measures
    # based on the user input in the UI.  
    g.user <- induced_subgraph(g.terms, v = V(g.terms)[1:input$nodes])
    
    
    plot(g.user,
         v = V(g.user),
         vertex.color = if (input$centrality == 'Degree Centrality') {sub.color[as.factor(vertex_attr(g.terms, 'degree_color'))]}
         else if (input$centrality == 'Betweenness Centrality') {sub.color[as.factor(vertex_attr(g.terms, 'btw_color'))]}, 
         layout = layout_nicely(g.user))
  })
  
  output$int.plot <- renderVisNetwork({
    # This render function displays the interactive network of the words network.
    # This is shown in the tab: Exploration. 
    g.terms <- graph.input() # We use the graph object from the reactive function as an input. 
    
    # introduce the nodes degree and betweenness
    V(g.terms)$degree <- degree(g.terms)
    V(g.terms)$betweenness <- betweenness(g.terms)
    
    # We extract the vertex attributes from the graph object and convert it into a data frame
    dt.g.terms <- data.table(get.data.frame(g.terms, "vertices"))
    
    # color the nodes based on defined condition
    V(g.terms)$color <- 
      if(input$centrality == 'Degree Centrality'){
        ifelse(V(g.terms)$degree < median(dt.g.terms$degree), "lightblue", "green") 
      } else if(input$centrality == 'Betweenness Centrality'){
        ifelse(V(g.terms)$betweennes < median(dt.g.terms$degree), "lightblue", "green")
      }
    
    # This function creates an interactive graph object which the user can use.
    # The user is able to zoom in, zoom out, click on nodes and see all the vertices to that specific node
    
    visIgraph(igraph=induced.subgraph(g.terms, v = V(g.terms)[0:input$nodes]),
              idToLabel=TRUE, layout = "layout_nicely") %>%
      # explicit edge options
      visEdges(color = list(highlight = "green", hover = "green")) %>%   
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, labelOnly = FALSE, hover = TRUE))
    #,nodesIdSelection = list(selected = 6))
  })
  
  output$sentiment.word <- renderPlot({
    
    tweets <- tweets.input()
    custom_stop_words <- tribble(
      ~word, ~lexicon,
      input$topic, "CUSTOM"
    )
    
    stop_words_topic <- stop_words %>%
      rbind(custom_stop_words)
    
    ## Remove stopwords
    tweets.clean <- tweets %>%
      select(text) %>%
      unnest_tokens(word, text)
    tweets.clean <- tweets.clean %>%
      anti_join(stop_words_topic)
    
    tweet.corona.sentiment <- tweets.clean %>%
      inner_join(get_sentiments("nrc")) # positive and negative sentiments 
    
    top.tweet.corona.sentiment <- tweet.corona.sentiment %>%
      group_by(sentiment) %>%
      count(word) %>%
      top_n(input$word, n) %>% # arranging by the top words
      ungroup() %>%
      mutate(word2 = fct_reorder(word, n))
    
    head(top.tweet.corona.sentiment)
    
    ggplot(top.tweet.corona.sentiment, aes(x = word2, y = n, fill = factor(sentiment))) + 
      geom_col() + coord_flip() + facet_wrap(~factor(sentiment), scales = "free") + 
      labs(title = paste("Top Words on ", input$topic, " (by sentiment)"), 
           y = "Word Frequency", x = "Word")
  })
  
  output$centrality.exploration <- DT::renderDataTable({
    
    g.terms <- graph.input()
    V(g.terms)$degree <- degree(g.terms)
    V(g.terms)$betweenness <- betweenness(g.terms)
    
    # We extract the vertex attributes from the graph object and convert it into a data frame
    dt.g.terms <- data.table(get.data.frame(g.terms, "vertices"))
    # We create an extra column that is 1 if the centrality measures are above or equal to the mean, and 0 below  
    dt.g.terms$degree_check <- ifelse(dt.g.terms$degree >= mean(dt.g.terms$degree), 1, 0)
    dt.g.terms$btw_check <- ifelse(dt.g.terms$betweenness >= mean(dt.g.terms$betweenness), 1, 0)
    
    # We add this 'check' to the graph object, as an additional attribute 
    V(g.terms)$btw_color <- dt.g.terms$btw_check
    V(g.terms)$degree_color <- dt.g.terms$degree_check
    
    # The color will be lightgreen for the nodes that do not have high centrality measures and red if high
    sub.color <- c('lightgreen', 'red')
    
    
    # This is the final graph that includes an if-else statement that changes the degree measures
    # based on the user input in the UI.  
    g.user <- induced_subgraph(g.terms, v = V(g.terms)[1:input$nodes])
    
    dt.g.terms <- data.table(get.data.frame(g.user, "vertices"))
    
    #updateSelectInput(session = session, inputId = "custom", choices = dt.g.terms$degree)
    
    if (input$centrality == 'Degree Centrality') {dt.g.terms[, c('name', 'degree')][order(-degree)]}
    else if (input$centrality == 'Betweenness Centrality') {dt.g.terms[, c('name', 'betweenness')][order(-betweenness)]}
    
    #custom.words <- input$custom
    
    # dt.g.terms[name != input$custom]
  })
  
  output$centrality <- DT::renderDataTable({
    
    g.terms <- graph.input()
    
    #plot(induced_subgraph(g.terms, v = V(g.terms)[input$nodes[1]:input$nodes[2]]))
    
    V(g.terms)$degree <- degree(g.terms)
    V(g.terms)$betweenness <- betweenness(g.terms)
    
    # We extract the vertex attributes from the graph object and convert it into a data frame
    dt.g.terms <- data.table(get.data.frame(g.terms, "vertices"))
    # We create an extra column that is 1 if the centrality measures are above or equal to the mean, and 0 below  
    dt.g.terms$degree_check <- ifelse(dt.g.terms$degree >= mean(dt.g.terms$degree), 1, 0)
    dt.g.terms$btw_check <- ifelse(dt.g.terms$betweenness >= mean(dt.g.terms$betweenness), 1, 0)
    
    # We add this 'check' to the graph object, as an additional attribute 
    V(g.terms)$btw_color <- dt.g.terms$btw_check
    V(g.terms)$degree_color <- dt.g.terms$degree_check
    
    # The color will be lightgreen for the nodes that do not have high centrality measures and red if high
    sub.color <- c('lightgreen', 'red')
    
    
    # This is the final graph that includes an if-else statement that changes the degree measures
    # based on the user input in the UI.  
    g.user <- induced_subgraph(g.terms, v = V(g.terms)[1:input$nodes])
    
    dt.g.terms <- data.table(get.data.frame(g.user, "vertices"))
    dt.g.terms[, c('name', 'degree', 'betweenness')]
    
    
  })
  
  output$retweet <- renderVisNetwork({
    tweets <- tweets.input()
    # From the tweets data table, we only need these two columns to show a retweet network. 
    df.rtwt <- tweets[, c("screen_name" , "retweet_screen_name")]
    dt.rtwt.new <- df.rtwt[complete.cases(df.rtwt), ] # We delete the missing value rows.
    m <- as.matrix(dt.rtwt.new) # For a network we need to create a matrix first 
    n.rtwt <- graph_from_edgelist(el = m, directed = TRUE) # We now have a network: n.rtwt.
    
    # For the retweet network we will measure the degree centrality and the betweenness scores. 
    # For the centrality measures we will have different in and out degree scores.
    # Out-degree: How many times a user retweets something
    # In-degree: How often is a user posts a retweetet by someone else
    out.degree <- degree(n.rtwt, mode = c('out'))
    out.degree.sort <- sort(out.degree, decreasing = TRUE)
    in.degree <- degree(n.rtwt, mode = c('in'))
    in.degree.sort <- sort(in.degree, decreasing = TRUE)
    # Betweenness scores in this network represents the user's role in allowing information to pass through the network. 
    # The users with a high betweenness score in our network are key bridges between people who retweet a lot and individuals whom's tweets are retweeted very frequently.
    n.betwn <- betweenness(n.rtwt, directed = TRUE) 
    
    betwn.sort <- n.betwn %>% 
      sort(decreasing = TRUE)
    
    vert.size <- (out.degree * 3) + 5 # The node will be bigger the more a user retweets
    
    followers <- tweets[, c("screen_name" , "followers_count")]
    
    followers$follow <- ifelse(followers$followers_count <= input$followers, 1, 0) # If this users has more than ... followers he will get a 1 in the column otherwise a 0. This step is important to visualize the different color of the vertex 
    
    V(n.rtwt)$followers <- followers$follow
    vertex_attr(n.rtwt) # Double check if the followers attribute was really added! 
    
    V(n.rtwt)$color <- ifelse(V(n.rtwt)$followers == 1, "green", "lightblue")
    
    g.user <- induced_subgraph(n.rtwt, v = V(n.rtwt)[1:input$nodes.retweet])
    
    visIgraph(validation(), igraph=g.user,
              idToLabel=TRUE,layout = "layout_nicely") %>%
      # explicit edge options
      visEdges(color = list(highlight = "green", hover = "green")) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1,labelOnly = FALSE, hover = TRUE))
    
  })
  
  output$retweet.table <- renderDataTable({
    tweets <- tweets.input()
    
    tweets[, c("text", "screen_name", "followers_count")]
  })
  
  output$distribution <- renderPlot({
    g.terms <- graph.input()
    
    V(g.terms)$degree <- degree(g.terms)
    V(g.terms)$betweenness <- betweenness(g.terms)
    
    # We extract the vertex attributes from the graph object and convert it into a data frame
    dt.g.terms <- data.table(get.data.frame(g.terms, "vertices"))
    # We create an extra column that is 1 if the centrality measures are above or equal to the mean, and 0 below  
    dt.g.terms$degree_check <- ifelse(dt.g.terms$degree >= mean(dt.g.terms$degree), 1, 0)
    dt.g.terms$btw_check <- ifelse(dt.g.terms$betweenness >= mean(dt.g.terms$betweenness), 1, 0)
    
    # We add this 'check' to the graph object, as an additional attribute 
    V(g.terms)$btw_color <- dt.g.terms$btw_check
    V(g.terms)$degree_color <- dt.g.terms$degree_check
    
    # The color will be lightgreen for the nodes that do not have high centrality measures and red if high
    sub.color <- c('lightgreen', 'red')
    
    
    # This is the final graph that includes an if-else statement that changes the degree measures
    # based on the user input in the UI.  
    g.user <- induced_subgraph(g.terms, v = V(g.terms)[1:input$nodes])
    
    dt.g.terms <- data.table(get.data.frame(g.user, "vertices"))
    dt.g.terms
    
    
    ggplot(dt.g.terms, aes(x=degree)) + geom_histogram(bins = input$bin) + 
      labs(title = paste("The Degree Distribution of", input$topic, '!'), x = "Node Degree") + xlim(0, input$lim) 
  })
  
  output$sentiment <- renderPlot({
    tweets <- tweets.input()
    g.terms <- graph.input()
    if(input$sentiment.show == 'Overall Sentiment'){
      
      text <- tweets$text
      sa.value <- get_nrc_sentiment(text) # I'm running a sentiment analysis on the data.
      score <- colSums(sa.value[,]) # I sum up all the columns to understand the sentiment
      
      df.score <- data.frame(score) # I create a data frame from that! 
      
      df2.score <- cbind(sentiment = row.names(df.score),  
                         df.score, row.names = NULL) # I change the name of the first column to 'sentiment'
      
      ggplot(data = df2.score, aes(x = sentiment, y = score, fill = sentiment)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        labs(title = paste("Overall Sentiment on ", input$topic))
      
    } else if(input$sentiment.show == 'Top Words'){
      
      ## add query as stopword
      custom_stop_words <- tribble(
        ~word, ~lexicon,
        input$topic, "CUSTOM"
      )
      
      stop_words_topic <- stop_words %>%
        rbind(custom_stop_words)
      
      ## Remove stopwords
      tweets.clean <- tweets %>%
        select(text) %>%
        unnest_tokens(word, text)
      tweets.clean <- tweets.clean %>%
        anti_join(stop_words_topic)
      
      tweet.corona.sentiment <- tweets.clean %>%
        inner_join(get_sentiments("bing"))
      tweet.corona.sentiment
      
      top.tweet.corona.sentiment <- tweet.corona.sentiment %>%
        group_by(sentiment) %>%
        count(word) %>%
        top_n(input$word, n) %>% # arranging by the top words
        ungroup() %>%
        mutate(word2 = fct_reorder(word, n))
      
      head(top.tweet.corona.sentiment)
      
      ggplot(top.tweet.corona.sentiment, aes(x = word2, y = n, fill = factor(sentiment))) + 
        geom_col() + coord_flip() + facet_wrap(~factor(sentiment), scales = "free") + 
        labs(title = paste("Top Words on",input$topic, "(by sentiment)"), 
             y = "Word Frequency", x = "Word")
    }
    
  })
  
  output$sentiment.net <- renderVisNetwork({
    g.terms <- graph.input()
    
    dt.g.terms <- get.data.frame(g.terms, "edges")
    
    dt.g.terms.sentiment <- dt.g.terms %>%
      select(word = from, to, weight) %>%
      inner_join(get_sentiments("bing")) %>%
      spread(sentiment, unique(sentiment)) %>%
      mutate_all(funs(str_replace_all(., "positive", "1"))) %>%
      #mutate_all(funs(str_replace_all(., "anger", "1"))) %>%
      #mutate_all(funs(str_replace_all(., "anticipation", "1"))) %>%
      #mutate_all(funs(str_replace_all(., "fear", "1"))) %>%
      #mutate_all(funs(str_replace_all(., "disgust", "1"))) %>%
      #mutate_all(funs(str_replace_all(., "joy", "1"))) %>%
      mutate_all(funs(str_replace_all(., "negative", "1"))) %>%
      #mutate_all(funs(str_replace_all(., "sadness", "1"))) %>%
      #mutate_all(funs(str_replace_all(., "surprise", "1"))) %>%
      #mutate_all(funs(str_replace_all(., "trust", "1"))) %>%
      mutate_all(funs(str_replace_na(.))) %>%
      mutate_all(funs(str_replace_all(., "NA", "0")))
    
    V(g.terms)$negative_color <- dt.g.terms.sentiment$negative
    #V(g.terms)$positive_color <- dt.g.terms.sentiment$positive
    #sub.color <- c('lightgreen', 'red') # if 1 then red, otherwise green
    
    V(g.terms)$color <- ifelse(V(g.terms)$negative_color == 1, "red", "green")
    
    g.user <- induced_subgraph(g.terms, v = V(g.terms)[1:input$nodes.sentiment])
    
    visIgraph(igraph=g.user,
              idToLabel=TRUE,layout = "layout_nicely") %>%
      # explicit edge options
      visEdges(color = list(highlight = "green", hover = "green")) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1,labelOnly = FALSE, hover = TRUE))
    
  })
  
}


shinyApp(ui = ui, server = server)

library(tidyverse)
library(tidytext)
library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(tidygraph)
library(RColorBrewer)
library(textdata)
library(shiny)
original_data = read.csv('/Users/samuelmurphy/stat133/u2-lyrics.csv')

# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "u2-lyrics.csv")


# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
    
    titlePanel("Text Analysis on U2 Songs"),
    fluidRow(
        # replace with your widgets
        column(3,
               p(em("Negative or Positive Words")),
               radioButtons(inputId = "choose", 
                            label = "Choose one option", 
                            choices = c("Both" = "both",
                                        "Positive" = "pos",
                                        "Negative" = "neg"), 
                            selected = "both")
        ),
        
        # replace with your widgets
        column(3,
               p(em("Albums to perform analysis on")),
               selectInput(inputId = "numbers", 
                           label = "Pick an album, any album",
                           choices = c("All" = "all",unique(original_data$album)),
                           selected = "all")
        ),
        
        # replace with your widgets
        column(3,
               p(em("Years of Albums to Choose From")),
               radioButtons(inputId = "arrange", 
                            label = "Order bars by:", 
                            choices = c("All Years" = "all_years",
                                        "1980s" = "eighties",
                                        "1990s" = "nineties",
                                        "2000s" = "twothousands",
                                        "2010s" = "twenty_ten"),
                            selected = "all_years")
        ),
        
        # replace with your widgets
        column(3,
               p(em("How many words should be included in the word graph?")),
               sliderInput(inputId = "how_many_words",
                           label = "Number of words",
                           min = 1,
                           max = 30,
                           value = 5)
        )
    ),
    hr(),
    
    tabsetPanel(type = "tabs",
                tabPanel("Sentiment Analysis",
                         h3("Extreme Words in U2's Songs"),
                         plotOutput("most_common_extreme_words"),
                         hr(),
                         h4("Top 10 Happiest Songs (Songs WIth The Highest Median Bigram Score)"),
                         verbatimTextOutput('table1'),
                         h4('Top 10 Saddest Songs (Songs With The Lowest Median Bigram Score)'),
                         verbatimTextOutput('table2')),
                tabPanel("Bigram Analysis", 
                         h3("Bigram Analysis"),
                         plotOutput("bigram_plot"),
                         hr(),
                         verbatimTextOutput('table3'))
    )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
    # Tab 1
    # 2 tables and 1 plot of the most extreme words
    tab2_graph = reactive({
        AFINN <- get_sentiments("afinn")
        original_data_bigrams = original_data %>%
            unnest_tokens(bigram, lyrics, token = 'ngrams', n = 2)
        original_separated <- original_data_bigrams %>%
            separate(bigram, c("word1", "word2"), sep = " ")
        
        if(input$arrange == "eighties"){
            original_seperated = original_separated %>%
                filter(year >= 1980 & year <= 1989)
        }
        if(input$arrange == "nineties"){
            original_seperated = original_separated %>%
                filter(year >= 1990 & year <= 1999)
        }
        if(input$arrange == "twothousands"){
            original_seperated = original_separated %>%
                filter(year >= 2000 & year <= 2009)
        }
        if(input$arrange == "twenty_ten"){
            original_seperated = original_separated %>%
                filter(year >= 2010 & year <= 2019)
        }
        
        if(input$numbers == 'Boy'){
            original_separated = original_separated %>%
                filter(album == 'Boy')
        }
        if(input$numbers == 'War'){
            original_separated = original_separated %>%
                filter(album == 'War')
        }
        if(input$numbers == 'October'){
            original_separated = original_separated %>%
                filter(album == 'October')
        }
        if(input$numbers == 'The Unforgettable Fire'){
            original_separated = original_separated %>%
                filter(album == 'The Unforgettable Fire')
        }
        if(input$numbers == 'The Joshua Tree'){
            original_separated = original_separated %>%
                filter(album == 'The Joshua Tree')
        }
        if(input$numbers == 'Rattle And Hum'){
            original_separated = original_separated %>%
                filter(album == 'Rattle And Hum')
        }
        if(input$numbers == 'Achtung Baby'){
            original_separated = original_separated %>%
                filter(album == 'Achtung Baby')
        }
        if(input$numbers == 'Zooropa'){
            original_separated = original_separated %>%
                filter(album == 'Zooropa')
        }
        if(input$numbers == 'Passengers'){
            original_separated = original_separated %>%
                filter(album == 'Passengers')
        }
        if(input$numbers == 'Pop'){
            original_separated = original_separated %>%
                filter(album == 'Pop')
        }
        if(input$numbers == 'The Best Of 1980-1990'){
            original_separated = original_separated %>%
                filter(album == 'The Best Of 1980-1990')
        }
        if(input$numbers == 'All That You Cant Leave Behind'){
            original_separated = original_separated %>%
                filter(album == 'All That You Cant Leave Behind')
        }
        if(input$numbers == 'How To Dismantle An Atomic Bomb'){
            original_separated = original_separated %>%
                filter(album == 'How To Dismantle An Atomic Bomb')
        }
        if(input$numbers == 'Medium Rare Remastered'){
            original_separated = original_separated %>%
                filter(album == 'Medium Rare Remastered')
        }
        if(input$numbers == 'No Line On The Horizon'){
            original_separated = original_separated %>%
                filter(album == 'No Line On The Horizon')
        }
        if(input$numbers == 'Songs Of Innocence'){
            original_separated = original_separated %>%
                filter(album == 'Songs Of Innocence')
        }
        if(input$numbers == 'Songs Of Experience'){
            original_separated = original_separated %>%
                filter(album == 'Songs Of Experience')
        }
        full_data_without_filter <- original_separated %>%
            inner_join(AFINN, by = c(word2 = "word")) %>%
            count(word1, word2, value, sort = TRUE)
        
        list_of_negative_sentiment_words = c()
        for(i in 1:length(AFINN$word)){
            if(AFINN[i,2] < 0){
                list_of_negative_sentiment_words = c(list_of_negative_sentiment_words, AFINN[i, 1]$word)
            }
        } # all this for loop is doing is making a list of negative words so I can use %!in% later on for a list rather than DF.
        '%!in%' <- function(x,y)!('%in%'(x,y))
        
        negation_words <- c("not", "no", "never", "without")
        
        updated_table_for_all_values = full_data_without_filter %>%
            mutate(value = ifelse(word1 %in% list_of_negative_sentiment_words & word2 %in% list_of_negative_sentiment_words, abs(value), value)) %>%
            mutate(value = ifelse(word1 %in% negation_words, -abs(value), value))
        if(input$choose == 'both'){
            updated_table_for_all_values = updated_table_for_all_values
        }
        if(input$choose == 'pos'){
            updated_table_for_all_values = updated_table_for_all_values %>%
                filter(value > 0)
        }
        if(input$choose == 'neg'){
            updated_table_for_all_values = updated_table_for_all_values %>%
                filter(value < 0)
        }
        counts_graph <- updated_table_for_all_values %>%
            filter(n >= input$how_many_words) %>%
            graph_from_data_frame()
        counts_graph
        
        #    p = na.omit(original_data_with_sentiment_scores)[-4] %>% group_by(album) %>%
        #     summarize(mean(scores_of_each_song))
        #    p = data.frame(p)
        #    df_with_each_album_scored = p[order(p$mean.scores_of_each_song.),]
        #    df_with_each_album_scored
        #    head(data.frame(updated_table_for_all_values))
        
        
        #MAKE THIS TOP 10 HAPPY SONGS
    })
    tab1_graph = reactive({
        AFINN <- get_sentiments("afinn")
        list_of_negative_sentiment_words = c()
        for(i in 1:length(AFINN$word)){
            if(AFINN[i,2] < 0){
                list_of_negative_sentiment_words = c(list_of_negative_sentiment_words, AFINN[i, 1]$word)
            }
        }
        '%!in%' <- function(x,y)!('%in%'(x,y))
        negation_words <- c("not", "no", "never", "without")
        scores_of_each_song = c()
        for(i in 1:length(original_data$album)){
            dat = original_data[i,]
            unfilted_bigram = dat %>%
                unnest_tokens(bigram, lyrics, token = 'ngrams', n = 2)
            seperated_into_2_words <- unfilted_bigram %>%
                separate(bigram, c("word1", "word2"), sep = " ")
            join_without_filter <- seperated_into_2_words %>%
                inner_join(AFINN, by = c(word2 = "word")) %>%
                count(word1, word2, value, sort = TRUE)
            tbl_with_value = join_without_filter %>%
                mutate(value = ifelse(word1 %in% list_of_negative_sentiment_words & word2 %in% list_of_negative_sentiment_words, abs(value), value)) %>%
                mutate(value = ifelse(word1 %in% negation_words, -abs(value), value))
            number = quantile(data.frame(na.omit(tbl_with_value))$value, 0.5)
            scores_of_each_song = c(scores_of_each_song, number)
        }
        #quantile(data.frame(na.omit(updated_table_for_all_values))$value, 0.5)
        #data.frame(scores_of_each_song)
        original_data_with_sentiment_scores = cbind(original_data, data.frame(scores_of_each_song))
        original_data_with_sentiment_scores = original_data_with_sentiment_scores %>%
            arrange(desc(scores_of_each_song))
        if(input$arrange == "eighties"){
            original_data_with_sentiment_scores = original_data_with_sentiment_scores %>%
                filter(year >= 1980 & year <= 1989)
        }
        if(input$arrange == "nineties"){
            original_data_with_sentiment_scores = original_data_with_sentiment_scores %>%
                filter(year >= 1990 & year <= 1999)
        }
        if(input$arrange == "twothousands"){
            original_data_with_sentiment_scores = original_data_with_sentiment_scores %>%
                filter(year >= 2000 & year <= 2009)
        }
        if(input$arrange == "twenty_ten"){
            original_data_with_sentiment_scores = original_data_with_sentiment_scores %>%
                filter(year >= 2010 & year <= 2019)
        }
        
        p = na.omit(original_data_with_sentiment_scores)[-4] %>% group_by(album) %>%
            summarize(mean(scores_of_each_song))
        p = data.frame(p)
        names(p) = c("album", "value")
        if(input$choose == 'both'){
            p = p
        }
        if(input$choose == 'pos'){
            p = p %>%
                filter(value > 0)
        }
        if(input$choose == 'neg'){
            p = p %>%
                filter(value < 0)
        }
        df_with_each_album_scored = p[order(p$value),]
        df_with_each_album_scored
        #####################################################################################################################
        
    })
    
    top_songs_happy = reactive({
        AFINN <- get_sentiments("afinn")
        original_data_bigrams = original_data %>%
            unnest_tokens(bigram, lyrics, token = 'ngrams', n = 2)
        original_separated <- original_data_bigrams %>%
            separate(bigram, c("word1", "word2"), sep = " ")
        
        
        
        full_data_without_filter <- original_separated %>%
            inner_join(AFINN, by = c(word2 = "word")) %>%
            count(word1, word2, value, sort = TRUE)
        
        list_of_negative_sentiment_words = c()
        for(i in 1:length(AFINN$word)){
            if(AFINN[i,2] < 0){
                list_of_negative_sentiment_words = c(list_of_negative_sentiment_words, AFINN[i, 1]$word)
            }
        } # all this for loop is doing is making a list of negative words so I can use %!in% later on for a list rather than DF.
        '%!in%' <- function(x,y)!('%in%'(x,y))
        
        negation_words <- c("not", "no", "never", "without")
        
        updated_table_for_all_values = full_data_without_filter %>%
            mutate(value = ifelse(word1 %in% list_of_negative_sentiment_words & word2 %in% list_of_negative_sentiment_words, abs(value), value)) %>%
            mutate(value = ifelse(word1 %in% negation_words, -abs(value), value))
        if(input$choose == 'both'){
            updated_table_for_all_values = updated_table_for_all_values
        }
        if(input$choose == 'pos'){
            updated_table_for_all_values = updated_table_for_all_values %>%
                filter(value > 0)
        }
        if(input$choose == 'neg'){
            updated_table_for_all_values = updated_table_for_all_values %>%
                filter(value < 0)
        }
        scores_of_each_song = c()
        for(i in 1:length(original_data$album)){
            dat = original_data[i,]
            unfilted_bigram = dat %>%
                unnest_tokens(bigram, lyrics, token = 'ngrams', n = 2)
            seperated_into_2_words <- unfilted_bigram %>%
                separate(bigram, c("word1", "word2"), sep = " ")
            join_without_filter <- seperated_into_2_words %>%
                inner_join(AFINN, by = c(word2 = "word")) %>%
                count(word1, word2, value, sort = TRUE)
            tbl_with_value = join_without_filter %>%
                mutate(value = ifelse(word1 %in% list_of_negative_sentiment_words & word2 %in% list_of_negative_sentiment_words, abs(value), value)) %>%
                mutate(value = ifelse(word1 %in% negation_words, -abs(value), value))
            number = quantile(data.frame(na.omit(tbl_with_value))$value, 0.5)
            scores_of_each_song = c(scores_of_each_song, number)
        }
        #quantile(data.frame(na.omit(updated_table_for_all_values))$value, 0.5)
        #data.frame(scores_of_each_song)
        original_data_with_sentiment_scores = cbind(original_data, data.frame(scores_of_each_song))
        original_data_with_sentiment_scores = original_data_with_sentiment_scores %>%
            arrange(desc(scores_of_each_song))
        head(original_data_with_sentiment_scores, 10)[-4]
        
        
        #MAKE THIS TOP 10 HAPPY SONGS
    })
    top_songs_sad = reactive({
        AFINN <- get_sentiments("afinn")
        original_data_bigrams = original_data %>%
            unnest_tokens(bigram, lyrics, token = 'ngrams', n = 2)
        original_separated <- original_data_bigrams %>%
            separate(bigram, c("word1", "word2"), sep = " ")
        
        full_data_without_filter <- original_separated %>%
            inner_join(AFINN, by = c(word2 = "word")) %>%
            count(word1, word2, value, sort = TRUE)
        
        list_of_negative_sentiment_words = c()
        for(i in 1:length(AFINN$word)){
            if(AFINN[i,2] < 0){
                list_of_negative_sentiment_words = c(list_of_negative_sentiment_words, AFINN[i, 1]$word)
            }
        } # all this for loop is doing is making a list of negative words so I can use %!in% later on for a list rather than DF.
        '%!in%' <- function(x,y)!('%in%'(x,y))
        
        negation_words <- c("not", "no", "never", "without")
        
        updated_table_for_all_values = full_data_without_filter %>%
            mutate(value = ifelse(word1 %in% list_of_negative_sentiment_words & word2 %in% list_of_negative_sentiment_words, abs(value), value)) %>%
            mutate(value = ifelse(word1 %in% negation_words, -abs(value), value))
        if(input$choose == 'both'){
            updated_table_for_all_values = updated_table_for_all_values
        }
        if(input$choose == 'pos'){
            updated_table_for_all_values = updated_table_for_all_values %>%
                filter(value > 0)
        }
        if(input$choose == 'neg'){
            updated_table_for_all_values = updated_table_for_all_values %>%
                filter(value < 0)
        }
        scores_of_each_song = c()
        for(i in 1:length(original_data$album)){
            dat = original_data[i,]
            unfilted_bigram = dat %>%
                unnest_tokens(bigram, lyrics, token = 'ngrams', n = 2)
            seperated_into_2_words <- unfilted_bigram %>%
                separate(bigram, c("word1", "word2"), sep = " ")
            join_without_filter <- seperated_into_2_words %>%
                inner_join(AFINN, by = c(word2 = "word")) %>%
                count(word1, word2, value, sort = TRUE)
            tbl_with_value = join_without_filter %>%
                mutate(value = ifelse(word1 %in% list_of_negative_sentiment_words & word2 %in% list_of_negative_sentiment_words, abs(value), value)) %>%
                mutate(value = ifelse(word1 %in% negation_words, -abs(value), value))
            number = quantile(data.frame(na.omit(tbl_with_value))$value, 0.5)
            scores_of_each_song = c(scores_of_each_song, number)
        }
        original_data_with_sentiment_scores = cbind(original_data, data.frame(scores_of_each_song))
        original_data_with_sentiment_scores = original_data_with_sentiment_scores %>%
            arrange(desc(scores_of_each_song))
        tail(na.omit(original_data_with_sentiment_scores), 10)[-4]
        
    })
    
    
    
    
    
    
    
    
    
    
    
    # Tab 2
    
    
    #  top_words_from_sad_song <- reactive({
    #  })
    
    
    # ===============================================
    # Outputs for the first TAB (i.e. barchart)
    # ===============================================
    
    # code for barplot
    output$bigram_plot <- renderPlot({
        # replace the code below with your code!!!
        counts_graph = tab2_graph()
        a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
        ggraph(counts_graph, layout = "fr") +
            geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                           arrow = a, end_cap = circle(.07, 'inches')) +
            geom_node_point(color = "lightblue", size = 5) +
            geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
            theme_void()
    })
    
    # code for numeric summaries of frequencies
    output$table1 <- renderDataTable({
        # replace the code below with your code!!!
        top_graph()
    })
    
    
    # ===============================================
    # Outputs for the second TAB (i.e. histogram)
    # ===============================================
    
    # code for histogram
    output$most_common_extreme_words <- renderPlot({
        var = tab1_graph()
        ggplot(var, aes(x = album, y = value))+
            geom_bar(stat = 'identity', width = 0.75, color = 'blue', fill = 'gold') +
            xlab('Mean Score of Each Album') + 
            ylab("Album")+ coord_flip()
        
    })
    
    # code for statistics
    output$table1 <- renderPrint({
        # replace the code below with your code!!!
        top_songs_happy()
    })
    output$table2 <- renderPrint({
        # replace the code below with your code!!!
        top_songs_sad()
    })
    
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)



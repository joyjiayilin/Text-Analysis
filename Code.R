# ===============================================
# Fill in the following fields
# ===============================================
# Title: Project 3
# Description: "State of the Union" Text Analysis
# Author: Joy Lin
# Date: 4/29/2022


# ===============================================
# Packages
# ===============================================
library(shiny)
library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(gridExtra)
library(grid)

# ===============================================
# Import data
# ===============================================
dat <- read.csv("state-union-2001-2022.csv")
message_tokens <- unnest_tokens(tbl = dat, 
                                output = word, 
                                input = message)

# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("'State of the Union' Text Analysis"),
  fluidRow(
    column(3,
           p(em("Most frequent words")),
           sliderInput(inputId = "top",
                       label = "Top",
                       min = 1,
                       max = 20,
                       value = 5)
    ),
    
    column(3,
           p(em("President(s)")),
           selectInput(inputId = "president", 
                       label = "Check names", 
                       choices = list("All" = "All", 
                                      "George W. Bush" = "George W. Bush",
                                      "Barack Obama" = "Barack Obama",
                                      "Donald J. Trump" = "Donald J. Trump",
                                      "Joseph R. Biden" = "Joseph R. Biden"), 
                       selected = "All")
    ),
    
    column(3,
           p(em("Per message")),
           checkboxInput(inputId = "per_message",
                         label = strong("Group by year"),
                         value = FALSE)
    ),    
    
    column(3,
           p(em("Stopwords")),
           checkboxInput(inputId = "stopwords",
                         label = strong("No stopwords"),
                         value = FALSE)
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("Word Frequency Analysis"),
                       plotOutput("barplot1",
                                  height = "800px"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3(" Sentiment Analysis"),
                       plotOutput("barplot2",
                                  height = "700px"),
                       hr(),
                       h3("Sentiment Score"),
                       verbatimTextOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  yes_all_all <- reactive({
    message_tokens %>% 
    count(word) %>%
    arrange(desc(n)) %>%
    slice_head(n = input$top)
  })
  
  yes_all_some <- reactive({
    message_tokens %>%
    filter(president == input$president) %>% 
    count(word) %>%
    arrange(desc(n)) %>%
    slice_head(n = input$top)
})
  
  yes_per_all <- reactive({
    message_tokens %>% 
    count(year,
          word,
          sort = TRUE) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    group_by(year) %>%
    slice_head(n = input$top)
  })
  
  yes_per_some <- reactive({
    message_tokens %>% 
    filter(president == input$president) %>% 
    count(year,
          word,
          sort = TRUE) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    group_by(year) %>%
    slice_head(n = input$top)
  })
  
  
  no_all_all <- reactive({
    message_tokens %>% 
    anti_join(stop_words, 
              by = "word") %>%
    count(word) %>% 
    arrange(desc(n)) %>%
    slice_head(n = input$top)
  })
  
  no_all_some <- reactive({
    message_tokens %>%
    filter(president == input$president) %>% 
    anti_join(stop_words, 
              by = "word") %>%
    count(word) %>%
    arrange(desc(n)) %>%
    slice_head(n = input$top)
  })
  
  no_per_all <- reactive({
    message_tokens %>% 
    anti_join(stop_words, 
              by = "word") %>%
    count(year,
          word,
          sort = TRUE) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    group_by(year) %>%
    slice_head(n = input$top)
  })
  
  no_per_some <- reactive({
    message_tokens %>% 
    filter(president == input$president) %>% 
    anti_join(stop_words, 
              by = "word") %>%
    count(year,
          word,
          sort = TRUE) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    group_by(year) %>%
    slice_head(n = input$top)
  })
  
  pos_sentiments <- dat %>%
    unnest_tokens(output = word, 
                  input = message) %>%
      anti_join(stop_words, 
                by = "word") %>%
      group_by(president) %>%
      count(word, 
            sort = TRUE) %>%
      ungroup() %>%
      inner_join(sentiments, 
                 by = "word") %>%
      filter(sentiment == "positive") %>%
      arrange(desc(n))
  
  neg_sentiments <- dat %>%
    unnest_tokens(output = word, 
                  input = message) %>%
      anti_join(stop_words, 
                by = "word") %>%
      group_by(president) %>%
      count(word, 
            sort = TRUE) %>%
      ungroup() %>%
      inner_join(sentiments, 
                 by = "word") %>%
      filter(sentiment == "negative") %>%
      arrange(n)
  
  dat_sentiments <- reactive({
    rbind(head(pos_sentiments, input$top / 2), 
          tail(neg_sentiments, input$top / 2))
  })
  
  
  test <- reactive({
    dat %>%
    unnest_tokens(output = word, 
                  input = message) %>%
    anti_join(stop_words, 
              by = "word") %>%
    count(president,
          word,
          sort = TRUE) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    inner_join(sentiments, 
               by = "word") %>%
    group_by(president) %>%
    arrange(desc(n)) %>%
    slice_head(n = input$top)
  })
  
  
  dat_sentiments_2 <- reactive({
    dat %>%
      mutate(total = str_count(dat$message, 
                               "\\w+")) %>%
      unnest_tokens(output = word, 
                    input = message) %>%
      anti_join(stop_words, 
                by = "word") %>%
      count(president,
            word,
            total,
            year,
            sort = TRUE) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      inner_join(sentiments, 
                 by = "word") %>%
      count(president, 
            index = year, 
            sentiment,
            total) %>%
      spread(sentiment, 
             n, 
             fill = 0) %>%
      mutate(sentiment = (positive - negative) / total)
  })
  
  # ===============================================
  # Outputs for the first TAB (i.e. barplot1)
  # ===============================================
  
  # code for barplot1
  output$barplot1 <- renderPlot({
  if (input$stopwords == FALSE) {
    if(input$per_message == FALSE) {
      if (input$president == "All") {                 # stopwords, all message, all president
        ggplot(data = yes_all_all(),
               aes(x = reorder(word, 
                               -n), 
                   y = n)) +
          geom_col(fill = "orange") + 
          labs(title = paste0("Top ", 
                              input$top, 
                              " frequent words")) +
          xlab("word") +
          ylab("count") +
          theme_classic()
      } else {                                        # stopwords, all message, some president
        ggplot(data = yes_all_some(),
               aes(x = reorder(word, 
                               -n), 
                   y = n)) +
          geom_col(fill = "orange") + 
          labs(title = paste0("Top ", 
                              input$top, 
                              " frequent words for ", 
                              input$president)) +
          xlab("word") +
          ylab("count") +
          theme_classic()
      }
    } else {       
      if (input$president == "All") {                 # stopwords, per message, all president
        ggplot(data = yes_per_all(),
               aes(x = reorder_within(word, 
                                      n, 
                                      year), 
                   y = n)) +
          geom_col(fill = "orange") + 
          scale_x_reordered() +
          facet_wrap(~ year, scales = "free") +
          xlab(NULL) +
          coord_flip() +
          labs(title = paste0("Top ", 
                              input$top, 
                              " frequent words")) +
          theme_classic()
      } else {                                         # stopwords, per message, some president
        ggplot(data = yes_per_some(),
               aes(x = reorder_within(word, 
                                      n, 
                                      year), 
                   y = n)) +
          geom_col(fill = "orange") + 
          scale_x_reordered() +
          facet_wrap(~ year, scales = "free") +
          xlab(NULL) +
          coord_flip() +
          labs(title = paste0("Top ", 
                              input$top, 
                              " frequent words for ", 
                              input$president)) +
          theme_classic()
      }
    }
    
  } else {
    if(input$per_message == FALSE) {
      if (input$president == "All") {                 # no stopwords, all message, all president
        ggplot(data = no_all_all(),
               aes(x = reorder(word, 
                               -n), 
                   y = n)) +
          geom_col(fill = "orange") + 
          labs(title = paste0("Top ", 
                              input$top, 
                              " frequent words")) +
          xlab("word") +
          ylab("count") +
          theme_classic()
      } else {                                        # no stopwords, all message, some president
        ggplot(data = no_all_some(),
               aes(x = reorder(word, 
                               -n), 
                   y = n)) +
          geom_col(fill = "orange") + 
          labs(title = "Top ", 
               input$top, 
               " frequent words for ", 
               input$president) +
          xlab("word") +
          ylab("count") +
          theme_classic()
      }
    } else {       
      if (input$president == "All") {                 # no stopwords, per message, all president
        ggplot(data = no_per_all(),
               aes(x = reorder_within(word, n, year), 
                   y = n)) +
          geom_col(fill = "orange") + 
          scale_x_reordered() +
          facet_wrap(~ year, scales = "free") +
          xlab(NULL) +
          coord_flip() +
          labs(title = "Top ", 
               input$top, 
               " frequent words") +
          theme_classic()
      } else {                                        # no stopwords, per message, some president
        ggplot(data = no_per_some(),
               aes(x = reorder_within(word, n, year), 
                   y = n)) +
          geom_col(fill = "orange") + 
          scale_x_reordered() +
          facet_wrap(~ year, scales = "free") +
          xlab(NULL) +
          coord_flip() +
          labs(title = "Top ", 
               input$top, 
               " frequent words for ", 
               input$president) +
          theme_classic()
      }
    }
  }
  })
  
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    if (input$stopwords == FALSE) {
      if(input$per_message == FALSE) {
        if (input$president == "All") {                 # stopwords, all message, all president
          yes_all_all()
        } else {                                        # stopwords, all message, some president
          yes_all_some()
        }
      } else {       
        if (input$president == "All") {                 # stopwords, per message, all president
          yes_per_all()
        } else {                                        # stopwords, per message, some president
          yes_per_some()
        }
      }
      
    } else {
      if(input$per_message == FALSE) {
        if (input$president == "All") {                 # no stopwords, all message, all president
          no_all_all()
        } else {                                        # no stopwords, all message, some president
          no_all_some()
        }
      } else {       
        if (input$president == "All") {                 # no stopwords, per message, all president
          no_per_all()
        } else {                                        # no stopwords, per message, some president
          no_per_some()
        }
      }
    }

  })
  
  
  # ===============================================
  # Outputs for the second TAB (i.e. barplot2)
  # ===============================================
  
  # code for barplot2
  output$barplot2 <- renderPlot({
    
    barplot2_1 <- test() %>%
      ggplot() +
      geom_col(aes(x = word,
                   y = n, 
                   fill = sentiment)) +
      scale_fill_manual(values=c("cornflowerblue", 
                                 "orange")) +
      scale_x_reordered() +
      facet_wrap(~ president, 
                 scales = "free") +
      xlab(NULL) + 
      coord_flip() +
      labs(title = paste0(input$top, 
                          " most common words of presidents with an associated sentiment"),
           x = "Word",
           y = "Count") +
      theme_classic()
    
    barplot2_2 <- ggplot(data = dat_sentiments_2(),
           aes(x = index, 
               y = sentiment, 
               fill = president)) +
      geom_col() +
      #facet_wrap(~ president, 
                 #ncol = 2, 
                 #scales = "free_x") +
      labs(title = "Sentiment through for each message of each president") +
      theme_classic()
    
    grid.arrange(barplot2_1, barplot2_2, ncol=1)
    
  })
  
  # code for statistics
  output$table2 <- renderPrint({
    summary(dat_sentiments_2()$sentiment)
  })
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)


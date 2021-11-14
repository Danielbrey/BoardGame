library(shiny)
library(tidyverse)
library(rvest)
library(shinydashboard)
library(DT)
library(shinythemes)
library(corrplot)
#install.packages("shinythemes")
#install.packages("DT")

#ADD GRAPHS BASED ON TIME LIMIT OR NUMBER OF PLAYERS LIKE THE CATEGORY

boardgames.data <- read_csv("boardgamesFinal.csv")

boardgames.data$details.description <- str_replace_all(boardgames.data$details.description,
                                                       "&#[:digit:]+;",
                                                       " ")
boardgames.data$details.description <- str_replace_all(boardgames.data$details.description,
                                                       "&quot",
                                                       "")
boardgames.data$details.description <- str_replace_all(boardgames.data$details.description,
                                                       "&rsquo;",
                                                       "'")
boardgames.data$details.description <- str_replace_all(boardgames.data$details.description,
                                                       "&mdash;",
                                                       "-")

bg.corrdata <- boardgames.data %>% 
    filter(stats.averageweight != 0,
           details.playingtime < 500,
           details.maxplayers < 15)

ui <- dashboardPage(
    dashboardHeader(title = "BoardTogether"),
    
    #These inputs go into the sidebar
    dashboardSidebar(width = 230,
                     sidebarMenu(id = "tabs",style = "position:fixed;width:230px;",
                                 menuItem(style = "position:fixed;width: inherit;",
                                          numericInput(inputId="choose.num.players",
                                                       label = "Number of players",
                                                       value = 2,
                                                       min = 1,
                                                       max = 50)),
                                 menuItem(style = "position:fixed;width: inherit;",
                                          sliderInput(inputId="choose.min.playtime",
                                                      label="Minimum playtime",
                                                      value = 30,
                                                      min = 1,
                                                      max = 450)),
                                 menuItem(style = "position:fixed;width: inherit;",
                                          sliderInput(inputId = "choose.max.playtime",
                                                      label = "Maximum playtime",
                                                      value = 60,
                                                      min = 1,
                                                      max = 450)),
                                 menuItem(style = "position:fixed;width: inherit;",
                                          selectInput(inputId = "choose.category",
                                                      label = "Board game category",
                                                      choices = c("Political",
                                                                  "Dice",
                                                                  "Bluffing",
                                                                  "Wargame",
                                                                  "Card Game",
                                                                  "Party",
                                                                  "Economic",
                                                                  "Abstract Strategy")))
                     )),
    dashboardBody(
        tags$head(tags$style(HTML("
       
       #This code dictates the colors & font for the dashboard
       
        @import url('//fonts.googleapis.com/css2?family=Josefin+Sans&display=swap');
 
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #015a60;
                                }
                                
                                .main-header .logo {
                                font-family: 'Josefin Sans', sans-serif;
                                font-weight: italic;
                                font-size: 24px;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: ##015a60;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #015a60;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #09666d;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #09666d;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #09666d;
                                color: #ffffff;
                                }

                            
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }

                                "))),
        tabsetPanel(
            tabPanel("Introduction",
                     htmlOutput(outputId = "intro")
            ),
            tabPanel("Chart", 
                     DT::dataTableOutput(outputId = "table1")
            ),
            tabPanel("Graphs", fluid = TRUE,
                     plotOutput(outputId = "plot1"),
                     plotOutput(outputId = "plot2")
            ),
            tabPanel("Hypotheses", fluid = TRUE,
                     htmlOutput(outputId = "hypotheses")),
            tabPanel("Findings", fluid = TRUE,
                     htmlOutput(outputId = "findings.intro"),
                     plotOutput(outputId = "findings.plot"),
                     htmlOutput(outputId = "findings.rest"),
                     verbatimTextOutput(outputId = "corr1"),
                     verbatimTextOutput(outputId = "corr2"),
                     verbatimTextOutput(outputId = "corr3"),
                     verbatimTextOutput(outputId = "corr4"))
            
            
            
        )
    )
    
)

server <- function(input, output) {
    
    output$intro <- renderUI({
        str1 <- "After the hundredth victory, even a good old game of Diplomacy can get old. Unfortunately, discovering fun new board games to play can be a puzzle of its own. We’ve found ourselves blindly searching Google, sifting through internet forums for recommendations, and even wading into the murky waters of crowdsourced Kickstarters. Looking for a fun way to spend the evening should not take up an entire evening."
        str2<- "This is where BoardTogether comes in. Our app allows users to find out what game to play, based on how much time (minutes) they have, what genre they are interested in, and how many people are in their group. Tell us what you’re looking for, and we’ll display a list of the top five games that meet those criteria, arranged in order of how liked the games are by other avid board game players."
        str3 <- "Our data comes from BoardGameGeek: a popular game database and online forum that hosts ratings, images, and videos for over 100,000 different tabletop games. We include a link to each game’s BoardGameGeek webpage in the Name column of our table. A brief summary of the game is under the Description column to the right. If a description intrigues you, feel free to learn more about the game by investigating its profile on BoardGameGeek."
        str4 <- "BoardGameGeek allows users to rate their experiences with a given game on a 1-10 scale. We display the average user rating for a game under the Average Rating column. Users can go on to assign a “weight” value to a game on a 1-5 scale, which is a metric used to describe its complexity. The higher the weight, the more complicated the game is. We display the average user-provided weight of a game under the Average Weight column. (Going forward, we use the terms “weight” and “complexity” interchangeably.)"
        str5 <- "Finally, BoardTogether also displays density plots for the Average Rating and Average Complexity of the game genre you’ve chosen. These are here to allow you to assess the rating or complexity of a game in relation to others in its genre—not unlike grading on a curve. You might notice that Economic games tend to have a higher complexity rating, and that Political games tend to receive higher user ratings than games in other genres. Our plots allow you to account for these nuances and take each metric with a grain of salt."
        HTML(paste(str1, str2, str3, str4, str5, sep = "<br/><br/>"))
    })
    
    
    output$table1 <- DT::renderDataTable({
    tryCatch({
        modified.games<- boardgames.data %>% 
            filter(details.maxplayers >= input$choose.num.players,
                   details.minplayers <= input$choose.num.players,
                   details.playingtime <= input$choose.max.playtime,
                   details.playingtime >= input$choose.min.playtime,
                   str_detect(boardgames.data$attributes.boardgamecategory,
                              input$choose.category) == TRUE) %>% 
            arrange(-stats.average) %>% 
            head(5)
        
        game.id.list <- modified.games$game.id
        game.names.list <- str_replace_all(modified.games$details.name,
                                           "[:blank:]",
                                           "_")
        links <- NULL
        for(i in 1:5){
            url1 <- paste0("https://www.boardgamegeek.com/boardgame/", game.id.list[i], "/", game.names.list[i])
            hyperlink <- paste0("<a href='",url1,"'>",modified.games$details.name[i],"</a>")
            links[[i]] <- hyperlink
        }
        modified.games$details.name <- links
        modified.games$details.description <- paste0(str_sub(modified.games$details.description, 1, 250), "...")
        mod.games.2 <- modified.games %>% 
            select(details.name, details.description, stats.average, stats.averageweight)
        colnames(mod.games.2)<- c("Name", "Description", "AverageRating", "AverageWeight")
        mod.games.2
    },
    
    warning=function(w) { 
        modified.games
    },
    error=function(e) {
        modified.games
    })
},
options = list(
    dom = 't',
    ordering = FALSE,
    #rowCallback = JS("function(r,d) {$(r).attr('height', '10px')}"),
    searching = FALSE,
    paging = FALSE),
escape = FALSE)
    
    #Plot 1: Density vs. Rating
    output$plot1 <- renderPlot({
        rating.data <- boardgames.data %>% 
            filter(! is.na(boardgames.data$attributes.boardgamecategory)) 
        
        rating.data %>% 
            ggplot(aes(x = stats.average))+
            geom_density(color = "white",
                         aes(fill = str_detect(rating.data$attributes.boardgamecategory,
                                               input$choose.category)),
                         alpha = 0.5)+
            scale_fill_manual(values = c("turquoise4", "lightskyblue"),
                              labels = c(paste0("Not ", input$choose.category, " Games"), paste0(input$choose.category, " Games"))) +
            labs(title = "Rating distribution based on game category",
                 subtitle='How does the rating for your game stack up to others in its category?',
                 y = "DENSITY",
                 x = "RATING",
                 fill = "Type of game")+
            theme(plot.title = element_text(family = 'Georgia'))
    })
    
    #Plot 2: Density vs. Complexity (Weight)
    output$plot2 <- renderPlot({
        weight.data<- boardgames.data %>% 
            filter(! is.na(boardgames.data$attributes.boardgamecategory))
        
        weight.data %>% 
            ggplot(aes(x = stats.averageweight))+
            geom_density(color = "white",
                         aes(fill = str_detect(weight.data$attributes.boardgamecategory,
                                               input$choose.category)),
                         alpha = 0.5) +
            scale_fill_manual(values = c("goldenrod", "coral"),
                              labels = c(paste0("Not ", input$choose.category, " Games"), paste0(input$choose.category, " Games"))) +
            labs(title = "Complexity distribution based on game category",
                 subtitle='How does the complexity of your game stack up to others in its category?',
                 y = "DENSITY",
                 x = "COMPLEXITY",
                 fill = "Type of game")+
            theme(plot.title = element_text(family = 'Georgia'))
    })
    
    
    #Hypotheses
    output$hypotheses <- renderUI({
        str_1<- "How do factors such as average user rating, playing time, complexity, and number of players interact with each other for a given boardgame? We hypothesize that there are several relationships of interest in this dataset."
        str_2 <- "First, it seems possible that a higher ‘maximum number of players’ will correlate with both greater complexity and a longer playing time. Our intuition here is that the time between turns for games of, for instance, eight players will pad how long the game takes to play, and that the many moving parts associated with so many players will coincide with how complicated the game itself is. We imagine lengthy, complex, many-player games such as Diplomacy, Monopoly, and Risk when making this assumption."
        str_3 <- "Second, it also seems possible that greater complexity will correlate with a higher average user ratings. Our reasoning here is based on the origin of our dataset: the standard user that interacts with the website BoardGameGeek, from which we derive our rating data, may prefer more complex games as a result of their special interest in board games. In other words, the type of person who takes the time to rate boardgames online might also be the type of person who prefers complicated or ‘advanced’ board games."
        HTML(paste(str_1, str_2, str_3, sep = "<br/><br/>"))
    })
    
    output$findings.intro <- renderUI({
        HTML("To test these hypotheses, we’ll construct a correlation plot that includes all of the variables listed above.")
    })
    
    output$findings.plot <- renderPlot({
        bg.corrplot<- bg.corrdata %>% 
            select(details.maxplayers, details.minplayers, details.playingtime, stats.average, stats.averageweight)
        
        cor.matrix <- cor(bg.corrplot)
        rownames(cor.matrix) <- c("Max Players",
                                  "Minimum Players",
                                  "Playing Time",
                                  "Average Rating",
                                  "Average Weight Rating")
        colnames(cor.matrix) <- c("Max Players",
                                  "Minimum Players",
                                  "Playing Time",
                                  "Average Rating",
                                  "Average Weight Rating")
        corrplot(cor.matrix)
        
    })
    
    output$findings.rest <- renderUI({
        str.0 <- "Note: The original data from BoardGameGeek.com has been modified for relevance. Because anyone can submit a game to BoardGameGeek, the website’s vast catalogue is peppered with major outliers—extremely long games or ones with extremely large player counts. As board game fans ourselves, we suspect that these niche games are very rarely what a player is looking for; the outliers seem more likely to clog search results than augment them. It is for this reason that we’ve excluded games with a playing time that is greater than 500 minutes or maximum player-count of more than 15 players. We have also excluded board games with an Average Complexity of 0, as such a rating suggests that the game was submitted by an individual user, as opposed to official board game manufacturer or organization."
        str.1 <- "Contrary to our first assumption, there was no significant correlation between the maximum number of players for a game and its average complexity (R-Squared = 0.050), and even less of a correlation between the maximum number of players for a game and its average playing time (R-Squared = 0.022). We initially believed that games with many players would be longer, because more players might result in more time being dedicated to taking turns. However, we did not account for the likelihood that the time it takes for a player to complete a turn varies widely from game to game, and depends largely on the game’s complexity. Consider the long, cerebral encounters of two-player Chess versus the fast-paced action of 10-player Egyptian Ratscrew. Indeed, it appears that the length of a tabletop game is far less determined by player count than it is by game complexity. The correlation plot validates such reasoning by showing a strong positive correlation between longer average playing time and higher average complexity (R-Squared = 0.374)."
        str.2 <- "Our second hypothesis suggested that more complex games would, on average, receive higher ratings from users. The correlation plot seems to suggest this view: we see a moderately positive correlation between average user rating and average game complexity (R-Squared = 0.232). There are several possible reasons for such a relationship. As previously mentioned, it may be because more complex games can be interpreted as “advanced” ones, and users of BoardGameGeek are likely to prefer such advanced games due to their special interest in the field. To illustrate this logic, consider cycling. Highly competent cyclists prefer multi-speed bikes to fixed-gear ones—although navigating multiple gear settings on a bike is a more complex task, with this increased challenge comes increased possibilities."
        str.3 <- 'Another potential reason for the positive correlation between complexity and user ratings has to do with conventional psychology. Commitment bias is defined as the tendency for an individual to maintain behaviors that align with, or build upon, their previous decisions and actions. For instance, someone who has spent a lot of money on a new sports car is likely to think and speak very highly of their experience driving it, regardless of how well the car actually drives. In fact, this person might choose to dedicate even more money to improving the car, even if selling it and purchasing an altogether different vehicle would be more financially rational. The phenomenon here is that the buyer made a significant commitment—time and money—to the purchase of the sports car, and so their assessment and decision-making going forward will be irrationally biased in favor of validating that original commitment. In the same way, a player who has dedicated much of their time and effort to playing a complex and intellectually challenging board game might be biased in favor of validating their original commitment, and do so by giving the boardgame a high rating. The internal thought process here is: “I spent a lot of time playing that game. I wouldn’t waste my own time, so the game must have been fun.” '
        HTML(paste(str.0, str.1, str.2, str.3, sep = "<br/><br/>"))
    })
    
    output$corr1 <- renderPrint({
        lm1 <- lm(stats.averageweight ~ details.maxplayers, data = bg.corrdata)
        summary(lm1)
    })
    
    output$corr2 <- renderPrint({
        lm2 <- lm(details.playingtime ~ details.maxplayers, data = bg.corrdata)
        summary(lm2)
    })
    
    output$corr3 <- renderPrint({
        lm3 <- lm(stats.averageweight ~ details.playingtime, data = bg.corrdata)
        summary(lm3)
    })
    
    output$corr4 <- renderPrint({
        lm4 <- lm(stats.average ~ stats.averageweight, data = bg.corrdata)
        summary(lm4)
    })
}


shinyApp(ui = ui, server = server)



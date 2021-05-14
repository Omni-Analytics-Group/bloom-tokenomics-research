#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(shinythemes)
library(ggrepel)
library(scales)
library(thematic)

options(scipen = 999)
thematic_shiny()

addResourcePath("images", "images")

x <- read_csv("etherscan_tokens_cleaned.csv")
t_choices <- x$Ticker
names(t_choices) <- paste0(x$Name, " (", x$Ticker, ")")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),

    # Application title
    titlePanel(
        fluidRow(
            column(11, "Etherscan Holder vs Market Capitalization Explorer - powered by Bloom Research"), 
            column(1, img(width = 80, src = "images/bloom.png"))
        )
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("Introduction"),
            helpText("Feel free to use this application to explore the relationship between the number of holders and the market capitalization of popular tokens tracked on Etherscan.  The plot highlights the positive correlation between the two factors, and is segmented into four regions colored according to the perceived risk to the token holders. We assume that tokens with few holders but a high market capitalization are more susceptible to a sudden reduction of liquidity."),
            hr(),
            h4("Configuration"),
            checkboxInput("ranks", "Use Ranks"),
            checkboxInput("fit", "Show Fit"),
            checkboxInput("labels", "Show All Labels"),
            checkboxInput("hide_all", "Hide All Points"),
            hr(),
            h4("Token Selection"),
            selectInput("token", "Highlight Token", choices = t_choices, multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("mcap", height = "600px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mcap <- renderPlot({
        
        y <- x %>%
            mutate(Label = ifelse(Holders >= quantile(Holders, .975) | `Market Cap` >= quantile(`Market Cap`, .975) | Holders <= quantile(Holders, .025) | `Market Cap`  <= quantile(`Market Cap`, .025), Ticker, NA),
                   AllLabel = Ticker)
        if (input$ranks) {
            y <- y %>%
                mutate(Holders = rank(Holders),
                       `Market Cap` = rank(`Market Cap`))
        }
        
        p <- ggplot(data = y, aes(x = Holders, y = `Market Cap`)) +
            scale_x_continuous(trans = 'log10',
                               breaks = trans_breaks('log10', function(x) 10^x, n = 10),
                               labels = function(.) scales::comma(., accuracy = 1),
                               limits = c(1, 10000000)) +
            scale_y_continuous(trans = 'log10',
                               breaks = trans_breaks('log10', function(x) 10^x, n = 10),
                               labels = scales::comma,
                               limits = c(10, 100000000000)) +
            coord_fixed(ratio = 0.75) +
            labs(
                x = "Number of Holders",
                y = "Market Capitalization",
                title = "Market Capitalization as a function of Wallet Holders",
                subtitle = "Data from Etherscan 5/14/21"
            )
        
        if (!input$hide_all) {
            p <- p + geom_point()
        }
        
        if (!input$ranks) {
            p <- p +             annotate("rect", xmin = 0, ymin = 0, xmax = median(y$Holders, na.rm = TRUE), ymax = median(y$`Market Cap`, na.rm = TRUE),
                                          fill = "orange", alpha = 0.2) +
                annotate("rect", xmin = 0, ymin = median(y$`Market Cap`), xmax = median(y$Holders, na.rm = TRUE), ymax = Inf,
                         fill = "red", alpha = 0.2) +
                annotate("rect", xmin = median(y$Holders), ymin = 0, xmax = Inf, ymax = median(y$`Market Cap`, na.rm = TRUE),
                         fill = "green", alpha = 0.2) + 
                annotate("rect", xmin = median(y$Holders), ymin = median(y$`Market Cap`, na.rm = TRUE), xmax = Inf, ymax = Inf,
                         fill = "blue", alpha = 0.2)
        }
        
        if (input$ranks) {
            p <- p +
                scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                                   labels = scales::comma) +
                scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                                   labels = scales::comma) +
                coord_fixed(ratio = 0.9) +
                annotate("rect", xmin = -Inf, ymin = -Inf, xmax = median(y$Holders, na.rm = TRUE), ymax = median(y$`Market Cap`, na.rm = TRUE),
                         fill = "orange", alpha = 0.2) +
                annotate("rect", xmin = -Inf, ymin = median(y$`Market Cap`), xmax = median(y$Holders, na.rm = TRUE), ymax = Inf,
                         fill = "red", alpha = 0.2) +
                annotate("rect", xmin = median(y$Holders), ymin = -Inf, xmax = Inf, ymax = median(y$`Market Cap`, na.rm = TRUE),
                         fill = "green", alpha = 0.2) + 
                annotate("rect", xmin = median(y$Holders), ymin = median(y$`Market Cap`, na.rm = TRUE), xmax = Inf, ymax = Inf,
                         fill = "blue", alpha = 0.2)
        }
        
        if (!is.null(input$token)) {
            p <- p +
                geom_point(data = y %>% filter(Ticker %in% input$token), size = 5, colour = "goldenrod3")
        }
        
        if (input$fit && input$ranks) {
            p <- p + geom_abline(slope = 1, intercept = 0, colour = "green4")
        }
        
        if (input$fit && !input$ranks) {
            p <- p + geom_smooth(method = "lm")
        }
        
        if (input$labels) {
            p <- p + geom_text(aes(label = AllLabel))
        } else if (!input$hide_all) {
            p <- p + geom_text_repel(aes(label = Label))
        } else {
            p <- p + geom_text_repel(data = y %>% filter(Ticker %in% input$token), aes(label = Label))
        }
        
        return(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)

df = bind_rows(read_csv("convert_to_tadj1.csv") %>%
              select(benchmark, n, time) %>%
              mutate(which = "convert_to_tadj"),
          read_csv("develop.csv") %>%
              select(benchmark, n, time) %>%
              mutate(which = "develop"),
          read_csv("fdf5db8.csv") %>%
              select(benchmark, n, time) %>%
              mutate(which = "release")) %>%
    spread(which, time) %>%
    mutate(develop = release / develop) %>%
    mutate(tadj = release / convert_to_tadj) %>%
    select(benchmark, n, develop, tadj) %>%
    gather(which, speedup, develop, tadj)

benchmarks = df %>% pull(benchmark) %>% unique

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Stan Math Benchmarks"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("select", "Select function", 
                        choices = benchmarks)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        df %>%
            filter(benchmark == input$select) %>%
            ggplot(aes(n, speedup)) +
            geom_hline(yintercept = 1.0, linetype = "dashed", color = "black", size = 1) +
            geom_point(aes(color = which), size = 1) +
            geom_line(aes(color = which), size = 1) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  text = element_text(size=20)) +
            scale_x_continuous(trans = log2_trans(), labels = 2^(1:10), breaks = 2^(1:10)) +
            scale_y_log10() +
            ggtitle("Speedup vs. 3.3.0 release\n (Dashed line is 3.3.0 release,\n   Higher is better)")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)

df = bind_rows(read_csv("convert_to_tadj1.csv") %>%
              mutate(which = "convert_to_tadj"),
          read_csv("develop.csv") %>%
              mutate(which = "develop"),
          read_csv("fdf5db8.csv") %>%
              mutate(which = "release")) %>%
    rename(time = real_time) %>%
    select(name, time, which) %>%
    filter(!str_detect(name, "_mean$") &
               !str_detect(name, "_median$") &
               !str_detect(name, "_stddev$") &
               !str_detect(name, "toss_me")) %>%
    separate(name, c("name", "n", "tmp"), "/") %>%
    mutate(n = as.integer(n)) %>%
    select(-tmp)

df = df %>%
    group_by(name, n, which) %>%
    mutate(r = row_number()) %>%
    spread(which, time) %>%
    mutate(develop = release / develop) %>%
    mutate(tadj = release / convert_to_tadj) %>%
    select(name, n, develop, tadj) %>%
    gather(which, speedup, develop, tadj) %>%
    group_by(name, n, which) %>%
    summarize(ql = quantile(speedup, 0.25),
              m = median(speedup),
              qh = quantile(speedup, 0.75))

benchmark_names = df %>% pull(name) %>% unique

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Stan Math Benchmarks"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("select", "Select function", 
                        choices = benchmark_names)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2("Speedup vs. 3.3.0 release"),
            h4("Dashed line is 3.3.0 release"),
            h4("Higher is better"),
            h4("Ribbons are 50% intervals on 30 samples"),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        df %>%
            filter(name == input$select) %>%
            ggplot(aes(n, m)) +
            geom_ribbon(aes(n, ymin = ql, ymax = qh, fill = which), alpha = 0.5) +
            geom_hline(yintercept = 1.0, linetype = "dashed", color = "black", size = 1) +
            geom_point(aes(color = which), size = 1) +
            geom_line(aes(color = which), size = 1) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  text = element_text(size=20)) +
            scale_x_continuous(trans = log2_trans(), labels = 2^(1:10), breaks = 2^(1:10)) +
            scale_y_log10()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

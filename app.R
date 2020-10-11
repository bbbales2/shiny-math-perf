library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)
library(DT)

df = readRDS("df.rds")

benchmarks = df %>% pull(benchmark) %>% unique

# Define UI for application that draws a histogram
ui <- fluidPage(
    title = "Performance explorer",
    hr(),
    fluidRow(
        column(6, selectInput('base_benchmark', 'Baseline', benchmarks, selected = "3492945_349294501c_matvar")),
        column(6, selectInput('comp_benchmark', 'Comparison', benchmarks, selected = "a_few_varmats_be2f717020_matvar"))
    ),
    hr(),
    dataTableOutput('table'),
    hr(),
    selectInput('func', 'Function', c()),
    plotOutput('plot_ratio'),
    plotOutput('plot')
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observe({
        names_intersection = intersect(unique(df %>% filter(benchmark == input$base_benchmark) %>% pull(name)),
                                       unique(df %>% filter(benchmark == input$comp_benchmark) %>% pull(name)))
        
        if(input$func %in% names_intersection) {
            to_select = input$func
        } else {
            to_select = head(names_intersection, 1)
        }
        
        updateSelectInput(session, 'func',
                          label = 'Function',
                          choices = names_intersection,
                          selected = to_select)
    })
    
    output$table <-
        renderDataTable({
            base_df = df %>%
                filter(benchmark == input$base_benchmark) %>%
                select(-benchmark) %>%
                mutate(which = "base")
            
            comp_df = df %>%
                filter(benchmark == input$comp_benchmark) %>%
                select(-benchmark) %>%
                mutate(which = "comp")
            
            names_intersection = intersect(unique(base_df %>% pull(name)),
                                           unique(comp_df %>% pull(name)))
            
            base_df = base_df %>% filter(name %in% names_intersection)
            comp_df = comp_df %>% filter(name %in% names_intersection)
            
            total_df = bind_rows(base_df, comp_df)
            
            if(nrow(total_df) > 0) {
                to_render_df = total_df %>%
                    group_by(name, n, which) %>%
                    mutate(r = row_number()) %>%
                    spread(which, time) %>%
                    drop_na() %>%
                    mutate(speedup = base / comp) %>%
                    select(name, n, speedup) %>%
                    group_by(name, n) %>%
                    summarize(median_speedup = median(speedup)) %>%
                    group_by(name) %>%
                    summarize(min_median_speedup = min(median_speedup),
                              max_median_speedup = max(median_speedup),
                              N_of_min_median_speedup = n[which.min(median_speedup)],
                              N_of_max_median_speedup = n[which.max(median_speedup)]) %>%
                    datatable() %>%
                    formatRound(columns = c('min_median_speedup', 'max_median_speedup'), digits = 3)

                return(to_render_df)
            }
        })
    
    output$plot_ratio <- renderPlot({
        base_df = df %>%
            filter(benchmark == input$base_benchmark,
                   name == input$func) %>%
            select(-benchmark) %>%
            mutate(which = "base")
        
        comp_df = df %>%
            filter(benchmark == input$comp_benchmark,
                   name == input$func) %>%
            select(-benchmark) %>%
            mutate(which = "comp")
        
        names_intersection = intersect(unique(base_df %>% pull(name)),
                                       unique(comp_df %>% pull(name)))
        
        base_df = base_df %>% filter(name %in% names_intersection)
        comp_df = comp_df %>% filter(name %in% names_intersection)
        
        total_df = bind_rows(base_df, comp_df)
        
        if(nrow(total_df) > 0) {
            to_render_plot = total_df %>%
                group_by(name, n, which) %>%
                mutate(r = row_number()) %>%
                spread(which, time) %>%
                drop_na() %>%
                mutate(speedup = base / comp) %>%
                group_by(name, n) %>%
                summarize(ql = quantile(speedup, 0.25),
                          m = median(speedup),
                          qh = quantile(speedup, 0.75)) %>%
                ggplot(aes(n, m)) +
                    geom_ribbon(aes(n, ymin = ql, ymax = qh), alpha = 0.5) +
                    geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", size = 1) +
                    geom_point(size = 1) +
                    geom_line(size = 1) +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1),
                          text = element_text(size=20)) +
                    scale_x_continuous(trans = log2_trans(), labels = 2^(1:10), breaks = 2^(1:10)) +
                    scale_y_log10() +
                ylab("speedup")
            
            return(to_render_plot)
        }
    })
    
    output$plot <- renderPlot({
        base_df = df %>%
            filter(benchmark == input$base_benchmark,
                   name == input$func) %>%
            select(-benchmark) %>%
            mutate(which = "base")
        
        comp_df = df %>%
            filter(benchmark == input$comp_benchmark,
                   name == input$func) %>%
            select(-benchmark) %>%
            mutate(which = "comp")
        
        names_intersection = intersect(unique(base_df %>% pull(name)),
                                       unique(comp_df %>% pull(name)))
        
        base_df = base_df %>% filter(name %in% names_intersection)
        comp_df = comp_df %>% filter(name %in% names_intersection)
        
        total_df = bind_rows(base_df, comp_df)
        
        if(nrow(total_df) > 0) {
            to_render_plot = total_df %>%
                drop_na() %>%
                group_by(name, n, which) %>%
                summarize(ql = quantile(time, 0.25),
                          m = median(time),
                          qh = quantile(time, 0.75)) %>%
                ggplot(aes(n, m)) +
                geom_ribbon(aes(n, ymin = ql, ymax = qh, fill = which), alpha = 0.5) +
                geom_point(aes(color = which), size = 1) +
                geom_line(aes(color = which), size = 1) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      text = element_text(size=20)) +
                scale_x_continuous(trans = log2_trans(), labels = 2^(1:10), breaks = 2^(1:10)) +
                scale_y_log10() +
                ylab("time (ns)")
            
            return(to_render_plot)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

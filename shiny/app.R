#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyMatrix)
library(tidyverse)



# m <- 1:20
# 
# colnames(m) <- c("Var1-A", "Var1-B", "Var2-A", "Var2-B")



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Picaretagem Covid"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            numericInput(
                inputId = "crescimento",
                label = "Crescimento real das mortes (%)",
                value = 5,
                min = -5,
                max = 10,
                step = 0.1

            ),
            
            
            numericInput(
                inputId = "lambda",
                label = "Decaimento das mortes (% de mortes anunciadas em d+1 em relação a d",
                value = 70,
                step = 1,
                min = 0,
                max = 100
            ),
            
            uiOutput(
                outputId = "matriz"
            )
            
            
            )
            

        ,    
        

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "plot")
        )
    )   
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$matriz <- renderUI({
        
        nao_norm <- (input$lambda/100)^(1:20)
        
        perc_norm <- nao_norm/sum(nao_norm) *100
        
        m <- matrix(perc_norm, ncol = 1)
        rownames(m) <- str_glue("% em d+{1:20}")
        
        matrixInput(
            inputId = "matrix",
            value = m,
            rows = list(
                names = TRUE
            ),
            cols = list(
                names = FALSE
            )
        )
    })    
    
    mortes_reais_simples <-  reactive(
        100*(1 + input$crescimento/100 )^(1:90)
    )

    info_mortes <- reactive({

        nao_norm <- (input$lambda/100)^(1:20)
        perc_norm <- nao_norm/sum(nao_norm)
        
        
        info_mortes <- mortes_reais_simples() %*% t(perc_norm) %>% 
            t() %>% 
            c() %>% 
            enframe() %>%
            mutate(
                dia_morte = name %/% 20,
                dia_anuncio = dia_morte + name %% 20
            ) 
    })
        
    mortes_anunciadas <- reactive(
        info_mortes() %>% 
            group_by(
                dia_anuncio
            ) %>% 
            summarise(mortes_anunciadas = sum(value)) %>% 
            filter(dia_anuncio <= 89) %>% 
            select(
                dia = dia_anuncio,
                mortes_anunciadas
            )
    )
        
    mortes_reais <- reactive(
        info_mortes() %>% 
            group_by(
                dia_morte
            ) %>% 
            summarise(mortes_reais = sum(value)) %>% 
            filter(dia_morte <= 89) %>% 
            select(
                dia = dia_morte,
                mortes_reais
            )
    )
        
    mortes_na_vespera <- reactive(
        
        info_mortes() %>% 
            filter(
                dia_morte + 1 == dia_anuncio 
            ) %>% 
            group_by(
                dia_anuncio
            ) %>% 
            summarise(
                mortes_na_vespera = sum(value)
            ) %>% 
            filter(dia_anuncio <= 89) %>% 
            select(
                dia = dia_anuncio,
                mortes_na_vespera
            )
    )
        
        
    mortes_conhecidas_ultimo_dia <- reactive (
        info_mortes() %>% 
            filter(dia_anuncio <= 90 ) %>% 
            group_by(
                dia_morte
            ) %>% 
            summarise(
                mortes_conhecidas_ultimo_dia = sum(value)
            ) %>% 
            filter(dia_morte < 90) %>% 
            select(
                dia = dia_morte,
                mortes_conhecidas_ultimo_dia
            )
    )

    tudo <- reactive(
        
        mortes_anunciadas() %>% 
            left_join(
                mortes_conhecidas_ultimo_dia(),
                by = c("dia")
            ) %>% 
            left_join(
                mortes_na_vespera(),
                by = c("dia")
            ) %>% 
            left_join(
                mortes_reais(),
                by = c("dia")
            ) %>% 
            pivot_longer(
                cols = c(mortes_anunciadas, mortes_conhecidas_ultimo_dia, mortes_na_vespera, mortes_reais ),
                names_to = "tipo_dado",
                values_to = "mortes"
            )
    )
        
        
    output$plot <-  renderPlot(
        
        ggplot(tudo()) +
            geom_line(
                aes(
                    x = dia,
                    y = mortes,
                    color = tipo_dado
                )
            ) +
            theme_minimal() +
            theme(
                legend.position = "top"
            )
        
        
    )
        
        


    
}

# Run the application 
shinyApp(ui = ui, server = server)

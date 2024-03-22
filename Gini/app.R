
library(shiny)
library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)



# Import data
gini <- read.csv("./out/gini.csv")
CGG <- readRDS("./out/CGG.rds")

text_Groups <- "<p><strong>W145</strong>: 145 countries<br>
<strong>ADV37</strong>: 37 Advanced countries<br>
<strong>EME108</strong>: 108 Emerging and developing economies<br>
<strong>ADV13</strong>: 13 Non-EU advanced countries<br>
<strong>AF32</strong>: 32 Sub-Saharan African countries<br>
<strong>AS17</strong>: 17 Emerging and Developing Asian countries and ASEAN-5<br>
<strong>CIS12</strong>: 12 Commonwealth of Independent States countries<br>
<strong>EU28</strong>: 28 European Union countries (including the UK)<br>
<strong>EU27</strong>: The current 27 European Union countries (not including the UK)<br>
<strong>EU15</strong>: The first 15 members of the European Union<br>
<strong>EU13</strong>: 13 countries that joined the EU in 2004-2013<br>
<strong>LA24</strong>: 24 Latin American and the Caribbean countries<br>
<strong>MENA12</strong>: 12 Middle East and North African countries<br>
<strong>EA20</strong>: 20 euro-area members<br>
<strong>EA19</strong>: 19 euro-area members<br>
</p>
"

text_Types <- "<p><strong>MKT</strong>: Market distribution<br>
<strong>NET</strong>: Disposable income after redistribution<br>
</p>
"

text_MI <- "<p><strong>GDPPC</strong>: GDP per capita at purchasing power parity primarily from IMF World Economic Outlook dataset (available for all countries we study for the full sample period)<br>
<strong>SURVEY</strong>: Survey-based mean household income measured at purchasing power standards (PPS) from Eurostat (available for EU and a few other countries and only for a shorter period)<br>
</p>
"

text_Method <- "<p><strong>LD</strong>: Log-normal distribution, deterministic<br>
<strong>LA</strong>: Log-normal distribution, stochastic<br>
<strong>PD</strong>: Pareto distribution, deterministic<br>
<strong>PA</strong>: Pareto distribution, stochastic<br>
<strong>WD</strong>: Weibull distribution, deterministic<br>
<strong>WA</strong>: Weibull distribution, stochastic<br>
<strong>H2</strong>: Lorenz-curve regression method using all Eurostat quantile income share data (1, 2, 3, 4 and 5 percentiles, deciles, quartiles, 95, 96, 97, 98, 99 and 100 percentiles)<br>
<strong>B2</strong>: Identical income share methods using all Eurostat quantile income share data (1, 2, 3, 4 and 5 percentiles, deciles, quartiles, 95, 96, 97, 98, 99 and 100 percentiles)<br>
<strong>S4</strong>: Kernel-density method using only Eurostat quintile income share data<br>
<strong>Average_U</strong>: Unweighted average of country-specific Gini coefficients<br>
<strong>Average_W</strong>: Population-weighted average of country-specific Gini coefficients<br>
</p>
"

text_Subtitle1 <- "The Gini coefficient is a measure of inequality.<br>In this case, it shows <b>inequality</b> in the <b>distribution of income</b> on a scale from 0 to 100, where 0 indicates <i>perfect equality</i> (income is equally distributed, everyone has the same income), and 100 indicates <i>perfect inequality</i> (one person has all the income and everyone else has nothing).<br>
<br>As societies become more equal, the Gini coefficient decreases."

text_Subtitle2 <- "<p>This database (accessible from <a href=https://www.bruegel.org/dataset/global-and-regional-gini-coefficients-income-inequality>bruegel</a>) includes updated global and regional Gini coefficients of income inequality estimates from the paper: '<i>Global interpersonal income inequality decline: the role of China and India</i>' (<a href=https://doi.org/10.1016/j.worlddev.2019.04.011>Darvas 2019</a>).</p>
<br>Data Visualization by <b>Giulia Puntin</b>."
  


# Define UI
ui <- fluidPage(
  
  titlePanel("Gini coefficients of income inequality estimates by regions (1980 to 2021)"),
  
  tags$h4(HTML(text_Subtitle1)),
  tags$h4(HTML(text_Subtitle2)),
  tags$br(),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("group", "Group of Countries:", 
                  choices = unique(gini$Group), 
                  selected = "W145",
                  multiple = TRUE),
      helpText(HTML(text_Groups)),
      tags$br(),
      
      selectInput("type", "Types of income inequality indicators:", 
                  choices = unique(gini$Type)),
      
      helpText(HTML(text_Types)),
      tags$br(),
      
      selectInput("mean_income", "Mean income indicator:", 
                  choices = unique(gini$Mean_income)),
      helpText(HTML(text_MI)),
      tags$br(),
      
      selectInput("method", "Method:", 
                  choices = unique(gini$Method)),
      helpText(HTML(text_Method))
      
    ),
    
    mainPanel(
      plotOutput("plot"),
      plotOutput("map")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # Filter gini data based on user input
  filtered_gini <- reactive({
    gini %>%
      filter(Type == input$type,
             Mean_income == input$mean_income,
             Method == input$method,
             Group %in% input$group)
  })
  
  # Render plot based on filtered gini data
  output$plot <- renderPlot({
    ggplot(data = filtered_gini(), aes(x = Year, y = Gini, color = Group, group = Group)) +
      geom_line(size = 1) +
      geom_point(shape = 21,
                 fill = "white",
                 size = 1.5,
                 stroke = 2) +
      scale_x_continuous(expand = c(0,0), limits = c(1979, 2023)) +
      scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
      scale_color_viridis_d() +
      labs(y = "Gini Index") +
      theme(
        text = element_text(size = 17),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey89"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "transparent"),
        plot.margin = margin(rep(0.5, 4), unit = "cm")
      )
  })
  
  # Filter CGG data based on selected group
  filtered_CGG <- reactive({
    CGG %>%
      filter(Group %in% input$group)
  })
  
  # Render map based on filtered CGG data
  output$map <- renderPlot({
    ggplot(data = filtered_CGG()) +
      geom_sf(aes(fill = Group), alpha = 0.5) +
      coord_sf(crs = "+proj=moll") +
      scale_fill_viridis_d() +
      theme_void() +
      theme(
        text = element_text(size = 17),
        legend.position = "top",
        plot.margin = margin(rep(1, 4), unit = "cm"),
        panel.margin = margin(rep(1, 4), unit = "cm")
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

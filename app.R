# Creating fake data using a shiny app

# Packages ----
library(shiny)
library(ggplot2)

# Functions ----

# Row inputs
textInputRow<-function (inputId, label, value = "") 
{
	div(style="display:inline-block",
			tags$label(label, `for` = inputId), 
			tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

# Input options ----
dat_type_choice <- c("Categorical", "Continuous")



# Define UI ----
ui <- fluidPage(
   
   # Application title
   titlePanel("Create fake bivariate data"),
   sidebarLayout(
   	sidebarPanel(
   		h1("X"),
   		textInput(inputId = "x_label" , label = "Define X column label", placeholder = "e.g. Species_name"),
   		selectInput(inputId = "x_type", label = "What type of data is X?", choices = dat_type_choice, selected = dat_type_choice[1]),
   		conditionalPanel(
   			condition = "input.x_type == 'Continuous'",
   			textInputRow(inputId = "x_lim_min", label = "X min", value = 0.0),
   			textInputRow(inputId = "x_lim_max", label = "X max", value = 0.5),
   			sliderInput(inputId = "x_n", label = "X n", min = 2, max = 100, value = 5, step = 1),
   			sliderInput(inputId = "x_sd", label = "X Standard Deviation", min = 0, max = 100, value = 1, step = 1)),
   		conditionalPanel(
   			condition = "input.x_type == 'Categorical'",
   			textInput(inputId = "x_cat", label = "What Categories are in X", placeholder = "e.g. c('A', 'B', 'C')")),
   		hr(),
   		h1("Y"),
   		textInput(inputId = "y_label" , label = "Define Y column label", placeholder = "e.g. Relative_abundance"),
   		selectInput(inputId = "y_type", label = "What type of data is Y?", choices = dat_type_choice, selected = dat_type_choice[2]),
   		conditionalPanel(
   			condition = "input.y_type == 'Continuous'",
   			textInputRow(inputId="y_lim_min", label="y min", value = 0.0),
   			textInputRow(inputId="y_lim_max", label="y max", value = 0.5),
   			sliderInput(inputId = "y_n", label = "Y n", min = 2, max = 100, value = 5, step = 1),
   			sliderInput(inputId = "y_sd", label = "Y Standard Deviation", min = 0, max = 100, value = 1, step = 1)),
   		conditionalPanel(
   			condition = "input.y_type == 'Categorical'",
   			textInput(inputId = "y_cat", label = "What Categories are in Y", placeholder = "e.g. c('A', 'B', 'C')"))
   	),
   	mainPanel(
   		h3("Head of data"),
   		tableOutput("table"),
   		hr(),
   		h3("Plot of data"),
   		plotOutput("plot"),
   		downloadButton("downloadData", "Download")
   		)
   	)
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
	
	# Table of dataset ----
	
	selectedData <- reactive({data.frame(
		"y" = rnorm(
			mean = as.numeric(input$y_lim_min) + (0.5*as.numeric(input$y_lim_max)), 
			sd = as.numeric(input$y_sd), 
			n = as.numeric(input$y_n)),
		"x" = ifelse(input$x_type == "Continuous", 
								 rnorm(
								 	mean = as.numeric(input$x_lim_min) + (0.5*as.numeric(input$x_lim_max)), 
								 	sd = as.numeric(input$x_sd), 
								 	n = as.numeric(input$x_n)),
								 input$x_cat))
		})
	
	output$table <- renderTable(head(selectedData()))
	
	output$plot <- renderPlot(
		ggplot(selectedData(), aes(x = x, y = y)) + 
			geom_point()
		)
}
# Run the application 
shinyApp(ui = ui, server = server)


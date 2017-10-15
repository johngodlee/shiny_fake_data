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

# ggplot2 theme
theme.ggplot <- function(){
	theme_bw()+
		theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),
					axis.text.y=element_text(size=12),
					axis.title.x=element_text(size=14, face="plain"),             
					axis.title.y=element_text(size=14, face="plain"),             
					panel.grid.major.x=element_blank(),                                          
					panel.grid.minor.x=element_blank(),
					panel.grid.minor.y=element_blank(),
					panel.grid.major.y=element_blank(),  
					plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
					plot.title = element_text(size=20, vjust=1, hjust=0.5),
					legend.text = element_text(size=12, face="italic"),          
					legend.title = element_blank(),                              
					legend.position=c(0.9, 0.9))
}


# Define UI ----
ui <- fluidPage(
   
   # Application title
   titlePanel("Create fake bivariate data"),
   sidebarLayout(
   	sidebarPanel(
   		sliderInput(inputId = "xy_n", label = "Number of data points", min = 2, max = 100, value = 5, step = 1),
   		h1("X"),
   		textInput(inputId = "x_label" , label = "Define X column label", placeholder = "e.g. Species_name"),
   			textInputRow(inputId = "x_lim_min", label = "X min", value = 0.0),
   			textInputRow(inputId = "x_lim_max", label = "X max", value = 0.5),
   			sliderInput(inputId = "x_sd", label = "X Standard Deviation", min = 0, max = 100, value = 1, step = 1),
   		hr(),
   		h1("Y"),
   		textInput(inputId = "y_label" , label = "Define Y column label", placeholder = "e.g. Relative_abundance"),
   			textInputRow(inputId="y_lim_min", label="Y min", value = 0.0),
   			textInputRow(inputId="y_lim_max", label="Y max", value = 0.5),
   			sliderInput(inputId = "y_sd", label = "Y Standard Deviation", min = 0, max = 100, value = 1, step = 1)
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
		"x" = rnorm(
			mean = as.numeric(input$x_lim_min) + (0.5*as.numeric(input$x_lim_max)), 
			sd = as.numeric(input$x_sd), 
			n = as.numeric(input$xy_n)),
		"y" = rnorm(
			mean = as.numeric(input$y_lim_min) + (0.5*as.numeric(input$y_lim_max)), 
			sd = as.numeric(input$y_sd), 
			n = as.numeric(input$xy_n)))
		})
	
	output$table <- renderTable(head(selectedData()))
	
	output$plot <- renderPlot(
		ggplot(selectedData(), aes(x = x, y = y)) + 
			geom_point() + 
			theme.ggplot() + 
			xlab(input$x_label) +
			ylab(input$y_label)
		
			
		)
}
# Run the application 
shinyApp(ui = ui, server = server)


# Creating fake data using a shiny app

# Packages ----
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Functions ----

# Row inputs
textInputRow<-function (inputId, label, value = "") 
{
	div(style="display:inline-block",
			tags$label(label, `for` = inputId), 
			tags$input(id = inputId, type = "number", value = value,class="input-small"))
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
   		sliderInput(inputId = "xy_n", 
   								label = "Number of data points", 
   								min = 2, max = 100, value = 5, step = 1),
   		selectInput(inputId = "distrib_type", 
   								label = "Distribution Type", 
   								choices  = c("Linear" = "lin", 
   														 "Quadratic" = "quad",
   														 "Exponential" = "exp",
   														 "Sigmoidal" = "sig")),
   		h1("X"),
   			textInputRow(inputId = "x_lim_min", label = "X min", value = 0.0),
   			textInputRow(inputId = "x_lim_max", label = "X max", value = 5),
   		hr(),
   		h1("Y"),
   			textInputRow(inputId="y_lim_min", label="Y min", value = 0.0),
   			textInputRow(inputId="y_lim_max", label="Y max", value = 5),
   			sliderInput(inputId = "y_noise", 
   									label = "Y Noise", 
   									min = 0, max = 100, value = 1, step = 1),
   		conditionalPanel(condition = "input.distrib_type == 'quad'", 
   										 hr(),
   										 tags$p(withMathJax("$$Y = aX^2 + bX + c$$")),
   										 numericInput(inputId = "quad_quad_coef", 
   										 						 label = "Set the Quadratic Coefficient (a)",
   										 						 value = 2, min = 1, step = 1),
   										 numericInput(inputId = "quad_lin_coef", 
   										 						 label = "Set the Linear Coefficient (b)",
   										 						 value = 2, min = 1, step = 1)),
   		conditionalPanel(condition = "input.distrib_type == 'exp'",
   										 hr(),
   										 tags$p(withMathJax("$$Y = ab^X$$")),
   										 numericInput(inputId = "exp_coef", "Set the Exponential Coefficient (a)",
   										 						 value = 2, min = 1, step = 1),
   										 numericInput(inputId = "exp_base", "Set the Exponential Base (b)",
   										 						 value = 2, min = 1, step = 1)),
   		conditionalPanel(condition = "input.distrib_type == 'sig'",
   										 hr(),
   										 tags$p(withMathJax("$$Y = \\frac{1}{a+e^-X}$$")),
   										 numericInput(inputId = "sig_coef", "Set the Sigmoidal Coefficient (a)",
   										 						 value = 2, min = 1, step = 1)),
   		hr(),
   		downloadButton("download", "Download `.csv`")
   	),
   	mainPanel(
   		fluidRow(
   			column(4,
   						 wellPanel(h3("Head of data"),
   						 					tableOutput("head"))),
   			column(7,
   						 wellPanel(h3("Summary table"), 
   						 					tableOutput("summ")))
   			),
   		hr(),
   		h3("Plot of data"),
   		plotOutput("plot")
   		)
   	)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
	
	# Functions for data distributions
	fn_quad <- function(x){input$quad_quad_coef *x^2 + input$quad_lin_coef * x}
	fn_exp <- function(x){input$exp_coef * input$exp_base^x}
	fn_sig <- function(x){1 / (input$sig_coef + exp(1)^-x)}
	
	# Create a dataframe
	df <- reactive({
	df <- data.frame("x" = seq(from = input$x_lim_min, to = input$x_lim_max, length.out = input$xy_n), 
									 "y" = if (input$distrib_type == "lin") {
									 	jitter(seq(from = input$y_lim_min, 
									 						 to = input$y_lim_max, 
									 						 length.out = input$xy_n),
									 				 factor = input$y_noise)
									 	} else { 
									 		if (input$distrib_type == "exp") {
									 	jitter(fn_exp(seq(from = input$y_lim_min, 
									 										 to = input$y_lim_max, 
									 										 length.out = input$xy_n)),
									 				 factor = input$y_noise)
									 			} else {
									 				if (input$distrib_type == "quad") {
									 	jitter(fn_quad(seq(from = input$y_lim_min, 
									 										 to = input$y_lim_max, 
									 										 length.out = input$xy_n)),
									 				 factor = input$y_noise)
									 				} else {
									 					jitter(fn_sig(seq(from = input$y_lim_min, 
									 														 to = input$y_lim_max, 
									 														 length.out = input$xy_n)),
									 								 factor = input$y_noise)}
									 				}})
	})
	
	# Create an output table of the head of the data frame
	output$head <- renderTable(head(df()))
	
	# Create an output table summary
	output$summ <- renderTable({
		df() %>%
			gather("var", "val", 1:2) %>%
			group_by(var) %>%
			summarise(mean = mean(val),
								median = median(val),
								max = max(val),
								min = min(val),
								"25% Quant." = quantile(val, probs = 0.25),
								"75% Quant." = quantile(val, probs = 0.75))
	})
		

	# Create an output plot of the dataframe
	output$plot <- renderPlot(
		ggplot(df(), aes(x = x, y = y)) + 
			geom_point() + 
			geom_smooth(method = "loess", colour = "#00CDCD") + 
			theme.ggplot() + 
			xlab(input$x_label) +
			ylab(input$y_label) 
		)
	
	# Create a csv file to download
	output$download <- downloadHandler(
		filename = "fake_data.csv",
		content = function(file){
			write.csv(df(), 
								file, 
								row.names = FALSE)}
	)
}
# Run the application 
shinyApp(ui = ui, server = server)


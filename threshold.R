library(shiny)
library(colourpicker)

thresholdUI <- function(id, colour, value) {
	ns <- NS(id)
	tagList(
		colourInput(ns("colour"), NULL, value=colour),
		numericInput(ns("value"), NULL, value=value),
		actionButton(ns("remove"), "Retirer", icon=icon("trash"))
	)
}

threshold <- function(input, output, session, container) {
	value <- reactive({
		print(file=stderr(), "Triggered refresh")
		list(colour=input$colour, value=input$value, removal=input$remove)
	})

	return(value)
}


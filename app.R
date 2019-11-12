library(gstat)
library(rgdal)
library(rgeos)
library(raster)
library(RColorBrewer)
library(sysfonts)
library(shiny)
library(colourpicker)
library(DBI)
library(RSQLite)
library(RMariaDB)

source('threshold.R')

contour <- readOGR(dsn="./brittany.json")
contour <- spTransform(contour, CRS("+init=epsg:3857"))
communes <- readOGR(dsn="./selection.json")
communes <- spTransform(communes, CRS("+init=epsg:3857"))
communes$TEMP <- 0

ext <- extent(contour)
grd <- expand.grid(x=seq(from=xmin(ext)-5000, to=xmax(ext)+5000, by=500), y=seq(from=ymin(ext)-5000, to=ymax(ext)+5000, by=500))
coordinates(grd) <- ~ x+y
crs(grd) <- crs(contour)
gridded(grd) <- TRUE

mapCorners <- cbind(rep(c(-7.3,-0.1),2),rep(c(46.6,49.7),each=2))
mapCorners <- SpatialPoints(mapCorners, proj4string=CRS("+init=epsg:4326"))
mapCorners <- spTransform(mapCorners, crs(contour))

paletteConn <- dbConnect(
	drv = RSQLite::SQLite(),
	dbname = "palettes.sqlite"
)

mid <- trunc(length(communes)/2)
ui <- fluidPage(
	titlePanel("Carte"),
	sidebarLayout(
		sidebarPanel(
			h1("Paramètres"),
			h2("Couleurs"),
			selectInput("palette", "Palettes prédéfinies",  NULL),
			wellPanel(
				textInput("palette-name", "Nom"),
				tags$div(id="thresholds_container",
					uiOutput("thresholds")
				)
			),
			fluidRow(
				column(4, colourInput("new_colour",NULL,value="grey")),
				column(4, numericInput("new_value",NULL,value=0)),
				column(4, actionButton("new_threshold","Ajouter",icon=icon("plus")))
			),
			actionButton("save_palette", "Sauver", icon=icon("save")),
			actionButton("delete_palette", "Supprimer", icon=icon("trash-alt")),
			h2("Villes"),
			p("Entrez une valeur vide pour ne pas tenir compte de la ville. Donnez au moins une valeur pour générer la carte."),
			fluidRow(
				column(6,
					lapply(1:mid, function(n) numericInput(inputId=paste0("city-",n),label=communes[n,]$NOM_COM, value=NA))
					),
				column(6,
					lapply((mid+1):length(communes), function(n) numericInput(inputId=paste0("city-",n),label=communes[n,]$NOM_COM, value=NA))
				)
			)
		),
		mainPanel(
			fluidRow(
				column(12, plotOutput(outputId="map", height="auto")), #, height="600px"
				column(12, plotOutput(outputId="mapvalues", height="auto")) #, height="600px"
			)
		)
	)
)


server <- function(input, output, session) {
	updateSelectInput(session, 'palette', choices={
		palettes <- dbReadTable(paletteConn, "palettes")
		groups <- unique(palettes$owner)
		names(groups) <- groups
		lapply(groups, function (o) {
			relevant <- palettes[palettes$owner=='Système',]
			p <- as.list(relevant$id);
			names(p) <- relevant$name
			return (p)
		})
	})

	reactivePalette <- reactiveValues(
		raw_thresholds=list(),
		thresholds=list(),
		last_id=0,
		ids=list(),
		ordering=NULL
	)


	observe({
		choice <- input$palette
		req(choice)
		isolate({
			print(file=stderr(), "User has selected a palette")
			sql <- "SELECT colour,value FROM thresholds WHERE palette_id = ?palette_id ORDER BY value ASC"
			query <- sqlInterpolate(paletteConn, sql, palette_id=choice)
			thresholds <- dbGetQuery(paletteConn, query)
			#print(file=stderr(), thresholds)

			last_id <- reactivePalette$last_id
			nb <- nrow(thresholds)
			raw_thresholds <- lapply(1:nb, function (i) {
				newId <- i + last_id
				newModule <- debounce(callModule(threshold, paste0("threshold_",newId), container=paste0("container_",newId)), 500)

				observeEvent(newModule()$removal, {
					reactivePalette$raw_thresholds <- reactivePalette$raw_thresholds[sapply(reactivePalette$raw_thresholds, function (th) th$id != newId)]
					reactivePalette$ids <- reactivePalette$ids[sapply(reactivePalette$ids, function (id) id != newId)]
				})

				list(
					module=newModule,
					initialColour=thresholds[i, 'colour'],
					initialValue=thresholds[i, 'value'],
					id=newId
				)
			})

			reactivePalette$raw_thresholds <- raw_thresholds
			reactivePalette$last_id <- last_id + nb
			reactivePalette$ids <- as.list(last_id + 1:nb)
			reactivePalette$ordering <- 1:nb
		})
	})


	observeEvent(input$new_threshold, {
		newId <- reactivePalette$last_id + 1
		newModule <- debounce(callModule(threshold, paste0("threshold_",newId), container=paste0("container_",newId), colour=input$new_colour, value=input$new_value), 500)
		reactivePalette$raw_thresholds <- c(reactivePalette$raw_thresholds,
			list(
				list(
					module=newModule,
					initialColour=input$new_colour,
					initialValue=input$new_value,
					id=newId
				)
			)
		)
		reactivePalette$last_id <- newId
		reactivePalette$ids <- c(reactivePalette$ids, newId)

		observeEvent(newModule()$removal, {
			reactivePalette$raw_thresholds <- reactivePalette$raw_thresholds[sapply(reactivePalette$raw_thresholds, function (th) th$id != newId)]
			reactivePalette$ids <- reactivePalette$ids[sapply(reactivePalette$ids, function (id) id != newId)]
		})
	}, ignoreInit=TRUE)

	reactivePalette$thresholds <- reactive({
		# take a dependency on the list of ids but not on all individual modules
		ids <- reactivePalette$ids
		ordering <- reactivePalette$ordering
		print(file=stderr(), "New thresholds")
		the_thresholds <- isolate({
			if (length(ordering) != length(reactivePalette$raw_thresholds)) {
				# this can occur when thresholds appear or disappear
				values <- sapply(reactivePalette$raw_thresholds, function (th) {
					module <- th$module()
					if (is.null(module$value))
						return(th$initialValue)
					else
						return(module$value)
				})
				ordering <- order(values)
			}
			reactivePalette$raw_thresholds <- reactivePalette$raw_thresholds[ordering]
			#print(file=stderr(), reactivePalette$raw_thresholds)
			lapply(reactivePalette$raw_thresholds, function (th) {
				module <- th$module()
				print(file=stderr(), module)
				if (is.null(module$value) || is.null(module$colour))
					return(list(colour=th$initialColour, value=th$initialValue, id=th$id))
				else
					return(c(module, id=th$id))
			})
		})
		#print(file=stderr(), the_thresholds)
		return(the_thresholds)
	})

	paletteInput <- debounce(reactive({
		print(file=stderr(), "A module has changed")
		the_thresholds <- lapply(reactivePalette$raw_thresholds, function (th) th$module())
		values <- sapply(the_thresholds, function (l) l$value)
		#print(file=stderr(), "Values to check are: ")
		#print(file=stderr(), values)
		if (is.null(values) || length(values) < 2 || any(sapply(values,is.null))) {
			print(file=stderr(), "Palette is not usable yet")
			return(list(domain=NULL, range=NULL))
		}

		#print(file=stderr(), "Palette is usable")
		#print(file=stderr(), "Values are: ")
		#print(file=stderr(), values)
		if (is.unsorted(values)) {
			# Trigger the refresh of the thresholds above
			reactivePalette$ordering <- order(values)
			# ... and bail out
			return(list(domain=NULL, range=NULL))
		}
		colors <- sapply(the_thresholds, function (l) l$colour)
		steps <- (values[-1] - values[-length(values)]) * 10
		#print(file=stderr(), "Colors and steps are: ")
		#print(file=stderr(), colors)
		#print(file=stderr(), steps)

		list(
			domain=seq(min(values), max(values), 0.1),
			range=unlist(sapply(1:(length(colors)-1), function (i) colorRampPalette(c(colors[i], colors[i+1]))(steps[i])))
		)
	}), 1000)


	communesInput <- debounce(reactive({
		for (i in 1:length(communes))
			communes[i,'TEMP'] <- input[[paste0("city-",i)]]
		communes[!is.na(communes$TEMP),]
	}), 1000)

	output$thresholds <- renderUI({
		print(file=stderr(), "Refreshing the UI")
		the_thresholds <- reactivePalette$thresholds()
		if (length(the_thresholds) > 0) {
			tagList(
				lapply(the_thresholds,
					function (l) {
						fluidRow(
							id=paste0("container_",l$id),
							lapply(thresholdUI(paste0("threshold_",l$id), colour=l$colour, value=l$value), function (elem) column(4, elem))
						)
					}
				)
			)
		}
	})

	output$map <- renderPlot({
		values <- communesInput()
		palette <- paletteInput()
		print(file=stderr(), paste("Length of palette:", length(palette$domain)))

		if (length(values) > 0 && length(palette$domain) > 1) {
			pred <- idw(TEMP~1, locations=values, newdata=grd)
			par(bg=NA, bty="n", xpd=NA, xaxs='i', xaxt='n', yaxs='i', yaxt='n', plt=c(0,1,0,1), oma=c(0,0,0,0))
			r <- mask(raster(pred), contour, updatevalue=NA)
			plot(extent(mapCorners),col="transparent")
			plot(r, col=palette$range, breaks=palette$domain,add=T, axis=F, legend=F)
		}
	}, bg="transparent", height=function() {
		session$clientData$output_map_width * 9./16
	})

	output$mapvalues <- renderPlot({
		values <- communesInput()

		if (length(values) > 0) {
			par(bg=NA, bty="n", xpd=NA, xaxs='i', xaxt='n', yaxs='i', yaxt='n', plt=c(0,1,0,1), oma=c(0,0,0,0))
			plot(extent(mapCorners),col="transparent")
			raster::text(values, labels=values$TEMP,
				family=c("D-DIN Condensed"), halo=TRUE, hc="black", col="white", cex=2)
		}
	}, bg="transparent", height=function() {
		session$clientData$output_mapvalues_width * 9./16
	})
}


shinyApp(ui = ui, server = server)


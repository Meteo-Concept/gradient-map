library(gstat)
library(rgdal)
library(rgeos)
library(raster)
library(RColorBrewer)
library(sysfonts)
library(shiny)
library(shinyjs)
library(shinyalert)
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
	useShinyjs(),
	useShinyalert(),
	titlePanel("Carte"),
	sidebarLayout(
		sidebarPanel(
			h1("Paramètres"),
			h2("Couleurs"),
			selectInput("palette", "Palettes prédéfinies",  NULL),
			wellPanel(
				textInput("palette_name", "Nom"),
				numericInput("palette_increment", "Incrément", value=0.1),
				p(textOutput("palette_size", inline=T), " couleurs dans le dégradé"),
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

refreshPaletteSelector <- function(session) {
	updateSelectInput(session, 'palette', choices={
		palettes <- dbReadTable(paletteConn, "palettes")
		groups <- unique(palettes$owner)
		names(groups) <- groups
		lapply(groups, function (o) {
			relevant <- palettes[palettes$owner==o,]
			p <- relevant$id
			names(p) <- relevant$name
			p
		})
	})
}

server <- function(input, output, session) {
	refreshPaletteSelector(session)

	reactivePalette <- reactiveValues(
		raw_thresholds=list(),
		thresholds=list(),
		last_id=0,
		ids=list(),
		ordering=NULL
	)

	observeEvent(input$save_palette, {
		name <- input$palette_name
		increment <- input$palette_increment
		the_thresholds <- reactivePalette$thresholds()
		if (name == "") {
			shinyalert("Erreur", "Il faut donner un nom à cette palette pour l'enregistrer", type="error")
			req(FALSE)
		}
		sql <- "SELECT COUNT(*) AS count FROM palettes WHERE name = ?name"
		query <- sqlInterpolate(paletteConn, sql, name=name)
		alreadyExisting <- dbGetQuery(paletteConn, query)
		if (alreadyExisting$count > 0) {
			shinyalert("Erreur", "Une palette existe déjà avec ce nom. Si vous souhaitez la remplacer, vous devez la supprimer auparavant.", type="error")
			req(FALSE)
		}

		sql <- "INSERT INTO palettes (name,owner,mutable,increment) VALUES (?name, ?owner, ?mutable, ?increment)"
		query <- sqlInterpolate(paletteConn, sql, name=name, owner='Utilisateur', mutable=T, increment=increment)
		insertion <- dbExecute(paletteConn, query)
		lastInsertId <- dbGetQuery(paletteConn, "SELECT last_insert_rowid() AS id")
		sql <- "INSERT INTO thresholds (palette_id, colour, value) VALUES (?palette_id, ?colour, ?value)"
		lapply(the_thresholds, function (th) {
			query <- sqlInterpolate(paletteConn, sql, palette_id=lastInsertId$id, colour=th$colour, value=th$value)
			dbExecute(paletteConn, query)
		})

		print(file=stderr(), "saved")
		refreshPaletteSelector(session)
	})

	observeEvent(input$delete_palette, {
		id <- input$palette
		sql <- "SELECT name,owner,mutable FROM palettes WHERE id = ?id"
		query <- sqlInterpolate(paletteConn, sql, id=id)
		paletteToDelete <- dbGetQuery(paletteConn, query)
		if (nrow(paletteToDelete) != 1) {
			shinyalert("Erreur", "La palette n'existe déjà plus.", type="error")
			req(FALSE)
		}
		if (paletteToDelete$owner == "Système" || paletteToDelete$mutable == 0) {
			shinyalert("Erreur", "Cette palette ne peut pas être supprimée", type="error")
			req(FALSE)
		}
		sql <- "DELETE FROM thresholds WHERE palette_id = ?id"
		query <- sqlInterpolate(paletteConn, sql, id=id)
		dbExecute(paletteConn, query)
		sql <- "DELETE FROM palettes WHERE id = ?id"
		query <- sqlInterpolate(paletteConn, sql, id=id)
		dbExecute(paletteConn, query)

		print(file=stderr(), "deleted")
		refreshPaletteSelector(session)
	})

	observe({
		choice <- input$palette
		req(choice)
		isolate({
			print(file=stderr(), "User has selected a palette")
			sql <- "SELECT name,owner,increment FROM palettes WHERE id = ?id"
			query <- sqlInterpolate(paletteConn, sql, id=choice)
			selectedPalette <- dbGetQuery(paletteConn, query)
			sql <- "SELECT colour,value FROM thresholds WHERE palette_id = ?palette_id ORDER BY value ASC"
			query <- sqlInterpolate(paletteConn, sql, palette_id=choice)
			updateNumericInput(session, "palette_increment", value=selectedPalette$increment)
			thresholds <- dbGetQuery(paletteConn, query)
			#print(file=stderr(), thresholds)

			updateTextInput(session, "palette_name", value=selectedPalette[1,'name'])

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
			reactivePalette$modified <- 0
		})
	})


	observeEvent(input$new_threshold, {
		newId <- reactivePalette$last_id + 1
		newModule <- debounce(callModule(threshold, paste0("threshold_",newId), container=paste0("container_",newId)), 500)
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
		reactivePalette$modified <- 1
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
				#print(file=stderr(), module)
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
		increment <- input$palette_increment
		validate(
			need(!is.null(increment) && !is.na(increment) && increment > 0, "Incrément invalide (essayez une valeur de 1 ou 0,1)")
		)
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
		reactivePalette$modified <- 1
		colors <- sapply(the_thresholds, function (l) l$colour)
		steps <- (values[-1] - values[-length(values)]) / increment
		#print(file=stderr(), "Colors and steps are: ")
		#print(file=stderr(), colors)
		#print(file=stderr(), steps)

		list(
			domain=seq(min(values), max(values), increment),
			range=unlist(sapply(1:(length(colors)-1), function (i) colorRampPalette(c(colors[i], colors[i+1]))(steps[i])))
		)
	}), 1000)


	communesInput <- debounce(reactive({
		for (i in 1:length(communes))
			communes[i,'TEMP'] <- input[[paste0("city-",i)]]
		communes[!is.na(communes$TEMP),]
	}), 1000)

	output$palette_size <- renderText({
		palette <- paletteInput()
		length(palette$domain)
	})

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


runApp(shinyApp(ui = ui, server = server), launch.browser=F)


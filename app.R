library(gstat)
library(rgdal)
library(rgeos)
library(raster)
library(RColorBrewer)
library(sysfonts)
library(shiny)
library(shinyjs)
library(shinyalert)
library(grid)
library(magick)
library(httr)
library(data.table)

breaks <- list(
	temp=seq(-15,40,0.1)
)
col <- list(
	temp=c(
		colorRampPalette(c("#D202B4","#660066"))(50),
		colorRampPalette(c("#660066","#0F2267"))(50),
		colorRampPalette(c("#0F2267","#0069BA"))(50),
		colorRampPalette(c("#0069BA","#3399FF"))(50),
		colorRampPalette(c("#3399FF","#11B34F"))(50),
		colorRampPalette(c("#11B34F","#BDCC40"))(50),
		colorRampPalette(c("#BDCC40","#F7C72A"))(30),
		colorRampPalette(c("#F7C72A","#FF6100"))(40),
		colorRampPalette(c("#FF6100","#FE0F03"))(40),
		colorRampPalette(c("#FE0F03","#9B0002"))(40),
		colorRampPalette(c("#9B0002","#480000"))(50),
		colorRampPalette(c("#480000","#000000"))(50)
	)
)

apikey <- Sys.getenv("METEOCONCEPT_API_KEY")

contour <- readOGR(dsn="./brittany.json")
contour <- spTransform(contour, CRS("+init=epsg:3857"))
communes <- readOGR(dsn="./selection.json")
communes <- spTransform(communes, crs(contour))
communes$TEMP <- 0
sea <- readOGR(dsn="./sea.json")
sea <- spTransform(sea, crs(contour))
sea$TEMP <- 0

ext <- extent(contour)
grd <- expand.grid(x=seq(from=xmin(ext)-5000, to=xmax(ext)+5000, by=500), y=seq(from=ymin(ext)-5000, to=ymax(ext)+5000, by=500))
coordinates(grd) <- ~ x+y
crs(grd) <- crs(contour)
gridded(grd) <- TRUE

mapCorners <- cbind(rep(c(-8.00, -0.20),2),rep(c(46.65, 49.58),each=2))
mapCorners <- SpatialPoints(mapCorners, proj4string=CRS("+init=epsg:4326"))
mapCorners <- spTransform(mapCorners, crs(contour))

paletteConn <- dbConnect(
	drv = RSQLite::SQLite(),
	dbname = "palettes.sqlite"
)

mid <- trunc(length(communes)/2)
midsea <- trunc(length(sea)/2)
ui <- fluidPage(
	useShinyjs(),
	useShinyalert(),
	titlePanel("Carte"),
	sidebarLayout(
		sidebarPanel(
			h1("Paramètres"),
			tags$div(id="accordion",
			    tags$div(class="panel panel-primary",
			         tags$div(class="panel-heading", id="panel-heading-1",
			            tags$h2(role="button", `data-toggle`="collapse", `data-target`="#collapse-1", "Date et heure")
			         ),
			        tags$div(id="collapse-1", class="panel-body collapse", `data-parent`="#accordion",
			        p("Pré-remplissage"),
			        fluidRow(
			           column(12,
			              dateInput(inputId="date",label="Date", value=Sys.Date() + 1, language="fr")
			           ),
			        ),
			        fluidRow(
			           column(12,
			              selectInput(inputId="period",label="Période", choices=c("Nuit"="0", "Matin"="1", "Après-midi"="2", "Soirée"="3"))
			           )
			        ),
			        fluidRow(
			          column(12,
			             actionButton(inputId="loadData",label="Charger les données depuis le modèle")
			          )
			        )
			     )
			  ),
				tags$div(class="panel panel-primary",
					tags$div(class="panel-heading", id="panel-heading-2",
						tags$h2(role="button", `data-toggle`="collapse", `data-target`="#collapse-2", "Titres")
					),
					tags$div(id="collapse-2", class="panel-body collapse", `data-parent`="#accordion",
						p("Décoration"),
						fluidRow(
							column(12,
								textInput(inputId="title",label="Titre", value=stringr::str_to_upper(strftime(Sys.Date() + 1, "%A %d %B"), locale="fr"))
							),
						),
						fluidRow(
							column(12,
								textInput(inputId="subtitle",label="Sous-titre", value="MATIN")
							)
						),
						fluidRow(
							column(12,
								selectInput(inputId="tendency",label="Tendance", choices=c("En hausse"="./rising.png", "Stable"="./stable.png", "En baisse"="./lowering.png"))
							)
						)
					)
				),
				tags$div(class="panel panel-primary",
					tags$div(class="panel-heading", id="panel-heading-3",
						tags$h2(role="button", `data-toggle`="collapse", `data-target`="#collapse-3", "Villes")
					),
					tags$div(id="collapse-3", class="panel-body collapse", `data-parent`="#accordion",
						p("Entrez une valeur vide pour ne pas tenir compte de la ville. Donnez au moins une valeur pour générer la carte."),
						fluidRow(
							column(6,
									lapply(1:mid, function(n) numericInput(inputId=paste0("city-",n),label=communes[n,]$NOM_COM, value=NA))
							),
							column(6,
									lapply((mid+1):length(communes), function(n) numericInput(inputId=paste0("city-",n),label=communes[n,]$NOM_COM, value=NA))
							)
						)
					)
				),
				tags$div(class="panel panel-primary",
					tags$div(class="panel-heading", id="panel-heading-4",
						tags$h2(role="button", `data-toggle`="collapse", `data-target`="#collapse-4", "Mer")
					),
					tags$div(id="collapse-4", class="panel-body collapse", `data-parent`="#accordion",
						fluidRow(
							column(6,
									lapply(1:midsea, function(n) numericInput(inputId=paste0("sea-",n),label=sea[n,]$NOM, value=15))
							),
							column(6,
									lapply((midsea+1):length(sea), function(n) numericInput(inputId=paste0("sea-",n),label=sea[n,]$NOM, value=15))
							)
						)
					)
				)
			)
		),
		mainPanel(
			h1("Carte de température"),
			p("L'ancien outil est disponible ", a("ici", href="/gradient_map_v1"), "."),
			fluidRow(
				 column(12, h2("Aperçu")),
				 column(12, plotOutput(outputId="map", height="auto")) #, height="600px"
			),
			fluidRow(
				column(12, downloadButton(outputId="download", label="Télécharger l'image"))
			)
		)
	)
)

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

	seaInput <- debounce(reactive({
		for (i in 1:length(sea))
			sea[i,'TEMP'] <- input[[paste0("sea-",i)]]
		sea[!is.na(sea$TEMP),]
	}), 2000)

	predInput <- reactive({
		values <- communesInput()
		if (length(values) > 0) {
			return(idw(TEMP~1, locations=values, newdata=grd))
		}
	})

	observeEvent(input$loadData, {
	  req(input$date)
	  req(input$period)

	  day <- as.character(as.numeric(input$date - Sys.Date(), units="days"))
	  period <- input$period

	  queryCities <- paste0("https://api.meteo-concept.fr/api/forecast/daily/",day,"/period/",period,"/map")
	  result <- GET(queryCities, query=list(listCity=paste(communes$INSEE_COM, collapse=","), token=apikey))
		if (result$status_code == 200) {
			parsed <- content(result, "parsed")
			forecast <- rbindlist(parsed$forecast, use.names=T)
			updateTextInput(session, "title", value=stringr::str_to_upper(strftime(input$date, "%A %d %B"), locale="fr"))
			updateTextInput(session, "subtitle", value=c("NUIT","MATIN","APRÈS-MIDI","SOIRÉE")[as.numeric(period) + 1])
			for (i in 1:length(communes)) {
				newValue <- forecast[insee == as.character(communes[i,]$INSEE_COM),temp2m]
				updateNumericInput(session, paste0("city-",i), value=newValue)
			}
		}
	})

	makePlot <- function() {
		values <- communesInput()
		pred <- predInput()
		seaValues <- seaInput()
		title <- input$title
		subtitle <- input$subtitle
		tendency <- input$tendency
		if (length(values) > 0) {
			par(bg=NA, bty="n", xpd=NA, xaxs='i', xaxt='n', yaxs='i', yaxt='n',
				plt=c(0,1,0,1), oma=c(0,0,0,0))

			r <- mask(raster(pred), contour, updatevalue=NA)
			plot(extent(mapCorners),col="transparent")

			grid.raster(image_read('./sea_big.png'))
			plot(r, col=col[['temp']], breaks=breaks[['temp']],add=T, axis=F, legend=F)
			grid.raster(image_read('./meteo_bretagne_holes.png'))
			raster::text(values, labels=values$TEMP,
				family=c("D-DIN Condensed"), font=2, halo=TRUE, hc="black", hw=0.15, col="white", cex=2.5)

			raster::text(seaValues, labels=seaValues$TEMP,
				family=c("D-DIN Condensed"), halo=TRUE, col="#8AAFCF", cex=2)

			grid.text(title, x=unit(0.10, "npc"), y=unit(0.85, "npc"),
							gp=gpar(fontfamily="D-DIN Condensed", fontface="bold", cex=4, col="white"),
							just=c("left","bottom"))
			grid.text(subtitle, x=unit(0.10, "npc"), y=unit(0.82, "npc"),
							gp=gpar(fontfamily="Liberation", cex=3, col="white"),
							just=c("left","top"))

			grid.raster(image_read(tendency), x=unit(0.95, "npc"), y=unit(0.5, "npc"), width=unit(0.040, "npc"))

			rasterImage(image_read("./wave.png"),
				xleft=(coordinates(sea)[,1] - xinch(.2)),
				xright=(coordinates(sea)[,1] + xinch(.2)),
				ytop=(coordinates(sea)[,2] - yinch(.1)),
				ybottom=(coordinates(sea)[,2] - yinch(.4))
			)
		}
	}

	output$map <- renderPlot({
		makePlot()
	}, height = function() { session$clientData$output_map_width * 9./16 })

	output$download <- downloadHandler(
		filename = function() {
			paste(input$title, ".png", sep="")
		},
		content = function(file) {
			plotPNG(makePlot, filename=file, width=1920, height=1080, res=72 * 1920/session$clientData$output_map_width)
		}
	)
}


runApp(shinyApp(ui = ui, server = server), launch.browser=F)


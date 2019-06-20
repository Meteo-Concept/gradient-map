library(gstat)
library(rgdal)
library(rgeos)
library(raster)
library(RColorBrewer)
library(shiny)

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

mid <- trunc(length(communes)/2)
ui <- fluidPage(
		titlePanel("Carte"),
		sidebarLayout(
			      sidebarPanel(
					   h1("Villes"),
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
						 column(12, plotOutput(outputId="map", height="auto")) #, height="600px"
						)
			      )
		)
	)

server <- function(input, output, session) {
	communesInput <- debounce(reactive({
		for (i in 1:length(communes))
			communes[i,'TEMP'] <- input[[paste0("city-",i)]]
		communes[!is.na(communes$TEMP),]
	}), 2000)

	output$map <- renderPlot({
		values <- communesInput()
		if (length(values) > 0) {
			pred <- idw(TEMP~1, locations=communesInput(), newdata=grd)
			par(bg=NA, bty="n", xpd=NA, xaxs='i', xaxt='n', yaxs='i', yaxt='n', plt=c(0,1,0,1), oma=c(0,0,0,0))
			r <- mask(raster(pred), contour, updatevalue=NA)
			plot(extent(mapCorners),col="transparent")
			plot(r, col=col[['temp']], breaks=breaks[['temp']],add=T, axis=F, legend=F)
		}
	}, bg="transparent", height=function() {
		session$clientData$output_map_width * 9./16
	})
}


shinyApp(ui = ui, server = server)


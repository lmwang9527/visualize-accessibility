#Visualize trip matrix (skims)
# adopted from Hadley Wickham's vis-migration:
# https://github.com/hadley/vis-migration

library(qtpaint)
library(shapefiles)

alpha <- function (colour, alpha) {
  col <- col2rgb(colour, TRUE)/255
  col[4, ] <- alpha
  rgb(col[1, ], col[2, ], col[3, ], col[4, ])
}
rescale01 <- function(x) (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))

#load('travel_data.rda')

# zone boundaries
if (!exists("zones.df")) {
  options(stringsAsFactors = FALSE)
  zones.shp <- read.shp("zones/taz2000_polygon.shp")
  zones.df <- convert.to.simple(zones.shp)
  names(zones.df) <- c('id', 'x', 'y')
  
  polys <- split(zones.df, zones.df$id)
  source("poly.r")
  centers <- plyr::ddply(zones.df, c("id"), info)
}

if (!exists("flow")) {
  flow <- read.csv("travel_data.csv.bz2")
  flow <- merge(flow, centers, by.x = c("from_zone_id"), by.y = c("id"))
}

# Draw borders
render_borders <- function(item, painter, exposed) { 
  qstrokeColor(painter) <- "black" 
  for(poly in polys) {
    qdrawPolygon(painter, poly$x, poly$y)
  }
}

# Draw highlighted and selected zones
render_highlight <- function(item, painter, exposed) {
  if (is.na(highlighted)) return()
   
  h_poly <- polys[[highlighted + 1]]
  qdrawPolygon(painter, h_poly$x, h_poly$y, 
    stroke = "NA", fill = "grey70")
  
  loc <- as.list(h_poly[1, "id"])
  #zone <- with(fips, name[state == loc$state & zone == loc$zone])
  #state <- with(fips, name[state == loc$state & zone == 0])

  in_f = round(sum(subset(flow, to_zone_id==loc, accvar)))
  out_f = round(sum(subset(flow, from_zone_id==loc, accvar)))
  
  qstrokeColor(painter) <- "black"
  qdrawText(painter, paste(as.character(loc), as.character(in_f), as.character(out_f)), 
    min(zones.df$x), min(zones.df$y), "left", "bottom")

  s_poly <- polys[[selected + 1]]
  qdrawPolygon(painter, s_poly$x, s_poly$y, 
    stroke = "black", fill = "grey50")
}

highlighted <<- NA
selected <<- NA
accvar <<- 'am_biking_person_trips'

# Figure out which polygon is currently under the mouse
hover_zone <- function(layer, event) {
  mat <- layer$deviceTransform(event)$inverted()

  rect <- qrect(-1, -1, 1, 1)
  rect <- mat$mapRect(rect) # now in data space
  pos <- event$pos()
  rect$moveCenter(pos) # centered on the pointer data pos
  highlighted <<- layer$locate(rect)[1] # get indices in rectangle
  
  qupdate(highlight)
}

# On click, select highlighted polygon
select_zone <- function(layer, event) {
  selected <<- highlighted
  qupdate(flow_layer)
}

# Render movement between counties
render_flow <- function(item, painter, exposed) {

  arguments <- as.list(match.call()[-1])
    
  if (is.na(selected)) return()
  zone <- as.list(polys[[selected + 1]][1, 1])
  
  #dst_trips = 
  movement_to <- subset(flow, to_zone_id == zone[[1]])
  value <- movement_to[, accvar]
  movement_to$size <- sqrt(abs(value) / max(abs(value)))
  
  flow_in <- subset(movement_to, size > 0)
  
  circle <- qglyphCircle(2)
  
  qdrawGlyph(painter, circle, flow_in$x, flow_in$y, 
    stroke = "NA", fill = alpha("green", 0.5), cex = 3 * flow_in$size + 1)

  movement_from <- subset(flow, from_zone_id == zone[[1]])
  value <- movement_from[, accvar]
  movement_from$size <- sqrt(abs(value) / max(abs(value)))
  
  flow_out <- subset(movement_from, size > 0, c('to_zone_id', 'size'))
  flow_out <- merge(flow_out, centers, by.x = c("to_zone_id"), by.y = c("id"))
  
  qdrawGlyph(painter, circle, flow_out$x, flow_out$y, 
    stroke = "NA", fill = alpha("red", 0.5), cex = 3 * flow_out$size + 1)
}

if (exists("view")) view$close()

scene <- Qt$QGraphicsScene()
root <- qlayer(scene)
view <- qplotView(scene = scene)

borders <- qlayer(root, render_borders, 
  hoverMoveFun = hover_zone, mousePressFun = select_zone)
borders$setLimits(qrect(range(zones.df$x), range(zones.df$y)))

highlight <- qlayer(root, render_highlight)
highlight$setLimits(borders$limits())

flow_layer <- qlayer(root, render_flow)
flow_layer$setLimits(borders$limits())

print(view)


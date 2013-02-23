
library(plumbr)
library(maps)
library(ggplot2)
library(plyr)
library(shapefiles)
library(foreign)

##to install cranvas following instructions: 
##https://github.com/ggobi/cranvas/wiki
library(qtbase)
library(qtpaint)
library(cranvas)

#load('hh_survey.rda')
if (!exists("qzs")) {
        #convert shp to format acceptable to qdata
        #attribute data
	dbf <- read.dbf('zones/taz2000_polygon.dbf')$dbf
        zs = data.frame(names=dbf$ID, labels=dbf$TAZ, stringsAsFactors = FALSE)
	qzs = qdata(zs, color = 'gray50', border = 'gray90', size = 4)

        #geometry
	shpfile <- read.shp("zones/taz2000_polygon.shp")
	shp.df <- convert.to.simple(shpfile)
	groups = split(shp.df, shp.df$Id)
	geom.df <- NULL
	for (g in groups) {
	   g <- rbind(g, c(g[1,1], NA, NA)); geom.df <- rbind(geom.df, g)
	}
	names(geom.df) <- c('id', 'x', 'y')
	xy = as.data.frame(geom.df[2:3])
	attr(qzs, 'MapData') = xy
}

#PSRC HH survey data, available at http://www.surveyarchive.org/
hs <- read.csv("households.csv.bz2")
qhs <- qdata(hh)
ps <- read.csv("persons.csv.bz2")
qps <- qdata(ps)
ts <- read.csv("trips.csv.bz2")
qts <- qdata(ts)

link_cat(qhs, 'hmtaz', qzs, 'labels')
link_cat(qhs, 'qno', qps, 'qno')
link_cat(qhs, 'qno', qts, 'qno')

qmap(qzs)
#household attributes
qscatter(hhnumveh, totalinc, qhs)

#person attributes
qparallel(c('agerng', 'edu', 'sex', 'ldrv', 'perstype'), qps)
#qscatter(sex, age, qps)

#trip attributes
qbar(mode2, qts, main="primary type of transportation")
qbar(act1, qts, main="primary activity type")


library(ggplot2)

data("ChickWeight")
head(ChickWeight)

ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) + geom_line()

ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) + geom_point(alpha=.3)

ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) + geom_smooth(alpha=.4, size=3)

ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) + geom_smooth(alpha=.2, size=1)+ geom_point(alpha=.3)

ggplot(subset(ChickWeight, Time=21), aes(x=weight, fill=Diet)) + 
  geom_histogram(colour="black", binwidth=50) + facet_grid(Diet ~.)

data(mtcars)
head(mtcars)

qplot(wt, mpg, colour=hp, data=mtcars) +
  coord_cartesian(ylim=c(0, 40)) +
  scale_color_continuous(breaks=c(100,300)) +
  guides(colour="colourbar")

m <- mtcars[1:10,]

ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill='white', colour="red")

install.packages("ggplot2movies")
library(ggplot2movies)
ggplot(movies, aes(x=rating)) +
  geom_histogram() +
  geom_histogram(aes(fill=..count..))

data("economics")
head(economics)

ggplot(economics, aes(date, unemploy)) + geom_line(colour="blue", size=.3, linetype=3)

data("diamonds")
head(diamonds)

ggplot(diamonds, aes(carat, ..density..)) + geom_histogram(binwidth = .2) + facet_grid(.~cut)

ggplot(diamonds, aes(clarity, fill=cut)) + 
  geom_bar() + 
  geom_bar(aes(order=desc(cut)))

df <- data.frame(x=1:10, y=1:10)
f <- ggplot(df, aes(x=x, y=y))
f + geom_line(linetype=2)
f + geom_line(linetype="dotdash")


df = data.frame(x = rnorm(5000), y = rnorm(5000))
h = ggplot(df, aes(x, y))
h + geom_point(alpha=1/10)

ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour = factor(cyl)), size=4)

ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(shape = factor(cyl)), size=4)

ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(size=qsec))

ggplot(mtcars, aes(wt, mpg)) + geom_point(size=2.5) + geom_hline(yintercept = 25, size=3.5)

ggplot(mtcars, aes(wt, mpg))+ geom_point(shape=5)

ggplot(mtcars, aes(wt, mpg)) + geom_point(shape="k", size=3)

ggplot(mtcars, aes(wt, mpg)) + geom_point(shape=NA)

df2 <- data.frame(x=1:5, y=1:25, z=1:25)

ggplot(df2, aes(x=x, y=y)) + geom_point(aes(shape=z), size=4) + scale_shape_identity()

dmod <- lm(price~cut, data=diamonds)
cuts <- data.frame(cut=unique(diamonds$cut), 
                   predict(dmod, data.frame(cut=unique(diamonds$cut)), se=TRUE)[c('fit', 'se.fit')])

ggplot(cuts, aes(x=cut, y=fit, ymin = fit - se.fit, ymax = fit + se.fit, colour = cut)) + 
  geom_pointrange()

ggplot(mtcars, aes(wt, mpg)) + geom_point() + 
  annotate("rect", xmin=2, xmax=3.5,  ymin=2, ymax=25, fill="dark grey", alpha=.5)

qplot(disp, wt, data=mtcars) + geom_smooth() + scale_x_continuous(limits=c(325, 500))

qplot(cut, price, data=diamonds, geom="boxplot")

last_plot() + coord_flip()

time <- seq(7000, 3400, -200)
pop <- c(200, 400, 450, 500, 300, 100, 400, 700, 830, 1200, 400, 350, 200, 700, 370, 800, 200, 100, 120)
grp <- c(2,5,8,3,2,2,4,7,9,4,4,2,2,7,5,12,5,4,4)
med <- c(1.2, 1.3, 1.2, 0.9, 2.1, 1.4, 2.9, 3.4, 2.1, 1.1, 1.2, 1.5, 1.2, .9, .5, 3.3, 2.2, 1.1, 1.2)
par(mar=c(5, 12, 4, 4) + .1)

plot(time, pop, axes = F, xlim=c(7000, 3400), ylim=c(0, max(pop)),
     xlab="", ylab="", type="l", col="black", main="")
points(time, pop, pch=20, col="black")
axis(2, ylim = c(0, max(pop)), col="black", lwd=2)
mtext(2, text="Population", line=2)

par(new=T)
plot(time, med, axes = F, xlim=c(7000, 3400), ylim=c(0, max(med)),
     xlab="", ylab="", type="l", lty=2, lwd=2, col="black", main="", )
points(time, med, pch=20, col="black")
axis(2, ylim = c(0, max(med)), col="black", lwd=2, line=3.5)
mtext(2, text="Medain Group size", line=5.5)

par(new=T)
plot(time, grp, axes = F, xlim=c(7000, 3400), ylim=c(0, max(grp)),
     xlab="", ylab="", type="l", lty=3, lwd=2, col="black", main="", )
axis(2, ylim = c(0, max(grp)), col="black", lwd=2, line=7)
points(time, grp, pch=20, col="black")
mtext(2, text="Number of Groups", line=9)
axis(1, pretty(range(time), 10))
mtext(side=1, text="cal BP", col='black', line=2)
legend(x=7000, y=12, legend = c("Population", "Medain Group size", "Number of Groups"), lty=c(1,2,3))

# install.packages("aplpack")
library(aplpack)

score=c(1,2,3,4,10,2,30,42,31,50,80,76,90,87,21,43,65,76,32,12,34,54)
stem.leaf(score)

WorldPhones
faces(WorldPhones)

stars(WorldPhones)

# install.packages("googleVis")
library(googleVis)

data(Fruits)
head(Fruits)

M1 <- gvisMotionChart(Fruits, idvar = "Fruit", timevar = "Year")
plot(M1)

gvisGeoChart(data, locationvar = "", colorvar = "", sizevar = "", hovervar = "", options=list(), chartid)

data(Exports)
head(Exports)
G1 <- gvisGeoChart(Exports, locationvar = 'Country', colorvar='Profit')
plot(G1)

G2 <- gvisGeoChart(Exports, 'Country', 'Profit', options=list(region="150"))
plot(G2)

require(datasets)
states <- data.frame(state.name, state.x77)
head(states)

G3 <- gvisGeoChart(states, 'state.name', 'Illiteracy', options=list(region="US",
                                                                    displayMode="regions",
                                                                    resolution="provinces",
                                                                    width=600, height=400))
plot(G3)

G5 <- gvisGeoChart(Andrew, 'LatLong', colorvar = "Speed_kt", options=list(region='US'))
plot(G5)

G6 <- gvisGeoChart(Andrew, 'LatLong', sizevar = "Speed_kt", colorvar = "Pressure_mb", options=list(region='US'))
plot(G6)

require(stats)
data("quakes")
head(quakes)
quakes$latlong <- paste(quakes$lat, quakes$long, sep=":")
head(quakes$latlong)

G7 <- gvisGeoChart(quakes, 'latlong', "depth", "mag", 
                   options=list(region="009",
                                displayMode="Markers",
                                colorAxis="{colors:['red','grey']}",
                                backgroundColor="lightblue"))
plot(G7)

# install.packages("XML")
library(XML)


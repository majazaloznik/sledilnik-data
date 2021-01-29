library(shape)
a <- 2

xmin = -6
x <- seq(xmin, 5, 0.01)
x1 <- seq(0, 8, 0.01)
y1 = x1
y2 <- a^x
y3 <- -x1 + 8
y4 <- (sqrt(a)/a)^x

## plot linear up
png("figures/criteria-up.png", width = 1400, height = 700)
par(mar = c(3, 3, 2, 1), xpd = TRUE, mfrow = c(1,2))
col1 = "gray"
dist <- 0.1
aw <- 0.2
al <- 0.3
lw <- 3
plot(x1, y1, type = "l", bty = "n", 
     lwd = lw, col = "blue",
     axes = FALSE,
     xlim = c(0, 8),
     xlab = "", ylab = "", ylim = c(0,8))
k <- c(1,3,5,7)
lapply(k, function(x) lines(c(0, x), c(x, x), col = col1))
lapply(k, function(x) lines(c(x,x), c(0,x),  col = col1))

u <- par("usr")
Arrows(u[1], u[3], u[2], u[3], code = 2, xpd = TRUE, arr.type="curved", arr.width=0.2)
Arrows(u[1], u[3], u[1], u[4], code = 2, xpd = TRUE,  arr.type="curved", arr.width=0.2)

Arrows(0, 1+dist,  0, 3-dist, code = 3, arr.type="curved", arr.width=aw, arr.length = al, col = "yellow", lwd = lw)
Arrows(0, 3+dist,  0, 5-dist, code = 3, arr.type="curved", arr.width=aw, arr.length = al, col = "orange", lwd = lw)
Arrows(0, 5+dist,  0, 7-dist, code = 3, arr.type="curved", arr.width=aw, arr.length = al, col = "red", lwd = lw)

Arrows( 1+dist,  0.5, 3-dist, 0.5, code = 3, arr.type="curved",arr.width=aw, arr.length = al, col = "yellow", lwd = lw)
Arrows( 3+dist,  0.5, 5-dist, 0.5, code = 3, arr.type="curved",arr.width=aw, arr.length = al, col = "orange", lwd = lw)
Arrows( 5+dist,  0.5, 7-dist, 0.5, code = 3, arr.type="curved",arr.width=aw, arr.length = al, col = "red", lwd = lw)

mtext(side = 1, line = 1.5, "Čas", cex = 1.5)
mtext(side = 2, line = 1.5, "Kriteriji", cex = 1.5)


plot(x, y2, type = "l", bty = "n", 
     lwd = lw, col = "blue",
     axes = FALSE,
     xlim = c(xmin, 4),
     xlab = "", ylab = "", ylim = c(0,8))
k <- c(1,3,5,7)
lapply(k, function(x) lines(c(xmin, log(x)/log(a)), c(x, x), col = col1))
lapply(k, function(x) lines(c(log(x)/log(a),log(x)/log(a)), c(0,x),  col = col1))

u <- par("usr")
Arrows(u[1], u[3], u[2], u[3], code = 2, xpd = TRUE, arr.type="curved", arr.width=0.2)
Arrows(u[1], u[3], u[1], u[4], code = 2, xpd = TRUE,  arr.type="curved", arr.width=0.2)

Arrows(-6, 1+0.3,  -6, 3-0.3, code = 3, arr.type="curved", arr.width=aw, arr.length = al, col = "yellow", lwd = lw)
Arrows(-6, 3+0.3,  -6, 5-0.3, code = 3, arr.type="curved", arr.width=aw, arr.length = al, col = "orange", lwd = lw)
Arrows(-6, 5+0.3,  -6, 7-0.3, code = 3, arr.type="curved", arr.width=aw, arr.length = al, col = "red", lwd = lw)

Arrows(xmin + dist , 0.5, log(1)/log(a) - dist , 0.5, code = 2, arr.type="curved", 
       arr.width=aw, arr.length = al,col = "green", lwd = lw)
Arrows(log(1)/log(a) + dist, 0.5, log(3)/log(a) - dist , 0.5, code = 3, arr.type="curved", 
       arr.width=aw, arr.length = al,col = "yellow",  lwd = lw)
Arrows(log(3)/log(a) + dist, 0.5, log(5)/log(a) - dist , 0.5, code = 3, arr.type="curved", 
       arr.width=aw, arr.length = al,col = "orange",  lwd = lw)
Arrows(log(5)/log(a) + dist, 0.5, log(7)/log(a) - dist , 0.5, code = 3, arr.type="curved", 
       arr.width=aw, arr.length = al,col = "red",  lwd = lw)
mtext(side = 1, line = 1.5, "Čas", cex = 1.5)
mtext(side = 2, line = 1.5, "Kriteriji", cex = 1.5)

dev.off()


###############################################################################
## plot down 
png("figures/criteria-down.png", width = 1400, height = 700)
par(mar = c(3, 3, 2, 1), xpd = TRUE, mfrow = c(1,2))
col1 = "gray"
dist <- 0.1
aw <- 0.2
al <- 0.3
lw <- 3
plot(x1, y3, type = "l", bty = "n", 
     lwd = lw, col = "blue",
     axes = FALSE,
     xlim = c(0, 8),
     xlab = "", ylab = "", ylim = c(0,8))
k <- c(1,3,5,7)
lapply(k, function(x) lines(c(0, x), c(8-x, 8-x), col = col1))
lapply(k, function(x) lines(c(x,x), c(0,8-x),  col = col1))

u <- par("usr")
Arrows(u[1], u[3], u[2], u[3], code = 2, xpd = TRUE, arr.type="curved", arr.width=0.2)
Arrows(u[1], u[3], u[1], u[4], code = 2, xpd = TRUE,  arr.type="curved", arr.width=0.2)

Arrows(0, 1+dist,  0, 3-dist, code = 3, arr.type="curved", arr.width=aw, arr.length = al, col = "yellow", lwd = lw)
Arrows(0, 3+dist,  0, 5-dist, code = 3, arr.type="curved", arr.width=aw, arr.length = al, col = "orange", lwd = lw)
Arrows(0, 5+dist,  0, 7-dist, code = 3, arr.type="curved", arr.width=aw, arr.length = al, col = "red", lwd = lw)

Arrows( 1+dist,  0.5, 3-dist, 0.5, code = 3, arr.type="curved",arr.width=aw, arr.length = al, col = "red", lwd = lw)
Arrows( 3+dist,  0.5, 5-dist, 0.5, code = 3, arr.type="curved",arr.width=aw, arr.length = al, col = "orange", lwd = lw)
Arrows( 5+dist,  0.5, 7-dist, 0.5, code = 3, arr.type="curved",arr.width=aw, arr.length = al, col = "yellow", lwd = lw)

mtext(side = 1, line = 1.5, "Čas", cex = 1.5)
mtext(side = 2, line = 1.5, "Kriteriji", cex = 1.5)


plot(x, y4, type = "l", bty = "n", 
     lwd = lw, col = "blue",
     axes = FALSE,
     xlim = c(xmin, 4),
     xlab = "", ylab = "", ylim = c(0,8))
k <- c(1,3,5,7)

lapply(k, function(x) lines(c(xmin, log(x)/(log(1) - log(sqrt(a)))), c(x, x), col = col1))
lapply(k, function(x) lines(c(log(x)/(log(1) - log(sqrt(a))),log(x)/(log(1) - log(sqrt(a)))), c(0,x),  col = col1))

u <- par("usr")
Arrows(u[1], u[3], u[2], u[3], code = 2, xpd = TRUE, arr.type="curved", arr.width=0.2)
Arrows(u[1], u[3], u[1], u[4], code = 2, xpd = TRUE,  arr.type="curved", arr.width=0.2)

Arrows(-6, 1+dist,  -6, 3-dist, code = 3, arr.type="curved", arr.width=aw, arr.length = al, col = "yellow", lwd = lw)
Arrows(-6, 3+dist,  -6, 5-dist, code = 3, arr.type="curved", arr.width=aw, arr.length = al, col = "orange", lwd = lw)
Arrows(-6, 5+dist,  -6, 7-dist, code = 3, arr.type="curved", arr.width=aw, arr.length = al, col = "red", lwd = lw)

Arrows( log(1)/(log(1) - log(sqrt(a))) + dist , 0.5, 4, 0.5,code = 2, arr.type="curved", 
       arr.width=aw, arr.length = al,col = "green", lwd = lw)
Arrows( log(3)/(log(1) - log(sqrt(a))) + dist , 0.5,log(1)/(log(1) - log(sqrt(a))) - dist, 0.5, code = 3, arr.type="curved", 
       arr.width=aw, arr.length = al,col = "yellow",  lwd = lw)
Arrows( log(5)/(log(1) - log(sqrt(a))) + dist , 0.5,log(3)/(log(1) - log(sqrt(a))) - dist, 0.5, code = 3, arr.type="curved", 
        arr.width=aw, arr.length = al,col = "orange",  lwd = lw)
Arrows( log(7)/(log(1) - log(sqrt(a))) + dist , 0.5,log(5)/(log(1) - log(sqrt(a))) - dist, 0.5, code = 3, arr.type="curved", 
        arr.width=aw, arr.length = al,col = "red",  lwd = lw)

mtext(side = 1, line = 1.5, "Čas", cex = 1.5)
mtext(side = 2, line = 1.5, "Kriteriji", cex = 1.5)

dev.off()




library(ndtv) # also loads animation and networkDynamic

wheel <- network.initialize(10) # create a toy network
add.edges.active(wheel,tail=1:9,head=c(2:9,1),onset=1:9, terminus=11)
add.edges.active(wheel,tail=10,head=c(1:9),onset=10, terminus=12)
plot(wheel) # peek at the static version
render.animation(wheel) # compute and render

ani.replay()

require(ndtv) # dynamic network animations
require(tergm) # dynamic ergm simulations

data("florentine") # an example network
plot(flobusiness,displaylabels=TRUE)

theta.diss <- log(9)
stergm.fit.1 <- stergm(flobusiness,
                         formation= ~edges+gwesp(0,fixed=TRUE),
                         dissolution = ~offset(edges),
                         targets="formation",
                         offset.coef.diss = theta.diss,
                         estimate = "EGMME" )

stergm.sim.1 <- simulate.stergm(stergm.fit.1,
                                nsim=1, time.slices = 100)

slice.par <-list(start=75,end=100,interval=1,
                aggregate.dur=1,rule="latest")

compute.animation(stergm.sim.1,slice.par=slice.par,
                  animation.mode='MDSJ')

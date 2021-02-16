#####################################
## Flood analysis                  ##
## Updated 10 January 2007         ##
## CJA Bradshaw                    ##
## Paper for Global Change Biology ##
#####################################

## Remove everything
rm(list = ls())

## Create working files
floodwk <- read.table("flooddata.csv", header = TRUE, sep = ",")

## Remove China as extreme outliers therein
floodwk <- floodwk[-24,]

## Define variables
floods <- sqrt(floodwk$floods+1)
slope <- floodwk$slope
area <- log(10*floodwk$area)
nfc2000 <- log(10*floodwk$nfc2000)
nfl <- log((10*floodwk$nfl) - (2*10*min(na.omit(floodwk$nfl))))
nfc1990 <- log(10*floodwk$nfc1990)
tfc2000 <- log(10*floodwk$tfc2000)
tfc1990 <- log(10*floodwk$tfc1990)
tfl <- log((10*floodwk$tfl) - (2*10*min(na.omit(floodwk$tfl))))
floodur <- log(floodwk$floods*floodwk$dur)
floodurar <- log(floodwk$floods*floodwk$dur*floodwk$areaflavg*10)
pnfc <- floodwk$nfc2000/floodwk$area
pnfcl <- floodwk$nfl/floodwk$area
ptfc <- floodwk$tfc2000/floodwk$area
ptfcl <- floodwk$tfl/floodwk$area
pkillpa <- log(floodwk$kill/floodwk$pop1999/floodwk$area)
pdisppa <- log(floodwk$disp/floodwk$pop1999/floodwk$area)
degrad <- log(floodwk$urban + floodwk$crop + floodwk$cropnat)
pdegrad <- degrad/floodwk$area
floodspa <- asin(sqrt(floodwk$floods/floodwk$area))
nnfc <- log(floodwk$tfc2000 - floodwk$nfc2000)
pnnfc <- nnfc/floodwk$area
lkill <- log(floodwk$floods*floodwk$kill)
ldisp <- log(floodwk$floods*floodwk$disp)
ldamage <- log(floodwk$floods*floodwk$damage)
lpop <- log(floodwk$pop1999)
ppp <- floodwk$PPPGNI/floodwk$GNI
damppp <- log(floodwk$floods*floodwk$damage*ppp)
rain1 <- floodwk$prec.med
country <- floodwk$country
cont <- floodwk$cont
humzone <- floodwk$humzone

input <- data.frame(country,cont,area,slope,nfc2000,nfl,nfc1990,tfc2000,tfc1990,tfl,floodur,floodurar,pnfc,pnfcl,ptfc,ptfcl,pkillpa,pdisppa,degrad,floodspa,nnfc,lkill,lpop,ldisp,ldamage,ppp,damppp,rain1,humzone)

## For predictions
reduct <- 0.90 ## desired reduction in natural forest cover

input.test <- input
input.test$nfc2000 <- log(reduct*exp(input.test$nfc2000)) ## 10 % reduction in natural forest cover
input.test$nfl <- log(as.integer(round((as.integer(exp(input$nfc2000) - exp(input.test$nfc2000)) + exp(input.test$nfl)),0)))

####################################################
## DEFINE RESPONSE ##
resp <- "floods"
#resp <- "floodur"
#resp <- "kill"
#resp <- "disp"
#resp <- "damppp"

####################################################

##############################################################
## DELETION FROM SATURATED (take) OR ADDITION TO NULL (add) ##
## 'apriori' for 'a priori' set
#type <- "add"
#type <- "take"
type <- "apriori"
##############################################################

#########################################
## model type(1 = 'lmer', 2 = 'glm', 3 = 'glmmPQL')
#model.type <- "lmer"
#model.type <- "glmmPQL"
model.type <- "glm"
#########################################

#########
## lmer ##
#########

## New a priori mod.vec sets
if (resp == "floods" & type == "apriori" & model.type == "lmer") {
	mod.1 <- "floods~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl+(1|humzone)"
	mod.2 <- "floods~area+rain1+slope+degrad+nfc2000+nfl+nnfc+(1|humzone)"
	mod.3 <- "floods~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+(1|humzone)"
	mod.4 <- "floods~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*slope+(1|humzone)"
	mod.5 <- "floods~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfl*slope+(1|humzone)"
	mod.6 <- "floods~area+rain1+slope+degrad+nfc2000+nnfc+(1|humzone)"
	mod.7 <- "floods~area+rain1+slope+degrad+nfl+nnfc+(1|humzone)"
	mod.8 <- "floods~area+rain1+slope+degrad+nfc2000+nfl+(1|humzone)"
	mod.9 <- "floods~area+rain1+slope+degrad+(1|humzone)"
	mod.10 <- "floods~1+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10)
}

## New a priori mod.vec sets
if (resp == "floods" & type == "take" & model.type == "lmer") {
	mod.1 <- "floods~area+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl+(1|humzone)"
	mod.2 <- "floods~area+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+(1|humzone)"
	mod.3 <- "floods~area+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+(1|humzone)"
	mod.4 <- "floods~area+rain1+slope+nfc2000+nfl+degrad+nnfc+(1|humzone)"
	mod.5 <- "floods~area+rain1+slope+degrad+nfc2000+nfl+(1|humzone)"
	mod.6 <- "floods~area+rain1+slope+degrad+nfl+(1|humzone)"
	mod.7 <- "floods~area+rain1+slope+degrad+(1|humzone)"
	mod.8 <- "floods~area+rain1+slope+(1|humzone)"
	mod.9 <- "floods~area+rain1+(1|humzone)"
	mod.10 <- "floods~area+(1|humzone)"
	mod.11 <- "floods~1+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11)
}

## New a priori mod.vec sets
if (resp == "floods" & type == "add" & model.type == "lmer") {
	mod.1 <- "floods~1+(1|humzone)"
	mod.2 <- "floods~degrad+(1|humzone)"
	mod.3 <- "floods~degrad+rain1+(1|humzone)"
	mod.4 <- "floods~degrad+rain1+slope+(1|humzone)"
	mod.5 <- "floods~degrad+rain1+slope+area+(1|humzone)"
	mod.6 <- "floods~degrad+rain1+slope+area+nnfc+(1|humzone)"
	mod.7 <- "floods~degrad+rain1+slope+area+nnfc+nfc2000+(1|humzone)"
	mod.8 <- "floods~degrad+rain1+slope+area+nnfc+nfl+nfc2000+(1|humzone)"
	mod.9 <- "floods~degrad+rain1+slope+area+nnfc+nfl+nfc2000+nfc2000*nfl+(1|humzone)"
	mod.10 <- "floods~degrad+rain1+slope+area+nnfc+nfl+nfc2000+nfc2000*nfl+slope*nfc2000+(1|humzone)"
	mod.11 <- "floods~degrad+rain1+slope+area+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+nfc2000*nfl+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11)
}

if (resp == "floodur" & type == "apriori" & model.type == "lmer") {
	mod.1 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl+(1|humzone)"
	mod.2 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl+nnfc+(1|humzone)"
	mod.3 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+(1|humzone)"
	mod.4 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*slope+(1|humzone)"
	mod.5 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfl*slope+(1|humzone)"
	mod.6 <- "floodur~area+rain1+slope+degrad+nfc2000+nnfc+(1|humzone)"
	mod.7 <- "floodur~area+rain1+slope+degrad+nfl+nnfc+(1|humzone)"
	mod.8 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl+(1|humzone)"
	mod.9 <- "floodur~area+rain1+slope+degrad+(1|humzone)"
	mod.10 <- "floodur~1+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10)
}

## New a priori mod.vec sets
if (resp == "floodur" & type == "take" & model.type == "lmer") {
	mod.1 <- "floodur~area+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl+(1|humzone)"
	mod.2 <- "floodur~area+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+(1|humzone)"
	mod.3 <- "floodur~area+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+(1|humzone)"
	mod.4 <- "floodur~area+rain1+slope+nfc2000+nfl+degrad+nnfc+(1|humzone)"
	mod.5 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl+(1|humzone)"
	mod.6 <- "floodur~area+rain1+slope+degrad+nfl+(1|humzone)"
	mod.7 <- "floodur~area+rain1+slope+degrad+(1|humzone)"
	mod.8 <- "floodur~area+rain1+slope+(1|humzone)"
	mod.9 <- "floodur~area+rain1+(1|humzone)"
	mod.10 <- "floodur~area+(1|humzone)"
	mod.11 <- "floodur~1+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11)
}

## New a priori mod.vec sets
if (resp == "floodur" & type == "add" & model.type == "lmer") {
	mod.1 <- "floodur~1+(1|humzone)"
	mod.2 <- "floodur~degrad+(1|humzone)"
	mod.3 <- "floodur~degrad+rain1+(1|humzone)"
	mod.4 <- "floodur~degrad+rain1+slope+(1|humzone)"
	mod.5 <- "floodur~degrad+rain1+slope+area+(1|humzone)"
	mod.6 <- "floodur~degrad+rain1+slope+area+nfc2000+(1|humzone)"
	mod.7 <- "floodur~degrad+rain1+slope+area+nnfc+nfc2000+(1|humzone)"
	mod.8 <- "floodur~degrad+rain1+slope+area+nnfc+nfl+nfc2000+(1|humzone)"
	mod.9 <- "floodur~degrad+rain1+slope+area+nnfc+nfl+nfc2000+nfc2000*nfl+(1|humzone)"
	mod.10 <- "floodur~degrad+rain1+slope+area+nnfc+nfl+nfc2000+nfc2000*nfl+slope*nfc2000+(1|humzone)"
	mod.11 <- "floodur~degrad+rain1+slope+area+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+nfc2000*nfl+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11)
}

if (resp == "kill" & type == "apriori" & model.type == "lmer") {
	mod.1 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl+(1|humzone)"
	mod.2 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+(1|humzone)"
	mod.3 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+(1|humzone)"
	mod.4 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*slope+(1|humzone)"
	mod.5 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfl*slope+(1|humzone)"
	mod.6 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nnfc+(1|humzone)"
	mod.7 <- "lkill~area+lpop+rain1+slope+degrad+nfl+nnfc+(1|humzone)"
	mod.8 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl+(1|humzone)"
	mod.9 <- "lkill~area+lpop+rain1+slope+degrad+(1|humzone)"
	mod.10 <- "lkill~1+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10)
}

## New a priori mod.vec sets
if (resp == "kill" & type == "take" & model.type == "lmer") {
	mod.1 <- "lkill~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl+(1|humzone)"
	mod.2 <- "lkill~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+(1|humzone)"
	mod.3 <- "lkill~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+(1|humzone)"
	mod.4 <- "lkill~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+(1|humzone)"
	mod.5 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl+(1|humzone)"
	mod.6 <- "lkill~area+lpop+rain1+slope+degrad+nfl+(1|humzone)"
	mod.7 <- "lkill~area+lpop+rain1+slope+degrad+(1|humzone)"
	mod.8 <- "lkill~area+lpop+rain1+slope+(1|humzone)"
	mod.9 <- "lkill~area+lpop+rain1+(1|humzone)"
	mod.10 <- "lkill~area+lpop+(1|humzone)"
	mod.11 <- "lkill~area+(1|humzone)"
	mod.12 <- "lkill~1+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11,mod.12)
}

## New a priori mod.vec sets
if (resp == "kill" & type == "add" & model.type == "lmer") {
	mod.1 <- "lkill~1+(1|humzone)"
	mod.2 <- "lkill~degrad+(1|humzone)"
	mod.3 <- "lkill~degrad+rain1+(1|humzone)"
	mod.4 <- "lkill~degrad+rain1+slope+(1|humzone)"
	mod.5 <- "lkill~degrad+rain1+slope+area+(1|humzone)"
	mod.6 <- "lkill~degrad+rain1+slope+area+lpop+(1|humzone)"
	mod.7 <- "lkill~degrad+rain1+slope+area+lpop+nfc2000+(1|humzone)"
	mod.8 <- "lkill~degrad+rain1+slope+area+lpop+nnfc+nfc2000+(1|humzone)"
	mod.9 <- "lkill~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+(1|humzone)"
	mod.10 <- "lkill~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+(1|humzone)"
	mod.11 <- "lkill~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+(1|humzone)"
	mod.12 <- "lkill~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+nfc2000*nfl+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11,mod.12)
}

if (resp == "disp" & type == "apriori" & model.type == "lmer") {
	mod.1 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl+(1|humzone)"
	mod.2 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+(1|humzone)"
	mod.3 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+(1|humzone)"
	mod.4 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*slope+(1|humzone)"
	mod.5 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfl*slope+(1|humzone)"
	mod.6 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nnfc+(1|humzone)"
	mod.7 <- "ldisp~area+lpop+rain1+slope+degrad+nfl+nnfc+(1|humzone)"
	mod.8 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl+(1|humzone)"
	mod.9 <- "ldisp~area+lpop+rain1+slope+degrad+(1|humzone)"
	mod.10 <- "ldisp~1+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10)
}

if (resp == "disp" & type == "take" & model.type == "lmer") {
	mod.1 <- "ldisp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl+(1|humzone)"
	mod.2 <- "ldisp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+(1|humzone)"
	mod.3 <- "ldisp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+(1|humzone)"
	mod.4 <- "ldisp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+(1|humzone)"
	mod.5 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl+(1|humzone)"
	mod.6 <- "ldisp~area+lpop+rain1+slope+degrad+nfl+(1|humzone)"
	mod.7 <- "ldisp~area+lpop+rain1+slope+degrad+(1|humzone)"
	mod.8 <- "ldisp~area+lpop+rain1+slope+(1|humzone)"
	mod.9 <- "ldisp~area+lpop+rain1+(1|humzone)"
	mod.10 <- "ldisp~area+lpop+(1|humzone)"
	mod.11 <- "ldisp~area+(1|humzone)"
	mod.12 <- "ldisp~1+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11,mod.12)
}

## New a priori mod.vec sets
if (resp == "disp" & type == "add" & model.type == "lmer") {
	mod.1 <- "ldisp~1+(1|humzone)"
	mod.2 <- "ldisp~degrad+(1|humzone)"
	mod.3 <- "ldisp~degrad+rain1+(1|humzone)"
	mod.4 <- "ldisp~degrad+rain1+slope+(1|humzone)"
	mod.5 <- "ldisp~degrad+rain1+slope+area+(1|humzone)"
	mod.6 <- "ldisp~degrad+rain1+slope+area+lpop+(1|humzone)"
	mod.7 <- "ldisp~degrad+rain1+slope+area+lpop+nnfc+(1|humzone)"
	mod.8 <- "ldisp~degrad+rain1+slope+area+lpop+nnfc+nfc2000+(1|humzone)"
	mod.9 <- "ldisp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+(1|humzone)"
	mod.10 <- "ldisp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfc2000+(1|humzone)"
	mod.11 <- "ldisp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+(1|humzone)"
	mod.12 <- "ldisp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+nfc2000*nfl+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11,mod.12)
}

## New a priori mod.vec sets
if (resp == "damppp" & type == "apriori" & model.type == "lmer") {
	mod.1 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl+(1|humzone)"
	mod.2 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+(1|humzone)"
	mod.3 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+(1|humzone)"
	mod.4 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*slope+(1|humzone)"
	mod.5 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfl*slope+(1|humzone)"
	mod.6 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nnfc+(1|humzone)"
	mod.7 <- "damppp~area+lpop+rain1+slope+degrad+nfl+nnfc+(1|humzone)"
	mod.8 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl+(1|humzone)"
	mod.9 <- "damppp~area+lpop+rain1+slope+degrad+(1|humzone)"
	mod.10 <- "damppp~1+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10)
}

if (resp == "damppp" & type == "take" & model.type == "lmer") {
	mod.1 <- "damppp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl+(1|humzone)"
	mod.2 <- "damppp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+(1|humzone)"
	mod.3 <- "damppp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+(1|humzone)"
	mod.4 <- "damppp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+(1|humzone)"
	mod.5 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl+(1|humzone)"
	mod.6 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+(1|humzone)"
	mod.7 <- "damppp~area+lpop+rain1+slope+degrad+(1|humzone)"
	mod.8 <- "damppp~area+lpop+rain1+slope+(1|humzone)"
	mod.9 <- "damppp~area+lpop+rain1+(1|humzone)"
	mod.10 <- "damppp~area+lpop+(1|humzone)"
	mod.11 <- "damppp~area+(1|humzone)"
	mod.12 <- "damppp~1+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11,mod.12)
}

## New a priori mod.vec sets
if (resp == "damppp" & type == "add" & model.type == "lmer") {
	mod.1 <- "damppp~1+(1|humzone)"
	mod.2 <- "damppp~degrad+(1|humzone)"
	mod.3 <- "damppp~degrad+rain1+(1|humzone)"
	mod.4 <- "damppp~degrad+rain1+slope+(1|humzone)"
	mod.5 <- "damppp~degrad+rain1+slope+area+(1|humzone)"
	mod.6 <- "damppp~degrad+rain1+slope+area+lpop+(1|humzone)"
	mod.7 <- "damppp~degrad+rain1+slope+area+lpop+nnfc+(1|humzone)"
	mod.8 <- "damppp~degrad+rain1+slope+area+lpop+nnfc+nfc2000+(1|humzone)"
	mod.9 <- "damppp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+(1|humzone)"
	mod.10 <- "damppp~degrad+rai1n+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfc2000+(1|humzone)"
	mod.11 <- "damppp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+(1|humzone)"
	mod.12 <- "damppp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+nfc2000*nfl+(1|humzone)"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11,mod.12)
}

###################
## glm & glmmPQL ##
###################

## New a priori mod.vec sets
if (resp == "floods" & type == "apriori" & (model.type == "glm" | model.type == "glmmPQL")) {
	mod.1 <- "floods~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl"
	mod.2 <- "floods~area+rain1+slope+degrad+nfc2000+nfl+nnfc"
	mod.3 <- "floods~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl"
	mod.4 <- "floods~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*slope"
	mod.5 <- "floods~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfl*slope"
	mod.6 <- "floods~area+rain1+slope+degrad+nfc2000+nnfc"
	mod.7 <- "floods~area+rain1+slope+degrad+nfl+nnfc"
	mod.8 <- "floods~area+rain1+slope+degrad+nfc2000+nfl"
	mod.9 <- "floods~area+rain1+slope+degrad"
	mod.10 <- "floods~1"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10)
}

## New a priori mod.vec sets
if (resp == "floods" & type == "take" & model.type == "glm") {
	mod.1 <- "floods~area+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl"
	mod.2 <- "floods~area+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000"
	mod.3 <- "floods~area+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl"
	mod.4 <- "floods~area+rain1+slope+nfc2000+nfl+degrad+nnfc"
	mod.5 <- "floods~area+rain1+slope+degrad+nfc2000+nfl"
	mod.6 <- "floods~area+rain1+slope+degrad+nfl"
	mod.7 <- "floods~area+rain1+slope+degrad"
	mod.8 <- "floods~area+rain1+slope"
	mod.9 <- "floods~area+rain1"
	mod.10 <- "floods~area"
	mod.11 <- "floods~1"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11)
}

## New a priori mod.vec sets
if (resp == "floods" & type == "add" & model.type == "glm") {
	mod.1 <- "floods~1"
	mod.2 <- "floods~degrad"
	mod.3 <- "floods~degrad+rain1"
	mod.4 <- "floods~degrad+rain1+slope"
	mod.5 <- "floods~degrad+rain1+slope+area"
	mod.6 <- "floods~degrad+rain1+slope+area+nnfc"
	mod.7 <- "floods~degrad+rain1+slope+area+nnfc+nfc2000"
	mod.8 <- "floods~degrad+rain1+slope+area+nnfc+nfl+nfc2000"
	mod.9 <- "floods~degrad+rain1+slope+area+nnfc+nfl+nfc2000+nfc2000*nfl"
	mod.10 <- "floods~degrad+rain1+slope+area+nnfc+nfl+nfc2000+nfc2000*nfl+slope*nfc2000"
	mod.11 <- "floods~degrad+rain1+slope+area+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+nfc2000*nfl"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11)
}

if (resp == "floodur" & type == "apriori" & model.type == "glm") {
	mod.1 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl"
	mod.2 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl+nnfc"
	mod.3 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl"
	mod.4 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*slope"
	mod.5 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl+nnfc+nfl*slope"
	mod.6 <- "floodur~area+rain1+slope+degrad+nfc2000+nnfc"
	mod.7 <- "floodur~area+rain1+slope+degrad+nfl+nnfc"
	mod.8 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl"
	mod.9 <- "floodur~area+rain1+slope+degrad"
	mod.10 <- "floodur~1"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10)
}

## New a priori mod.vec sets
if (resp == "floodur" & type == "take" & model.type == "glm") {
	mod.1 <- "floodur~area+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl"
	mod.2 <- "floodur~area+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000"
	mod.3 <- "floodur~area+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl"
	mod.4 <- "floodur~area+rain1+slope+nfc2000+nfl+degrad+nnfc"
	mod.5 <- "floodur~area+rain1+slope+degrad+nfc2000+nfl"
	mod.6 <- "floodur~area+rain1+slope+degrad+nfl"
	mod.7 <- "floodur~area+rain1+slope+degrad"
	mod.8 <- "floodur~area+rain1+slope"
	mod.9 <- "floodur~area+rain1"
	mod.10 <- "floodur~area"
	mod.11 <- "floodur~1"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11)
}

## New a priori mod.vec sets
if (resp == "floodur" & type == "add" & model.type == "glm") {
	mod.1 <- "floodur~1"
	mod.2 <- "floodur~degrad"
	mod.3 <- "floodur~degrad+rain1"
	mod.4 <- "floodur~degrad+rain1+slope"
	mod.5 <- "floodur~degrad+rain1+slope+area"
	mod.6 <- "floodur~degrad+rain1+slope+area+nfc2000"
	mod.7 <- "floodur~degrad+rain1+slope+area+nnfc+nfc2000"
	mod.8 <- "floodur~degrad+rain1+slope+area+nnfc+nfl+nfc2000"
	mod.9 <- "floodur~degrad+rain1+slope+area+nnfc+nfl+nfc2000+nfc2000*nfl"
	mod.10 <- "floodur~degrad+rain1+slope+area+nnfc+nfl+nfc2000+nfc2000*nfl+slope*nfc2000"
	mod.11 <- "floodur~degrad+rain1+slope+area+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+nfc2000*nfl"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11)
}

if (resp == "kill" & type == "apriori" & model.type == "glm") {
	mod.1 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl"
	mod.2 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc"
	mod.3 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl"
	mod.4 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*slope"
	mod.5 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfl*slope"
	mod.6 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nnfc"
	mod.7 <- "lkill~area+lpop+rain1+slope+degrad+nfl+nnfc"
	mod.8 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl"
	mod.9 <- "lkill~area+lpop+rain1+slope+degrad"
	mod.10 <- "lkill~1"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10)
}

## New a priori mod.vec sets
if (resp == "kill" & type == "take" & model.type == "glm") {
	mod.1 <- "lkill~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl"
	mod.2 <- "lkill~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000"
	mod.3 <- "lkill~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl"
	mod.4 <- "lkill~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc"
	mod.5 <- "lkill~area+lpop+rain1+slope+degrad+nfc2000+nfl"
	mod.6 <- "lkill~area+lpop+rain1+slope+degrad+nfl"
	mod.7 <- "lkill~area+lpop+rain1+slope+degrad"
	mod.8 <- "lkill~area+lpop+rain1+slope"
	mod.9 <- "lkill~area+lpop+rain1"
	mod.10 <- "lkill~area+lpop"
	mod.11 <- "lkill~area"
	mod.12 <- "lkill~1"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11,mod.12)
}

## New a priori mod.vec sets
if (resp == "kill" & type == "add" & model.type == "glm") {
	mod.1 <- "lkill~1"
	mod.2 <- "lkill~degrad"
	mod.3 <- "lkill~degrad+rain1"
	mod.4 <- "lkill~degrad+rain1+slope"
	mod.5 <- "lkill~degrad+rain1+slope+area"
	mod.6 <- "lkill~degrad+rain1+slope+area+lpop"
	mod.7 <- "lkill~degrad+rain1+slope+area+lpop+nfc2000"
	mod.8 <- "lkill~degrad+rain1+slope+area+lpop+nnfc+nfc2000"
	mod.9 <- "lkill~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000"
	mod.10 <- "lkill~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl"
	mod.11 <- "lkill~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000"
	mod.12 <- "lkill~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+nfc2000*nfl"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11,mod.12)
}

if (resp == "disp" & type == "apriori" & model.type == "glm") {
	mod.1 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl"
	mod.2 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc"
	mod.3 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl"
	mod.4 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*slope"
	mod.5 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfl*slope"
	mod.6 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nnfc"
	mod.7 <- "ldisp~area+lpop+rain1+slope+degrad+nfl+nnfc"
	mod.8 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl"
	mod.9 <- "ldisp~area+lpop+rain1+slope+degrad"
	mod.10 <- "ldisp~1"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10)
}

if (resp == "disp" & type == "take" & model.type == "glm") {
	mod.1 <- "ldisp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl"
	mod.2 <- "ldisp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000"
	mod.3 <- "ldisp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl"
	mod.4 <- "ldisp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc"
	mod.5 <- "ldisp~area+lpop+rain1+slope+degrad+nfc2000+nfl"
	mod.6 <- "ldisp~area+lpop+rain1+slope+degrad+nfl"
	mod.7 <- "ldisp~area+lpop+rain1+slope+degrad"
	mod.8 <- "ldisp~area+lpop+rain1+slope"
	mod.9 <- "ldisp~area+lpop+rain1"
	mod.10 <- "ldisp~area+lpop"
	mod.11 <- "ldisp~area"
	mod.12 <- "ldisp~1"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11,mod.12)
}

## New a priori mod.vec sets
if (resp == "disp" & type == "add" & model.type == "glm") {
	mod.1 <- "ldisp~1"
	mod.2 <- "ldisp~degrad"
	mod.3 <- "ldisp~degrad+rain1"
	mod.4 <- "ldisp~degrad+rain1+slope"
	mod.5 <- "ldisp~degrad+rain1+slope+area"
	mod.6 <- "ldisp~degrad+rain1+slope+area+lpop"
	mod.7 <- "ldisp~degrad+rain1+slope+area+lpop+nnfc"
	mod.8 <- "ldisp~degrad+rain1+slope+area+lpop+nnfc+nfc2000"
	mod.9 <- "ldisp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000"
	mod.10 <- "ldisp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfc2000"
	mod.11 <- "ldisp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000"
	mod.12 <- "ldisp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+nfc2000*nfl"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11,mod.12)
}

## New a priori mod.vec sets
if (resp == "damppp" & type == "apriori" & model.type == "glm") {
	mod.1 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl"
	mod.2 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc"
	mod.3 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*nfl"
	mod.4 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfc2000*slope"
	mod.5 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl+nnfc+nfl*slope"
	mod.6 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nnfc"
	mod.7 <- "damppp~area+lpop+rain1+slope+degrad+nfl+nnfc"
	mod.8 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl"
	mod.9 <- "damppp~area+lpop+rain1+slope+degrad"
	mod.10 <- "damppp~1"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10)
}

if (resp == "damppp" & type == "take" & model.type == "glm") {
	mod.1 <- "damppp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000+slope*nfl"
	mod.2 <- "damppp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl+slope*nfc2000"
	mod.3 <- "damppp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc+nfc2000*nfl"
	mod.4 <- "damppp~area+lpop+rain1+slope+nfc2000+nfl+degrad+nnfc"
	mod.5 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000+nfl"
	mod.6 <- "damppp~area+lpop+rain1+slope+degrad+nfc2000"
	mod.7 <- "damppp~area+lpop+rain1+slope+degrad"
	mod.8 <- "damppp~area+lpop+rain1+slope"
	mod.9 <- "damppp~area+lpop+rain1"
	mod.10 <- "damppp~area+lpop"
	mod.11 <- "damppp~area"
	mod.12 <- "damppp~1"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11,mod.12)
}

## New a priori mod.vec sets
if (resp == "damppp" & type == "add" & model.type == "glm") {
	mod.1 <- "damppp~1"
	mod.2 <- "damppp~degrad"
	mod.3 <- "damppp~degrad+rain1"
	mod.4 <- "damppp~degrad+rain1+slope"
	mod.5 <- "damppp~degrad+rain1+slope+area"
	mod.6 <- "damppp~degrad+rain1+slope+area+lpop"
	mod.7 <- "damppp~degrad+rain1+slope+area+lpop+nnfc"
	mod.8 <- "damppp~degrad+rain1+slope+area+lpop+nnfc+nfc2000"
	mod.9 <- "damppp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000"
	mod.10 <- "damppp~degrad+rai1n+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfc2000"
	mod.11 <- "damppp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000"
	mod.12 <- "damppp~degrad+rain1+slope+area+lpop+nnfc+nfl+nfc2000+slope*nfl+slope*nfc2000+nfc2000*nfl"
	mod.vec <- c(mod.1,mod.2,mod.3,mod.4,mod.5,mod.6,mod.7,mod.8,mod.9,mod.10,mod.11,mod.12)
}

## define n.mod
n.mod <- length(mod.vec)
library(lme4)
#if (model.type == "lmer") {
#  detach("package:nlme")
#  library(lme4)}
library(Matrix)
library(MASS)
#if (model.type == "glmmPQL") {
#  detach("package:lme4")
#  library(nlme)}

AICc.lmer <- function(...) {
 models <- list(...)
 num.mod <- length(models)
 AICcs <- numeric(num.mod)
 ns <- numeric(num.mod)
 ks <- numeric(num.mod)
 AICc.vec <- rep(0,num.mod)
 for (i in 1:num.mod) {
  n <- models[[i]]@XtX@x[1]
  k <- length(models[[i]]@fixef)+sum(models[[i]]@nc)+1
  AICcs[i] <- (-2*logLik(models[[i]])[1]) + ((2*k*n)/(n-k-1))
  ns[i] <- n
  ks[i] <- k
  AICc.vec[i] <- AICcs[i]}
 return(AICc.vec)}

BIC.lmer <- function(...) {
	models <- list(...)
	num.mod <- length(models)
	BICs <- numeric(num.mod)
	ns <- numeric(num.mod)
	ks <- numeric(num.mod)
	BIC.vec <- rep(0,num.mod)
	for (i in 1:num.mod) {
    n <- models[[i]]@XtX@x[1]
    k <- length(models[[i]]@fixef)+sum(models[[i]]@nc)+1
		BICs[i] <- (-2*logLik(models[[i]])) + k*log(n)
		ns[i] <- n
		ks[i] <- k
		BIC.vec[i] <- BICs[i]}
	return(BIC.vec)}

# Set functions
AICc.glm <- function(...) {
	models <- list(...)
	num.mod <- length(models)
	AICcs <- numeric(num.mod)
	ns <- numeric(num.mod)
	ks <- numeric(num.mod)
	AICc.vec <- rep(0,num.mod)
	for (i in 1:num.mod) {
		if (length(models[[i]]$df.residual) == 0) n <- models[[i]]$dims$N else n <- length(models[[i]]$residuals)
		if (length(models[[i]]$df.residual) == 0) k <- sum(models[[i]]$dims$ncol) else k <- (length(models[[i]]$coeff))+1
		AICcs[i] <- (-2*logLik(models[[i]])) + ((2*k*n)/(n-k-1))
		ns[i] <- n
		ks[i] <- k
		AICc.vec[i] <- AICcs[i]}
	return(AICc.vec)}

BIC.glm <- function(...) {
	models <- list(...)
	num.mod <- length(models)
	BICs <- numeric(num.mod)
	ns <- numeric(num.mod)
	ks <- numeric(num.mod)
	BIC.vec <- rep(0,num.mod)
	for (i in 1:num.mod) {
		if (length(models[[i]]$df.residual) == 0) n <- models[[i]]$dims$N else n <- length(models[[i]]$residuals)
		if (length(models[[i]]$df.residual) == 0) k <- sum(models[[i]]$dims$ncol) else k <- (length(models[[i]]$coeff))+1
		BICs[i] <- (-2*logLik(models[[i]])) + k*log(n)
		ns[i] <- n
		ks[i] <- k
		BIC.vec[i] <- BICs[i]}
	return(BIC.vec)}
                 
k.lmer <- function(x) {
  if (length(x$df.residual) == 0) length(x@fixef)+sum(x@nc)+1}

k.glm <- function(x) {
  if (length(x$df.residual) == 0) k <- sum(x$dims$ncol) else k <- (length(x$coeff)+1)}
 
delta.IC <- function(x) x - min(x) ## where x is a vector of an IC
weight.IC <- function(x) (exp(-0.5*x))/sum(exp(-0.5*x)) ## Where x is a vector of dIC
chdev.lmer <- function(x) ((( as.numeric(dev.null) - as.numeric(deviance(x)))/ as.numeric(dev.null)*100))
chdev.glm <- function(x) ((( as.numeric(x[12]) - as.numeric(x[10]))/ as.numeric(x[12]))*100) ## % change in deviance, where x is glm object

Modnum <- length(mod.vec)

# Load Model Data
# Remove NAs from model data
if (resp == "floods") {
  data <- data.frame(floods,area,rain1,slope,degrad,nfc2000,nfl,nnfc,humzone,cont)
  data.test <- data.frame(input.test$floods,input.test$area,input.test$rain1,input.test$slope,input.test$degrad,input.test$nfc2000,input.test$nfl,input.test$nnfc,input.test$humzone)
  colnames(data.test) <- c("floods","area","rain1","slope","degrad","nfc2000","nfl","nnfc","humzone")
  data.test <- na.omit(data.test)}
  data.ff <- data.frame(floods,area,rain1,slope,degrad,nfc2000,nfl,nnfc,humzone,cont)
  data.ff <- na.omit(data.ff)
  
if (resp == "floodur") {
  data <- data.frame(floodur,area,rain1,slope,degrad,nfc2000,nfl,nnfc,humzone,cont)
  data.test <- data.frame(input.test$floodur,input.test$area,input.test$rain1,input.test$slope,input.test$degrad,input.test$nfc2000,input.test$nfl,input.test$nnfc,input.test$humzone)
  colnames(data.test) <- c("floodur","area","rain1","slope","degrad","nfc2000","nfl","nnfc","humzone")
  data.test <- na.omit(data.test)}
  data.fd <- data.frame(floodur,area,rain1,slope,degrad,nfc2000,nfl,nnfc,humzone,cont)
  data.fd <- na.omit(data.fd)
  
if (resp == "kill") {
  data <- data.frame(lkill,lpop,area,rain1,slope,degrad,nfc2000,nfl,nnfc,humzone)
  data.test <- data.frame(input.test$lkill,input.test$lpop,input.test$area,input.test$rain1,input.test$slope,input.test$degrad,input.test$nfc2000,input.test$nfl,input.test$nnfc,input.test$humzone)
  colnames(data.test) <- c("lkill","lpop","area","rain1","slope","degrad","nfc2000","nfl","nnfc","humzone")
  rem.sub1 <- which(is.na(data$lkill)); rem.sub2 <- which(is.na(data$nfl))
  rem.sub <- sort(unique(c(rem.sub1,rem.sub2)))
  data.test[-rem.sub,]
  data.test <- na.omit(data.test)}
  data.pk <- data.frame(lkill,lpop,area,rain1,slope,degrad,nfc2000,nfl,nnfc,humzone)
  data.pk <- na.omit(data.pk)
  
if (resp == "disp") {
  data <- data.frame(ldisp,lpop,area,rain1,slope,degrad,nfc2000,nfl,nnfc,humzone)
  data.test <- data.frame(input.test$ldisp,input.test$lpop,input.test$area,input.test$rain1,input.test$slope,input.test$degrad,input.test$nfc2000,input.test$nfl,input.test$nnfc,input.test$humzone)
  colnames(data.test) <- c("ldisp","lpop","area","rain1","slope","degrad","nfc2000","nfl","nnfc","humzone")
  rem.sub1 <- which(is.na(data$ldisp)); rem.sub2 <- which(is.na(data$nfl))
  rem.sub <- sort(unique(c(rem.sub1,rem.sub2)))
  data.test[-rem.sub,]
  data.test <- na.omit(data.test)}
  data.pd <- data.frame(ldisp,lpop,area,rain1,slope,degrad,nfc2000,nfl,nnfc,humzone)
  data.pd <- na.omit(data.pd)

if (resp == "damppp") {
  data <- data.frame(damppp,lpop,area,rain1,slope,degrad,nfc2000,nfl,nnfc,humzone,cont)
  data.test <- data.frame(input.test$damppp,input.test$lpop,input.test$area,input.test$rain1,input.test$slope,input.test$degrad,input.test$nfc2000,input.test$nfl,input.test$nnfc,input.test$humzone)
  colnames(data.test) <- c("damppp","lpop","area","rain1","slope","degrad","nfc2000","nfl","nnfc","humzone")
  rem.sub1 <- which(is.na(data$damppp)); rem.sub2 <- which(is.na(data$nfl))
  rem.sub <- sort(unique(c(rem.sub1,rem.sub2)))
  data.test[-rem.sub,]
  data.test <- na.omit(data.test)}
  data.dm <- data.frame(damppp,lpop,area,rain1,slope,degrad,nfc2000,nfl,nnfc,humzone,cont)
  data.dm <- na.omit(data.dm)

data <- na.omit(data)
n <- dim(data)[1]
n

# Model fitting and logLik output loop

  LL.vec <- SaveCount <- AICc.vec <- BIC.vec <- pc.dev.vec <- k.vec <- 0
  pred.orig.fit <- pred.orig.se <- matrix(0,nrow=dim(data)[1],ncol=length(mod.vec))
  pred.new.fit <- pred.new.se <- matrix(0,nrow=dim(data.test)[1],ncol=length(mod.vec))
  
  mod.num <- seq(1,Modnum,1)

  if (model.type == "lmer" & resp == "floods") dev.null <- deviance(lmer(floods~1+(1|humzone),family=gaussian(link="sqrt"),data=data,method="Laplace",control=(list(PQLmaxIt=1000,maxIter=1000,msMaxIter=1000,niterEM=1000))))
  if (model.type == "glmmPQL" & resp == "floods") dev.null <- -2*as.numeric(logLik(glmmPQL(floods~1,random= ~ 1 | humzone, family=gaussian(link=identity),data=data)))
  if (model.type == "lmer" & resp == "floodur") dev.null <- deviance(lmer(floodur~1+(1|humzone),family=gaussian(link="sqrt"),data=data,control=(list(PQLmaxIt=1000,maxIter=1000,msMaxIter=1000,niterEM=1000))))
  if (model.type == "lmer" & resp == "kill") dev.null <- deviance(lmer(lkill~1+(1|humzone),family=gaussian(link="sqrt"),data=data,control=(list(PQLmaxIt=1000,maxIter=1000,msMaxIter=1000,niterEM=1000))))
  if (model.type == "lmer" & resp == "disp") dev.null <- deviance(lmer(ldisp~1+(1|humzone),family=gaussian(link="identity"),data=data,control=(list(PQLmaxIt=1000,maxIter=1000,msMaxIter=1000,niterEM=1000))))
  if (model.type == "lmer" & resp == "damppp") dev.null <- deviance(lmer(damppp~1+(1|humzone),family=gaussian(link="sqrt"),data=data,control=(list(PQLmaxIt=1000,maxIter=1000,msMaxIter=1000,niterEM=1000))))

  for(i in 1:Modnum) {
      
  	if (model.type == "lmer" & resp == "floods") fit <- lmer(as.formula(mod.vec[i]),family=gaussian(link="sqrt"), data=data, method="Laplace", na.action=na.omit,control=(list(PQLmaxIt=1000,maxIter=1000,msMaxIter=1000,niterEM=1000)))
  	if (model.type == "glmmPQL" & resp == "floods") fit <- glmmPQL(as.formula(mod.vec[i]), random = ~ 1 | humzone, family=gaussian(link="identity"), data=data)    
  	if (model.type == "glm" & resp == "floods") fit <- glm(as.formula(mod.vec[i]),family=gaussian(link="sqrt"), data=data)

  	if (model.type == "lmer" & resp == "floodur") fit <- lmer(as.formula(mod.vec[i]),family=gaussian(link="sqrt"), data=data, na.action=na.omit,control=(list(PQLmaxIt=1000,maxIter=1000,msMaxIter=1000,niterEM=1000)))
  	if (model.type == "glm" & resp == "floodur") fit <- glm(as.formula(mod.vec[i]),family=gaussian(link="sqrt"), data=data)

  	if (model.type == "lmer" & resp == "kill") fit <- lmer(as.formula(mod.vec[i]),family=gaussian(link="sqrt"), data=data, na.action=na.omit,control=(list(PQLmaxIt=1000,maxIter=1000,msMaxIter=1000,niterEM=1000)))
  	if (model.type == "glm" & resp == "kill") fit <- glm(as.formula(mod.vec[i]),family=gaussian(link="sqrt"), data=data)

  	if (model.type == "lmer" & resp == "disp") fit <- lmer(as.formula(mod.vec[i]),family=gaussian(link="identity"), data=data, na.action=na.omit,control=(list(PQLmaxIt=1000,maxIter=1000,msMaxIter=1000,niterEM=1000)))
  	if (model.type == "glm" & resp == "disp") fit <- glm(as.formula(mod.vec[i]),family=gaussian(link="identity"), data=data)

  	if (model.type == "lmer" & resp == "damppp") fit <- lmer(as.formula(mod.vec[i]),family=gaussian(link="sqrt"), data=data, na.action=na.omit,control=(list(PQLmaxIt=1000,maxIter=1000,msMaxIter=1000,niterEM=1000)))
  	if (model.type == "glm" & resp == "damppp") fit <- glm(as.formula(mod.vec[i]),family=gaussian(link=identity), data=data)

    LL.vec[i] <- ifelse(model.type == "glmmPQL",fit@logLik[1],as.numeric(logLik(fit)))
    if (model.type == "lmer") AICc.vec[i] <- AICc.lmer(fit)
    if (model.type == "glmmPQL") {
        k.vec[i] <- sum(as.vector(fit@rep@nc)) - fit@rep@nc[length(fit@rep@nc)]
        n.mod <- fit@rep@nc[length(fit@rep@nc)]
        AICc.vec[i] <- (-2*LL.mod + ((2*k.mod*n.mod)/(n.mod-k.mod-1)))}
    if (model.type == "glm") AICc.vec[i] <- AICc.glm(fit)
    BIC.vec[i] <- ifelse(model.type == "lmer",BIC.lmer(fit),BIC.glm(fit))
    k.vec[i] <- ifelse(model.type == "lmer",k.lmer(fit),k.glm(fit))
  	if (model.type == "lmer" | model.type == "glmmPQL") pc.dev.vec[i] <- chdev.lmer(fit)
    if (model.type == "glm") pc.dev.vec[i] <- chdev.glm(fit)
  	print(i)

    ## Predictions
    if (model.type == "glm") pred.orig.fit[,i] <- (predict.glm(fit,data,type="response",se.fit=T))$fit
    if (model.type == "glm") pred.orig.se[,i] <- (predict.glm(fit,data,type="response",se.fit=T))$se.fit
    
    if (model.type == "glm") pred.new.fit[,i] <- (predict.glm(fit,data.test,type="response",se.fit=T))$fit
    if (model.type == "glm") pred.new.se[,i] <- (predict.glm(fit,data.test,type="response",se.fit=T))$se.fit
  }

 #AIC weights
	dAICc <- delta.IC(AICc.vec)
	wAICc <- weight.IC(dAICc)

 #AIC weights
	dBIC <- delta.IC(BIC.vec)
	wBIC <- weight.IC(dBIC)

## Create results dataframe
table<-cbind(mod.num,k.vec,LL.vec,AICc.vec,dAICc,wAICc,BIC.vec,dBIC,wBIC,pc.dev.vec)
colnames(table)<-c("model","k","-LogL","AICc","dAICc","wAIC","BIC","dBIC","wBIC","pcdev")
rownames(table)<- mod.vec

##shows table sorted by wAIC
summary.table<-table[order(table[,6],decreasing=TRUE),1:10]
summary.table


## Model-averaged predictions
len.orig <- dim(pred.orig.fit)[1]
len.new <- dim(pred.new.fit)[1]
ma.pred.orig <- ma.pred.new <- ma.pred.orig.se <- ma.pred.new.se <- 0
for (p in 1:len.orig) {
	ma.pred.orig[p] <- sum(wAICc*pred.orig.fit[p,])
	ma.pred.orig.se[p] <- sqrt(sum((wAICc*pred.orig.se[p,])^2))}

for (p in 1:len.new) {
	ma.pred.new[p] <- sum(wAICc*pred.new.fit[p,])
	ma.pred.new.se[p] <- sqrt(sum((wAICc*pred.new.se[p,])^2))}

## % increase in flood frequency variable
if (resp == "floods") {
mean.incr <- mean(na.omit(100*(((ma.pred.new^2) - (ma.pred.orig^2))/(ma.pred.orig^2))))
mean.incr
hist(na.omit(100*(((ma.pred.new^2) - (ma.pred.orig^2))/(ma.pred.orig^2))))
med.incr <- median(na.omit(100*(((ma.pred.new^2) - (ma.pred.orig^2))/(ma.pred.orig^2))))
med.incr

## Bootstrapped 95 % CI for med.incr
iter <- 10000
pc.change <- na.omit(100*(((ma.pred.new^2) - (ma.pred.orig^2))/(ma.pred.orig^2)))
med.vec <- rep(0,iter)
	for (i in 1:iter) {
		resamp <- sample(pc.change,replace=T)
		med.vec[i] <- median(resamp)
		#print(i)
	} ## end i loop
}

## % increase in flood severity variables
if (resp == "floodur" | resp == "kill" | resp == "disp" | resp == "damppp") {
mean.incr <- mean(na.omit(100*((exp(ma.pred.new) - exp(ma.pred.orig))/exp(ma.pred.orig))))
mean.incr
hist(na.omit(100*((exp(ma.pred.new) - exp(ma.pred.orig))/exp(ma.pred.orig))))
med.incr <- median(na.omit(100*((exp(ma.pred.new) - exp(ma.pred.orig))/exp(ma.pred.orig))))
med.incr

## Bootstrapped 95 % CI for med.incr
iter <- 10000
pc.change <- na.omit(100*((exp(ma.pred.new) - exp(ma.pred.orig))/exp(ma.pred.orig)))
med.vec <- rep(0,iter)
	for (i in 1:iter) {
		resamp <- sample(pc.change,replace=T)
		med.vec[i] <- median(resamp)
		#print(i)
	} ## end i loop
}

med.up95 <- (quantile(med.vec,probs=0.95))
med.lo95 <- (quantile(med.vec,probs=0.05))
med <- median(pc.change)
med.lo95; med; med.up95
min(pc.change); max(pc.change)






##############################
## Partial residual plots
## Construct 'best' model glm
##############################

## Re-define termplot to accept 'ylims' arg
termplot <- function(...) { 
	plot <- function(...) { # replace ylim= with ylims=
		args <- list(...)
		if ("ylims" %in% names(args)) {
			args$ylim <- args$ylims
			args$ylims <- NULL}
		do.call(graphics::plot, args)}
	proto(f = stats::termplot)[["f"]](...)} 
library(plotrix)
library(proto)

## resp = floods
fit.floods.best <- glm(floods~area+rain1+slope+degrad+nfc2000+nnfc,family=gaussian(link="sqrt"),data=data.ff)

## Control variables
split.screen(c(2,2))
## Country area
screen(1)
## mar=(bottom,left,top,right)
par(mar=c(4.5,4,1,0),pty="m")
termplot(fit.floods.best,terms="area",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log Area",ylab="Flood Frequency Partial Residual",ylims=c(-0.8,0.8))
boxed.labels(15.8,-0.7,"A",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=2.5)

## Rainfall
screen(2)
## mar=(bottom,left,top,right)
par(mar=c(4.5,2,1,2),pty="m",yaxt="n")
termplot(fit.floods.best,terms="rain1",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="Average Annual Precipitation",ylab="Flood Frequency Partial Residual",ylims=c(-0.8,0.8))
boxed.labels(2900,-0.7,"B",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=2.5)

## Slope
screen(3)
## mar=(bottom,left,top,right)
par(mar=c(4.5,4,1,0),pty="m")
termplot(fit.floods.best,terms="slope",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="Average Slope",ylab="Flood Frequency Partial Residual",ylims=c(-0.8,0.8))
boxed.labels(4.8,-0.7,"C",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=2.5)

## Degraded area
screen(4)
## mar=(bottom,left,top,right)
par(mar=c(4.5,2,1,2),pty="m",yaxt="n")
termplot(fit.floods.best,terms="degrad",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log Degraded Area",ylab="Flood Frequency Partial Residual",ylims=c(-0.8,0.8))
boxed.labels(14.4,-0.7,"D",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=2.5)
close.screen(all = TRUE)
#dev2bitmap("fig3.png", type = "png256", res = 1200, width = 8, height = 8)


## Main effects - flood frequency
split.screen(c(3,1))
## NFC
screen(1)
## mar=(bottom,left,top,right)
par(mar=c(4.5,4.5,0.5,1.5),pty="m")
termplot(fit.floods.best,terms="nfc2000",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log NFC",ylab="Flood Frequency Partial Residual",cex.lab=0.8,cex.axis=0.8)
#termplot(fit.floods.best,terms="nfc2000",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log Natural Forest Cover",ylab="Flood Frequency Partial Residual",cex.lab=1.5,cex.axis=1.5)
boxed.labels(7.3,0.9,"A",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=2.3)

## NNFC
screen(2)
## mar=(bottom,left,top,right)
par(mar=c(4.5,4.5,0.5,1.5),pty="m")
termplot(fit.floods.best,terms="nnfc",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log NNFC",ylab="Flood Frequency Partial Residual",cex.lab=0.8,cex.axis=0.8)
boxed.labels(0.35,0.6,"B",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=2.3)

## NFL
screen(3)
## mar=(bottom,left,top,right)
par(mar=c(4.5,4.5,0.5,1.5),pty="m")
fit.floods.second <- glm(floods~area+rain1+slope+degrad+nfc2000+nfl+nnfc,family=gaussian(link="sqrt"),data=data.ff)
termplot(fit.floods.second,terms="nfl",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log NFL",ylab="Flood Frequency Partial Residual",cex.lab=0.8,cex.axis=0.8)
boxed.labels(5.45,0.47,"C",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=2.3)
close.screen(all = TRUE)
#dev2bitmap("fig4.png", type = "png256", res = 1200, width = 3.6, height = 10)


## Main effects - flood duration
split.screen(c(3,1))
## NFC
screen(1)
## mar=(bottom,left,top,right)
par(mar=c(4.5,4.5,0.5,1.5),pty="m")
screen(1)
fit.floodur.second <- glm(floodur~area+rain1+slope+degrad+nfc2000+nnfc,family=gaussian(link="sqrt"),data=data.fd)
termplot(fit.floodur.second,terms="nfc2000",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log NFC",ylab="Flood Duration Partial Residual",cex.lab=0.8,cex.axis=0.8)
boxed.labels(7.33,0.58,"A",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=2.3)

## NNFC
screen(2)
## mar=(bottom,left,top,right)
par(mar=c(4.5,4.5,0.5,1.5),pty="m")
termplot(fit.floodur.second,terms="nnfc",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log NNFC",ylab="Flood Duration Partial Residual",cex.lab=0.8,cex.axis=0.8)
boxed.labels(0.35,0.35,"B",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=2.3)

## NFL
screen(3)
## mar=(bottom,left,top,right)
par(mar=c(4.5,4.5,0.5,1.5),pty="m")
fit.floodur.second <- glm(floodur~area+rain1+slope+degrad+nfc2000+nfl+nnfc,family=gaussian(link="sqrt"),data=data.fd)
termplot(fit.floodur.second,terms="nfl",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log NFL",ylab="Flood Duration Partial Residual",cex.lab=0.8,cex.axis=0.8)
boxed.labels(5.5,0.47,"C",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=2.3)
close.screen(all = TRUE)
#dev2bitmap("fig5.png", type = "png256", res = 1200, width = 3.6, height = 10)


## Main effects - other severity responses (people killed, people displaced, damage)
split.screen(c(3,2))
## people killed - NFC
screen(1)
## mar=(bottom,left,top,right)
par(mar=c(4.5,4,0.5,1),pty="m")
fit.kill.second <- glm(lkill~area+rain1+slope+degrad+nfc2000+nnfc,family=gaussian(link="sqrt"),data=data.pk)
termplot(fit.kill.second,terms="nfc2000",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log NFC",ylab="People Killed Partial Residual",cex.lab=0.8,cex.axis=0.8)
boxed.labels(7.3,1.4,"A",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=1.5)

## people killed - NNFC
screen(2)
## mar=(bottom,left,top,right)
par(mar=c(4.5,2,0.5,3),pty="m")
termplot(fit.kill.second,terms="nnfc",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log NNFC",ylab="People Killed Partial Residual",cex.lab=0.8,cex.axis=0.8)
boxed.labels(0.3,1.3,"B",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=1.5)

## people displaced - NFC
screen(3)
## mar=(bottom,left,top,right)
par(mar=c(4.5,4,0.5,1),pty="m")
fit.disp.second <- glm(ldisp~area+rain1+slope+degrad+nfc2000+nfl,family=gaussian(link="identity"),data=data.pd)
termplot(fit.disp.second,terms="nfc2000",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log NFC",ylab="People Displaced Partial Residual",cex.lab=0.8,cex.axis=0.8)
boxed.labels(7.3,7.5,"C",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=1.5)

## people displaced - NFL
screen(4)
## mar=(bottom,left,top,right)
par(mar=c(4.5,2,0.5,3),pty="m")
termplot(fit.disp.second,terms="nfl",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log NFL",ylab="People Displaced Partial Residual",cex.lab=0.8,cex.axis=0.8)
boxed.labels(5.5,3.4,"D",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=1.5)

## damage - NNFC
screen(5)
## mar=(bottom,left,top,right)
par(mar=c(4.5,4,0.5,1),pty="m")
fit.damppp.second <- glm(damppp~area+rain1+slope+degrad+nnfc+nfl,family=gaussian(link="identity"),data=data.dm)
termplot(fit.damppp.second,terms="nnfc",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log NNFC",ylab="Flood Damage Partial Residual",cex.lab=0.8,cex.axis=0.8)
boxed.labels(3.85,3.4,"E",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=1.5)

## damage - NFL
screen(6)
## mar=(bottom,left,top,right)
par(mar=c(4.5,2,0.5,3),pty="m")
termplot(fit.damppp.second,terms="nfl",partial.resid=T,rug=F,se=F,col.term="black",col.res="black",col.se="black",pch=19,xlab="log NFL",ylab="Flood Damage Partial Residual",cex.lab=0.8,cex.axis=0.8)
boxed.labels(7.1,2.8,"F",col="black",border=FALSE,xpad=0.0,ypad=0.0,font=1,cex=1.5)
close.screen(all = TRUE)
#dev2bitmap("fig6.png", type = "png256", res = 1200, width = 7, height = 10)

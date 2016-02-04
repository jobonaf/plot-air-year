
## parametri
years=2001:2015

Opt <- list()

## media PM10
Opt[[1]] <- list(
  id.elab=30,
  nome.elab="media annua",
  id.param=5,
  unit=expression("("~mu*g/m^3~")"),
  unit2="microgrammi/metro cubo",
  thr=40,
  type.elab="F",
  code="PM10_med"
)

## superamenti CO
Opt[[2]] <- list(
  id.elab=118,
  nome.elab="superamenti max media 8 ore",
  id.param=10,
  unit="(giorni)",
  unit2="giorni",
  thr=-999,
  type.elab="I",
  code="CO_sup8h"
)

## superamenti giornalieri ozono
Opt[[3]] <- list(
  id.elab=128,
  nome.elab="superamenti max media 8 ore",
  id.param=7,
  unit="(giorni)",
  unit2="giorni",
  thr=-999,
  type.elab="I",
  code="O3_sup8h"
)

## superamenti orari SI ozono
Opt[[4]] <- list(
  id.elab=85,
  nome.elab="superamenti orari della soglia di informazione",
  id.param=7,
  unit="(ore)",
  unit2="ore",
  thr=-999,
  type.elab="I",
  code="O3_sup180"
)

## superamenti orari NO2
Opt[[5]] <- list(
  id.elab=119,
  nome.elab="superamenti orari",
  id.param=8,
  unit="(ore)",
  unit2="ore",
  thr=18,
  type.elab="I",
  code="NO2_supH"
)

## media NO2
Opt[[6]] <- list(
  id.elab=30,
  nome.elab="media annua",
  id.param=8,
  unit=expression("("~mu*g/m^3~")"),
  unit2="microgrammi/metro cubo",
  thr=40,
  type.elab="F",
  code="NO2_med"
)

## media PM2.5
Opt[[7]] <- list(
  id.elab=30,
  nome.elab="media annua",
  id.param=111,
  unit=expression("("~mu*g/m^3~")"),
  unit2="microgrammi/metro cubo",
  thr=25,
  type.elab="F",
  code="PM2.5_med"
)

## media benzene
Opt[[8]] <- list(
  id.elab=30,
  nome.elab="media annua",
  id.param=20,
  unit=expression("("~mu*g/m^3~")"),
  unit2="microgrammi/metro cubo",
  thr=5,
  type.elab="F",
  code="C6H6_med"
)

## media SO2
Opt[[9]] <- list(
  id.elab=30,
  nome.elab="media annua",
  id.param=1,
  unit=expression("("~mu*g/m^3~")"),
  unit2="microgrammi/metro cubo",
  thr=20,
  type.elab="F",
  code="SO2_med"
)

## superamenti PM10
Opt[[10]] <- list(
  id.elab=130,
  nome.elab="superamenti giornalieri",
  id.param=5,
  unit="(giorni)",
  unit2="giorni",
  thr=35,
  type.elab="I",
  code="PM10_sup"
)

## AOT40 ozono vegetazione
Opt[[11]] <- list(
  id.elab=33,
  nome.elab="AOT40, valore bersaglio per la protezione della vegetazione",
  id.param=7,
  unit=expression("("~mu*g/m^3 %.% h~")"),
  unit2="microgrammi/metro cubo per ora",
  thr=6000,
  type.elab="F",
  code="O3_AOT40veg"
)

nelab=11

## connessione al DB
library(arpautils)
con <- dbqa.connect()
source("~/R/projects/plot-aria/R/prepara-andamento.R")
source("~/R/projects/plot-aria/R/plot-andamento.R")
source("~/R/projects/plot-aria/R/plot-nstaz.R")

## PLOT
setwd("~/R/projects/plot-aria/img/")
for (i in 1:nelab) {
  opt <- Opt[[i]]
  Dat <- get.elab.years(id.param=opt$id.param,id.elab=opt$id.elab,type.elab=opt$type.elab,years=years)
  boxplot.elab(Dat=Dat,id.param=opt$id.param,id.elab=opt$id.elab,nome.elab=opt$nome.elab,unit=opt$unit,unit2=opt$unit2,thr=opt$thr,code=opt$code)
  if(opt$thr>0) barplot.elab(Dat=Dat,thr=opt$thr,code=opt$code,id.param=opt$id.param,nome.elab=opt$nome.elab)
}

## disconnessione dal DB
dbDisconnect(con)

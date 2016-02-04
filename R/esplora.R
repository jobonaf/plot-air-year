library(arpautils)
con <- dbqa.connect()

source("/home/giovanni/R/projects/plot-aria/R/prepara-andamento.R")

# guardo quali elaborazioni ho disponibili
# per quell'anno, per quel parametro
dd <- get.elab(id.param = 7,  ## ozono
               id.elab = NULL,
               year = 2015,
               only.valid = F) ## voglio vedere anche quelle invalidate
View(descr.elab(unique(dd$ID_ELAB)))

# estraggo quella che mi interessa
dd <- get.elab(id.param = 7,  ## ozono
               id.elab = 128, ## scelgo i superamenti del valore obiettivo
               year = 2015,
               type.elab = "I", ## devo specificare che si tratta di un'intero
               only.valid = F) ## voglio vedere anche quelle invalidate
View(dd)

dbDisconnect(con)

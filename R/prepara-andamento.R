## restituisce descrizione di una o pi√π elaborazioni statistiche
descr.elab <- function(id.elab) {
  idelabs <- paste("(",paste(id.elab,collapse = ","),")")
  qqq <- paste("select ID_ELABORAZIONE,DES_ELABORAZIONE from WEB_ELAB where",
               "ID_ELABORAZIONE IN",idelabs)
  dat <- dbGetQuery(con,qqq)
  return(dat)
}


## funzione di estrazione di un'elaborazione annuale
get.elab <- function(id.param=5, id.elab=30, type.elab="F",
                     year, only.rrqa=T, only.valid=T,
                     keep.all=F) {
  qqq <- paste("select * from WEB_STAT where ",
               "TO_CHAR(GIORNO,'YYYY')='",year,"' ",
               " and ID_PARAMETRO=",id.param,
               sep="")
  if(!is.null(id.elab)) qqq <- paste(qqq,"and ID_ELABORAZIONE=",id.elab)
  if(only.valid) qqq <- paste(qqq,"and FLG_ELAB=1")
  #print(qqq)
  dat <- dbGetQuery(con,qqq)
  
  # tiene solo RRQA se richiesto
  if(only.rrqa) {
    idx <- dbqa.isrrqa(con,dat$ID_CONFIG_STAZ)
    idx[is.na(idx)] <- FALSE
    if(length(idx)==0) return(NULL)
    dat <- dat[which(idx),]
  }
  # esclude doppioni piu' vecchi
  dat <- dat[order(dat$TS_UPD, decreasing=T),]
  dat <- dat[order(dat$TS_INS, decreasing=T),]
  idx <- paste(dat$ID_CONFIG_STAZ,dat$ID_ELABORAZIONE,dat$ID_EVENTO)
  dat <- dat[which(!duplicated(idx)),]
  
  
  if(keep.all) { # output originale completo
    out <- dat  
  } else {  # oppure colonne selezionate (pi˘ leggibile)
  out <- data.frame(ID_CONFIG_STAZ=dat$ID_CONFIG_STAZ, 
                    V_ELAB=dat[,paste("V_ELAB_",type.elab,sep="")],
                    ID_ELAB=dat$ID_ELABORAZIONE,
                    VERSION=dat$TS_INS,
                    FLAG_ELAB=dat$FLG_ELAB)
  }
  return(out)
}

## funzione di estrazione di un'elaborazione su piu' anni
get.elab.years <- function(id.param=5, id.elab=30, type.elab="F", years, only.rrqa=T) {
  ## estrae
  DAT <- list()
  ny <- length(years)
  st <- NULL
  for (i in 1:ny) {
    Dat <- get.elab(id.param=id.param, id.elab=id.elab, type.elab=type.elab, years[i], only.rrqa=only.rrqa)
    DAT[[i]] <- Dat
    st <- sort(unique(c(st,Dat$ID_CONFIG_STAZ)))
  }
  
  ## mette tutti gli anni in un unico dataframe
  ns <- length(st)
  out <- data.frame(ID_CONFIG_STAZ=st)
  for (i in 1:ny) {
    idx <- match(DAT[[i]]$ID_CONFIG_STAZ,st)
    dum <- rep(NA,ns)
    dum[idx] <- DAT[[i]]$V_ELAB
    out <- data.frame(out,dum)
  }
  colnames(out) <- c("ID_CONFIG_STAZ",paste("V_ELAB_",years,sep=""))
  
  ## aggiunge info sulle stazioni
  Ana <- dbqa.view.staz(con, FUN=return)
  idx <- match(out$ID_CONFIG_STAZ,Ana$ID_STAZIONE)
  out <- data.frame(NOME=Ana$NOME_STAZIONE[idx], PROV=Ana$PROVINCIA[idx], TIPO_STAZ=Ana$TIPOSTAZ[idx], TIPO_ZONA=Ana$ZONA[idx], out)
  return(out)
}

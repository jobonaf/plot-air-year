boxplot.elab <- function(Dat,
                         id.param,
                         id.elab,
                         nome.elab,
                         unit,
                         unit2=gsub(gsub(unit,pattern="\\(",replacement=""),
                                    pattern="\\)",replacement=""),
                         thr,
                         code) {
  colStaz <- which(colnames(Dat)=="NOME")
  colTipoStaz <- which(colnames(Dat)=="TIPO_STAZ")
  colTipoZona <- which(colnames(Dat)=="TIPO_ZONA")
  colID <- which(colnames(Dat)=="ID_CONFIG_STAZ")
  
  ## nome inquinante
  inq <- ""
  if (id.param==5)   {inq <- "PM10"}
  if(id.param==7)   {inq <- "ozono"}
  if(id.param==111) {inq <- "PM2.5"}
  if(id.param==8)   {inq <- "biossido di azoto"}
  if(id.param==10)  {inq <- "monossido di carbonio"}
  if(id.param==1)   {inq <- "biossido di zolfo"}
  if(id.param==20)  {inq <- "benzene"}
  if(inq=="") {
    ppp <- dbqa.view.param(con,FUN=return)
    inq <- ppp$NOME[which(ppp$ID_PARAMETRO==id.param)]
  }
  
  
  ## riorganizza i dati per i boxplot
  dat <- NULL
  for (i in (colID+1):ncol(Dat)) {
    anno <- gsub(colnames(Dat)[i], pattern="V_ELAB_", replacement="")
    tipo <- paste(substr(as.character(Dat[,colTipoStaz]),1,1),
                  substr(as.character(Dat[,colTipoZona]),1,1),
                  sep="")
    dat <- rbind(dat,data.frame(as.character(Dat[,colStaz]),
                                tipo,
                                Dat[,i],
                                rep(anno,
                                    nrow(Dat))))
  }
  colnames(dat) <- c("stazione", "tipo", "indicatore", "anno")
  dat <- as.data.frame(dat)
  annoi <- min(as.numeric(as.character(dat$anno)))
  annof <- max(as.numeric(as.character(dat$anno)))
  
  ## raggruppa in tipi
  if(inq=="ozono") {
    levs <- c("FU",
              "FS",
              "FR")
    Tipi <- c("fondo urbano", 
              "fondo suburbano", 
              "fondo rurale")
    Cols <- c("blueviolet", "sienna","olivedrab")
    lab3 <- "tutte le stazioni regionali di fondo, divise per tipologia"
  } else {
    levs <- c("TU","TS","TR",
              "FU","FS",
              "FR")
    Tipi <- c("traffico","traffico","traffico",
              "fondo urbano/suburbano","fondo urbano/suburbano",
              "fondo rurale")  
    Cols <- c("steelblue", "orange","olivedrab")
    lab3 <- "tutte le stazioni regionali (escluse industriali), divise per tipologia"
  }
  tipi <- unique(Tipi)
  data <- droplevels(subset(dat, subset=tipo%in%levs))
  tt <- Tipi[match(data$tipo,levs)]
  data <- data.frame(stazione=data$stazione, indicatore=data$indicatore, anno=data$anno, tipo=tt)
  
  
  ## parametri
  nt <- length(unique(data$tipo))
  na <- length(unique(data$anno))
  cols <- rep(Cols,na)
  b.cols <- cols
  empty <- TRUE
  
  ## imposta i diversi layout
  pdf(paste(code,"_boxplot_",annoi,"-",annof,".pdf",sep=""), height=7.4, width=5, paper="special")
  dum <- layout(1:5, heights=c(0.7,2,2,2,0.7))
  dd <- list()
  dd[[1]] <- dev.cur()
  for (idev in 1:3) {
    pdf(paste(code,"_boxplot_",annoi,"-",annof,"_group",idev,".pdf",sep=""), height=3.4, width=5, paper="special")
    dum <- layout(1:3, heights=c(0.7,2,0.7))
    dd[[idev+1]] <- dev.cur()
  }  
  
  ## scrive il titolo
  for (idev in 1:4) {
    dev.set(dd[[idev]])
    par(mar=c(0,0,0,0))
    plot(0:15,type="n",xaxt="n",yaxt="n",xlab="",ylab="",frame=F,ylim=c(0,55))
    text(x=4,y=c(40,25,10),
         label=c(paste(inq," (",annoi,"-",annof,")",sep=""),
                 nome.elab,
                 lab3),
         adj=c(0,0),cex=c(1.2,1.2,0.9),font=c(2,2,1))
    lines(x=c(1.5,1.5),y=c(5,50),xpd=T,lwd=2)
    rect(1,12,2,35,xpd=T,lwd=2,col="white")
    lines(x=c(1,2),y=c(24,24),xpd=T,lwd=2)
    text(x=c(1.6,2.1,2.1,2.1,1.6),y=c(50,35,24,12,5),
         labels=c("max","75%",
                  "mediana","25%","min"),
         cex=0.9,adj=0,xpd=T)
  }
  
  ## definisce boxplot per ciascuna tipologia
  for (i in 1:nt) {
    subdata <- subset(data, tipo==tipi[i])
    bx.p <- boxplot(indicatore~anno,
                    data=subdata,
                    range=0,
                    plot=F)
  
    
    ## plotta boxplot per ciascuna tipologia
    for (idev in 1:4) {
      if(idev==1 || idev==(i+1)) {
        dev.set(dd[[idev]])
        par(mar=c(0,5,1.5,1))
        bxp(bx.p,
            border=b.cols[i],
            lty=1,lwd=2,
            xaxt="n",
            ylim=c(0,max(data$indicatore,na.rm=T)),
            xlim=c(1,na),
            las=1,boxfill=ifelse(empty,"white",b.cols[i]),
            medlwd=2, boxwex=0.6, staplewex=0,
            staplelty=0
        )
        abline(h=thr,col="red",lwd=2, lty=2)
        title(ylab=unit)
        mtext(side=3,adj=c(0,0),text=tipi[i],col=b.cols[i])        
      }
    }
    
    ## prepara la tabella dei percentili 
    ## per la successiva scrittura
    tab <- rbind(bx.p$stats[c(1,3,5),],bx.p$n)
    colnames(tab) <- paste("anno.",bx.p$names,sep="")
    tab <- data.frame(tipologia=rep(tipi[i],4),
                      descrizione=c("minimo tra le stazioni",
                                    "mediana tra le stazioni",
                                    "massimo tra le stazioni",
                                    "n.stazioni valide"),
                      tab)
    if(i==1)  Tab <- tab
    if(i>1)  Tab <- rbind(Tab,tab)
  }
  
  ## plotta asse inferiore
  for (idev in 1:4) {
    dev.set(dd[[idev]])
    par(mar=c(1,5,4,1))
    plot(1:na,type="n",xaxt="n",yaxt="n",xlab="",ylab="",frame=F)
    axis(3, tick=F, lty=0, at=1:na, labels=levels(dat$anno), las=2)
    mtext(3,text="anno",line=-1)  
    dev.off()
  }
  
  ## scrive intestazioni tabella
  header <- data.frame(c("Inquinante:",
                         "Elaborazione:",
                         "Unita' di misura",
                         "Copertura spaziale:",
                         "Copertura temporale:",
                         "Tipologia di stazione:"),
                       c(inq,
                         nome.elab,
                         unit2,
                         "Emilia-Romagna",
                         paste(annoi,"-",annof,sep=""),
                         lab3))
  filecsv <- paste(code,"_boxplot_",annoi,"-",annof,".csv",sep="")
  separator=";"
  decimal=","
  write.table(header,file=filecsv,quote=T,row.names=F,col.names=F,
              sep=separator,dec=decimal)
  write.table(Tab,file=filecsv,quote=T,row.names=F,col.names=T,
              append=T,sep=separator,na="n.d.",dec=decimal)
}




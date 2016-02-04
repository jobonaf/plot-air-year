barplot.elab <- function(Dat,thr,code,id.param,nome.elab) {
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
  
  nc <- ncol(Dat)
  data <- Dat[,nc]
  exc <- data>thr
  exc[is.na(exc)] <- FALSE
  not <- data<=thr
  not[is.na(not)] <- FALSE
  Exc <- tapply(exc,Dat$PROV,sum,na.rm=T)
  Not <- tapply(not,Dat$PROV,sum,na.rm=T)
  dat <- rbind(Exc,Not)
  
  year <- gsub(colnames(Dat)[nc],pattern="V_ELAB_",replacement="")
  
  pdf(paste(code,"_barplot_",year,".pdf",sep=""), height=4, width=5, paper="special")
  barplot(height=dat,beside=F,ylim=c(0,max(Exc+Not)*1.2),
          las=1,col=c("orangered3","steelblue"),
          yaxt="n",xlab="province",ylab="numero di stazioni")
  axis(2,at=0:max(Exc+Not),labels=0:max(Exc+Not),las=2)
  mtext(side=3,adj=c(0,0),line=2,font=2,text=inq)
  mtext(side=3,adj=c(0,0),line=1,font=1,text=nome.elab)
  mtext(side=3,adj=c(0,0),line=0,font=1,text=year)
  np <- length(unique(Dat$PROV))
  legend("top",horiz=T,
         legend=c("oltre il limite","entro il limite"),
         fill=c("orangered3","steelblue"),
         bty="n")
  dev.off()
}

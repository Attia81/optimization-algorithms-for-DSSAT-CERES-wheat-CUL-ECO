library(sirad)
library(Dasst)
library(hydroGOF)
library(optimr)
library(dfoptim)
library(FME)
library(optimx)

wd = "D:/DSSAT-Wheat_"

OBSdata <- read.csv(paste(wd,"/OBSGYBio.csv",sep=""))     #read observed data

#-------------------------------------------------------


par <- read.csv(paste(wd,"/DSSAT_ECO.csv",sep=""))       #read par initial values

par_initial <- par$initial
#X <- par_initial
par_min <- par$min
par_max <- par$max


#----write batch file---------
TREF1111.MZX = "BUPV9703.WHX"
Trt=c(1:10)

out_stringb <- paste0("$BATCH(WHEAT)\n",
                      "!\n",
                      "! Directory    : C:\\DSSAT47\\Wheat\n",
                      "! Command Line : C:\\DSSAT47\\DSCSM047.EXE CSCER047 B DSSBatch.v47\n",
                      "! Crop         : Wheat\n",
                      "! Experiment   : BUPV9703.WHX\n",
                      "! ExpNo        : 1\n",
                      "! Debug        : C:\\DSSAT47\\DSCSM047.EXE CSCER047 ' B DSSBatch.v47\n",
                      "!\n",
                      "@FILEX                                                                                        TRTNO     RP     SQ     OP     CO")

for(i in 1:length(Trt)){
  Trtp <- paste0("C:\\DSSAT47\\Wheat\\BUPV9703.WHX                                                                    ",Trt[i],"      1      0      1      0\n")
  if(i==1){
    TrtR=Trtp
  }else{
    TrtR=rbind(TrtR,Trtp)
  }
}

TrtR2 = gsub("TREF1111.MZX",TREF1111.MZX,TrtR)
cat(out_stringb,
    file="C:/DSSAT47/Wheat/DSSBatch.v47",fill=T,append = F)
cat(TrtR2,
    file="C:/DSSAT47/Wheat/DSSBatch.v47",fill=T,append = T)




myfunction <- function(X,Optfig){
  
  
  #-----edit the CUL&ECO files----------
  
  out_string <- paste0("$ECOTYPES:WHCER047.08102017   Last edit: 03/28/2019 GH\n",
                       "!WHEAT CULTIVAR COEFFICIENTS: WHCER047 MODEL\n",
                       "!AHMED ATTIA (MAR-2019)\n",
                       "*ECOTYPE:WHCER047\n",
                       "@ECO#     P1 P2FR1    P2    P3 P4FR1 P4FR2    P4  VEFF PARUE  PARU2  PHL2  PHF3  LA1S  LAFV   LAFR  SLAS LSPHS LSPHE TIL#S TIPHE TIFAC TDPHS TDPHE TDFAC  RDGS HTSTD  AWNS   KCAN  RS%S  GN%S GN%MN  TKFH\n",
                       "USWH01  ")
  
  CalibP <- c(formatC(X[1],format="f",digits=0),noquote(paste(".",25,sep="")),formatC(X[2],format="f",digits=0),
              formatC(X[3],format="f",digits=0), noquote(paste(".",25,sep="")),noquote(paste(".",10,sep="")),formatC(X[4],format="f",digits=0),noquote(paste0("0.",6,sep="")),
              formatC(X[5],format="f",digits=1),formatC(X[6],format="f",digits=1),formatC(13,format="f",digits=0),formatC(1,format="f",digits=1),
              formatC(X[7],format="f",digits=1),formatC(X[8],format="f",digits=2),formatC(X[9],format="f",digits=2),formatC(X[10],format="f",digits=0),
              formatC(X[11],format="f",digits=1),formatC(X[12],format="f",digits=1),formatC(4.5,format="f",digits=1),formatC(2,format="f",digits=1),
              formatC(0.8,format="f",digits=1),formatC(2.2,format="f",digits=1),formatC(6,format="f",digits=1),formatC(4,format="f",digits=1),
              formatC(3,format="f",digits=1),formatC(100,format="f",digits=0),formatC(5,format="f",digits=1),noquote(paste(".",85,sep="")),
              formatC(30,format="f",digits=0),formatC(2.2,format="f",digits=1),formatC(1.9,format="f",digits=1),formatC(-20,format="f",digits=0))
  
  CalibP[2] <- paste("  ",CalibP[2],sep="")      
  CalibP[3] <- paste("  ",CalibP[3],sep="")
  CalibP[4] <- paste("  ",CalibP[4],sep="")
  CalibP[5] <- paste("  ",CalibP[5],sep="")
  CalibP[6] <- paste("  ",CalibP[6],sep="")
  CalibP[7] <- paste("  ",CalibP[7],sep="")
  CalibP[8] <- paste("  ",CalibP[8],sep="")
  CalibP[9] <- paste("  ",CalibP[9],sep="")
  CalibP[10] <- paste("   ",CalibP[10],sep="")
  CalibP[11] <- paste("   ",CalibP[11],sep="")
  CalibP[12] <- paste("  ",CalibP[12],sep="")
  CalibP[13] <- paste("  ",CalibP[13],sep="")
  CalibP[14] <- paste(" ",CalibP[14],sep="")
  CalibP[15] <- paste("  ",CalibP[15],sep="")
  CalibP[16] <- paste("  ",CalibP[16],sep="")
  CalibP[17] <- paste("  ",CalibP[17],sep="")
  CalibP[18] <- paste("  ",CalibP[18],sep="")
  CalibP[19] <- paste("  ",CalibP[19],sep="")
  CalibP[20] <- paste("  ",CalibP[20],sep="")
  CalibP[21] <- paste("  ",CalibP[21],sep="")
  CalibP[22] <- paste("  ",CalibP[22],sep="")
  CalibP[23] <- paste("  ",CalibP[23],sep="")
  CalibP[24] <- paste("  ",CalibP[24],sep="")
  CalibP[25] <- paste("  ",CalibP[25],sep="")
  CalibP[26] <- paste("  ",CalibP[26],sep="")
  CalibP[27] <- paste("  ",CalibP[27],sep="")
  CalibP[28] <- paste("   ",CalibP[28],sep="")
  CalibP[29] <- paste("   ",CalibP[29],sep="")
  CalibP[30] <- paste("  ",CalibP[30],sep="")
  CalibP[31] <- paste("  ",CalibP[31],sep="")
  CalibP[32] <- paste("  ",CalibP[32],sep="")
  
  
  cat(out_string,CalibP,file="C:/DSSAT47/Genotype/WHCER047.ECO",fill=F,append = F)
  
  out_string2 <- paste0("$CULTIVARS:WHCER047.08102017   Last edit: 03/28/2019 GH\n",
                        "*CULTIVARS:WHCER047\n",
                        "@VAR#  VAR-NAME........  EXP#   ECO#   P1V   P1D    P5    G1    G2    G3 PHINT\n",
                        "IB0488 NEWTON             1,6 USWH01")
  
  
  CalibP2 <- c(formatC(X[13],format="f",digits=0),formatC(X[14],format="f",digits=0),formatC(X[15],format="f",digits=0),
               formatC(X[16],format="f",digits=0),formatC(X[17],format="f",digits=0),
               formatC(X[18],format="f",digits=1),formatC(X[19],format="f",digits=0))
  
  CalibP2[1] <- paste("   ",CalibP2[1],sep="")
  CalibP2[2] <- paste("   ",CalibP2[2],sep="")
  CalibP2[3] <- paste("  ",CalibP2[3],sep="")
  CalibP2[4] <- paste("   ",CalibP2[4],sep="")
  CalibP2[5] <- paste("   ",CalibP2[5],sep="")
  CalibP2[6] <- paste("  ",CalibP2[6],sep="")
  CalibP2[7] <- paste("  ",CalibP2[7],sep="")

  
  cat(out_string2,CalibP2,file="C:/DSSAT47/Genotype/WHCER047.CUL",fill=F,append = F)

  #----edit the irrigation efficiency par-------
  
 Xbuild <- readLines("C:/DSSAT47/Wheat/BUPV9703.WHX")
 Xbuild[77] = paste(" 1 ",formatC(X[20],format="f",digits=2),"   30    50   100 GS000 IR001    10 1997 I0")
 Xbuild[82] = paste(" 2 ",formatC(X[21],format="f",digits=2),"   30    50   100 GS000 IR001    10 1997 I25")
 Xbuild[103] = paste(" 3 ",formatC(X[22],format="f",digits=2),"   30    50   100 GS000 IR001    10 1997 I50")
 Xbuild[124] = paste(" 4 ",formatC(X[23],format="f",digits=2),"   30    50   100 GS000 IR001    10 1997 I75")
 Xbuild[145] = paste(" 5 ",formatC(X[24],format="f",digits=2),"   30    50   100 GS000 IR001    10 1997 I100")
 Xbuild[166] = paste(" 6 ",formatC(X[25],format="f",digits=2),"   30    50   100 GS000 IR001    10 1997 I0")
 Xbuild[175] = paste(" 7 ",formatC(X[26],format="f",digits=2),"   30    50   100 GS000 IR001    10 1997 I25")
 Xbuild[195] = paste(" 8 ",formatC(X[27],format="f",digits=2),"   30    50   100 GS000 IR001    10 1997 I50")
 Xbuild[215] = paste(" 9 ",formatC(X[28],format="f",digits=2),"   30    50   100 GS000 IR001    10 1997 I75")
 Xbuild[235] = paste(" 10",formatC(X[29],format="f",digits=2),"   30    50   100 GS000 IR001    10 1997 I100")
 
 write(Xbuild,file="C:/DSSAT47/Wheat/BUPV9703.WHX")
 
 setwd(paste("C:/DSSAT47/Wheat",sep = ""))
 
  #--- write paramters used on the screen
  message("")
  message("Running DSSAT-WheatCERES...")
  
  #--- Call DSSAT047.exe and run X files list within DSSBatch.v47
  system("C:/DSSAT47/DSCSM047.EXE CSCER047 B DSSBatch.v47",show.output.on.console = F)
  
  #---read simulated data----
  
  plantgro <- read.dssat("C:/DSSAT47/Wheat/PlantGro.OUT")

SIMBIOM <- 0
SIMGY <- 0 

for(i in 1:length(plantgro)){
  
  data=as.data.frame(plantgro[[i]])
  SIMBIOM[i] <- tail(data$CWAD,n=1)
  SIMGY[i] <- tail(data$HWAD,n=1)

  }
 #---cmpare with the observed-----

BIomass_rmse <- rmse(SIMBIOM,OBSdata$ObsBio)
GY_rmse <- rmse(SIMGY,OBSdata$ObsG)

y <- BIomass_rmse/100+
  GY_rmse/50

y1 <- GY_rmse

 #----option fig----

if(Optfig==1){
  
  plot(OBSdata$ObsG/1000,SIMGY/1000,xlim=c(0,8),ylim=c(0,8),las=1)
  SimregGY <- SIMGY/1000
  ObsregGY <- OBSdata$ObsG/1000
  reg1<- lm(SimregGY~ObsregGY)
  abline(reg1,pch=4,col=2,lwd=2, lty=2)
  abline(0:1)
  modeleval_GY <- modeval(SimregGY,ObsregGY)
  text(1,5,label=bquote("R"^2~":" ~ .(round(modeleval_GY$R2[[1]],digits=2))),cex=0.7) 
  text(5,2,label=noquote(paste0("RMSE: ",round(modeleval_GY$RMSE[[1]],digits=2))),cex=0.7)

  }

print(c(X,GY_rmse,y1))

return(y1)

}

 #------------------

myfunction(par_initial,Optfig=1)

par(mfrow=c(5,2),mar=c(4,4,3,2)+0.1,mgp=c(2.5,0.7,0))

reswheat=hjkb(par=par_initial,myfunction,Optfig=1,
         lower=par_min,upper=par_max,control=list(maxfeval=100000))    #hjkb optim method one at a time


resoptimr=optimx::optimx(par=par_initial,myfunction,Optfig=0,itnmax=100000,
                         lower=par_min,upper=par_max,method=c("bobyqa","hjkb","L-BFGS-B"),
                         control=list(maxit=100000,all.methods=T,follow.on=T))  #seq of methods will take more time

write.csv(reswheat,paste(wd,"/Calib.csv",sep=""))


plot(OBStotalDW,simtotalDW,xlim=c(0,8000),ylim=c(0,8000))
reg1<- lm(simtotalDW~OBStotalDW)
abline(reg1,pch=4,col=2,lwd=2, lty=2)
abline(0:1)
modeleval_GY <- modeval(simtotalDW,OBStotalDW)
text(7000,50,label=bquote("R"^2~":" ~ .(round(modeleval_GY$R2[[1]],digits=2))),cex=0.7) 
text(50,7000,label=noquote(paste0("RMSE: ",round(modeleval_GY$RMSE[[1]],digits=2))),cex=0.7)


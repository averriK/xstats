library(data.table)
library(bida)

# **********************************************************
# Build Parametric Model

devtools::load_all()

DT <- ppvTable #fread(file.path("inst/data/table.csv")) |> na.omit()
DT.l <- DT[,.(DIR="L",R=R*1000,Q=Qmax,PPV=Vl,PPVc=2*pi*Fl*dl)]
DT.v <- DT[,.(DIR="V",R=R*1000,Q=Qmax,PPV=Vv,PPVc=2*pi*Fv*dv)]
DT.t <- DT[,.(DIR="T",R=R*1000,Q=Qmax,PPV=Vt,PPVc=2*pi*Ft*dt)]
DT <- rbind(DT.l,DT.v,DT.t)

DT.train <- DT[,.(DIR,LnV=log(PPV),R,LnR=log(R),IR=1/R,Q,LnQ=log(Q),IQ=1/Q,Vc=PPVc,LnVc=log(PPVc),IVc=1/PPVc)]

Y <- "LnV"
X <- colnames(DT.train)[!(colnames(DT.train) %in% Y)]

fitModel(.data=DT.train[DIR=="L",-"DIR"],y=Y,x=X,regression="lm")
fitModel(.data=DT.train[DIR=="L",-"DIR"],y=Y,x=X,regression="rf")
fitModel(.data=DT.train[DIR=="L",-"DIR"],y=Y,x=X,regression="qrf")
fitModel(.data=DT.train[DIR=="L",-"DIR"],y=Y,x=X,regression="knn")


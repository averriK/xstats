library(data.table)
library(bida)


# **********************************************************
# Build Training Data
devtools::load_all()
DT <- ppvTable #fread(file.path("inst/data/table.csv")) |> na.omit()
DT.l <- DT[,.(DIR="L",R=R*1000,Q=Qmax,PPV=Vl,PPVc=2*pi*Fl*dl)]
DT.v <- DT[,.(DIR="V",R=R*1000,Q=Qmax,PPV=Vv,PPVc=2*pi*Fv*dv)]
DT.t <- DT[,.(DIR="T",R=R*1000,Q=Qmax,PPV=Vt,PPVc=2*pi*Ft*dt)]
DT <- rbind(DT.l,DT.v,DT.t)
DATA <- DT[,.(DIR,LnV=log(PPV),R,LnR=log(R),IR=1/R,Q,LnQ=log(Q),IQ=1/Q,Vc=PPVc,LnVc=log(PPVc),IVc=1/PPVc)]

# Training
devtools::load_all()
model.lm <- fitModel(.data=DATA,response="LnV",regression="lm")
model.rf <- fitModel(.data=DATA,response="LnV",regression="rf")
model.qrf <- fitModel(.data=DATA,response="LnV",regression="qrf")
model.kknn <- fitModel(.data=DATA,response="LnV",regression="kknn")

# **********************************************************
# Prediction
devtools::load_all()
variables <- colnames(DATA)[!(colnames(DATA) %in% response)]
NEWDATA <- DATA[sample(50),mget(variables)]
fitModel(.data=DATA,.newdata=NEWDATA,response="LnV",regression="lm",level=0.05)

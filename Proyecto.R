attach(BDatosFinalRep)
names(BDatosFinalRep)

fDefault=as.factor(Default)
fAgente=as.factor(Agente)
fTamano=as.factor(Tamano)

Modelo=glm(fDefault~fAgente+T_Ope+Exp_Compra+Rent,
           family = "binomial")
summary(Modelo)
AIC(Modelo)

Ajust=fitted.values(Modelo)

Rev=data.frame(Ajust,BDatosFinalRep)

u=-1,05612+0,86058(Agente=2)-1,03404(Agente=3)+0,07003(T_Ope )+
  0,02408(Exp_Compra )-0,04090(Rent)

u=-1.05612+0.86058*(1)-1.03404*(0)+
  0.07003*(6.1397260)+0.02408*(37.45387)-0.04090*(5.07)

Nom=exp(u)
Den=1+Nom

P=Nom/Den

log.odds=predict(Modelo, BDatosFinalRep)
log.odds

Prob=exp(log.odds)/(1+exp(log.odds))

vitE = read.table("berrydat.txt", header=T)

df = vitE[, c(1, 6:12)]



df$piE = vitE$Edeaths/vitE$Etot
df$piU = vitE$Udeaths/ vitE$Utot
df$diff = df$piE - df$piU
df$RR= df$piE/df$piU
df$oddsR = ( df$piE/(1 - df$piE))/(df$piU/(1 -df$piU))
df$logit = log(df$oddsR)
df$varlogit = 1/vitE$Edeaths + 1/(vitE$Etot - vitE$Edeaths) + 1/vitE$Udeaths + 1/(vitE$Utot - vitE$Udeaths)
plot(df$ID, df$logit )

abline(v=0)



plot(df)
library(mgcv)
vit.gam = (gam(logit ~  s(VitEIU) , data=df, weights = 1/varlogit))
summary(vit.gam)
plot(vit.gam)
vit.lm2 = lm(logit ~ poly(VitEIU,2) + NatE + meanage + VitCmg +  b.carmg + followup, weights=1/varlogit,data=df)
vit.lm = lm(logit ~ VitEIU + NatE + meanage + VitCmg +  b.carmg + followup, weights=1/varlogit,data=df)

summary(vit.lm)
plot(vit.lm)


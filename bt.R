library(ordBTL)

# Berechne Spielstärken für die Saison 17/18

design_score_home = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/design_score_home.csv')

mod.scorehome = ordBTL(ordresp~., data=design_score_home)

# Spielstärken pro Team mit globalem Heimspieleffekt 

result_score_home = getRank(mod.scorehome, reference="GAMMA.Wolfsburg")
print(result_score_home)
write.csv(result_score_home, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/result_score_home.csv')

# team-spezifischer Heimspieleffekt 

design_score_specific = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/design_score_spec.csv')

mod.scoreteamhome <- ordBTL(ordresp~., data=design_score_specific)
# team 'abilities' (should be the ranking for the away table)
result_score_specific = getRank(mod.scoreteamhome, reference="GAMMA.Wolfsburg")
print(result_score_specific)
write.csv(result_score_specific, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/result_score_spec.csv')
# team-specific home advantages
getRank(mod.scoreteamhome, prefix="ALPHA")


# jetzt für xG2, also dem selbst errechneten xG-Wert bzw. den daraus resultierenden Alternativergebnissen  

design_xG2_home = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/design_xG2_home.csv')
#design_xG2_home = design_xG2_home[1:18]
mod.xG2home = ordBTL(ordroundxG~., data=design_xG2_home)

# Spielstärken für Teams mit globalem Heimspieleffekt 

result_xG2_home = getRank(mod.xG2home, reference="GAMMA.Wolfsburg")
print(result_xG2_home)
write.csv(result_xG2_home, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/result_xG2_home.csv')


# spezifischer Heimspieleffekt für jedes Team 

design_xG2_specific = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/design_xG2_spec.csv')

mod.xG2teamhome <- ordBTL(ordroundxG~., data=design_xG2_specific)
# team 'abilities' (should be the ranking for the away table)
result_xG2_specific = getRank(mod.xG2teamhome, reference="GAMMA.Wolfsburg")
print(result_xG2_specific)
write.csv(result_xG2_specific, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/result_xG2_spec.csv')
# team-specific home advantages
getRank(mod.xG2teamhome, prefix="ALPHA")



# jetzt für die externen xG Werte von Opta 


design_extxG = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/design_ext_xG.csv')
#design_xG2_home = design_xG2_home[1:18]
mod.extxGhome = ordBTL(ordextxG~., data=design_extxG)

# Spielstärken für Teams mit globalem Heimspieleffekt 

result_extxGhome = getRank(mod.extxGhome, reference="GAMMA.Wolfsburg")
print(result_extxGhome)
write.csv(result_extxGhome, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/result_extxGhome.csv')


# spezifischer Heimspieleffekt für jedes Team 

design_extxGts = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/design_ext_xGts.csv')

mod.extxGts <- ordBTL(ordextxG~., data=design_extxGts)
# team 'abilities' (should be the ranking for the away table)
result_extxGts = getRank(mod.extxGts, reference="GAMMA.Wolfsburg")
print(result_extxGts)
write.csv(result_extxGts, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/result_extxGts.csv')
# team-specific home advantages
getRank(mod.extxGts, prefix="ALPHA")








## QSE kalkulieren (quasi-Standardabweichung)

library(MASS)
library(qvcalc)

## 17/18 Ergebnis mit globalem Heimspielvorteil  

covscorehome = vcov(mod.scorehome)[2:19,2:19]
covscorehome[1:18,1] = covscorehome[1,1:18] = 0
colnames(covscorehome)[1] = "GAMMA.Wolfsburg"
rownames(covscorehome)[1] = "GAMMA.Wolfsburg"
covscorehome
qsescorehome = qvcalc(covscorehome)


merged_data = merge(qsescorehome$qvframe, result_score_home, by.x = "row.names", by.y = "row.names", all.x = TRUE)

#Übertrage die Werte aus "Estimate" von result in "estimate" von qse
merged_data$estimate.y[!is.na(merged_data$Estimate)] = merged_data$Estimate[!is.na(merged_data$Estimate)]

#Entferne die nicht mehr benötigte "Estimate"-Spalte
merged_data = merged_data[, !grepl("Estimate", names(merged_data))]

#benenne die "estimate.y"-Spalte um
names(merged_data)[names(merged_data) == "estimate.y"] = "estimate"
names(merged_data)[names(merged_data) == "Row.names"] = "Team"

colnames(merged_data)
qsescorehomedf = merged_data[,c(1,9,3:8)]
qsescorehomedf
write.csv(qsescorehomedf, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//qsescorehome.csv' )

## 17/18 mit teamspezifischem Heimspieleffekt 
covtest = vcov(mod.scoreteamhome)[2:37,2:37]
covtest
covscoreteamhome = vcov(mod.scoreteamhome)[2:37,2:37]
covscoreteamhome[1:36,1] = covscoreteamhome[1,1:36] = 0
colnames(covscoreteamhome)[1] = "GAMMA.Wolfsburg"
rownames(covscoreteamhome)[1] = "GAMMA.Wolfsburg"
covscoreteamhome
qsescoreteamhome = qvcalc(covscoreteamhome)

merged_data = merge(qsescoreteamhome$qvframe, result_score_specific, by.x = "row.names", by.y = "row.names", all.x = TRUE)

#Übertrage die Werte aus "Estimate" von result in "estimate" von qse
merged_data$estimate.y[!is.na(merged_data$Estimate)] = merged_data$Estimate[!is.na(merged_data$Estimate)]

#Entferne die nicht mehr benötigte "Estimate"-Spalte
merged_data = merged_data[, !grepl("Estimate", names(merged_data))]

#benenne die "estimate.y"-Spalte um
names(merged_data)[names(merged_data) == "estimate.y"] = "estimate"
names(merged_data)[names(merged_data) == "Row.names"] = "Team"

colnames(merged_data)
qsescoreteamhomedf = merged_data[,c(1,9,3:8)]
qsescoreteamhomedf

write.csv(qsescoreteamhomedf, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//qsescoreteamhome.csv' )


## 17/18 xG2 Alternativergebnis mit globalem Heimspieleffekt 
covxG2home = vcov(mod.xG2home)[2:19,2:19]
covxG2home[1:18,1] = covxG2home[1,1:18] = 0
colnames(covxG2home)[1] = "GAMMA.Wolfsburg"
rownames(covxG2home)[1] = "GAMMA.Wolfsburg"
covxG2home
qsexG2home = qvcalc(covxG2home)

merged_data = merge(qsexG2home$qvframe, result_xG2_home, by.x = "row.names", by.y = "row.names", all.x = TRUE)

#Übertrage die Werte aus "Estimate" von result in "estimate" von qse
merged_data$estimate.y[!is.na(merged_data$Estimate)] = merged_data$Estimate[!is.na(merged_data$Estimate)]

#Entferne die nicht mehr benötigte "Estimate"-Spalte
merged_data = merged_data[, !grepl("Estimate", names(merged_data))]

#benenne die "estimate.y"-Spalte um
names(merged_data)[names(merged_data) == "estimate.y"] = "estimate"
names(merged_data)[names(merged_data) == "Row.names"] = "Team"

colnames(merged_data)
qsexG2homedf = merged_data[,c(1,9,3:8)]
qsexG2homedf

write.csv(qsexG2homedf, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//qsexG2home.csv' )


## 17/18 xG2 Alternativergebnis mit teamspezifischem Heimspieleffekt 

covxG2teamhome = vcov(mod.xG2teamhome)[2:19,2:19]
covxG2teamhome[1:18,1] = covxG2teamhome[1,1:18] = 0
colnames(covxG2teamhome)[1] = "GAMMA.Wolfsburg"
rownames(covxG2teamhome)[1] = "GAMMA.Wolfsburg"
covxG2teamhome
qsexG2teamhome = qvcalc(covxG2teamhome)

merged_data = merge(qsexG2teamhome$qvframe, result_xG2_specific, by.x = "row.names", by.y = "row.names", all.x = TRUE)

# Übertrage die Werte aus "Estimate" von result in "estimate" von qse
merged_data$estimate.y[!is.na(merged_data$Estimate)] = merged_data$Estimate[!is.na(merged_data$Estimate)]

# Entferne die nicht mehr benötigte "Estimate"-Spalte
merged_data = merged_data[, !grepl("Estimate", names(merged_data))]

#benenne die "estimate.y"-Spalte um
names(merged_data)[names(merged_data) == "estimate.y"] = "estimate"
names(merged_data)[names(merged_data) == "Row.names"] = "Team"

colnames(merged_data)
qsexG2teamhomedf = merged_data[,c(1,9,3:8)]
qsexG2teamhomedf

write.csv(qsexG2teamhomedf, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//qsexG2teamhome.csv' )






## 17/18 externexG score mit globalem Heimspieleffekt 
covextxGhome = vcov(mod.extxGhome)[2:19,2:19]
covextxGhome[1:18,1] = covextxGhome[1,1:18] = 0
colnames(covextxGhome)[1] = "GAMMA.Wolfsburg"
rownames(covextxGhome)[1] = "GAMMA.Wolfsburg"
covextxGhome
qseextxGhome = qvcalc(covextxGhome)

merged_data = merge(qseextxGhome$qvframe, result_extxGhome, by.x = "row.names", by.y = "row.names", all.x = TRUE)

#Übertrage die Werte aus "Estimate" von result in "estimate" von qse
merged_data$estimate.y[!is.na(merged_data$Estimate)] = merged_data$Estimate[!is.na(merged_data$Estimate)]

#Entferne die nicht mehr benötigte "Estimate"-Spalte
merged_data = merged_data[, !grepl("Estimate", names(merged_data))]

#benenne die "estimate.y"-Spalte um
names(merged_data)[names(merged_data) == "estimate.y"] = "estimate"
names(merged_data)[names(merged_data) == "Row.names"] = "Team"

colnames(merged_data)
qseextxGhomedf = merged_data[,c(1,9,3:8)]
qseextxGhomedf

write.csv(qseextxGhomedf, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/qseextxGhome.csv' )


## 17/18 externer xG mit teamspezifischen Heimspieleffekt  

covextxGts = vcov(mod.extxGts)[2:19,2:19]
covextxGts[1:18,1] = covextxGts[1,1:18] = 0
colnames(covextxGts)[1] = "GAMMA.Wolfsburg"
rownames(covextxGts)[1] = "GAMMA.Wolfsburg"
covextxGts
qseextxGts = qvcalc(covextxGts)

merged_data = merge(qseextxGts$qvframe, result_extxGts, by.x = "row.names", by.y = "row.names", all.x = TRUE)

# Übertrage die Werte aus "Estimate" von result in "estimate" von qse
merged_data$estimate.y[!is.na(merged_data$Estimate)] = merged_data$Estimate[!is.na(merged_data$Estimate)]

# Entferne die nicht mehr benötigte "Estimate"-Spalte
merged_data = merged_data[, !grepl("Estimate", names(merged_data))]

#benenne die "estimate.y"-Spalte um
names(merged_data)[names(merged_data) == "estimate.y"] = "estimate"
names(merged_data)[names(merged_data) == "Row.names"] = "Team"

colnames(merged_data)
qseextxGtsdf = merged_data[,c(1,9,3:8)]
qseextxGtsdf

write.csv(qseextxGtsdf, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/qseextxGts.csv' )




## Likelihood ratio Test 


library(lmtest) 


#lrtest_vglm(mod.1415teamhome, mod.1415home)
lrtest_vglm(mod.scoreteamhome, mod.scorehome)
lrtest_vglm(mod.xG2teamhome, mod.xG2home)
lrtest_vglm(mod.extxGts, mod.extxGhome)



## Modell für k=3 


## 17/18 home 


design_score_home_3 = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//design_score_home_3.csv')

mod.scorehome3 = ordBTL(ordresp~., data=design_score_home_3)

# team 'abilities', also nur Ergebnis mit globalem HSE
result_score_home3 = getRank(mod.scorehome3, reference="GAMMA.Wolfsburg")
print(result_score_home3)
write.csv(result_score_home3, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//result_score_home3.csv')


# teamspezifischer HSE

design_score_specific3 = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//design_score_spec_3.csv')

mod.scoreteamhome3 <- ordBTL(ordresp~., data=design_score_specific3)
# team 'abilities' (should be the ranking for the away table)
result_score_specific3 = getRank(mod.scoreteamhome3, reference="GAMMA.Wolfsburg")
print(result_score_specific3)
write.csv(result_score_specific3, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//result_score_spec3.csv')
# team-specific home advantages
getRank(mod.scoreteamhome3, prefix="ALPHA")


lrtest_vglm(mod.scoreteamhome3,mod.scorehome3)


# gleiche für xG2

# globaler HSE 

design_xG2_home_3 = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//design_xG2_home_3.csv')

mod.xG2home3 = ordBTL(ordroundxG~., data=design_xG2_home_3)

result_xG2_home3 = getRank(mod.xG2home3, reference="GAMMA.Wolfsburg")
print(result_xG2_home3)
write.csv(result_xG2_home3, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//result_xG2_home3.csv')

# team spezifischer HSE

design_xG2_spec_3 = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//design_xG2_spec_3.csv')

mod.xG2teamhome3 = ordBTL(ordroundxG~., data=design_xG2_spec_3)

result_xG2_specific3 = getRank(mod.xG2teamhome3, reference="GAMMA.Wolfsburg")
print(result_xG2_specific3)
write.csv(result_xG2_specific3, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//result_xG2_spec3.csv')

lrtest_vglm(mod.xG2teamhome3,mod.xG2home3)

# für externe xG, k=3

# globaler Heimspieleffekt 

design_extxG3 = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/design_ext_xG3.csv')

mod.extxGhome3 = ordBTL(ordextxG~., data=design_extxG3)

result_extxGhome3 = getRank(mod.extxGhome3, reference="GAMMA.Wolfsburg")
print(result_extxGhome3)
write.csv(result_extxGhome3, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/result_extxGhome3.csv')

# teamspezifischer Heimspieleffekt 

design_extxGts3 = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//design_ext_xGts3.csv')

mod.extxGts3 = ordBTL(ordextxG~., data=design_extxGts3)

result_extxGts3 = getRank(mod.extxGts3, reference="GAMMA.Wolfsburg")
print(result_extxGts3)
write.csv(result_extxGts3, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//result_extxGts3.csv')

lrtest_vglm(mod.extxGts3,mod.extxGhome3)








## Validation 

# test für einzelne Stichprobe 


Z=sample(34,1)
Z
games = c()
for (i in Z){
  print(i)
  gg = c((i*9-8):(i*9))
  print(gg)
  games= c(games, gg)
}
games
training=design_score_home[-games,]
test = design_score_home[games,]


test

mod.scorehomeval = ordBTL(ordresp~., data=training)


result_score_homeval = getRank(mod.scorehomeval, reference="GAMMA.Wolfsburg")
print(result_score_homeval)


prediction = predictvglm(mod.scorehomeval, newdata=test, type="response")
prediction
preddec = apply(prediction,1,sim_erg)
preddec
sum((preddec-test$ordresp)^2)

#rps(predictvglm(mod.scorehomeval, newdata=test, type="response"), test$ordresp,5)


# Erstelle als Refernzmodell eine Funktion, die aus einem gegebenen Vektor (aus Responsekategorien)
# einem neuen Ereignis den Wert zuweist, der die höchste emprische Wahrscheinlichkeit besitzt. 



empir = function(train, test){
  pred = c()
  for (i in test){
    predi = which(table(train)==max(table(train)))
    
    if (length(predi)>1){
      predi = sample(x=predi, size=1,prob = rep(1/length(predi),length(predi)), replace="TRUE") 
    }
    #print(predi)
    
    train = c(train, i)
    #print(train)
    pred = c(pred, predi)
    #print(pred)
  }
  
  return(pred)
}


# überprüfe, ob empir als Modell einfach immer nur Heimsiege modelliert
# uU gibt eseinen Zeitpunkt in der Saison, an dem das nicht so ist
# es genügen hier die Spieltage ab Spieltag 7

stz = 7
gms = c((306-(stz*9)+1): 306) #erste 7 Spieltage 
gms

trn = design_score_home[gms,] 
#benutze dies da ordresp von design_score_home eben die Ergebnisse sind 
tst = design_score_home[-gms,]

table(trn$ordresp)
which(table(trn$ordresp)==max(table(trn$ordresp)))


empir(trn$ordresp, tst$ordresp)
table(tst$ordresp)
table(trn$ordresp)

# im Fall von k=5 also nicht; wie sieht es bei k=3 aus? 


trn3 = design_score_home_3[gms,] 

#benutze dies da ordresp von design_score_home eben die Ergebnisse sind 

tst3 = design_score_home_3[-gms,]

table(trn3$ordresp)
which(table(trn3$ordresp)==max(table(trn3$ordresp)))


empir(trn3$ordresp, tst3$ordresp)
table(tst3$ordresp)
table(trn3$ordresp)

# Im Fall von k=3 werden also nur Heimsiege projeziert bei Betrachtungen ab Spieltag 7 
# das wird später bei der Berechnung wichtig sein, da es die Berechnung zeitsparender machen kann 


# Test für den Fall, dass zwei Ereignisse gleiche Häufigkeit haben 
trnn = c(1,2,1,2,1)
tstt = c(2,1,2,1,2)
empir(trnn, tstt)

table(trnn)/length(trnn)

## Es wird für das Modell "emp" von oben auch eine Funktion benötigt, die die jeweiligen
## empirischen Wahrscheinlichkeiten für ein Ergebnis zurückgibt 


empirw = function(train, test){
  pred = c()
  for (i in test){
    predi = table(train)/length(train)
    
    train = c(train, i)

    pred = rbind(pred, predi)

  }
  
  return(pred)
}

empirw(trn3$ordresp, tst3$ordresp)


## jetzt iterieren als leave-p-out-Algoritmus mit 
## p Anzahl Spieltage 
## n Anzahl Iterationen
## design_list ist Liste der Designmatritzen wobei die Respone als "ordresp" bzw. "ordroundxG" benannt ist
## sie enthält zudem 4 Designmatritzen "sc", "scts", "xG" und "xGts" 
## k Anzahl der Responsekategorien

# Bemerke: Die letzten 9 Zeilen der Designmatrix beschreiben den ersten Spieltag, 
# die vorletzten 9 den den zweiten Spieltag etc. 
# Dadurch Umparametrisieren 

  lpo = function(p,n, design_list,k){
  res_sc = c()
  res_xG = c()
  res_extxG = c()
  res_guess = c()
  res_emp = c()
  
  # Trefferwahrscheinlichkeiten
  tw_sc = c()
  tw_xG = c()
  tw_extxG = c()
  tw_guess = c()
  tw_emp = c()
  
  for (j in 1:n){
    
    Z=sample(34,p)
  
    games = c()
    for (i in Z){
      gg = c((306-(i*9)+1): (306-((i-1)*9)))
      games= c(games, gg)
    }
    # train test split for normal score with global home 
    
    train_sc= (design_list$"sc")[-games,]
    test_sc = (design_list$"sc")[games,]
    
    train_xG = (design_list$"xG")[-games,]
    test_xG = (design_list$"xG")[games,]
    
    train_extxG = (design_list$"extxG")[-games,]
    test_extxG = (design_list$"extxG")[games,]
    
    mod.scorehomeval = ordBTL(ordresp~., data=train_sc)
    mod.xGhomeval = ordBTL(ordroundxG~., data=train_xG)
    mod.extxGhomeval = ordBTL(ordextxG~., data=train_extxG)
    
  
    
    pred_sc = predictvglm(mod.scorehomeval, newdata=test_sc, type="response")
    pred_xG = predictvglm(mod.xGhomeval, newdata=test_xG, type="response")
    pred_extxG = predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response")
    
    #prediction wenn geraten 
    pred_guess = sample(k,p*9, replace = TRUE)
    
    #prediction bei empi 
    pred_emp = empir(train_sc$ordresp, test_sc$ordresp)
    
  
    preddec_sc = apply(pred_sc,1,which.max)
    preddec_xG = apply(pred_xG,1,which.max)
    preddec_extxG = apply(pred_extxG,1,which.max)
    
    #response wenn geraten 
    
    res_guess = c(res_guess, sum((pred_guess-test_sc$ordresp)^2))
    
    # selbe für emp 
    
    res_emp = c(res_emp, sum((pred_emp - test_sc$ordresp)^2))
    
    # response vom Rest 
    
    res_sc = c(res_sc,sum((preddec_sc-test_sc$ordresp)^2))
    res_xG = c(res_xG,sum((preddec_xG-test_sc$ordresp)^2))
    res_extxG = c(res_extxG,sum((preddec_extxG-test_sc$ordresp)^2))
    
    # Trefferwahrscheinlichkeiten für eine Saison 
    
    tw_sc = c(tw_sc, sum(preddec_sc == test_sc$ordresp))
    tw_xG = c(tw_xG, sum(preddec_xG == test_sc$ordresp))
    tw_extxG = c(tw_extxG, sum(preddec_extxG == test_sc$ordresp))
    tw_guess = c(tw_guess, sum(pred_guess == test_sc$ordresp))
    tw_emp = c(tw_emp, sum(pred_emp == test_sc$ordresp))
    
    
  }
  return_list = list("tw_sc" = tw_sc, "tw_xG" = tw_xG, "tw_extxG" = tw_extxG, "tw_guess" = tw_guess, "tw_emp" = tw_emp, "res_sc" = res_sc, "res_xG" = res_xG, "res_extxG" = res_extxG, "res_guess" = res_guess, "res_emp" = res_emp)
  return(return_list)
  }
  
# check results mit list(res_sc, res_scts, res_xG, res_xGts)
design_list = list("sc" = design_score_home, "xG" = design_xG2_home, "extxG" = design_extxG)

result_list = lpo(7,10000,design_list, 5)

res_sc_lpo = result_list$"res_sc"
res_xG_lpo = result_list$"res_xG"
res_extxG_lpo = result_list$"res_extxG"
res_guess = result_list$"res_guess"
res_emp = result_list$"res_emp"


mean(result_list$res_sc)
median(result_list$res_sc)
quantile(result_list$res_sc, 0.25)
quantile(result_list$res_sc, 0.75)

mean(result_list$res_xG)
median(result_list$res_xG)
quantile(result_list$res_xG, 0.25)
quantile(result_list$res_xG, 0.75)

mean(result_list$res_extxG)
median(result_list$res_extxG)
quantile(result_list$res_extxG, 0.25)
quantile(result_list$res_extxG, 0.75)

mean(result_list$res_emp)
median(result_list$res_emp)
quantile(result_list$res_emp, 0.25)
quantile(result_list$res_emp, 0.75)

mean(result_list$res_guess)
median(result_list$res_guess)
quantile(result_list$res_guess, 0.25)
quantile(result_list$res_guess, 0.75)

#selbe für trefferwahrscheinlichkeiten 

mean(result_list$tw_sc)
median(result_list$tw_sc)
quantile(result_list$tw_sc, 0.25)
quantile(result_list$tw_sc, 0.75)

mean(result_list$tw_xG)
median(result_list$tw_xG)
quantile(result_list$tw_xG, 0.25)
quantile(result_list$tw_xG, 0.75)

mean(result_list$tw_extxG)
median(result_list$tw_extxG)
quantile(result_list$tw_extxG, 0.25)
quantile(result_list$tw_extxG, 0.75)

mean(result_list$tw_emp)
median(result_list$tw_emp)
quantile(result_list$tw_emp, 0.25)
quantile(result_list$tw_emp, 0.75)

mean(result_list$tw_guess)
median(result_list$tw_guess)
quantile(result_list$tw_guess, 0.25)
quantile(result_list$tw_guess, 0.75)



#plotting 
library(ggplot2)
library(tidyr)

result = list("Sc" = (result_list$res_sc), "xG" = (result_list$res_xG),"extxG" = (result_list$res_extxG), "guess" = (result_list$res_guess), "emp" = (result_list$res_emp))
result_cent = list("cSc" = (result_list$res_sc) - mean_ress_sc, "cxG" = (result_list$res_xG) - mean_ress_xG, "cextxG" = (result_list$res_extxG) - mean_ress_extxG,"cguess" = (result_list$res_guess) - mean_ress_guess, "cemp" = (result_list$res_emp) - mean_ress_emp)

df_cent <- data.frame(result_cent) %>% 
  gather(key, value) 

df <- data.frame(result) %>% 
  gather(key, value)
df

plot = ggplot(df, aes(value, colour = key)) +
  geom_density() +
  theme_minimal() +

  labs(title = "Modellvergleich durch ResSS mit k=5 Responsemöglichkeiten", x = "Residuenquadratsumme", y = "relative Häufigkeit", fill = "Modell") +

  scale_color_manual(name = "Modell", values = c("Sc" = "red", "xG" = "blue", "extxG" = "green", "guess" = "yellow", "emp" = "black"))

plot 

ggsave(
  "Model_comparison_ResSS_5.png",
  plot,
  width = 6.5,
  height = 3.25,
  dpi = 1200,
  bg = "white"
)


# boxplot 

png("resSSbox.png", width = 600, height = 450)
boxplot(data.frame(result), main="ResSS verschiedener Modelle für k=5")
points(sapply(data.frame(result), mean), col="red", pch=4)
dev.off()

## das gleiche für k=3

design_list3 = list("sc" = design_score_home_3, "xG" = design_xG2_home_3,"extxG" = design_extxG3)


result_list3 = lpo(7,10000,design_list3, 3)

res_sc_lpo3 = result_list3$"res_sc"
res_xG_lpo3 = result_list3$"res_xG"
res_extxG_lpo3 = result_list3$"res_extxG"
res_guess3 = result_list3$"res_guess"
res_emp3 = result_list3$"res_emp"


mean(result_list3$res_sc)
median(result_list3$res_sc)
quantile(result_list3$res_sc, 0.25)
quantile(result_list3$res_sc, 0.75)

mean(result_list3$res_xG)
median(result_list3$res_xG)
quantile(result_list3$res_xG, 0.25)
quantile(result_list3$res_xG, 0.75)

mean(result_list3$res_extxG)
median(result_list3$res_extxG)
quantile(result_list3$res_extxG, 0.25)
quantile(result_list3$res_extxG, 0.75)

mean(result_list3$res_emp)
median(result_list3$res_emp)
quantile(result_list3$res_emp, 0.25)
quantile(result_list3$res_emp, 0.75)

mean(result_list3$res_guess)
median(result_list3$res_guess)
quantile(result_list3$res_guess, 0.25)
quantile(result_list3$res_guess, 0.75)


#selbe für Trefferwahrscheinlichkeiten 

mean(result_list3$tw_sc)
median(result_list3$tw_sc)
quantile(result_list3$tw_sc, 0.25)
quantile(result_list3$tw_sc, 0.75)

mean(result_list3$tw_xG)
median(result_list3$tw_xG)
quantile(result_list3$tw_xG, 0.25)
quantile(result_list3$tw_xG, 0.75)

mean(result_list3$tw_extxG)
median(result_list3$tw_extxG)
quantile(result_list3$tw_extxG, 0.25)
quantile(result_list3$tw_extxG, 0.75)

mean(result_list3$tw_emp)
median(result_list3$tw_emp)
quantile(result_list3$tw_emp, 0.25)
quantile(result_list3$tw_emp, 0.75)

mean(result_list3$tw_guess)
median(result_list3$tw_guess)
quantile(result_list3$tw_guess, 0.25)
quantile(result_list3$tw_guess, 0.75)

#plotting 

result3 = list("Sc" = (result_list3$res_sc), "xG" = (result_list3$res_xG), "extxG" = (result_list3$res_extxG), "guess" = (result_list3$res_guess), "emp" = (result_list3$res_emp))
result_cent3 = list("cSc" = (result_list3$res_sc) - mean_ress_sc3, "cxG" = (result_list3$res_xG) - mean_ress_xG3, "cextxG" = (result_list3$res_extxG) - mean_ress_extxG3, "cguess" = (result_list3$res_guess) - mean_ress_guess3, "cemp" = (result_list3$res_emp) - mean_ress_emp3)

df_cent3 <- data.frame(result_cent3) %>% 
  gather(key, value) 

df3 <- data.frame(result3) %>% 
  gather(key, value)
df3

plot = ggplot(df3, aes(value, colour = key)) +
  geom_density() +
  theme_minimal() +
  labs(title = "Modellvergleich durch ResSS mit k=3 Responsemöglichkeiten", x = "Residuenquadratsumme", y = "relative Häufigkeit", fill = "Modell") +

  scale_color_manual(name = "Modell", values = c("Sc" = "red", "xG" = "blue", "extxG" = "green", "guess" = "yellow", "emp" = "black"))

plot 

ggsave(
  "Model_comparison_ResSS_3.png",
  plot,
  width = 6.5,
  height = 3.25,
  dpi = 1200,
  bg = "white"
)

# boxplot 

png("resSS3box.png", width = 600, height = 450)
boxplot(data.frame(result3), main="ResSS verschiedener Modelle für k=3")
points(sapply(data.frame(result3), mean), col="red", pch=4)
dev.off()


# Rank probability score 

rps = function(pred, obs, k){
  npred= dim(pred)[1]

  RPS = rep(0,npred)
  
  for (x in (1:npred)){
    obsvec = rep(0,k)
    obsvec[obs[x]] = 1
    cum = 0
    for (j in (1:(k-1))){
    cum = cum + (sum((pred[x,1:j] - obsvec[1:j])))^2
    }
    RPS[x] = cum/(k-1)
  }
return(RPS)
}

#Teste RPS

rps(diag(3),c(1,1,1),3)


# iterieren als Funktion lpo mit rps als score-Funktion 
# Variablen wie oben 

lpo_rps = function(p, n, design_list, k){
  rps_sc = c()
  rps_xG = c()
  rps_extxG = c()
  rps_emp = c()
  
  for (j in 1:n){
    
    Z=sample(34,p)
    
    games = c()
    for (i in Z){
      gg = c((306-(i*9)+1): (306-((i-1)*9)))
      games= c(games, gg)
    }
    # train test split for normal score with global home 
    

    train_sc= (design_list$"sc")[-games,]
    test_sc = (design_list$"sc")[games,]
    
    train_xG = (design_list$"xG")[-games,]
    test_xG = (design_list$"xG")[games,]
    
    train_extxG = (design_list$"extxG")[-games,]
    test_extxG = (design_list$"extxG")[games,]
    
    
    
    mod.scorehomeval = ordBTL(ordresp~., data=train_sc)
    mod.xGhomeval = ordBTL(ordroundxG~., data=train_xG)
    mod.extxGhomeval = ordBTL(ordextxG~., data=train_extxG)
    
    
    
    pred_sc = predictvglm(mod.scorehomeval, newdata=test_sc, type="response")
    pred_xG = predictvglm(mod.xGhomeval, newdata=test_xG, type="response")
    pred_extxG = predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response")
    pred_emp = empirw(train_sc$ordresp, test_sc$ordresp)
    
    rps_sc_n = rps(pred_sc, test_sc$ordresp, k)
    rps_xG_n = rps(pred_xG, test_sc$ordresp, k)
    rps_extxG_n = rps(pred_extxG, test_sc$ordresp, k)
    rps_emp_n = rps(pred_emp, test_sc$ordresp, k)
    
    rps_sc = c(rps_sc, rps_sc_n)
    rps_xG = c(rps_xG, rps_xG_n)
    rps_extxG = c(rps_extxG, rps_extxG_n)
    rps_emp = c(rps_emp, rps_emp_n)
    
  }
  return_list = list("rps_sc" = rps_sc, "rps_xG" = rps_xG, "rps_extxG" = rps_extxG, "rps_emp" = rps_emp)
  return(return_list)
}


# für k=5

result_rps = lpo_rps(7,10000, design_list, 5)
result_rps$rps_emp

mean(result_rps$rps_sc)
median(result_rps$rps_sc)
quantile(result_rps$rps_sc, 0.25)
quantile(result_rps$rps_sc, 0.75)

mean(result_rps$rps_xG)
median(result_rps$rps_xG)
quantile(result_rps$rps_xG, 0.25)
quantile(result_rps$rps_xG, 0.75)

mean(result_rps$rps_extxG)
median(result_rps$rps_extxG)
quantile(result_rps$rps_extxG, 0.25)
quantile(result_rps$rps_extxG, 0.75)

mean(result_rps$rps_emp)
median(result_rps$rps_emp)
quantile(result_rps$rps_emp, 0.25)
quantile(result_rps$rps_emp, 0.75)
# Plotting 

df_rps = data.frame("Sc" = result_rps$rps_sc, "xG" = result_rps$rps_xG, "extxG" = result_rps$rps_extxG, "emp" = result_rps$rps_emp)

png("l7orps.png", width = 450, height = 450)
boxplot(df_rps, main="RPS verschiedener Modelle bei L7o-CV mit k=5")
points(sapply(df_rps, mean), col="red", pch=4)
dev.off()



#für k=3 

result_rps3 = lpo_rps(7,1000, design_list3, 3)
result_rps3$rps_emp

mean(result_rps3$rps_sc)
median(result_rps3$rps_sc)
quantile(result_rps3$rps_sc, 0.25)
quantile(result_rps3$rps_sc, 0.75)

mean(result_rps3$rps_xG)
median(result_rps3$rps_xG)
quantile(result_rps3$rps_xG, 0.25)
quantile(result_rps3$rps_xG, 0.75)

mean(result_rps3$rps_extxG)
median(result_rps3$rps_extxG)
quantile(result_rps3$rps_extxG, 0.25)
quantile(result_rps3$rps_extxG, 0.75)

mean(result_rps3$rps_emp)
median(result_rps3$rps_emp)
quantile(result_rps3$rps_emp, 0.25)
quantile(result_rps3$rps_emp, 0.75)

# Plotting 

df_rps3 = data.frame("Sc" = result_rps3$rps_sc, "xG" = result_rps3$rps_xG, "extxG" = result_rps3$rps_extxG, "emp" = result_rps3$rps_emp)
# df_rps3 = data.frame("rps_sc" = result_rps3$rps_sc, "rps_scts" = result_rps3$rps_scts, "rps_xG" = result_rps3$rps_xG, "rps_xGts" = result_rps3$rps_xGts)
# boxplot(df_rps3)

png("l7orps3.png", width = 450, height = 450)
boxplot(df_rps3, main="RPS verschiedener Modelle bei L7o-CV mit k=3")
points(sapply(df_rps3, mean), col="red", pch=4)
dev.off()



#Test
staart = 2
gaaames = c((306-(staart*9)+1): (306-((staart-1)*9)))
gaaames
design_score_home[(306-(staart*9)+1):306,]
((306-((staart-1)*9)+1):306)

## zur Prüfung nochmal Modell ohne zeitlichen Verlauf aber keine Residuenquadrate, 
# sondern die Absolutbeträge der Residuen addieren 


lpoabs = function(p,n, design_list,k){
  res_sc = c()
  res_xG = c()
  res_extxG = c()
  res_guess = c()
  res_emp = c()
  
  
  for (j in 1:n){
    
    Z=sample(34,p)
    
    games = c()
    for (i in Z){
      gg = c((306-(i*9)+1): (306-((i-1)*9)))
      games= c(games, gg)
    }
    # train test split for normal score with global home 
    
    train_sc= (design_list$"sc")[-games,]
    test_sc = (design_list$"sc")[games,]
    
    
    train_xG = (design_list$"xG")[-games,]
    test_xG = (design_list$"xG")[games,]
    
    train_extxG = (design_list$"extxG")[-games,]
    test_extxG = (design_list$"extxG")[games,]
    
    
    mod.scorehomeval = ordBTL(ordresp~., data=train_sc)
    mod.xGhomeval = ordBTL(ordroundxG~., data=train_xG)
    mod.extxGhomeval = ordBTL(ordextxG~., data=train_extxG)

    
    
    
    pred_sc = predictvglm(mod.scorehomeval, newdata=test_sc, type="response")
    pred_xG = predictvglm(mod.xGhomeval, newdata=test_xG, type="response")
    pred_extxG = predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response")

    
    #prediction wenn geraten 
    pred_guess = sample(k,p*9, replace = TRUE)
    
    #prediction bei empi 
    pred_emp = empir(train_sc$ordresp, test_sc$ordresp)
    
    
    preddec_sc = apply(pred_sc,1,which.max)
    preddec_xG = apply(pred_xG,1,which.max)
    preddec_extxG = apply(pred_extxG,1,which.max)
    
    #response wenn geraten 
    
    res_guess = c(res_guess, sum(abs(pred_guess-test_sc$ordresp)))
    
    # selbe für emp 
    
    res_emp = c(res_emp, sum(abs(pred_emp - test_sc$ordresp)))
    
    # response vom Rest 
    
    res_sc = c(res_sc,sum(abs(preddec_sc-test_sc$ordresp)))
    res_xG = c(res_xG,sum(abs(preddec_xG-test_sc$ordresp)))
    res_extxG = c(res_extxG,sum(abs(preddec_extxG-test_sc$ordresp)))
    
    
  }
  return_list = list("res_sc" = res_sc, "res_xG" = res_xG, "res_extxG" = res_extxG, "res_guess" = res_guess, "res_emp" = res_emp)
  return(return_list)
}


result_abs = lpoabs(7,10000,design_list3, 5)

mean(result_abs$res_sc)
median(result_abs$res_sc)
quantile(result_abs$res_sc, 0.25)
quantile(result_abs$res_sc, 0.75)

mean(result_abs$res_xG)
median(result_abs$res_xG)
quantile(result_abs$res_xG, 0.25)
quantile(result_abs$res_xG, 0.75)

mean(result_abs$res_extxG)
median(result_abs$res_extxG)
quantile(result_abs$res_extxG, 0.25)
quantile(result_abs$res_extxG, 0.75)

mean(result_abs$res_emp)
median(result_abs$res_emp)
quantile(result_abs$res_emp, 0.25)
quantile(result_abs$res_emp, 0.75)

mean(result_abs$res_gues)
median(result_abs$res_gues)
quantile(result_abs$res_gues, 0.25)
quantile(result_abs$res_gues, 0.75)






result_abs3 = lpoabs(7,10000,design_list3, 3)

mean(result_abs3$res_sc)
median(result_abs3$res_sc)
quantile(result_abs3$res_sc, 0.25)
quantile(result_abs3$res_sc, 0.75)

mean(result_abs3$res_xG)
median(result_abs3$res_xG)
quantile(result_abs3$res_xG, 0.25)
quantile(result_abs3$res_xG, 0.75)

mean(result_abs3$res_extxG)
median(result_abs3$res_extxG)
quantile(result_abs3$res_extxG, 0.25)
quantile(result_abs3$res_extxG, 0.75)

mean(result_abs3$res_emp)
median(result_abs3$res_emp)
quantile(result_abs3$res_emp, 0.25)
quantile(result_abs3$res_emp, 0.75)

mean(result_abs3$res_gues)
median(result_abs3$res_gues)
quantile(result_abs3$res_gues, 0.25)
quantile(result_abs3$res_gues, 0.75)






## rps für die Modelle mit zeitlichem Verlauf 

# Sage alle Spieltage ab Spieltag "start" vorher 
# Trainiere zuerst auf allen Spieltagen bis start
# dann zur Vorhersage von start+1 auf allen Spieltagen bis und inklusive start etc. 
# es gilt also start>=2

# design_list und k wie oben 

# Man beachte erneut die Umparametrisierung der Spieltage 


rps_time = function(start, design_list, k){
  rps_sc = c()
  rps_xG = c()
  rps_extxG = c()
  rps_emp = c()
  for (i in (start:34)){
    #print(i)
    test_set = c((306-(i*9)+1): (306-((i-1)*9)))
    train_set = c((306-((i-1)*9)+1):306)
    
    train_sc= (design_list$"sc")[train_set,]
    test_sc = (design_list$"sc")[test_set,]
    #print(test_sc)
    
    train_xG = (design_list$"xG")[train_set,]
    test_xG = (design_list$"xG")[test_set,]
    
    train_extxG = (design_list$"extxG")[train_set,]
    test_extxG = (design_list$"extxG")[test_set,]
    
    mod.scorehomeval = ordBTL(ordresp~., data=train_sc)
    mod.xGhomeval = ordBTL(ordroundxG~., data=train_xG)
    mod.extxGhomeval = ordBTL(ordextxG~., data=train_extxG)
    
    
    pred_sc = predictvglm(mod.scorehomeval, newdata=test_sc, type="response")
    pred_xG = predictvglm(mod.xGhomeval, newdata=test_xG, type="response")
    pred_extxG = predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response")
    pred_emp = empirw(train_sc$ordresp, test_sc$ordresp)
    
    rps_sc_n = rps(pred_sc, test_sc$ordresp, k)
    rps_xG_n = rps(pred_xG, test_sc$ordresp, k)
    rps_extxG_n = rps(pred_extxG, test_sc$ordresp, k)
    rps_emp_n = rps(pred_emp, test_sc$ordresp, k)
    
    
    rps_sc = c(rps_sc, rps_sc_n)
    rps_xG = c(rps_xG, rps_xG_n)
    rps_extxG = c(rps_extxG, rps_extxG_n)
    rps_emp = c(rps_emp, rps_emp_n)
  }
  return_list = list("rps_sc" = rps_sc, "rps_xG" = rps_xG, "rps_extxG" = rps_extxG, "rps_emp" = rps_emp)
  return(return_list)
}

result_timerps = rps_time(7, design_list, 5)
result_timerps


mean(result_timerps$rps_sc)
mean(result_timerps$rps_xG)
mean(result_timerps$rps_extxG)
mean(result_timerps$rps_emp)

# Plotting 

df_rpstime = data.frame("Sc" = result_timerps$rps_sc, "xG" = result_timerps$rps_xG, "extxG" = result_timerps$rps_extxG, "emp" = result_timerps$rps_emp)
boxplot(df_rpstime)


png("rpstime.png", width = 450, height = 450)
boxplot(df_rpstime, main="RPS verschiedener Modelle für k=5")
points(sapply(df_rpstime, mean), col="red", pch=4)
dev.off()


# für k=3 

result_timerps3 = rps_time(7, design_list3, 3)
result_timerps3


mean(result_timerps3$rps_sc)
mean(result_timerps3$rps_xG)
mean(result_timerps3$rps_extxG)
mean(result_timerps3$rps_emp)

# Plotting 
result_timerps3
df_rpstime3 = data.frame("Sc" = result_timerps3$rps_sc, "xG" = result_timerps3$rps_xG, "extxG" = result_timerps3$rps_extxG, "emp" = result_timerps3$rps_emp)


png("rpstime3.png", width = 450, height = 450)
boxplot(df_rpstime3, main="RPS verschiedener Modelle für k=3")
points(sapply(df_rpstime3, mean), col="red", pch=4)
dev.off()


#Simuliere eine Saison ab Spieltag start = 7
# Iterieren auf 10.000 Durchläufe 
# nach jedem Spieltag Modellupdate
# Angabe wie viel Prozent der Spieltage richtig vorhergesagt 

#Funktion, um bei gegebenen Wahrscheinlichkeiten Ergebnis zu simulieren
erg_sim5 = function(x){
  erg = sample(x=c(1:5), size=1,prob = x, replace="TRUE")
  return(erg)
}

erg_sim3 = function(x){
  erg = sample(x=c(1:3), size=1,prob = x, replace="TRUE")
  return(erg)
}

saisonsim = function(start, n, design_list, k){
  ar_sc = c()
  ar_xG = c()
  ar_extxG = c()
  ar_emp = c()
  for (j in 1:n){
    ir_sc = c()
    ir_xG = c()
    ir_extxG = c()
    ir_emp = c()
    for (i in (start:34)){
      test_set = c((306-(i*9)+1): (306-((i-1)*9)))
      train_set = c((306-((i-1)*9)+1):306)
      
      train_sc= (design_list$"sc")[train_set,]
      test_sc = (design_list$"sc")[test_set,]
      
      train_xG = (design_list$"xG")[train_set,]
      test_xG = (design_list$"xG")[test_set,]
      
      train_extxG = (design_list$"extxG")[train_set,]
      test_extxG = (design_list$"extxG")[test_set,]

      
      mod.scorehomeval = ordBTL(ordresp~., data=train_sc)
      mod.xGhomeval = ordBTL(ordroundxG~., data=train_xG)
      mod.extxGhomeval = ordBTL(ordextxG~., data=train_extxG)
      if (k==3){
        pred_sc = apply(predictvglm(mod.scorehomeval, newdata=test_sc, type="response"),1,erg_sim3)
        pred_xG = apply(predictvglm(mod.xGhomeval, newdata=test_xG, type="response"),1,erg_sim3)
        pred_extxG = apply(predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response"),1,erg_sim3)
        pred_emp = apply(empirw(train_sc$ordresp, test_sc$ordresp),1,erg_sim3)
      } else {
        pred_sc = apply(predictvglm(mod.scorehomeval, newdata=test_sc, type="response"),1,erg_sim5)
        pred_xG = apply(predictvglm(mod.xGhomeval, newdata=test_xG, type="response"),1,erg_sim5)
        pred_extxG = apply(predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response"),1,erg_sim5)
        pred_emp = apply(empirw(train_sc$ordresp, test_sc$ordresp),1,erg_sim5)
      }
      
      ir_sc = c(ir_sc, (pred_sc == test_sc$ordresp))
      ir_xG = c(ir_xG, (pred_xG == test_sc$ordresp))
      ir_extxG = c(ir_extxG, (pred_extxG == test_sc$ordresp))
      ir_emp = c(ir_emp, (pred_emp == test_sc$ordresp))

    }
    
    ar_sc = c(ar_sc, mean(ir_sc))
    ar_xG = c(ar_xG, mean(ir_xG))
    ar_extxG = c(ar_extxG, mean(ir_extxG)) 
    ar_emp = c(ar_emp, mean(ir_emp))
    
    
  }
  return_list = list("ar_sc" = ar_sc, "ar_xG" = ar_xG, "ar_extxG" = ar_extxG, "ar_emp" = ar_emp)
  return(return_list) 
}


return_sim = saisonsim(7,10000,design_list3,3)
write.csv(return_sim, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//sim10000.csv')
#warnings()

df_sim = data.frame("Sc" = return_sim$ar_sc, "xG" = return_sim$ar_xG, "extxG" = return_sim$ar_extxG, "emp" = return_sim$ar_emp)
sapply(df_sim, max)
sapply(df_sim, min)
sapply(df_sim, mean)
sapply(df_sim, quantile(0.05))



# wende die Funktionen max, min, mean sowie 
# die beiden 0.05 und 0.95-Quantile auf den df an 
apply_sim <- function(x) {
  result <- c(
    max = max(x),
    min = min(x),
    mean = mean(x),
    sim5 = quantile(x,0.05),
    sim95 = quantile(x,0.95)
  )
  return(result)
}


lapply(df_sim,apply_sim)


png("sim10000.png", width = 650, height = 450)
boxplot(df_sim, main="Anteil korrekt vorhergesagter Spiele pro Saison verschiedener Modelle für k=3")
points(sapply(df_sim, mean), col="red", pch=4)
dev.off()






# jetzt mit Entscheidung für die höchste Wahrscheinlichkeit 



saisonsimw = function(start, design_list, k){
  ar_sc = c()
  ar_xG = c()
  ar_extxG = c()
  ar_emp = c()

  for (i in (start:34)){
    test_set = c((306-(i*9)+1): (306-((i-1)*9)))
    train_set = c((306-((i-1)*9)+1):306)
    
    train_sc= (design_list$"sc")[train_set,]
    test_sc = (design_list$"sc")[test_set,]
    
    train_xG = (design_list$"xG")[train_set,]
    test_xG = (design_list$"xG")[test_set,]
    
    train_extxG = (design_list$"extxG")[train_set,]
    test_extxG = (design_list$"extxG")[test_set,]
    
    mod.scorehomeval = ordBTL(ordresp~., data=train_sc)
    mod.xGhomeval = ordBTL(ordroundxG~., data=train_xG)
    mod.extxGhomeval = ordBTL(ordextxG~., data=train_extxG)
    

    
    pred_sc = apply(predictvglm(mod.scorehomeval, newdata=test_sc, type="response"),1,which.max)
    pred_xG = apply(predictvglm(mod.xGhomeval, newdata=test_xG, type="response"),1,which.max)
    pred_extxG = apply(predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response"),1,which.max)
    # empir wieder wie bekannt wählt das Ergebnis, das am häufigsten aufgetreten 
    pred_emp = empir(train_sc$ordresp, test_sc$ordresp)
    
    
    ar_sc = c(ar_sc, (pred_sc == test_sc$ordresp))
    ar_xG = c(ar_xG, (pred_xG == test_sc$ordresp))
    ar_extxG = c(ar_extxG, (pred_extxG == test_sc$ordresp))
    ar_emp = c(ar_emp, (pred_emp == test_sc$ordresp))
    
  }
  
    

  return_list = list("ar_sc" = ar_sc, "ar_xG" = ar_xG, "ar_extxG" = ar_extxG, "ar_emp" = ar_emp)
  return(return_list) 
}


return_simw = saisonsimw(7,design_list3,3)
write.csv(return_simw, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften//simw.csv')


df_simw = data.frame("SC" = return_simw$ar_sc, "xG" = return_simw$ar_xG, "extxG" = return_simw$ar_extxG, "emp" = return_simw$ar_emp)
sapply(df_simw, max)
sapply(df_simw, min)
sapply(df_simw, mean)
sapply(df_simw, sum)




# Boxplot ungeeignet 

png("simw.png", width = 650, height = 450)
boxplot(df_simw, main="Anteil korrekt vorhergesagter Spiele pro Saison verschiedener Modelle für k=3")
points(sapply(df_simw, mean), col="red", pch=4)
dev.off()



# Simulationsstudie kombiniert mit Wettquoten
# Quoten von betfair 
# -1€ bei falschem Tipp, +(Quote-1)€ bei richtigem Tipp 

betqt = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/sorted_odds.csv')
#design_score_home_3$ordresp
betqt[,4]-1


## Simuliere eine Saison 10.000-mal mit
# oddsresp der Quote von betfair für das eingetretene Ergebnis

saisonsimbet = function(start, n, design_list, k, oddsresp){
  ar_sc = c()
  ar_xG = c()
  ar_extxG = c()
  ar_emp = c()
  for (j in 1:n){
    ir_sc = c()
    ir_xG = c()
    ir_extxG = c()
    ir_emp = c()
    for (i in (start:34)){
      test_set = c((306-(i*9)+1): (306-((i-1)*9)))
      train_set = c((306-((i-1)*9)+1):306)
      
      # setze die Quoten auf Testset fest für Validierung/Gewinne und Verluste
      
      oddstest = oddsresp[(306-(i*9)+1): (306-((i-1)*9))]
      
      train_sc= (design_list$"sc")[train_set,]
      test_sc = (design_list$"sc")[test_set,]
      
      train_xG = (design_list$"xG")[train_set,]
      test_xG = (design_list$"xG")[test_set,]
      
      train_extxG = (design_list$"extxG")[train_set,]
      test_extxG = (design_list$"extxG")[test_set,]
      
      mod.scorehomeval = ordBTL(ordresp~., data=train_sc)
      mod.xGhomeval = ordBTL(ordroundxG~., data=train_xG)
      mod.extxGhomeval = ordBTL(ordextxG~., data=train_extxG)
      
      if (k==3){
        pred_sc = apply(predictvglm(mod.scorehomeval, newdata=test_sc, type="response"),1,erg_sim3)
        pred_xG = apply(predictvglm(mod.xGhomeval, newdata=test_xG, type="response"),1,erg_sim3)
        pred_extxG = apply(predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response"),1,erg_sim3)
        pred_emp = apply(empirw(train_sc$ordresp, test_sc$ordresp),1,erg_sim3)
      } else {
        pred_sc = apply(predictvglm(mod.scorehomeval, newdata=test_sc, type="response"),1,erg_sim5)
        pred_xG = apply(predictvglm(mod.xGhomeval, newdata=test_xG, type="response"),1,erg_sim5)
        pred_extxG = apply(predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response"),1,erg_sim5)
        pred_emp = apply(empirw(train_sc$ordresp, test_sc$ordresp),1,erg_sim5)
      }
      

      out_sc = (pred_sc == test_sc$ordresp)*(oddstest) -1
      out_xG = (pred_xG == test_sc$ordresp)*(oddstest) -1
      out_extxG = (pred_extxG == test_sc$ordresp)*(oddstest) -1
      out_emp = (pred_emp == test_sc$ordresp)*(oddstest) -1

      
      ir_sc = c(ir_sc, out_sc)
      ir_xG = c(ir_xG, out_xG)
      ir_extxG = c(ir_extxG, out_extxG)
      ir_emp = c(ir_emp, out_emp)

    }

    ar_sc = c(ar_sc, sum(ir_sc))
    ar_xG = c(ar_xG, sum(ir_xG))
    ar_extxG = c(ar_extxG, sum(ir_extxG))
    ar_emp = c(ar_emp, sum(ir_emp))
    
    
  }
  return_list = list("ar_sc" = ar_sc, "ar_xG" = ar_xG, "ar_extxG" = ar_extxG, "ar_emp" = ar_emp)
  return(return_list) 
}


return_simbet = saisonsimbet(7,10000,design_list3,3, betqt[,4])
return_simbet

write.csv(return_simbet, '/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/simbet10000.csv')

return_simbet_fin = read.csv('/Users/colinkrag/Library/Mobile Documents/com~apple~CloudDocs/Masterarbeit/Modellierung_Spielstärke_Fußballmannschaften/simbet10000.csv')
df_simbet = data.frame("Sc" = return_simbet_fin$ar_sc, "xG" = return_simbet_fin$ar_xG, "extxG" = return_simbet_fin$ar_extxG, "emp" = return_simbet_fin$ar_emp)
df_simbet = df_simbet
sapply(df_simbet, max)
sapply(df_simbet, min)
sapply(df_simbet, mean)

png("simbet.png", width = 650, height = 450)
boxplot(df_simbet, main="Gewinne pro Saison verschiedener Modelle für k=3")
points(sapply(df_simw, mean), col="red", pch=4)
dev.off()

lapply(return_simbet, apply_sim)



## Wette mit höchster Wahrscheinlichkeit 

# genügt wieder "nur" die eingetretene Quote zum Vergleich zu nehmen 


saisonsimbetmax = function(start, n, design_list, k, oddsresp){
  ar_sc = c()
  ar_xG = c()
  ar_extxG = c()
  ar_emp = c()
  for (i in (start:34)){

    test_set = c((306-(i*9)+1): (306-((i-1)*9)))
    train_set = c((306-((i-1)*9)+1):306)
    
    # setze die Quoten auf Testset fest für Validierung/Gewinne und Verluste
    
    oddstest = oddsresp[(306-(i*9)+1): (306-((i-1)*9))]
    
    train_sc= (design_list$"sc")[train_set,]
    test_sc = (design_list$"sc")[test_set,]

    
    train_xG = (design_list$"xG")[train_set,]
    test_xG = (design_list$"xG")[test_set,]
    
    train_extxG = (design_list$"extxG")[train_set,]
    test_extxG = (design_list$"extxG")[test_set,]
    
    mod.scorehomeval = ordBTL(ordresp~., data=train_sc)
    mod.xGhomeval = ordBTL(ordroundxG~., data=train_xG)
    mod.extxGhomeval = ordBTL(ordextxG~., data=train_extxG)
    
    pred_sc = apply(predictvglm(mod.scorehomeval, newdata=test_sc, type="response"),1,which.max)
    pred_xG = apply(predictvglm(mod.xGhomeval, newdata=test_xG, type="response"),1,which.max)
    pred_extxG = apply(predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response"),1,which.max)
    # empir wieder wie bekannt wählt das Ergebnis, das am häufigsten aufgetreten 
    pred_emp = empir(train_sc$ordresp, test_sc$ordresp)

    
    out_sc = (pred_sc == test_sc$ordresp)*(oddstest) -1
    out_xG = (pred_xG == test_sc$ordresp)*(oddstest) -1
    out_extxG = (pred_extxG == test_sc$ordresp)*(oddstest) -1
    out_emp = (pred_emp == test_sc$ordresp)*(oddstest) -1

     
    ar_sc = c(ar_sc, out_sc)
    ar_xG = c(ar_xG, out_xG)
    ar_extxG = c(ar_extxG, out_extxG)
    ar_emp = c(ar_emp, out_emp)
    
  }
  return_list = list("ar_sc" = ar_sc, "ar_xG" = ar_xG, "ar_extxG" = ar_extxG, "ar_emp" = ar_emp)
  return(return_list) 
}




return_simbetmax = saisonsimbetmax(7,1,design_list3,3, betqt[,4])
return_simbetmax


df_simbetmax = data.frame("Sc" = return_simbetmax$ar_sc, "xG" = return_simbetmax$ar_xG, "extxG" = return_simbetmax$ar_extxG, "emp" = return_simbetmax$ar_emp)

sapply(df_simbetmax, max)
sapply(df_simbetmax, sum)
sapply(df_simbetmax, mean)
colSums((df_simbetmax>0)*1)
colSums((df_simbetmax>0)*1)/252



## jetzt noch Rentabilität 


# im Folgenden wird nur getippt, falls der Tipp mit höchster Wahrscheinlichkeit 
# eine positive Rentabilität hat 


saisonsimbetrent = function(start, n, design_list, k, oddsresp){
  ar_sc = c()
  ar_xG = c()
  ar_extxG = c()
  ar_emp = c()
  for (i in (start:34)){

    test_set = c((306-(i*9)+1): (306-((i-1)*9)))
    train_set = c((306-((i-1)*9)+1):306)
    
    # setze die Quoten auf Testset fest für Validierung/Gewinne und Verluste
    
    oddstest = oddsresp[(306-(i*9)+1): (306-((i-1)*9)),]
    
    train_sc= (design_list$"sc")[train_set,]
    test_sc = (design_list$"sc")[test_set,]

    
    train_xG = (design_list$"xG")[train_set,]
    test_xG = (design_list$"xG")[test_set,]
    
    train_extxG = (design_list$"extxG")[train_set,]
    test_extxG = (design_list$"extxG")[test_set,]
    
    mod.scorehomeval = ordBTL(ordresp~., data=train_sc)
    mod.xGhomeval = ordBTL(ordroundxG~., data=train_xG)
    mod.extxGhomeval = ordBTL(ordextxG~., data=train_extxG)
    
    predp_sc = apply(predictvglm(mod.scorehomeval, newdata=test_sc, type="response"),1,max)
    predp_xG = apply(predictvglm(mod.xGhomeval, newdata=test_xG, type="response"),1,max)
    predp_extxG = apply(predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response"),1,max)
    
    # empir wieder wie bekannt wählt das Ergebnis, das am häufigsten aufgetreten 
    predp_emp = apply(empirw(train_sc$ordresp, test_sc$ordresp),1,max)
    
    pred_sc = apply(predictvglm(mod.scorehomeval, newdata=test_sc, type="response"),1,which.max)
    pred_xG = apply(predictvglm(mod.xGhomeval, newdata=test_xG, type="response"),1,which.max)
    pred_extxG = apply(predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response"),1,which.max)
    # empir wieder wie bekannt wählt das Ergebnis, das am häufigsten aufgetreten 
    pred_emp = rep(1,9)
      
    
    odds = oddstest[,1:3]
    
    adjodds_sc = odds[cbind(1:9, pred_sc)]*predp_sc
    adjodds_xG = odds[cbind(1:9,pred_xG)]*predp_xG
    adjodds_extxG = odds[cbind(1:9, pred_extxG)]*predp_extxG
    adjodds_emp = odds[,1]*predp_emp
    
    out_sc = (pred_sc == test_sc$ordresp)*(oddstest[,4]) -1
    out_xG = (pred_xG == test_sc$ordresp)*(oddstest[,4]) -1
    out_extxG = (pred_extxG == test_sc$ordresp)*(oddstest[,4]) -1
    out_emp = (pred_emp == test_sc$ordresp)*(oddstest[,4]) -1
    
    out_sc = out_sc[adjodds_sc>1]
    out_xG = out_xG[adjodds_xG>1]
    out_extxG = out_extxG[adjodds_extxG>1]
    out_emp = out_emp[adjodds_emp>1]

    ar_sc = c(ar_sc, out_sc)
    ar_xG = c(ar_xG, out_xG)
    ar_extxG = c(ar_extxG, out_extxG)
    ar_emp = c(ar_emp, out_emp)
    
  }
  return_list = list("ar_sc" = ar_sc, "ar_xG" = ar_xG, "ar_extxG" = ar_extxG, "ar_emp" = ar_emp)
  return(return_list) 
}

return_simbetrent = saisonsimbetrent(7,1,design_list3,3, betqt)



# wende die Funktionen max, min, sum, mean sowie 
# die Anzahl der Einträge >0 (Anzahl richtige Tipps) und Gesamtzahl der Einträge (Anzahl Tipps)
# auf return_simbetrent an
# Bem: Nicht mit df Methode möglich, da unterschiedliche Längen 
apply_functions <- function(x) {
  result <- c(
    sum = sum(x),
    max = max(x),
    min = min(x),
    mean = mean(x),
    len = length(x),
    count_gt_0 = sum(x > 0)
  )
  return(result)
}

# Wende die Funktion auf jeden Vektor in der Liste an und speichere die Ergebnisse in einem DataFrame
results = lapply(return_simbetrent, apply_functions)

# Konvertiere die Liste der Ergebnisse in einen DataFrame
results_df_rent = as.data.frame(do.call(rbind, results))

# Zeige den DataFrame an
print(results_df_rent)





## Abschließend noch dafür, dass immer auf den Tipp der höchsten Rentabilität getippt wird; 
# unabhängig davon, ob dies der Tipp mit höchster Wahrscheinlichkeit ist. 



saisonsimmaxrent = function(start, n, design_list, k, oddsresp){
  ar_sc = c()
  ar_xG = c()
  ar_extxG = c()
  ar_emp = c()
  for (i in (start:34)){

    test_set = c((306-(i*9)+1): (306-((i-1)*9)))
    train_set = c((306-((i-1)*9)+1):306)
    
    # setze die Quoten auf Testset fest für Validierung/Gewinne und Verluste
    
    oddstest = oddsresp[(306-(i*9)+1): (306-((i-1)*9)),]
    
    train_sc= (design_list$"sc")[train_set,]
    test_sc = (design_list$"sc")[test_set,]
    #print(test_sc)
    
    train_xG = (design_list$"xG")[train_set,]
    test_xG = (design_list$"xG")[test_set,]
    
    train_extxG = (design_list$"extxG")[train_set,]
    test_extxG = (design_list$"extxG")[test_set,]
    
    mod.scorehomeval = ordBTL(ordresp~., data=train_sc)
    mod.xGhomeval = ordBTL(ordroundxG~., data=train_xG)
    mod.extxGhomeval = ordBTL(ordextxG~., data=train_extxG)
    
    predp_sc = predictvglm(mod.scorehomeval, newdata=test_sc, type="response")
    predp_xG = predictvglm(mod.xGhomeval, newdata=test_xG, type="response")
    predp_extxG = predictvglm(mod.extxGhomeval, newdata=test_extxG, type="response")
    
    predp_emp = empirw(train_sc$ordresp, test_sc$ordresp)
    

    odds = oddstest[,1:3]
    
    odds_sc = odds*predp_sc
    odds_xG = odds*predp_xG
    odds_extxG = odds*predp_extxG
    odds_emp = odds*predp_emp

    
    #wähle odds mit höchster Wahrscheinlichkeit 
    pred_sc = apply(odds_sc,1,which.max)
    pred_xG = apply(odds_xG,1,which.max)
    pred_extxG = apply(odds_extxG,1,which.max)
    pred_emp = apply(odds_emp,1,which.max)
    

    print(pred_xG)
    print(pred_extxG)
    
    
    out_sc = (pred_sc == test_sc$ordresp)*(oddstest[,4]) -1
    out_xG = (pred_xG == test_sc$ordresp)*(oddstest[,4]) -1
    out_extxG = (pred_extxG == test_sc$ordresp)*(oddstest[,4]) -1
    out_emp = (pred_emp == test_sc$ordresp)*(oddstest[,4]) -1
    

    
    out_sc = out_sc[max(odds_sc)>1]
    out_xG = out_xG[max(odds_xG)>1]
    out_extxG = out_extxG[max(odds_extxG)>1]
    out_emp = out_emp[max(odds_emp)>1]
    
    ar_sc = c(ar_sc, out_sc)
    ar_xG = c(ar_xG, out_xG)
    ar_extxG = c(ar_extxG, out_extxG)
    ar_emp = c(ar_emp, out_emp)
    
  }
  return_list = list("ar_sc" = ar_sc, "ar_xG" = ar_xG, "ar_extxG" = ar_extxG, "ar_emp" = ar_emp)
  return(return_list) 
}

return_simmaxrent = saisonsimmaxrent(7,1,design_list3,3, betqt)
return_simmaxrent

# Wende die Funktion auf jeden Vektor in der Liste an und speichere die Ergebnisse in einem DataFrame
results_max = lapply(return_simmaxrent, apply_functions)

# Konvertiere die Liste der Ergebnisse in einen DataFrame
results_df_maxrent = as.data.frame(do.call(rbind, results_max))

# Zeige den DataFrame an
print(results_df_maxrent)


return_simmaxrenttest = saisonsimmaxrent(30,1,design_list3,3,betqt)
return_simmaxrenttest

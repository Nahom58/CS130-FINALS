#Genetic Matching

mout_m5=Match(Y=y1982yppnscal[!is.na(y1982yppnscal)],Tr=college[!is.na(y1982yppnscal)], X=Xmat[!is.na(y1982yppnscal),], estimand='ATT', M = 3, ties=T, Weight.matrix=genout_m2) 
summary(mout_m5)

matbal_m5 = MatchBalance(college ~ yGPA + yGen + yBlack + yRep+ yKnowledge + yNextSch + pVote + pPersuade + pParticipate2 + pEmploy + pEducHH + pEducW + pHHInc + pOwnHome + pRep  + pKnowledge, match.out = mout_m5,nboots=1000)
matbal_all5=MatchBalance(college ~ out, match.out = mout_m5)
mb5=percent.bal(matbal_all5)
mb5[[1]] 

# Sensitivity Test

gamma_m5=hl.rbound(mout_m5, gamma=c(2,3,4,5), pr=.01, Y=y1982yppnscal[!is.na(y1982yppnscal)], paired=TRUE)



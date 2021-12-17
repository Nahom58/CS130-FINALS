# Propensity score matching of Kam and Palmer (2008)

model=formula(college ~ yPubAff + yNewspaper + yRadio + yMagazine + yFamTalk + yFrTalk + yAdultTalk + ySPID + yGovtOpinion + yGovtCrook + yGovtWaste + yTrGovt + yGovtSmart + yGovt4All + yLifeWish + yGLuck + yFPlans + yWinArg + yStrOpinion + 
                yMChange + yTrOthers + yOthHelp + yOthFair + yKnowledge + yNextSch + yGPA + ySchOfficer + ySchPublish + yHobby + ySchClub + yOccClub + yNeighClub + yRelClub + yYouthOrg + yClubLev + yPhone + yGen + yRace + pNewspaper + pRadioZ + pTV + 
                pMagazine + pLifeWish + pGLuck + pFPlans + pWinArg + pStrOpinion + pMChange + pTrOthers + pOthHelp + pOthFair + pSPID + pVote + pPersuade + pRally + pOthAct + pPolClub + pButton + pMoney + pGovtOpinion + pGovtCrook + pGovtWaste + pTrGovt + 
                pGovtSmart + pGovt4All + pEmploy + pEducHH + pChurchOrg + pFratOrg + pProOrg + pCivicOrg + pCLOrg + pNeighClub + pSportClub + pInfClub + pFarmGr + pWomenClub + pClubLev + pHHInc + pOwnHome + pKnowledge)
pscore=glm(model,family=binomial(link=logit))
ps_values=pscore$fitted.values
# Propensity Score Matching

mout_r1 = Match(Y = yppnscal, Tr = college, X = ps_values, estimand="ATT", M = 1) 
summary(mout_r1)


matbal_r1 = MatchBalance(college ~ etahat + yGPA + yGen + yBlack + yRep+ yKnowledge + yNextSch + pVote + pPersuade + pParticipate2 + pEmploy + pEducHH + pEducW + pHHInc + pOwnHome + pRep  + pKnowledge, match.out = mout_r1,nboots=1000)
matbal_all1=MatchBalance(college ~ out, match.out = mout_r1)
mb1=percent.bal(matbal_all1)
mb1[[1]]
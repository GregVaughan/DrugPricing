### Hypothetical number of drugs entering clinical trials
### Baseline 400 corresponds to approximate number of drugs approved per year
numDrugs <- 400

### Distribution of work/clinical trials done in each phase
### Small Q1 Q2 Q3 Q4
### Calculated based on company size participation in clinicla trials 
### as reported by ClinicalTrials.gov
Phase1WorkDone <- c(0.598, 0.012, 0.021, 0.142, 0.227)
Phase2WorkDone <- c(0.601, 0.015, 0.03, 0.12, 0.234)
Phase3WorkDone <- c(0.567, 0.017, 0.032, 0.127, 0.257)

### Price reduction (as decimal)
### Upperbound reveneue reduction considered is 10%
### corresponding price reduction is 10/(48) 
priceReduction <- 10/(48) #.21

### cost cutting strategy/distribution where columns are company size
### and rows are the three phases of clinical trials

### Baseline assumes reduction in proportion to amount of spending across the 
### three phases for each company size
                          #Small           Q1           Q2            Q3             Q4
companyStrategy <- matrix(c(0.226958038, 0.124600931,	0.136308132, 0.247253245, 0.212457225, #phase1
                            0.312885607, 0.343551065, 0.281872717, 0.295416066, 0.292894706, #phase2
                            0.460156355, 0.531848004, 0.581819151, 0.457330688, 0.494648068), #phase3
                          nrow = 3, byrow = TRUE)


### Hypothetical strategy where companies primarily cut spending in phase 1 and 2
# 
# companyStrategy <- matrix(c(0.45, 0.45,	0.45, 0.45, 0.45, #phase1
#                             0.45, 0.45, 0.45, 0.45, 0.45, #phase2
#                             0.1, 0.1, 0.1, 0.1, 0.1), #phase3
#                           nrow = 3, byrow = TRUE)



### Hardcoded Constants
### Values come from DiMasi, Grabowski, and Hansen (2016)

## clinical trials Success rates for each phase
phase1SuccessRate <- 0.5952
phase2SuccessRate <- 0.3552
phase3SuccessRate <- 0.6195
approvalSuccessRate <- 0.9035

## Costs per drug by Phase
phase1Cost <- 25.3
phase2Cost <- 58.6
phase3Cost <- 255.4


## Relationship between revenue reduction and R&D reduction
## Calculated based on regression estimates of slope for
## each company size between the percent change in R&D expense 
## and percent change in Revenue for publicly listed 
## pharmaceutical and biotechnology companies from 200-2018

## upper and lower bound estimates from 95% Confidence interval 
## for the slope for each company size
# Small Q1 Q2 Q3 Q4
lowerBoundEst <- c(0, 0.0011, 0.4672, 0.6235, 0.6235)
upperBoundEst <- c(0, 0.3049, 0.8608, 1.057, 0.9483)
## not used below but reported here are the midpoint, i.e. the 
## point estimates for the slopes
midEst <- c(0, 0.0026, 0.7017, 0.8097, 0.8387)


################
### Calculations
################



#################################################
### Typical Year (No price/revenue/R&D reduction)

##Entering Phase 1
numDrugsPhase1 <- numDrugs * Phase1WorkDone

phase1Budget <- numDrugsPhase1 * phase1Cost

numDrugsCompletePhase1 <- numDrugsPhase1 * phase1SuccessRate



##Entering Phase 2 
numDrugsPhase2<- sum(numDrugsCompletePhase1) * Phase2WorkDone

phase2Budget <- numDrugsPhase2 * phase2Cost

numDrugsCompletePhase2 <- numDrugsPhase2 * phase2SuccessRate



##Entering Phase 3 
numDrugsPhase3<- sum(numDrugsCompletePhase2) * Phase3WorkDone

phase3Budget <- numDrugsPhase3 * phase3Cost

numDrugsCompletePhase3 <- numDrugsPhase3 * phase3SuccessRate

##Entering Approval

numDrugsCompleteApproval <- numDrugsCompletePhase3 * approvalSuccessRate

totalBudget <- phase1Budget + phase2Budget + phase3Budget



####################################################
### Reduced Budget for given price/Revenue Reduction

revRndRatio <- upperBoundEst

## Revenue reduction based on Price reduction
# Note, price reduction is only in US drug prices
# which only account for 48% of Us Pharmaceutical revenue
revReduction <- (0.48)*priceReduction

## R&D reduction based on Revenue Reduction
rndReduction <- revReduction * revRndRatio



budgetCut <- totalBudget * rndReduction

reducedBudget <- totalBudget * (1 - rndReduction)


budgetCutByPhase <- t(t(companyStrategy) * budgetCut)

reducedBudgetByPhase <- rbind(phase1Budget, phase2Budget, phase3Budget)- budgetCutByPhase

reducedCapacityByPhase <- reducedBudgetByPhase / c(phase1Cost, phase2Cost, phase3Cost)


##Entering Phase 1
reducedDrugsPhase1 <- reducedCapacityByPhase[1,] 


reducedDrugsCompletePhase1 <- reducedDrugsPhase1 * phase1SuccessRate
# Numbers vary, but some evidence suggests 20-30% of failures are 
# "commercial" failures not clinical failures. 
# This model assumes a more conservative 10%
phase1EconomicFailures <- reducedDrugsPhase1 * (1 - phase1SuccessRate) * 0.1




##Entering Phase 2 
#first consider how many drugs would theoretically enter phase2
reducedDrugsLeavePhase1<- sum(reducedDrugsCompletePhase1 + phase1EconomicFailures) * Phase2WorkDone


# the actual number of drugs entering phase 2 for each group assuming each group can take up to 
# their reduced capacity as long as the additional number of drugs does not exceed
# the number of economic failures
reducedDrugsPhase2 <- pmin(reducedCapacityByPhase[2,], reducedDrugsLeavePhase1)

# finally calculate the number of drugs that complete phase 2
reducedDrugsCompletePhase2 <- reducedDrugsPhase2 * phase2SuccessRate
phase2EconomicFailures <- reducedDrugsPhase2 * (1 - phase2SuccessRate) * 0.1



##Entering Phase 3
#first consider how many drugs would theoretically enter phase3
reducedDrugsLeavePhase2<- sum(reducedDrugsCompletePhase2 + phase2EconomicFailures) * Phase3WorkDone


# the actual number of drugs entering phase 3 for each group assuming each 
# group can take up to their reduced capacity as long as the additional 
# number of drugs does not exceed the number of economic failures
reducedDrugsPhase3 <- pmin(reducedCapacityByPhase[3,], reducedDrugsLeavePhase2)

# finally calculate the number of drugs that complete phase 3
reducedDrugsCompletePhase3 <- reducedDrugsPhase3 * phase3SuccessRate


##Entering Approval
reducedDrugsCompleteApproval <- reducedDrugsCompletePhase3 * approvalSuccessRate




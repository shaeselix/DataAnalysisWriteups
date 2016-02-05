library(grid)
library(rmeta)
library(sqldf)

RefineryData <- read.csv("RefineryDataPresentation.csv")

Subgroup <- sqldf("SELECT * FROM RefineryData
                  WHERE SubGroup='I'")

Subgroup$Weight <- 1/(Subgroup$StandardError)**2

Subgroup$LogRiskEstimate <- log(Subgroup$RiskEstimate)

Summary_Estimate <- exp(sum(Subgroup$Weight*Subgroup$LogRiskEstimate)/sum(Subgroup$Weight))

Summary_Estimate

LowerBound <- exp(log(Summary_Estimate) - 1.96 * sqrt(1/sum(Subgroup$Weight)))

LowerBound

UpperBound <- exp(log(Summary_Estimate) + 1.96 * sqrt(1/sum(Subgroup$Weight)))

UpperBound

Subgroup$Qi <- Subgroup$Weight*(Subgroup$LogRiskEstimate - log(Summary_Estimate))**2

Q <- sum(Subgroup$Qi)

I2 <- (Q - (length(Subgroup$RiskEstimate) - 1))/Q

I2

if (I2 > .5) {
  
  tau2 <- (Q - (length(Subgroup$RiskEstimate) - 1)) / (sum(Subgroup$Weight) - sum(Subgroup$Weight**2)/sum(Subgroup$Weight))

  Subgroup$Weight <- 1/(Subgroup$StandardError**2 + tau2)

  Summary_Estimate <- exp(sum(Subgroup$Weight*Subgroup$LogRiskEstimate)/sum(Subgroup$Weight))

  LowerBound <- exp(log(Summary_Estimate) - 1.96 * sqrt(1/sum(Subgroup$Weight)))
  
  UpperBound <- exp(log(Summary_Estimate) + 1.96 * sqrt(1/sum(Subgroup$Weight)))

  
}

Summary_Estimate

LowerBound

UpperBound

tabletext_MOP <- cbind(c("Author","",as.character(Subgroup$author),NA,"Summary",NA),
                       c("Year","",Subgroup$year,NA,NA,NA),
#                       c("Type","",as.character(Subgroup$type),NA,NA,NA),
                        c("Cohort Year","",round(Subgroup$MedianYear),NA,NA,NA),
                       c("Location","",as.character(Subgroup$location),NA,NA,NA),
#                       c("Comparison Group","",as.character(Subgroup$comparison),NA,NA,NA),
                       c("Smoking","",as.character(Subgroup$smoking2),NA,NA,NA),
                       c("Race","",as.character(Subgroup$race2),NA,NA,NA),
                       c("Risk","",round(Subgroup$RiskEstimate,digits=2),NA,round(Summary_Estimate, digits=2),NA),
                       c("","",as.character(Subgroup$rr),NA,
                         NA, paste0("(", round(LowerBound, digits=2), "-", round(UpperBound, digits=2), ")")))
m_MOP <- c(NA,NA,Subgroup$LogRiskEstimate,NA,log(Summary_Estimate))
l_MOP <- c(NA,NA,log(Subgroup$lb),NA,log(LowerBound))
u_MOP <- c(NA,NA,log(Subgroup$ub),NA,log(UpperBound))

forestplot(tabletext_MOP,m_MOP,l_MOP,u_MOP,zero=0,
           is.summary=c(TRUE,FALSE,rep(FALSE,length(Subgroup$rr)),TRUE,TRUE,TRUE),
           #clip=c(log(0.3),log(1.5)),
           xlog=TRUE,
           boxsize=c(NA,NA,sqrt(Subgroup$Weight)*.75/mean(sqrt(Subgroup$Weight)),NA,.75),
           col=meta.colors("black"))

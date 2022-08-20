library(pwr)
power.prop.test(p1=0.15,p2=0.05,power=0.90,sig.level = 0.05)

####Testing Proportions
library(data.table)
library(DT)
library(purrr)

set.seed(seed = 329)

###Research Question 1

n <- 6000

bp.dat <- data.table(Group = sample(x = c("Short", "Long"), size = n, replace = T))
bp.dat[Group == "Short", PA := round(x = rbernoulli(n = .N, p = 0.15), digits = 1)] #PA means Purchase Action
bp.dat[Group == "Long", PA := round(x = rbernoulli(n = .N,p = 0.12), digits = 1)]



analyze.experiment <- function(the.dat) {
  setDT(the.dat)
  table <-table(the.dat$Group, the.dat$PA)
  the.test <- prop.test( x=c(table[1,2],table[2,2]), n=c(sum(table[1,]),sum(table[2,])), alternative = "less")

  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  lower.bound <- the.test$conf.int[1]
  p <- the.test$p.value

  result <- data.table(effect = the.effect, lower_ci = lower.bound,p = p)

  return(result)
}
analyze.experiment(bp.dat)


B <- 1000

Experiment <- rep.int(x = 1:B, times = n)
Group = sample(x = c("Short", "Long"), size = n * B, replace = T)
sim.dat <- data.table(Experiment, Group)
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Short", PA := round(x = rbernoulli(n = .N, p = 0.15), digits = 1)]
sim.dat[Group == "Long",  PA := round(x = rbernoulli(n = .N,p = 0.12), digits = 1)]
dim(sim.dat)

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD), 
                       keyby = "Experiment"]

power= sum(exp.results$p<0.05)/B
effect.size= mean(c(exp.results$effect))
effect.size
round(100*sum(exp.results$p<0.05)/B,1)

exp.results[,mean(p<0.05)]

library(caTools)
set.seed(1234)
split = sample.split(sim.dat$PA,SplitRatio = 0.7)
train = sim.dat[split,]
test = sim.dat[!split,]
model <- glm(PA~Group, train,family = 'binomial')
model
pred <- predict(model, test)
ct = table(test$PA,pred>0.5)
ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(test)
accuracy


###Research Question 2

n <- 6000

bp.dat <- data.table(Group = sample(x = c("Short", "No"), size = n, replace = T))
bp.dat[Group == "Short", PA := round(x = rbernoulli(n = .N, p = 0.07), digits = 1)] #PA means Purchase Action
bp.dat[Group == "No", PA := round(x = rbernoulli(n = .N,p = 0.05), digits = 1)]



analyze.experiment <- function(the.dat) {
  setDT(the.dat)
  the.test <- t.test(x = the.dat[Group == "Short", PA],
                     y = the.dat[Group == "No", PA], alternative = "greater")
  
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  lower.bound <- the.test$conf.int[1]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, lower_ci = lower.bound,p = p)
  
  return(result)
}


analyze.experiment(bp.dat)
B <- 1000

Experiment <- rep.int(x = 1:B, times = n)
Group = sample(x = c("Short", "No"), size = n * B, replace = T)
sim.dat <- data.table(Experiment, Group)
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Short", PA := round(x = rbernoulli(n = .N, p = 0.07), digits = 1)]
sim.dat[Group == "No",  PA := round(x = rbernoulli(n = .N,p = 0.05), digits = 1)]
dim(sim.dat)

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD), 
                       keyby = "Experiment"]

power= sum(exp.results$p<0.05)/B
effect.size= mean(c(exp.results$effect))

round(100*sum(exp.results$p<0.05)/B,1)



###Research Question 3

n <- 6000

bp.dat <- data.table(Group = sample(x = c("Long", "No"), size = n, replace = T))
bp.dat[Group == "Long", PA := round(x = rbernoulli(n = .N, p = 0.06), digits = 1)] #PA means Purchase Action
bp.dat[Group == "No", PA := round(x = rbernoulli(n = .N,p = 0.05), digits = 1)]



analyze.experiment <- function(the.dat) {
  setDT(the.dat)
  the.test <- t.test(x = the.dat[Group == "Long", PA],
                     y = the.dat[Group == "No", PA], alternative = "greater")
  
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  lower.bound <- the.test$conf.int[1]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, lower_ci = lower.bound,p = p)
  
  return(result)
}


analyze.experiment(bp.dat)
B <- 1000

Experiment <- rep.int(x = 1:B, times = n)
Group = sample(x = c("Long", "No"), size = n * B, replace = T)
sim.dat <- data.table(Experiment, Group)
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1))
sim.dat[Group == "Long", PA := round(x = rbernoulli(n = .N, p = 0.06), digits = 1)]
sim.dat[Group == "No",  PA := round(x = rbernoulli(n = .N,p = 0.05), digits = 1)]
dim(sim.dat)

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD), 
                       keyby = "Experiment"]

power= sum(exp.results$p<0.05)/B
effect.size= mean(c(exp.results$effect))

round(100*sum(exp.results$p<0.05)/B,1)



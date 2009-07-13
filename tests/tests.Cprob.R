require(Cprob)

### test 1
data(mgus)

aa <- cpf(Hist(time, ev) ~ 1, mgus)

aa

summary(aa)

### test 2
mgus$A <- ifelse(mgus$age < 64, 0, 1)

bb <- cpf(Hist(time, ev) ~ A, mgus)

bb

summary(bb)

### test 3
fit <- cpfpo(Hist(time, ev) ~ age + creat, mgus,
             tis=seq(10, 30, 0.3), w=rep(1,67))

fit

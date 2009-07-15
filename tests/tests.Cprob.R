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

### test 4
time <- c(rep(2, 20), rep(3, 60), rep(4, 20))
ev <- c(rep(1, 20), rep(2, 60), rep(0, 20))
data <- data.frame(time, ev)

a <- predict(cpf(Hist(time, ev) ~ 1, data), 3)$cp
all.equal(a, 1/2)

### test 5

fit <- pseudocpf(Hist(time, ev) ~ age + creat, mgus, id = id, timep = cutoffs,
                 corstr = "independence", scale.value = TRUE, jack = TRUE)

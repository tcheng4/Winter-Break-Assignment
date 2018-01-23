#常態分佈的大數法則
#選取n個樣本之平均
sample_mean <- function(n=30){
  sum(rnorm(n))/n
}


#n個樣本 做i次取樣的柱狀圖(大數法則)
LLN <- function(n,i=1000){
normal=0
for (x in 1:i){
  normal[x] <- sample_mean(n)
}
hist(normal,xlim = range(-2,2),col="blue")
}



par(mfrow=c(2,2))
LLN(50)
LLN(100)
LLN(1000)
LLN(10000)

#指數分布的大數法則
sample_mean_exp <- function(n=30){
  sum(rexp(n))/n
}
LLN_exp <- function(n,i=1000){
  exp=0
  for (x in 1:i){
    exp[x] <- sample_mean_exp(n)
  }
  hist(exp,xlim = range(-2,2),col="red")
}
LLN_exp(50)
LLN_exp(100)
LLN_exp(1000)
LLN_exp(10000)

#中央極限定理(以指數函數當範例)

clt_exp_normal <- function(n,i=1000){
  exp=0
  for (x in 1:i){
    exp[x] <- sample_mean_exp(n)
  }
  sd <- sd(exp)
  mean <- mean(exp)
  par(mfrow=c(1,1))
  hist(exp,xlim = range(-2,2),col="red",main="red=exp_shape\nblue=nor_shape")
  par(new=T)
  hist(rnorm(i,mean,sd),col="blue",xlim = range(-2,2),main = "")

}

#n是抽樣的個體數 i是抽幾次 兩種分配的形狀會隨著N增加而趨近
clt_exp_normal(n=10,i=1000)
clt_exp_normal(50)
clt_exp_normal(100)
clt_exp_normal(1000)



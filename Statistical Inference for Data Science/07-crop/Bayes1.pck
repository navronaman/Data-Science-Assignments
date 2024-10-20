Bayes1.pck <-
c("Bayes1.pck", "bayes.calc.test", "lik.calc", "dfunc.norm", 
"rfunc.norm")
bayes.calc.test <-
function(data,dfunc0,rfunc0,vpar0,dfuncA,vparA,p0){
#Bayes rule
prior1<-c(p0,1-p0)
lik0<-lik.calc(data,dfunc0,vpar0)
lik1<-lik.calc(data,dfuncA,vparA)
r1<-lik1/lik0
denom<-p0*lik0+(1-p0)*lik1
post1<-c((p0*lik0/denom),((1-p0)*lik1/denom))
#pvalue.calc
n1<-length(data)
n0<-0
for(i in 1:10000){
dataz<-rfunc0(n1,vpar0)
llikz0<-lik.calc(dataz,dfunc0,vpar0)
llikzA<-lik.calc(dataz,dfuncA,vparA)
rlik<-llikzA/llikz0
n0<-n0+(rlik>r1)
}
pval1<-n0/10000
list(prior=prior1,post=post1,pval=pval1)
}
lik.calc <-
function(data,dfunc,parvec){
exp(sum(log(dfunc(data,parvec))))
}
dfunc.norm <-
function(data,vpar){
mu<-vpar[1]
s1<-vpar[2]
dnorm(data,mu,s1)
}
rfunc.norm <-
function(n,vpar){
mu<-vpar[1]
s1<-vpar[2]
dnorm(n,mu,s1)
}

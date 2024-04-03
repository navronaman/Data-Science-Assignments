regday2.pck <-
c("regday2.pck", "fixregday1.pck", "regday1.pck", "anscombe.fit1", 
"anscombe.fit2", "gauss.reg1", "NOAAGISSWD", "my.hat.w", "bootmatsmooth", 
"bootmatlin", "gen.inv1", "is.inf", "gauss.reg", "gauss.reg1a", 
"my.hat.w1", "gen.inv.sing", "cv.train.test", "cv.train.test0", 
"kfold.cv", "cv.press", "Fun.in.Tstats", "matrix.2ndorder.make"
)
fixregday1.pck <-
c("fixregday1.pck", "regday1.pck", "anscombe.fit1", "anscombe.fit2", 
"gauss.reg1", "NOAAGISSWD", "my.hat.w", "bootmatsmooth", "bootmatlin", 
"gen.inv1", "is.inf")
regday1.pck <-
c("regday1.pck", "anscombe.fit1", "anscombe.fit2", "gauss.reg1", 
"NOAAGISSWD", "my.hat.w", "bootmatsmooth", "bootmatlin", "gen.inv1", 
"is.inf")
anscombe.fit1 <-
function(){
for(i in 1:4){
 ls.print(lsfit(anscombe[[i]],anscombe[[i+4]]))
 }
}
anscombe.fit2 <-
function(){
par(mfrow=c(2,2))
for(i in 1:4){
 plot(anscombe[[i]],anscombe[[i+4]])
abline(lsfit(anscombe[[i]],anscombe[[i+4]]),col=2)
}
}
gauss.reg1 <-
function(x,y,lambda,xcol=4,do.plot=T)
{
o1<-order(x)
x1<-x[o1]
y1<-y[o1]
r1<-range(x)
smat<-NULL
n1<-length(x1)
for(i in 1:n1){
        v1<-dnorm(x1,x1[i],lambda)
        v1<-v1/sum(v1)
        H1<-my.hat.w(x1,v1)
        smat<-rbind(smat,H1[i,])
		
}
yhat<-smat%*%y1
if(do.plot){
lines(x1,yhat,col=xcol)
}
n99<-length(x1)
dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
delta2<-2*sum(diag(R%*%R))
resid<-y1-smat%*%y1
ypred<-y1
ypred[o1]<-smat%*%y1
PRESS<-sum((resid/(1-diag(smat)))^2)
list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred.out=ypred[o1],x.out=x1,press=PRESS,cor=cor(ypred,y))
}
NOAAGISSWD <-
structure(list(Year = 1980:2023, Drought.Count = c(1L, 0L, 0L, 
1L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 
1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L), Flooding.Count = c(1L, 0L, 
0L, 3L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 2L, 2L, 
1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 2L, 2L, 0L, 2L, 
1L, 2L, 4L, 2L, 0L, 3L, 0L, 2L, 1L, 4L), Freeze.Count = c(0L, 
1L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L), Severe.Storm.Count = c(0L, 
1L, 2L, 0L, 2L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 4L, 1L, 1L, 2L, 0L, 
1L, 4L, 1L, 1L, 2L, 2L, 4L, 2L, 1L, 5L, 1L, 6L, 6L, 4L, 10L, 
7L, 7L, 7L, 6L, 8L, 11L, 10L, 8L, 13L, 11L, 11L, 19L), Tropical.Cyclone.Count = c(1L, 
0L, 0L, 1L, 0L, 3L, 0L, 0L, 0L, 2L, 0L, 1L, 2L, 0L, 1L, 3L, 1L, 
0L, 3L, 1L, 0L, 1L, 2L, 1L, 4L, 4L, 0L, 0L, 3L, 0L, 0L, 2L, 2L, 
0L, 0L, 0L, 1L, 3L, 2L, 2L, 7L, 4L, 3L, 2L), Wildfire.Count = c(0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 
0L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 
0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), Winter.Storm.Count = c(0L, 
0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 2L, 0L, 1L, 
0L, 1L, 2L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 2L, 0L, 
0L, 1L, 1L, 0L, 0L, 2L, 0L, 0L, 1L, 1L, 1L), All.Disasters.Count = c(3L, 
2L, 3L, 6L, 2L, 7L, 3L, 0L, 1L, 6L, 4L, 4L, 7L, 5L, 6L, 7L, 5L, 
3L, 11L, 5L, 5L, 3L, 6L, 7L, 6L, 6L, 8L, 5L, 12L, 9L, 7L, 18L, 
11L, 10L, 10L, 11L, 15L, 19L, 16L, 14L, 22L, 20L, 18L, 28L), 
    delta.temp = c(0.27000000000000002, 0.33000000000000002, 
    0.13, 0.29999999999999999, 0.16, 0.12, 0.19, 0.33000000000000002, 
    0.40999999999999998, 0.28999999999999998, 0.44, 0.42999999999999999, 
    0.23000000000000001, 0.23999999999999999, 0.32000000000000001, 
    0.45000000000000001, 0.34999999999999998, 0.47999999999999998, 
    0.63, 0.41999999999999998, 0.41999999999999998, 0.54000000000000004, 
    0.63, 0.62, 0.55000000000000004, 0.68999999999999995, 0.64000000000000001, 
    0.66000000000000003, 0.54000000000000004, 0.65000000000000002, 
    0.72999999999999998, 0.60999999999999999, 0.64000000000000001, 
    0.66000000000000003, 0.75, 0.90000000000000002, 1.02, 0.93000000000000005, 
    0.84999999999999998, 0.98999999999999999, 1.02, 0.84999999999999998, 
    0.89000000000000001, 1.1699999999999999)), class = "data.frame", row.names = c(NA, 
-44L))
my.hat.w <-
function(x,wt){
x1<-cbind(1,x)
x1%*%gen.inv1(t(x1)%*%diag(wt)%*%x1)%*%t(x1)%*%(diag(wt))
}
bootmatsmooth <-
function(x,y,lambda=.06,rfunc=gauss.reg1,nboot=1000){
#par(mfrow=c(2,2))
plot(x,y)
plot(x,y)
predmat<-NULL
m1<-cbind(y,x)
n1<-length(y)
corvec<-NULL
for(i in 1:nboot){
	b1<-sample(c(1:n1),replace=T)
	mboot<-m1[b1,]
	yboot<-mboot[,1]
	xboot<-mboot[,-1]
	dum<-rfunc(xboot,yboot,lambda,do.plot=F)
	lines(dum$x.out,dum$pred.out,col=2)
	corvec<-c(corvec,dum$cor)
}
dum1<-rfunc(x,y,lambda)
lines(dum1$x.out,dum1$pred.out)
plot(density(corvec))
lines(c(cor(x,y),cor(x,y)),c(0,12))
}
bootmatlin <-
function(x,y,lambda=.06,rfunc=lsfit,nboot=1000){
#par(mfrow=c(2,2))
plot(x,y)
plot(x,y)
predmat<-NULL
m1<-cbind(y,x)
n1<-length(y)
corvec<-NULL
for(i in 1:nboot){
	b1<-sample(c(1:n1),replace=T)
	mboot<-m1[b1,]
	yboot<-mboot[,1]
	xboot<-mboot[,-1]
	dum<-rfunc(xboot,yboot)
	dum$x.out<-sort(xboot)
	dum$pred.out<-dum$coef[1]+dum$coef[2]*dum$x.out
	lines(dum$x.out,dum$pred.out,col=2)
	corvec<-c(corvec,cor(xboot,yboot))
}
dum1<-rfunc(x,y)
dum1$x.out<-sort(x)
	dum1$pred.out<-dum1$coef[1]+dum1$coef[2]*dum1$x.out

lines(dum1$x.out,dum1$pred.out)
plot(density(corvec))
lines(c(cor(x,y),cor(x,y)),c(0,12))
}
gen.inv1 <-
function(mat, thresh = 1e-16)
{
	v1 <- sum(is.na(mat))
	v2 <- sum(is.inf(mat))
	if((v1 + v2) > 0.5) {
		print(mat)
	}
	e1 <- eigen(mat, symmetric = T)
	val <- Re(e1$val)
	vec <- Re(e1$vec)
	val1 <- val/max(val)
	#
	#	print("normalized eigen values")
	#	print(val1)	#
	#	n1 <- length(val1)
	#	plot(c(1:n1), abs(val1), log = "y", xlab = "eigen rank", ylab
	#		 = "log10 of value")
	I1 <- val1 > thresh
	I3 <- is.na(I1)
	if(sum(I3) < 0.5) {
		val2 <- val[I1]
		I2 <- I1
		if(sum(I2) > 1.5) {
			ret <- vec[, I1] %*% diag(1/val2) %*% t(vec[, I1])
		}
		else {
			v1 <- as.matrix(vec[, I1], length(c(vec[, I1])), 1)
			ret <- (1/val2) * v1 %*% t(v1)
		}
	}
	else {
		ret <- diag(length(I1)) * 0
	}
	ret
}
is.inf <-
function(z){z==Inf}
gauss.reg <-
function(x,y,lambda,xcol=4,do.plot=T)
{
o1<-order(x)
x1<-x[o1]
y1<-y[o1]
r1<-range(x)
smat<-NULL
n1<-length(x1)
for(i in 1:n1){
        v1<-dnorm(x1,x1[i],lambda)
        v1<-v1/sum(v1)
        doh<-my.hat.w1(x1,v1)
	H1<-doh$ret0
	#print(doh$sing)
        smat<-rbind(smat,H1[i,])
}
yhat<-smat%*%y1
if(do.plot){
lines(x1,yhat,col=xcol)
}
n99<-length(x1)
dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
delta2<-2*sum(diag(R%*%R))
resid<-y1-smat%*%y1
ypred<-y1
ypred[o1]<-smat%*%y1
PRESS<-sum((resid/(1-diag(smat)))^2)
list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred=ypred,lambda=lambda,PRESS=PRESS)
}
gauss.reg1a <-
function(x,y,lambda,xcol=4)
{
o1<-order(x)
x1<-x[o1]
y1<-y[o1]
r1<-range(x)
smat<-NULL
n1<-length(x1)
lambda1<-lambda
sing0<-F
for(i in 1:n1){
        v1<-dnorm(x1,x1[i],lambda)
        v1<-v1/sum(v1)
        retlist<-my.hat.w1(x1,v1)
	  H1<-retlist$ret0
	  if(retlist$sing){
		sing0<-T
	  }
        smat<-rbind(smat,H1[i,])
		
}
kk<-0
while(sing0){
kk<-kk+1
#print(kk)
lambda1<-lambda1*2.3
#print(lambda1)
smat<-NULL
n1<-length(x1)
sing0<-F
for(i in 1:n1){
        v1<-dnorm(x1,x1[i],lambda1)
        v1<-v1/sum(v1)
        retlist<-my.hat.w1(x1,v1)
	  H1<-retlist$ret0
#print(retlist$sing)
	  if(retlist$sing){
		sing0<-T
	}
        smat<-rbind(smat,H1[i,])
		
}
}
lambda0<-lambda1
resid<-y1-smat%*%y1
Press0<-sum((resid/(1-diag(smat)))^2)
lambda0a<-lambda0*1.05
Press0a<-gauss.reg(x,y,lambda0a,do.plot=F)$PRESS
lambda0b<-lambda0*1.1
Press0b<-gauss.reg(x,y,lambda0b,do.plot=F)$PRESS
Py<-c(Press0,Press0a,Press0b)
xlamb<-c(lambda0,lambda0a,lambda0b)
xL<-cbind(xlamb,xlamb^2)
print(xL)
print(Py)
c1<-lsfit(xL,Py)$coef
lambdatarg<-(-c1[2]/(2*c1[3]))
lambvec<-lambda0+c(1:15)/10*(lambdatarg-lambda0)
lambvec<-sort(c(lambvec,xlamb))
Pressvec<-NULL
for(lamb in lambvec){
Pressvec<-c(Pressvec,gauss.reg(x,y,lamb,do.plot=F)$PRESS)
}
plot(lambvec,Pressvec)
Imin<-Pressvec==min(Pressvec)
lambda00<-lambvec[Imin][1]
plot(x,y)
gauss.reg(x,y,lambda00,do.plot=T)
}
my.hat.w1 <-
function(x,wt){
x1<-cbind(1,x)
sing<-gen.inv.sing(t(x1)%*%diag(wt)%*%x1)
ret1<-x1%*%gen.inv1(t(x1)%*%diag(wt)%*%x1)%*%t(x1)%*%(diag(wt))
list(sing=sing,ret0=ret1)
}
gen.inv.sing <-
function(mat, thresh = 1e-16)
{
	v1 <- sum(is.na(mat))
	v2 <- sum(is.inf(mat))
	if((v1 + v2) > 0.5) {
		print(mat)
	}
	e1 <- eigen(mat, symmetric = T)
	val <- Re(e1$val)
	vec <- Re(e1$vec)
	val1 <- val/max(val)
	#
	#	print("normalized eigen values")
	#	print(val1)	#
	#	n1 <- length(val1)
	#	plot(c(1:n1), abs(val1), log = "y", xlab = "eigen rank", ylab
	#		 = "log10 of value")
	I1 <- val1 > thresh
#print(I1)
	I3 <- is.na(I1)
	sing<-F
	if(sum(I1) < length(I1)) {
		sing<-T
		
	}
	sing
}
cv.train.test <-
function(xmat,ystring,xstring,prop.test)
{
n1<-length(xmat[,1])   #How many data points are there
n2<-floor(n1*prop.test) #How many in prop.test proportion of the data
test.ID<-sample(n1,n2) # randomly sample that proportion
trainmat<-xmat[-test.ID,] #create the training matrix by removing that randomly sampled set
testmat<-xmat[test.ID,] #create the test matrix as the randomly sampled set
lm.str<-lm(as.formula(paste(ystring,"~",xstring)),trainmat,x=T,y=T) #paste the three strings "ystring","~", 
#and "xstring" together and evaluate them as the formula inside LM, fit train.mat. Return the original
# x and y as part of the output
ypred<-predict(lm.str,testmat) # the model fit to the training data to predict the test data
I1<-attributes(xmat)$names==ystring # identify the y variable position in the names vector of the data frame
ny<-c(1:length(I1))[I1] # change that position to a number
testy<-testmat[[ny]] #extract y from the testmat
par(mfrow=c(1,2)) # set up a plot region with 2 plots in a row
plot(lm.str$fitted.values,lm.str$y,xlab="fit",ylab="values",main="Training data") #Plot the fit to the training data
lines(lm.str$fitted.values,lm.str$fitted.values) # add a line showing what perfect would be.
plot(ypred,testy,xlab="prediction",ylab="values",main="Testing data") #plot the prediction of the test data
lines(ypred,ypred)  #add a lline showing what perfect would be.
}
cv.train.test0 <-
function(xmat,ystring,xstring,test.ID)
{
trainmat<-xmat[-test.ID,]
testmat<-xmat[test.ID,]
lm.str<-lm(as.formula(paste(ystring,"~",xstring)),trainmat,x=T,y=T)
ypred<-predict(lm.str,testmat)
I1<-attributes(xmat)$names==ystring
ny<-c(1:length(I1))[I1]
testy<-testmat[[ny]]
yfull<-c(lm.str$y,testy)
ss0<-sum((yfull-mean(yfull))^2)
list(pred=ypred,y=testy,ss=ss0)
}
kfold.cv <-
function(xmat,ystring,xstring,k,do.plot=T)
{
prop.test<-1/k
n1<-length(xmat[,1])
n2<-floor(n1*prop.test)
base.sample<-sample(n1) # this gives a random permutation of  the vector 1:n1
par(mfrow=c(2,k/2))
ss<-0
ss0<-sum(lm(as.formula(paste(ystring,"~",xstring)),xmat)$resid^2) # WHAT IS $resid
for(i in 1:k){ #This is a loop structure it takes i in order from the vector 1:k. and applies it to the algorithm in brackets WHY DO IT?
svec<-(i-1)*n2+c(1:n2) # this chooses the section of basevec, hence what part of the data frame is the test sample
strout<-cv.train.test0(xmat,ystring,xstring,base.sample[svec]) #Hey code reuse!
ss<-ss+sum((strout$y-strout$pred)^2)
if(do.plot){#if the statement in parantheses is true it does what is in brackets following, otherwise it skips it 
plot(strout$pred,strout$y,xlab="prediction",ylab="actual")
lines(strout$pred,strout$pred)
} # the if statement ends here
} # the loop ends here
list(ss0=ss0,ss=ss)
}
cv.press <-
function(xmat,ystring,xstring)
{
lm.str<-lm(as.formula(paste(ystring,"~",xstring)),xmat,x=T,y=T)
special.resid<-lm.str$resid/(1-hat(lm.str$x))
list(leave1out.resid=special.resid,leverage=hat(lm.str$x),PRESS=sum(special.resid^2))
}
Fun.in.Tstats <-
function(Bx=.1,By=1,n=50){
        x1<-rnorm(n)
        x2<-(x1*Bx+rnorm(n))
        x2<-x2/(sd(x2))
        y<-x1+x2+rnorm(n)+1
        par(mfrow=c(2,2))
        plot(x1,x2,main=paste("cor=",cor(x1,x2)))
        lstr<-lsfit(cbind(x1,x2),y)
        print(lstr$coef)
        yhat<-lstr$coef[1]+cbind(x1,x2)%*%c(lstr$coef[-1])
        mse<-sum(lstr$resid^2)/(length(y)-3)
        plot(yhat,y)
        plot(yhat,lstr$resid)
        ls.print((lstr))
        Hat=hat(cbind(x1,x2))
        xmat<-cbind(x1,x2,1)
        xmat1<-cbind(x1,x2)
        xtxinv<-solve(t(xmat)%*%xmat)
        
        list(Hat=Hat,xtx=t(xmat)%*%xmat,covBnos=xtxinv,mse=mse,cor.xmat=cor(xmat1),corinv=solve(cor(xmat1)),eigen=eigen(cor(xmat1)))    
}
matrix.2ndorder.make <-
function(x, only.quad=F){
x0<-x
dimn<-dimnames(x)[[2]] #extract the names of the variables
num.col<-length(x[1,]) # how many columns
for(i in 1:num.col){
# if we are doing all 2nd order
if(!only.quad){
for(j in i:num.col){
x0<-cbind(x0,x[,i]*x[,j])
dimn<-c(dimn,paste(dimn[i],dimn[j],sep=""))
#create interaction dimnames

}
}
else{
#in here only if doing only squared terms
x0<-cbind(x0,x[,i]*x[,i])
dimn<-c(dimn,paste(dimn[i],"2",sep="")) # squared dimmension names
}
}
dimnames(x0)[[2]]<-dimn
x0
}
regday3.pck <-
c("regday2.pck", "fixregday1.pck", "regday1.pck", "anscombe.fit1", 
"anscombe.fit2", "gauss.reg1", "NOAAGISSWD", "my.hat.w", "bootmatsmooth", 
"bootmatlin", "gen.inv1", "is.inf", "gauss.reg", "gauss.reg1a", 
"my.hat.w1", "gen.inv.sing", "cv.train.test", "cv.train.test0", 
"kfold.cv", "cv.press", "Fun.in.Tstats", "matrix.2ndorder.make", 
"regday3.pck", "regboot", "generalized.bootstrap", "clt.sim", 
"mrpois", "mrbinom", "sim.coverage", "logitboot")
regboot <-
function(x,y,nboot=10000,alpha=.05)
{
	beta0<-lsfit(x,y)$coef #calculate coefficient
	x0<-cbind(1,x) #create x matrix including intercept
	cov0<-gen.inv1(t(x0)%*%x0) #calculate covariance matrix for beta0
	betamat<-beta0 #initialize betamat
	distvec<-0      #initialize distance vector
	for(i in 1:nboot){ #for i in 1 to nboot
		Iboot<-sample(1:length(y),replace=T) #generate random sample of indeces
		xboot<-x[Iboot,]  #choose random X vectors
		yboot<-y[Iboot]	#choose corresponding random y's
		betaboot<-lsfit(xboot,yboot)$coef #calculate bootstrap beta
		x0boot<-cbind(1,xboot) #create xboot matrix including intercept
		covbootI<-(t(x0boot)%*%x0boot) #calculate covariance matrix for bootstrap beta
		distboot<-t(betaboot-beta0)%*%covboot%*%(betaboot-beta0) #calculate distance betaboot to beta0
		distvec<-c(distvec,distboot) #add in distance value 
		betamat<-rbind(betamat,betaboot) #add in beta boot
	}
	qbound<-function(x,alpha1=alpha){quantile(x,c(alpha1/2,1-alpha1/2))} #create function to calculate both quantiles
	bootindbound<-apply(betamat,2,qbound) #calculate percentile method individual quantiles for beta
	o1<-order(distvec) #create indeces to sort by distance
	b2<-betamat[o1,]    #order beta mat by distance from beta0
	n1<-ceiling((1-alpha)*(nboot+1)) # calculate index 1-alpha *100 percent from beta0
	b2b<-b2[1:n1,] #1-alpha percent closest beta vectors to beta0
	bootsimbound<-apply(b2b,2,range) # outer bounds of the betas
	list(alpha=alpha,coef=beta0,pointwiseCI=bootindbound,simultaneousCI=bootsimbound)	
}
generalized.bootstrap <-
function(data,stat.func,alpha,nboot.ci=10000,nboot.sd=200){
piv.stat<-NULL
stat.vec<-NULL
boot.sd<-function(x,stat.f){
	zvec<-NULL
	for(i in 1:nboot.sd){
	xb1<-sample(x,replace=T)
	zvec<-c(zvec,stat.f(xb1))
	}
	sd(zvec)
}
stat0<-stat.func(data)
sd0<-boot.sd(data,stat.func)
for(i in 1:nboot.ci){
#if((i/500)==floor(i/500)){print(i)}
	xb<-sample(data,replace=T)
	stat<-stat.func(xb)
	sdb<-boot.sd(xb,stat.func)
	piv.stat<-c(piv.stat,(stat-stat0)/sdb)
	stat.vec<-c(stat.vec,stat)
}
bnds<-quantile(piv.stat,c(alpha/2,1-alpha/2))
perc.bnds<-quantile(stat.vec,c(alpha/2,1-alpha/2))
CI<-c(stat0-bnds[2]*sd0,stat0-bnds[1]*sd0)
list(stat=stat0,sd=sd0,pivotal=CI,percentile=perc.bnds)


}
clt.sim <-
function(stat,nsamp,randfunc){
v1<-NULL
nsim<-10000
for(i in 1:nsim){
v0<-randfunc(nsamp)
v1<-c(v1,stat(v0))
}
mu<-mean(v1)
sd0<-sd(v1)
plot(density(v1),main=paste("Standard error=",sd0))
lines(sort(v1),dnorm(sort(v1),mu,sd0),col=2)
}
mrpois <-
function(n){rpois(n,1)}
mrbinom <-
function(n){rbinom(n,1,.5)}
sim.coverage <-
function(nsamp){
nsim<-200
perc.count<-0
piv.count<-0
t.count<-0
for(i in 1:nsim){
if((i/100)==floor(i/100)){print(i)}
v1<-rexp(nsamp,.1)
mu<-mean(v1)
sd00<-sd(v1)/sqrt(nsamp)
qt0<-qt(.95,length(v1)-1)
boot.out<-generalized.bootstrap(v1,mean,.05)
if((boot.out$percentile[1]<10)&&(10<boot.out$percentile[2])){
perc.count<-perc.count+1
}
if((boot.out$pivotal[1]<10)&&(10<boot.out$pivotal[2])){
piv.count<-piv.count+1
}

if(((mu-qt0*sd00)<10)&&((mu+qt0*sd00)>10)){
t.count<-t.count+1
}
print(c(perc.count,piv.count,t.count))
}
list(perc.cov=perc.count/nsim,piv.cov=piv.count/nsim,t.cov=t.count/nsim)
}
logitboot <-
function(Form.ob,DF,nboot=10000,alpha=.05)
{
	glm.out0<-glm(Form.ob,DF,family=binomial(link=logit)) #calculate coefficient
	beta0<-glm.out0$coef
	cov0<-vcov(glm.out0) #calculate covariance matrix for beta0
	betamat<-beta0 #initialize betamat
	distvec<-0      #initialize distance vector
	for(i in 1:nboot){ #for i in 1 to nboot
		Iboot<-sample(1:length(y),replace=T) #generate random sample of indeces
		DFboot<-DF[Iboot,]  #choose random X vectors
		glm.boot<-glm(Form.ob,DFboot,family=binomial(link=logit)) #calculate bootstrap beta
		betaboot<-glm.boot$coef
		covboot<-gen.inv1(vcov(glm.boot)) #calculate covariance matrix for bootstrap beta
		distboot<-t(betaboot-beta0)%*%covboot%*%(betaboot-beta0) #calculate distance betaboot to beta0
		distvec<-c(distvec,distboot) #add in distance value 
		betamat<-rbind(betamat,betaboot) #add in beta boot
	}
	qbound<-function(x,alpha1=alpha){quantile(x,c(alpha1/2,1-alpha1/2))} #create function to calculate both quantiles
	bootindbound<-apply(betamat,2,qbound) #calculate percentile method individual quantiles for beta
	o1<-order(distvec) #create indeces to sort by distance
	b2<-betamat[o1,]    #order beta mat by distance from beta0
	n1<-ceiling((1-alpha)*(nboot+1)) # calculate index 1-alpha *100 percent from beta0
	b2b<-b2[1:n1,] #1-alpha percent closest beta vectors to beta0
	bootsimbound<-apply(b2b,2,range) # outer bounds of the betas
	list(alpha=alpha,coef=beta0,pointwiseCI=bootindbound,simultaneousCI=bootsimbound)	
}

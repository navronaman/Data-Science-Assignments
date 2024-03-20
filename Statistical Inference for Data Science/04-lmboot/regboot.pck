regboot.pck <-
c("regboot.pck", "lmboot", "logitboot", "gen.inv1")
lmboot <-
function(Form.ob,DF,new.dat=NULL,interval.type="pred",nboot=10000,alpha=.05)
{
	lm.out0<-lm(Form.ob,DF,x=T) #calculate regression
	coef0<-lm.out0$coef
	pred<-predict(lm.out0,interval=interval.type,level=1-alpha)
	pred0<-c(pred[,1])
	bound<-pred[,-1]
print(dim(bound))
	predmat<-NULL
	coefmat<-NULL
	distvec<-NULL
	resid<-lm.out0$resid
	press<-sum((resid/(1-hat(lm.out0$x)))^2)

	n2<-length(resid)
	for(i in 1:nboot){ #for i in 1 to nboot
		if((i/500)==floor(i/500)){print(i)}
		Iboot<-sample(n2,replace=T) #generate random sample of indeces
		DFboot<-DF[Iboot,]  #choose random X vectors
		lm.boot<-lm(Form.ob,DFboot) #calculate bootstrap
		vcovboot<-vcov(lm.boot)
		coefboot<-lm.boot$coef
		coefmat<-rbind(coefmat,coefboot)
		distvec<-c(distvec,t(coefboot-coef0)%*%gen.inv1(vcovboot)%*%(coefboot-coef0))
		predboot<-c(predict(lm.boot,DF))
		n1<-length(predboot)
		nvec<-sample(n2,n1,replace=T)
		
		if(interval.type=="pred"){
			predboot<-c(predboot+resid[Iboot[nvec]])
		}
		predmat<-rbind(predmat,predboot)
	}


	qbound<-function(x,alpha1=alpha){quantile(x,c(alpha1/2,1-alpha1/2))} #create function to calculate both quantiles
	bootinbound<-apply(predmat,2,qbound) #calculate percentile method individual quantiles for predictions
	plot(rep(pred0,6),c(pred0,pred0+resid,c(bound[,1]),c(bound[,2]),c(bootinbound[1,]),c(bootinbound[2,])),type="n",xlab="prediction",ylab="values",main=paste(1-alpha,"X100%",interval.type))
	o1<-order(pred0)
	points(pred0[o1],(pred0+resid)[o1])
	lines(pred0[o1],pred0[o1],col=4)
	lines(pred0[o1],bound[o1,1],col=3)
	lines(pred0[o1],bound[o1,2],col=3)
	lines(pred0[o1],bootinbound[1,o1],col=2)
	lines(pred0[o1],bootinbound[2,o1],col=2)
	coefpoint<-apply(coefmat,2,qbound)
	o2<-order(distvec)
	n9<-ceiling(nboot*(1-alpha))
	coefballoon<-coefmat[o2[1:n9],]
	coefsim<-apply(coefballoon,2,range)

	
      list(coef=coef0,coef.point=coefpoint,simultaneous=coefsim,conf=1-alpha,PRESS=press)
		
}
logitboot <-
function(Form.ob,DF,nboot=10000,alpha=.05)
{
	glm.out0<-glm(Form.ob,DF,family=binomial(link=logit),y=T) #calculate coefficient
	beta0<-glm.out0$coef
	AIC<-glm.out0$aic
	pred0<-predict(glm.out0,type="response")
	pred1<-predict(glm.out0)
	cov0<-vcov(glm.out0) #calculate covariance matrix for beta0
	betamat<-beta0 #initialize betamat
	distvec<-0      #initialize distance vector
	predmat<-NULL
	for(i in 1:nboot){ #for i in 1 to nboot
		if((i/500)==floor(i/500)){print(i)}
		Iboot<-sample(1:length(pred0),replace=T) #generate random sample of indeces
		DFboot<-DF[Iboot,]  #choose random X vectors
		glm.boot<-glm(Form.ob,DFboot,family=binomial(link=logit)) #calculate bootstrap beta
		betaboot<-glm.boot$coef
		predb<-predict(glm.boot,DF,type="response")
		predmat<-rbind(predmat,c(predb))
		covboot<-gen.inv1(vcov(glm.boot)) #calculate covariance matrix for bootstrap beta
		distboot<-t(betaboot-beta0)%*%covboot%*%(betaboot-beta0) #calculate distance betaboot to beta0
		distvec<-c(distvec,distboot) #add in distance value 
		betamat<-rbind(betamat,betaboot) #add in beta boot
	}
	qbound<-function(x,alpha1=alpha){quantile(x,c(alpha1/2,1-alpha1/2))} #create function to calculate both quantiles
	bootindbound<-apply(betamat,2,qbound) #calculate percentile method individual quantiles for beta
      bootpredbound<-apply(predmat,2,qbound)
	o1<-order(distvec) #create indeces to sort by distance
	b2<-betamat[o1,]    #order beta mat by distance from beta0
	n1<-ceiling((1-alpha)*(nboot+1)) # calculate index 1-alpha *100 percent from beta0
	b2b<-b2[1:n1,] #1-alpha percent closest beta vectors to beta0
	bootsimbound<-apply(b2b,2,range) # outer bounds of the betas
	o1<-order(pred1)
	plot(rep(pred1[o1],4),c(glm.out0$y[o1],pred0[o1],bootpredbound[1,o1],bootpredbound[2,o1]),type="n",main="Fit,p,and bounds",xlab="pred",ylab="value")
	points(pred1[o1],glm.out0$y[o1])
	lines(pred1[o1],pred0[o1])
	lines(pred1[o1],bootpredbound[1,o1],col=2)
	lines(pred1[o1],bootpredbound[2,o1],col=2)
	list(alpha=alpha,aic=AIC,coef=beta0,pointwiseCI=bootindbound,simultaneousCI=bootsimbound)	
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

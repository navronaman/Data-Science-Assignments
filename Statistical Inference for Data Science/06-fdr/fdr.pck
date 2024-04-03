fdr.pck <-
c("fdr.pck", "fdr", "myt")
fdr <-
function(v1,Q, ind=F){
#v1 is the vector of p values order and sort below
        o1<-order(v1)
        pvec<-v1[o1]
#set m the number of tests and set Q line according to independence.
        m<-length(v1)
        qline<-Q*c(1:m)/m
        if(!ind){
                c1<-sum(1/(c(1:m)))
                qline<-Q*c(1:m)/(m*c1)
        }
#Create plot of points and qline
        plot(c(c(1:m),c(1:m)),c(qline,pvec),type="n",xlab="ordering",ylab="pvalue")
        lines(c(1:m),qline)
        points(c(1:m),pvec)
#Calculate Pstar (pmax) and identify all P<= pmax
        dv<-pvec-qline
	I22<-!is.na(dv)

        I1<-(dv[I22]<0)
		I0<-I1
	if(sum(I0)>.5){
        pmax<-max(pvec[I22][I1])
        I2<-pvec[I22]<=pmax
        points(c(1:m)[I2],pvec[I2],col="red")
		out<-list(interesting=o1[I22][I2], ind=ind)
	
	}
else{
	vec<-qbeta(c(.5,.95,.99,.999),1,length(v1)+1)
	out<-list(q.5=vec[1],q.95=vec[2],q.99=vec[3],q.999=vec[4])
}

out
        

}
myt <-
function(x){
duh<-t.test(x)
duh$p.value
}

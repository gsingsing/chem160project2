Sm<-14000 #sus male
Sf<-9000 #sus female
Im<-1000 #infected male
If<-1000 #infected female
gamma.m<-0.05 #male recovery rate
gamma.f<-0.007 #female recovery rate
alpha.m<-0.000002 #male transmission rate
alpha.f<-0.0000009 #female transmission rate
Sm.hist<-c() #to make vectors for the population
Sf.hist<-c()
Im.hist<-c()
If.hist<-c()
for (day in 1:2000) {
	Sm.hist[day]<-Sm #to update the population sizes
	Sf.hist[day]<-Sf
	Im.hist[day]<-Im
	If.hist[day]<-If
	delta.Sm<-(gamma.m*Im-alpha.m*Sm*If)
	delta.Sf<-(gamma.f*If-alpha.f*Sf*Im)
	delta.Im<-(alpha.m*Sm*If-gamma.m*Im)
	delta.If<-(alpha.f*Sf*Im-gamma.f*If)
	Sm<- Sm+delta.Sm #update the pop.size
	Sf<-Sf+delta.Sf
	Im<-Im+delta.Im
	If<-If+delta.If
	Sm<-max(Sm,0) #keep pop positive
	Sf<-max(Sf,0)
	Im<-max(Im,0)
	If<-max(If,0)
}
plot(Im.hist, type="l", ylim=c(0,14000), xlab="Days", ylab="Number of individuals")
lines(If.hist, col=4)

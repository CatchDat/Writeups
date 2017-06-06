
N=10000
M=10
B=M*M

# raw histograms
hist(runif(N),breaks=99, main="", xlab="Uniform pseudorandom variates")
hist(humanleague::sobolSequence(1,N), breaks=99, main="", xlab="Uniform quasirandom variates")

p=hist(runif(N),breaks=B-1,plot=F)$counts
p2=matrix(p,nrow=M)
# 1d Sobol sequence
#q=hist(humanleague::sobolSequence(1,N),breaks=B-1,plot=F)$counts

#2d Sobol sequence
s=humanleague::sobolSequence(2,N)
s=floor(s*M+1) # turn [0,1) into buckets
q2=matrix(rep(0,B),nrow=M)

# sample in square
for (i in 1:N) {
  x = s[i,1]
  y = s[i,2]
  #print(paste(x,y))
  q2[x,y] = q2[x,y]+1
}

s2dp=matrix(p,nrow=M)

pc=chisq.test(p2)
qc=chisq.test(q2)
print(pc)
print(qc)

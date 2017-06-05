
N=10000
M=10
B=M*M

p=hist(runif(N),breaks=B-1,plot=F)$counts
p2=matrix(p,nrow=M)
# 1d Sobol sequence
#q=hist(humanleague::sobolSequence(1,N),breaks=B-1,plot=F)$counts

#2d Sobol sequence
s=humanleague::sobolSequence(2,N)
s=floor(s*M+1)
q2=matrix(rep(0,B),nrow=M)

# sample in square
for (i in 1:N) {
  x = s2[i,1]
  y = s2[i,2]
  #print(paste(x,y))
  q2[x,y] = q2[x,y]+1
}

s2dp=matrix(p,nrow=M)

pc=chisq.test(p2)
qc=chisq.test(q2)
print(pc)

x <- rep(0,100000)

x[1] = 1

a = 7^5
c = 0
m = 2^31 + 1 #### why + 1 (or -1 )

for(i in 1:length(x)){
  x[i+1] = (a * x[i] + c) %% m
  
}

u <- x/m  

hist(u)

### We will see that there is no pattern between the numbers
#########from to by 
plot(x[seq(2,1000,2)], x[seq(3,1001,2)])
#### but since we generated it using the previous algortithm this is only pseudo random 

x<-runif(1, 0, 10) #generate a random number between 1 and 10
if(x>3){
    y<-10
}else{
    y<-0
}
print(c(x, y))

for(i in 1:10){
    print(i)
}

x<-c('a', 'b', 'c', 'd')
for(i in seq_along(x)){
    print(x[i])
}

for(letter in x){
    print(letter)
}

x<-matrix(1:6, 2, 3)

for(i in seq_len(nrow(x))){
    for(j in seq_len(ncol(x))){
        print(x[i, j])
    }
}

count<-0
while(count<10){
    print(count)
    count<-count+1
}

f<-function(num=1){
    hello <- "Hello, world!\n"
    for(i in seq_len(num)) {
        cat(hello)
    }
    chars <-nchar(hello)*num
    chars #return value is the very last expression in a function
}

fact<-function(num){
    if(num<0){
        return("not valid")
    }
    else if(num==1 || num == 0){
        return(1)
    }
    else {    
    return(num*fact(num-1))
    }
}

fib<-function(num){
    if(num==1 || num==2){ #base cases
        return(1)
    }
    else{
        return(fib(num-1)+fib(num-2))
    }
}

for(i in 1:20){
    print(c(i, fib(i)))
}

make.NegLogLike<-function(data, fixed=c(FALSE, FALSE)){
    params <- fixed
    function(p) {
        params[!fixed] <- p
        mu <- params[1]
        sigma <- params[2]
        
        ##calculate normal density
        a <- -0.5*length(data)*log(2*pi*sigma^2)
        b <- -0.5*sum((data-mu)^2)/(sigma^2)
        -(a+b)
    }
}

set.seed(1)
normals <- rnorm(100, 1, 2)
nLL<-make.NegLogLike(normals)
nLL

optim(c(mu=0, sigma=1), nLL)$par #estimate mu and sigma

nLL<-make.NegLogLike(normals, c(FALSE, 2)) #hold sigma=2
optimize(nLL, c(-1, 2))$minimum

nLL<-make.NegLogLike(normals, c(1, FALSE)) #hold mu=1
optimize(nLL, c(1e-6, 8))$minimum

x<-seq(1.7, 1.9, len=100)
y<-sapply(x, nLL) #evaluate nLL at every point in vector x
plot(x, exp(-(y-min(y))), type='l')
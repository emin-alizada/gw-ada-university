getwd();

ls()

history()

a = 49
typeof(a)
class(a)
sqrt(a)

a = "hello"
typeof(a)

b <- 5
c <- 4
d <- 3
e <- b < c + d
typeof(e)

x = c(100, 200, 300)
length(x)
x *2
x[x>150]
x[c(1,3)]

a = c(1, 3, 5, 7) 
b = c(1, 2, 4, 8, 9)

a + b

d = c(1, "str") #converts 1 to str

s = c("aa", "bb", "cc", "dd", "ee")
L = c(T, F)

s[L]

B = matrix( c(2, 4, 3, 1, 5, 7), nrow=3, ncol=2) 

B

C = matrix( c(7, 4), nrow=3, ncol=2)
C

cbind(B, C)
rbind(B,C)

d <- data.frame(x1 = c(2,3,4,5), x2 = c(5,4,3,2))
attach(d)
d$sumx <- x1 + x2
detach(d)
d$meanx <- (d$x1 + d$x2)/2
d

help("matrix")
C = matrix( c(7, 4), nrow=3, ncol=2, dimnames = list(c("f", "s", "t"), c(1,2)))
C

help("attach")

C
colnames(C) <- c("a", "b")
C
C[,"a"]

ls = list(1,2,1:3)

ls
typeof(ls[[1]])


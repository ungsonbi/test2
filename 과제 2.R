# 1
x <- c(17, 16, 20, 24, 22, 15, 21, 18)
x[x>=20]

y <- c()
for (i in 1:length(x))
{ 
  if(x[i]>=20) {
    y[i] <- 100
  } else {
    y[i] <- x[i]
  }
}
y


# 2
x <- matrix(-1, 5, 5)
for (i in 1:5) {
  x[i,i] <- i+2
}
x

y <- x[,-5]
y

yinfo <- dim(y)
yinfo

y1 <- matrix(0, 5, 5)
for (i in 1:5) 
  {
  for (j in 1:5) 
    {
    if (i != j) {
      y1[i,j] <- 0
    } else {
      y1[i,j] <- x[i,j]
    }
  }
}
y1


# 3
setwd('C:\\tmp')
rdata <- read.csv('rowdata.txt')
rdata
is.na(rdata)

rdata2 <- is.na(rdata)
for (i in 1:nrow(rdata2)) {
  if (rdata2[i, 2] == FALSE & rdata2[i, 3] == FALSE) {
    print(i)
  }
}

a <- c()
isrdata <- is.na(rdata)
for (i in 1:nrow(isrdata)) {
  if (isrdata[i, 2] == TRUE | isrdata[i, 3] == TRUE) {
   a[i] <- i
  }
}
a <- na.omit(a)
rdata1 <- rdata[-a,]
rdata1

# 4
four <- list()
four[[1]] <- c(TRUE, FALSE)
four[[2]] <- matrix(c(1, 0, 0, 1), 2, 2)
four[[3]] <- seq(0, 1, length = 100)
for (i in 4:7) 
  {
   four[[i]] <- i - 3
}
four


# 5

a1 <- -1:2
a2 <- 1:2
a1 + a2

a1 <- -(1:2)
a2 <- 1:2
a1 + a2

a1 <- matrix(0, 2, 2)
a2 <- c(3, 4)
a1 + a2

a1 <- matrix(1:4, 2, 2)
a1[a1 > 2]

a1 <- 1:5
a1[-1] - a1[-length(a1)]







# 1 

a = c()
for (i in 1:20)
{
  if (i < 3) 
  {
    a[1] = 1
    a[2] = 3
  } else {
    a[i] = 0.9 * a[i-1] - 0.1 * a[i-2] + 1
  }
}
a[20]

# 2 

a = c()
for (i in 1:20)
{
  if (i < 3) 
  {
    a[1] = 1
    a[2] = 3
  } else {
    a[i] = 0.9 * a[i-1] - 0.1 * a[i-2] + 1
  }
  if (a[i] > 4)
  {
    print(i)
    stop()
  }
}
a[5]

# 3

A <- matrix(runif(50), 50, 5)
if (class(A) != 'matrix') stop()
v = rep(0, nrow(A))
for (i in 1:nrow(A)) 
{
  v[i] = sum(A[i, ])
}
v

# 4

tmp = rep(0, 10)
a <- 10:1
idx = 1
for ( j in a)
{
  if (j<5)
  {
    tmp[idx] <- a[j]
    idx <- idx + 1
  }
}
tmp

# 5

set.seed(1)
x <- matrix(runif(5000), 1000, 5)
sid <- sample(1:10, 1000, replace = T)
str(sid)

# 6 (1)

m.mat <- matrix(0, 10, 5)
a <- c(rep(0,10))
for (i in 1:10) 
{ 
  for (j in 1:1000)
  {
  if(sid[j] == i)
  {
    m.mat[i,] <- m.mat[i,] + x[j,]
    a[i] <- a[i] + 1
    }
  }
}
a
m.mat <- m.mat/a
m.mat

# 6 (2)

idist <- matrix(0, 1000, 10)
for (i in 1:1000) 
  {
  for (j in 1:10)
  {
    d1 <- sum(x[i,]*m.mat[j,])
    d2 <- sqrt(sum((x[i,])^2))*sqrt(sum((m.mat[j,])^2))
    idist[i,j] <- d1/d2
}
}
str(idist)

# 7

ivec <- rep(0, 1000)
for (i in 1:1000)
{ 
  ivec[i] <- which.min(idist[i,])
}
str(ivec)

# 8 

set.seed(1)
a = list()
for (i in 1:1000)
{
  x = rpois(1,4)+1
  x = min(x,10)
  a[[i]] = sample(1:10, x)
}

y <- c()
for (i in 1:1000)
{
  y[i] <- length(a[[i]])
}
table(y)

z <- rep(0, 10)
for (i in 1:1000)
{
  if (y[i] > 0 & y[i] < 4)
  {
    b <- a[[i]][1]
    z[b] <- z[b] + 1
  } else if (y[i] > 3 & y[i] < 7)
  {
    c1 <- a[[i]][1]
    c2 <- a[[i]][2]
    z[c1] <- z[c1] + 2
    z[c2] <- z[c2] + 1
  } else {
    d1 <- a[[i]][1]
    d2 <- a[[i]][2]
    d3 <- a[[i]][3]
    z[d1] <- z[d1] + 3
    z[d2] <- z[d2] + 2
    z[d3] <- z[d3] + 1
  }
}
which.max(z)

# 9 - (1)

set.seed(1)
m1 = 10
m2 = 5
num = 0
  for (i in 1:4)
    {
    p <- rbinom(1, 1, 1/2)
    if (p == 0) 
           {
      m1 <- m1 - 1
      m2 <- m2 + 1
    } else {
      m1 <- m1 + 1
      m2 <- m2 - 1
           }
    num <- num + 1
    if (m1*m2 == 0) 
    {
      print(num)
      print(m1)
      print(m2)
      stop()
    }
    }
print(m1)
print(m2)

# 9 - (2)

set.seed(1)
m1 = 10
m2 = 5
num = 0
for (i in 1:10000)
{
  p <- rbinom(1, 1, 1/2)
  if (p == 0) 
  {
    m1 <- m1 - 1
    m2 <- m2 + 1
  } else {
    m1 <- m1 + 1
    m2 <- m2 - 1
  }
  num <- num + 1
  if (m1*m2 == 0) 
  {
    print(num)
    print(m1)
    print(m2)
    stop()
  }
}

# 9 - (3)

mm <- rep(0, 2)
for (k in 1:200)
{
 set.seed(k)
 m1 = 10
 m2 = 5
 num = 0
 for (i in 1:10000000)
 {
   p <- rbinom(1, 1, 1/2)
   if (p == 0) 
   {
     m1 <- m1 - 1
     m2 <- m2 + 1
   } else {
     m1 <- m1 + 1
     m2 <- m2 - 1
   }
   num <- num + 1
   if (m1*m2 == 0) 
   {
     if (m1 > m2)
      {
      mm[1] <- mm[1] + 1
     } else {
      mm[2] <- mm[2] + 1
     }
   break
    }
  }
} 
mm

# 10

mm <- rep(0, 2)
for (k in 1:200)
{
  set.seed(k)
  m1 = 10
  m2 = 10
  num = 0
  for (i in 1:10000000)
  {
    p <- rbinom(1, 1, 1/2)
    if (p == 0) 
    {
      m1 <- m1 - 1
      m2 <- m2 + 1
    } else {
      m1 <- m1 + 1
      m2 <- m2 - 1
    }
    num <- num + 1
    if (m1*m2 == 0) 
    {
      if (m1 > m2)
      {
        mm[1] <- mm[1] + 1
      } else {
        mm[2] <- mm[2] + 1
      }
      break
    }
  }
} 
mm

mm <- rep(0, 2)
for (k in 1:200)
{
  set.seed(k)
  m1 = 10
  m2 = 15
  num = 0
  for (i in 1:10000000)
  {
    p <- rbinom(1, 1, 1/2)
    if (p == 0) 
    {
      m1 <- m1 - 1
      m2 <- m2 + 1
    } else {
      m1 <- m1 + 1
      m2 <- m2 - 1
    }
    num <- num + 1
    if (m1*m2 == 0) 
    {
      if (m1 > m2)
      {
        mm[1] <- mm[1] + 1
      } else {
        mm[2] <- mm[2] + 1
      }
      break
    }
  }
} 
mm

mm <- rep(0, 2)
for (k in 1:200)
{
  set.seed(k)
  m1 = 10
  m2 = 20
  num = 0
  for (i in 1:10000000)
  {
    p <- rbinom(1, 1, 1/2)
    if (p == 0) 
    {
      m1 <- m1 - 1
      m2 <- m2 + 1
    } else {
      m1 <- m1 + 1
      m2 <- m2 - 1
    }
    num <- num + 1
    if (m1*m2 == 0) 
    {
      if (m1 > m2)
      {
        mm[1] <- mm[1] + 1
      } else {
        mm[2] <- mm[2] + 1
      }
      break
    }
  }
} 
mm

mm <- rep(0, 2)
for (k in 1:200)
{
  set.seed(k)
  m1 = 10
  m2 = 25
  num = 0
  for (i in 1:10000000)
  {
    p <- rbinom(1, 1, 1/2)
    if (p == 0) 
    {
      m1 <- m1 - 1
      m2 <- m2 + 1
    } else {
      m1 <- m1 + 1
      m2 <- m2 - 1
    }
    num <- num + 1
    if (m1*m2 == 0) 
    {
      if (m1 > m2)
      {
        mm[1] <- mm[1] + 1
      } else {
        mm[2] <- mm[2] + 1
      }
      break
    }
  }
} 
mm













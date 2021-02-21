# Alex Van Plantinga
# creative variations on the game of life
# September 2020

# function that takes in an iteration of a game of life
# outputs an iteration of a random game of life with different rules and patterns




foo = function(i,d,pop){
  if(i==1){
    d <- (pop == 3) | (d & (pop>=bitwXor(pop, pop-d)) & (pop<=bitwOr(pop, pop+d)))#fire
  }
  if(i==2){
    d <- (pop == 4) | (d & (pop>=bitwXor(pop, pop-d)) & (pop<=bitwOr(pop, pop+d)))# gooseneck gorge
  }
  if(i==3){  
    d <- (pop == 2) | (d & (pop>=bitwXor(pop, pop-d)) & (pop<=bitwOr(pop, pop+d)))# maze
  }
  if(i==4){
    d <- (pop == 5) | (d & (pop>=bitwXor(pop, pop-d)) & (pop<=bitwOr(pop, pop+d)))# quick fade
  }
  if(i==5){
    d <- (pop == c(5,3)) | (d & (pop>=bitwXor(pop, pop-d)) & (pop<=bitwOr(pop, pop+d)))#quick fade
  }
  if(i==6){
    d <- (pop %in% c(2,7)) | (d & (pop>=bitwXor(pop, pop+d)) & (pop<=bitwOr(pop, pop-d))) #goosenecks
  }
  if(i==7){
    d <- (pop %in% c(1,5)) |  (d & (pop>=bitwXor(pop, pop+d)) &  (pop<=bitwOr(pop, pop-d)))# negative space
  }
  if(i==8){
    d <- (pop %in% c(2,5)) | (d & (pop>=bitwXor(pop, pop+d)) & (pop<=bitwOr(pop, pop-d)))# negative space
  }
  if(i==9){
    d<- (pop %in% c(2,5)) | (d & (pop >=mean(bitwXor(pop, pop + d))) & (pop <=mean(bitwOr(pop, pop+d))))#horizontal maze
  }
  if(i==10){
    d <- (pop %in% c(1,5)) | (d & (pop>=mean(bitwXor(pop, pop+d))) & (pop<=mean(bitwOr(pop, pop+d))))# vertical maze
  }
  if(i==11){
    d<- (pop==3) | (d & (pop>=2) & (pop<=3))#Conway's game of life
  }
  if(i==12){
    d <- (pop == c(1,5)) | (d & (pop>=bitwAnd(pop+2, pop+d)) & (pop<=bitwOr(pop, pop-d)))#inchworms
  }
  
  if(i==13){
    d <- (pop == c(3,5)) | (d & (pop>=bitwAnd(pop+2, pop+d)) & (pop<=bitwOr(pop, pop-d)))#???
  }
  
  if(i==14){
    d <- (pop == c(1,3)) | (d & (pop>=bitwAnd(pop+2, pop+d)) & (pop<=bitwOr(pop, pop-d)))#inchworms
  }
  
  
  if(i==15){
    d <- (pop == c(4,6)) | (d & (pop>=bitwAnd(pop+2, pop+d)) & (pop<=bitwOr(pop, pop-d)))#quick fade?
  }
  if(i==16){
    d <- (pop == bitwXor(pop, pop-d)) | (d & (pop>=4) & (pop<=6))#new negative space
  }
  if(i==17){
    d <- (pop == bitwXor(pop, pop-d)) | (d & (pop>=5) & (pop<=7))#period 6 negative space
  }
  if(i==18){
    d <- (pop == bitwXor(pop, pop+d)) | (d & (pop>=5) & (pop<=7))#variable negative space
  }
  if(i==19){
    d <- (pop == bitwXor(pop, pop+d)) | (d & (pop>=3) & (pop<=4))#blocky negative space
  }
  return(d)
}


# -------------------
# new rule idea
library(reshape2)
library(numbers)
library(tidyr)
setwd("C://Users//Lenovo//Demo//gameoflifeImages")
ll=99
ww =99
tt = 1000
d = matrix(sample(c(0,0,1), ll*ww, replace=TRUE), ncol = ww, nrow = ll)
d = matrix(0, ncol = ww, nrow = ll)
u = c()
x1 = 16

cindex = matrix(c(rep(seq(0,3), ll*ww/4),0), ncol = ww, nrow = ll)

for(i in 1:tt){
  dd = data.frame(d)
  # form the neighboring sums
  nrow <- dim(d)[[1]]
  ncol <- dim(d)[[2]]
  d_eu <- rbind(d[-1, , drop = FALSE], 0)
  d_ed <- rbind(0, d[-nrow, , drop = FALSE])
  d_le <- cbind(d[ , -1, drop = FALSE], 0)
  d_re <- cbind(0, d[ , -ncol, drop = FALSE])
  d_lu <- cbind(d_eu[ , -1, drop = FALSE], 0)
  d_ru <- cbind(0, d_eu[ , -ncol, drop = FALSE])
  d_ld <- cbind(d_ed[ , -1, drop = FALSE], 0)
  d_rd <- cbind(0, d_ed[ , -ncol, drop = FALSE])
  pop <- d_eu + d_ed + d_le + d_re + d_lu + d_ru + d_ld + d_rd


  
  if(i > 14){if( sum(u[i-1], u[i-2]) > 0.995 * sum(u[i-4], u[i-5]) && sum(u[i-1], u[i-2]) < 1.005 * sum(u[i-4], u[i-5]) ){
      xs=sample(seq(1,19),4)
      x1=xs[1];
      #x2 = xs[2];
      #x3 = xs[3];
      cols= "red"}else{cols = "navy"}}
  
  dtemp = d
  
  if(i <= 14){
  d=foo(x1,d,pop); cols = "blue"
  }else{
    
    d = foo(x1,dtemp,pop);
    
    for(j in 2:length(xs)){
    
        d[which(cindex == j)] = foo(xs[j], dtemp, pop)[which(cindex == j)] 
    
        }
  }  

  colnames(d) = seq(1,ncol(d))
  rownames(d) = seq(1,nrow(d))
  
  md = as.data.frame(as.vector(d))
  md$key = unlist(lapply(seq(1,nrow(d)), function(x) rep(x,ncol(d))))
  md$variable2 = seq(1,nrow(d))
  
  md = md[which(as.data.frame(as.vector(d)) == "TRUE"),]
  
  if(i < 10){nnn = "000"} else if(i < 100){nnn = "00"} else if(i < 1000){nnn = "0"}else{nnn = ""}
  
  aa = sample(ll,1)
  bb=sample(ww,1)
  #if(i%%20==0){d[aa,bb] = !d[aa,bb]}
  
  d[d==TRUE] = 1
  d[d==FALSE] = 0
  
  u = c(u, sum(d))  
  #cols = rep(c("red", "magenta", "blue", "cyan", "green", "yellow", "orange"),tt/6)#rainbow colors
  png(paste0("gameoflife",nnn,i,".png"), width=650, heigh=800)
  
  layout(matrix(c(rep(1,12),rep(2,4)), 4, 4, byrow = TRUE))
  
  par(mar = c(0.1,0.1,0.1,0.1))
  
  if(i <= 14){mytitle = paste("Total Population  rule",x1)}else{mytitle = paste("Total Population  rules",xs[1],xs[2],xs[3],xs[4])}
  
  plot(md$key, md$variable2, pch = 15, cex = 1.5, 
       ylim = c(0,ww), xlim = c(0,ll),
       axes = "false", xlab = NA, ylab = NA, col = cols)
  plot(u, xlab = NA, ylab = NA, ylim = c(0, ll*ww), xlim = c(0,tt), type = "l")
  mtext(mytitle, 3, -1, outer=FALSE, cex = 0.8)
  dev.off()
  
  
  
}


# https://leonawicz.github.io/mapmate/articles/ffmpeg.html
library(mapmate)
library(dplyr)
library(purrr)

p <- "gameoflife%04d.png"
out <- "sequence132.gif"

ffmpeg(pattern = p, output = out, fps.out = 10, delay = 0.05, overwrite = TRUE)

dev.off()
file.remove(list.files(pattern=".png"))


# make a kaleidascope effect by using odd dimensions 99x99 and starting with all zero matrix and
# no random changing of grid cells
# even dimensions gives a rorschach effect
# even by odd dimensions gives 2-way assymmetry or rotational symmetry kind of

# 1/17/2021 cryptographic experiment

bitsToInt<-function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}


dvec = as.vector(d)

s = 7

cryptlist = c()

for(i in 1:floor(length(dvec)/s)){
  
  cryptlist = c(cryptlist, bitsToInt(dvec[(i*s-s+1) : (i*s)]) )  

  }

library(gtools)
mychars = chr(32:152)

message = ""
for(i in 1:length(cryptlist)){
  message = paste0(message, mychars[cryptlist[i]])
}


test = "Vargas and other census advocates are also concerned about other unresolved issues at the bureau aside from the need for new leadership. In a letter to Biden that cites NPR's reporting, Sen. Brian Schatz, a Democrat from Hawaii, suggests four priorities for fixing the 2020 Census and preventing future politicization. Schatz, who has been on the Senate appropriations subcommittee for the bureau, is urging the president-elect's administration to support four-month extensions to the legal reporting deadlines for census results, rescind both Trump's executive order for citizenship data and his presidential memo on excluding unauthorized immigrants from apportionment counts, and create a nonpartisan commission that would review the accuracy of the numbers for reapportioning House seats before they're delivered to Congress for certification."

test = unlist(strsplit(test, ""))

testchars = c()
for(i in 1:length(test)){#indices of mychars
  testchars = c(testchars, which(mychars == test[i]))
}

# turn indices into binary

bnary = function(n, e, q){
  e = floor((n)/2)
  q = n%%2
  if(n == 0 ){return(0)}
  if(e == 0){return(q)}
  else{return(c(bnary(e), (q)))}
}

testbinary = c()
for(i in 1:length(testchars)){
  testbinary = c(testbinary, bnary(testchars[i]))
}



# -------------------
# new rule idea
library(reshape2)
library(numbers)
library(tidyr)
setwd("C://Users//Lenovo//Demo//gameoflifeImages")
ll=99
ww =99
tt = 1000
d = matrix(sample(c(0,0,1), ll*ww, replace=TRUE), ncol = ww, nrow = ll)
d = matrix(0, ncol = ww, nrow = ll)
d = matrix(c(testbinary, rep(0, 4674)), ncol = ww, nrow = ll)
u = c()
x1 = 16
xs=sample(seq(1,19),4)
cindex = matrix(c(rep(seq(0,3), ll*ww/4),0), ncol = ww, nrow = ll)

for(i in 1:tt){
  dd = data.frame(d)
  # form the neighboring sums
  nrow <- dim(d)[[1]]
  ncol <- dim(d)[[2]]
  d_eu <- rbind(d[-1, , drop = FALSE], 0)
  d_ed <- rbind(0, d[-nrow, , drop = FALSE])
  d_le <- cbind(d[ , -1, drop = FALSE], 0)
  d_re <- cbind(0, d[ , -ncol, drop = FALSE])
  d_lu <- cbind(d_eu[ , -1, drop = FALSE], 0)
  d_ru <- cbind(0, d_eu[ , -ncol, drop = FALSE])
  d_ld <- cbind(d_ed[ , -1, drop = FALSE], 0)
  d_rd <- cbind(0, d_ed[ , -ncol, drop = FALSE])
  pop <- d_eu + d_ed + d_le + d_re + d_lu + d_ru + d_ld + d_rd
  
  
  
  #if(i > 14){if( sum(u[i-1], u[i-2]) > 0.995 * sum(u[i-4], u[i-5]) && sum(u[i-1], u[i-2]) < 1.005 * sum(u[i-4], u[i-5]) ){
  #xs=sample(seq(1,19),4)
  x1=xs[1];
  #x2 = xs[2];
  #x3 = xs[3];
  #   cols= "red"}else{cols = "navy"}}
  
  dtemp = d
  
  #  if(i <= 14){
  # d=foo(x1,d,pop); cols = "blue"
  #}else{
  
  d = foo(x1,dtemp,pop);
  
  for(j in 2:length(xs)){
    
    d[which(cindex == j)] = foo(xs[j], dtemp, pop)[which(cindex == j)] 
    
  }
  #}  
  
  colnames(d) = seq(1,ncol(d))
  rownames(d) = seq(1,nrow(d))
  
  md = as.data.frame(as.vector(d))
  md$key = unlist(lapply(seq(1,nrow(d)), function(x) rep(x,ncol(d))))
  md$variable2 = seq(1,nrow(d))
  
  md = md[which(as.data.frame(as.vector(d)) == "TRUE"),]
  
  if(i < 10){nnn = "000"} else if(i < 100){nnn = "00"} else if(i < 1000){nnn = "0"}else{nnn = ""}
  
  aa = sample(ll,1)
  bb=sample(ww,1)
  #if(i%%20==0){d[aa,bb] = !d[aa,bb]}
  
  d[d==TRUE] = 1
  d[d==FALSE] = 0
  
  u = c(u, sum(d))  
  #cols = rep(c("red", "magenta", "blue", "cyan", "green", "yellow", "orange"),tt/6)#rainbow colors
  png(paste0("gameoflife",nnn,i,".png"), width=650, heigh=800)
  
  layout(matrix(c(rep(1,12),rep(2,4)), 4, 4, byrow = TRUE))
  
  par(mar = c(0.1,0.1,0.1,0.1))
  
  if(i <= 14){mytitle = paste("Total Population  rule",x1)}else{mytitle = paste("Total Population  rules",xs[1],xs[2],xs[3],xs[4])}
  
  plot(md$key, md$variable2, pch = 15, cex = 1.5, 
       ylim = c(0,ww), xlim = c(0,ll),
       axes = "false", xlab = NA, ylab = NA, col = cols)
  plot(u, xlab = NA, ylab = NA, ylim = c(0, ll*ww), xlim = c(0,tt), type = "l")
  mtext(mytitle, 3, -1, outer=FALSE, cex = 0.8)
  dev.off()
  
  
  
}













endlist = c()

for(i in cryptlist){
  endlist = c(endlist, substr(mychars,i,i))
}

divs = seq(1,99,s)

for(i in 1:99){
  cryptlist = c(cryptlist, bitsToInt(as.vector(d[i, divs[1] : divs[2]-1])) )
  for(j in 2:(length(divs)-1)){
    cryptlist = c(cryptlist, bitsToInt(as.vector(d[i, (divs[j]+1): (divs[j]+s-1) ] ) ) )
  }
}

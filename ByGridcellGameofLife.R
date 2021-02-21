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
tt = 200
#d = matrix(sample(c(0,0,1), ll*ww, replace=TRUE), ncol = ww, nrow = ll)
s = sample(seq(1,19), 8)
cells = matrix(sample(s, ll*ww, replace = TRUE), ncol = ww, nrow = ll)

d = matrix(0, ncol = ww, nrow = ll)
u = c()
x = 16
for(i in 1:tt){
  dtemp = matrix(0, ncol = ww, nrow = ll)
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
  
  # conditions of changing the cells lineup
  if(i > 19){if( sum(u[i-1], u[i-2]) > 0.97 * sum(u[i-4], u[i-5]) && sum(u[i-1], u[i-2]) < 1.03 * sum(u[i-4], u[i-5]) ){
    print("yes")
    s = sample(seq(1,19), 8)
    cells = matrix(sample(s, ll*ww, replace = TRUE), ncol = ww, nrow = ll)
    cols = 'red'
  }else{cols = 'navy'}}
  
  if(i < 19){ dtemp=foo(x,d,pop); cols = "blue" }else{
      for(j in 1:ww){
        for(k in 1:ll){
          if(foo(cells[k,j], d, pop)[k,j]){dtemp[k,j] = "TRUE"}else{dtemp[k,j] = "FALSE"}
        }
      }
    }

  d = dtemp
  
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
  
  d = matrix(as.numeric(d), nrow = ll, ncol = ww)
  
  u = c(u, sum(d))  
  #cols = rep(c("red", "magenta", "blue", "cyan", "green", "yellow", "orange"),tt/6)#rainbow colors
  png(paste0("gameoflife",nnn,i,".png"), width=650, heigh=800)
  layout(matrix(c(rep(1,12),rep(2,4)), 4, 4, byrow = TRUE))
  par(mar = c(0.1,0.1,0.1,0.1))
  plot(md$key, md$variable2, pch = 15, cex = 1.5, 
       ylim = c(0,ww), xlim = c(0,ll),
       axes = "false", xlab = NA, ylab = NA, col = cols)
  plot(u, xlab = NA, ylab = NA, ylim = c(0, ll*ww), xlim = c(0,tt), type = "l")
  mtext(paste("Total Population  rule",x), 3, -1, outer=FALSE, cex = 0.8)
  dev.off()
  
  
  
}


# https://leonawicz.github.io/mapmate/articles/ffmpeg.html
library(mapmate)
library(dplyr)
library(purrr)

p <- "gameoflife%04d.png"
out <- "sequence126.gif"

ffmpeg(pattern = p, output = out, fps.out = 10, delay = 0.05, overwrite = TRUE)
dev.off()
file.remove(list.files(pattern=".png"))


# make a kaleidascope effect by using odd dimensions 99x99 and starting with all zero matrix and
# no random changing of grid cells
# even dimensions gives a rorschach effect
# even by odd dimensions gives 2-way assymmetry or rotational symmetry kind of
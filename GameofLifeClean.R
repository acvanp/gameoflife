# 8/15/2020 Alex Van Plantinga
# Conway's Game of Life

setwd("C://Users//Lenovo//Demo//gameoflifeImages") # dir for the plot images
d = read.csv("seedExcel.csv")
d = d[,-1] # in case the Excel sheet needs to be trimmed

library(reshape2) # the reshape2 melt function vectorizes the filled squares for the plot function

for(i in 1:3600){
  # vectorized game of life rules from:
  # # https://win-vector.com/2018/10/28/conways-game-of-life-in-r-or-on-the-importance-of-vectorizing-your-r-code/
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
  d <- (pop==3) | (d & (pop>=2) & (pop<=3))
  d
  
  colnames(d) = 1:nrow(d)
  rownames(d) = colnames(d)
  md = melt(d)
  md = md[which(md$value == TRUE),]
  
  # chronologicall readable filenames so the gif doesn't shuffle the images
  if(i < 10){nn = "000"} else if(i < 100){nn = "00"} else if(i < 1000){nn = "0"}else{nn = ""}
  
  png(paste0("gameoflife",nn,i,".png"), width=500, heigh=500)
  plot(md$Var1, md$Var2, pch = 15, cex = 0.6, ylim = c(0,nrow(d)), xlim = c(0,nrow(d)),
       axes = "false", xlab = NA, ylab = NA, col = "navy")
  
  dev.off()
  
}


library(magick)
library(purrr)

# use this for short gifs (<300frames)
example_gif <- list.files(path = "C://Users//Lenovo//Demo//gameoflifeImages/", pattern = "*.png", full.names = F) %>% 
  map(image_read) %>% 
  image_join() %>%
  image_animate(fps=10) %>% 
  image_write("example4.gif") 

file.remove(list.files(pattern=".png"))

# for gifs of > 300 frames
# https://leonawicz.github.io/mapmate/articles/ffmpeg.html
library(mapmate)
library(dplyr)
library(purrr)

p <- "gameoflife%04d.png"
out <- "gameoflifeGIF.gif"
# 10 fps gif
ffmpeg(pattern = p, output = out, rate = 10)
# Same as above. Set overwrite=TRUE if output exists.
ffmpeg(pattern = p, output = out, delay = 1/10, overwrite = TRUE)



# patterns
glider = matrix(c(0,1,0,
                  0,0,1,
                  1,1,1), nrow = 3)

lwss = matrix(c(0,1,0,0,1,
                1,0,0,0,0,
                1,0,0,0,1,
                1,1,1,1,0),ncol=4)#lightweight spaceship

burst = matrix(c(1,0,1,0,
             0,1,1,0,
             1,1,0,0,
             0,1,0,1), nrow = 4)

# gosper glider gun
gosper = matrix(c(0,0,0,0,1,1,0,0,0,
                  0,0,0,0,1,1,0,0,0,
                  rep(0,72),
                  0,0,0,0,1,1,1,0,0,
                  0,0,0,1,0,0,0,1,0,
                  0,0,1,0,0,0,0,0,1,
                  0,0,1,0,0,0,0,0,1,
                  0,0,0,0,0,1,0,0,0,
                  0,0,0,1,0,0,0,1,0,
                  0,0,0,0,1,1,1,0,0,
                  0,0,0,0,0,1,0,0,0,
                  rep(0,18),
                  0,0,1,1,1,0,0,0,0,
                  0,0,1,1,1,0,0,0,0,
                  0,1,0,0,0,1,0,0,0,
                  rep(0,9),
                  1,1,0,0,0,1,1,0,0,
                  rep(0,81),
                  0,0,1,1,0,0,0,0,0,
                  0,0,1,1,0,0,0,0,0)
                , nrow = 9)

library(haven)
library(magrittr)
library(stringr)
library(FactoMineR)
library(psych)
library(ggplot2)
library(heatmaply)
library(dplyr)
library(reshape2)
library(keras)
library(boot)

dat = read_sav('~/Documents/survey/pew_political/Apr17-public/Apr17 public.sav')
var.labels <- sapply(dat,  function(x){attr(x, 'label')})
var.labels <- lapply(var.labels, function(x){str_replace_all(x, ' \\[INSERT; RANDOMIZE\\]','')})
var.labels <- lapply(var.labels, function(x){str_replace_all(x, ' Have you heard … \\[READ\\]','')})
var.labels <- lapply(var.labels, function(x){str_replace_all(x, ' \\[READ IN ORDER\\]','')})
var.labels <- lapply(var.labels, function(x){str_replace_all(x, ' \\[INSERT ITEM; RANDOMIZE; OBSERVE FORM SPLITS\\]','')})
var.labels <- lapply(var.labels, function(x){str_replace_all(x, ' \\[INSERT ITEM; RANDOMIZE\\]','')})
var.labels <- lapply(var.labels, function(x){str_replace_all(x, ' \\[READ AND RANDOMIZE\\]','')})
var.labels <- lapply(var.labels, function(x){str_replace_all(x, ' \\[INSERT ITEM, RANDOMIZE ITEMS\\]','')})


var.labels[22:135]
y
ns <- names(dat)[22:135]


df <- dat %>% as.data.frame
levs <- lapply(ns, function(name){attr(dat[[name]], 'labels')})



#################################
###partial correlations
analysis <- apply(dat[,22:135], 2, function(x){
  x[x==9] <- NA
  return(x)
})
#106 3-1.5 ; 104 5-2.5; 96 3-1.5; 81-92 3-1.5 4-1.5; 72 3-1.5; 67 3-1.5; 65 3-1.5;49-64 3-1.5;
#44-46 exclude 5, 8; 18-19 3-1.5; 8-9 3-1.5
#20-43; 69,70,71 exclude
analysis[analysis[,c(106, 96, 81:92, 72, 67, 65, 49:64, 18:19, 8:9)]
==3] <- 1.5
analysis[analysis[,c(104)] ==5] <- 2.5
analysis[analysis[,c(81:92)] ==4] <- 1.5
analysis[analysis[,c(44:46)] %in% c(5,8)] <- NA
analysis <- analysis[, c(1:19, 44:68, 72:ncol(analysis))]

#################################
###factor analysis

analysis <- apply(analysis,2, function(x){
  x[is.na(x)]<- median(x, na.rm=T)
  x<- x+(runif(length(x))-.5)*.001
return(x)})

pc <- PCA(analysis,ncp=14,  row.w = dat$weight)

##############################
##cluster analysis
#I think there error here has to do with the chart, not the fit
fm <- HCPC(pc,nb.clust = 9,consol = T, graph = F)

########################
###profile the groups
qv <- descfreq(fm$data.clust[,1:(ncol(fm$data.clust)-1)],fm$data.clust$clust, proba =1)

qv <- lapply(1:length(qv), function(i){
  x <- qv[[i]]
  out <- cbind(cluster=i, v.test=x[,6], name=row.names(x))
  return(out)
  })
qv <- do.call(rbind, qv)
qv <- qv %>% data.frame
qv$question <- match(qv$name,names(var.labels))
qv$question <- factor(qv$question, levels=1:length(var.labels), labels=var.labels)

qv <- dcast(formula = question~cluster,data = qv, value.var = 'v.test')

n <- qv$question
qv <- qv[,2:10]
qv <- apply(qv, 2, as.numeric)
row.names(qv) <- n

heatmaply(qv, margins=c(100, 300),fontsize_row = 6)

###Map the segments

input = layer_input(shape=(14))

mid = input %>% 
  layer_dense(3,  activation="tanh",name='mid') %>%
  layer_dropout(.2)

output = mid %>%
  layer_dense(9, activation="softmax")

model = keras_model(inputs=list(input), outputs = output)


model %>% compile(
  loss='categorical_crossentropy', optimizer=optimizer_rmsprop(.002)
)

for.fit <- fm$call$t$res$ind$coord

for.fit <- fm$call$X[,1:14]  %>% as.matrix
for.fit <- apply(for.fit, 2, pnorm)
y <- to_categorical((fm$call$X$clust %>% as.numeric)-1, 9)

history <- model %>% fit(x = for.fit %>% as.matrix, 
                         y=y, 
                         batch_size = 512,epochs = 2000,
                         validation_split = .1, shuffle=T)

intermediate_layer_model <- keras_model(inputs = model$input,
                                        outputs = get_layer(model, 'mid')$output)
intermediate.output <- predict(intermediate_layer_model, for.fit)


saveRDS(intermediate.output, '~/Documents/survey/pew_political/positionreduction.rds')

#plot the groups
positions <- intermediate.output %>% data.frame
positions$cluster <- fm$call$X$clust  %>% factor
positions$opacity <- .5
saveRDS(positions, '~/Documents/survey/pew_political/positions.rds')

positions <- readRDS('~/Documents/survey/pew_political/positions.rds')

colors <- c('#a6cee3','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#cab2d6','#6a3d9a','#b15928')
# Create lists for axis properties
f1 <- list(
  family = "Arial, sans-serif",
  size = 18,
  color = "lightgrey")

f2 <- list(
  family = "Old Standard TT, serif",
  size = 14,
  color = "#ff9999")

axis <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

scene = list(
  xaxis = axis,
  yaxis = axis,
  zaxis = axis,
  camera = list(eye = list(x = -1.25, y = 1.25, z = 1.25)))

highlight.message1 <- lapply(1:3, function(i){which(positions$cluster==i)[[1]]}) %>% unlist
highlight.message2 <- lapply(4:6, function(i){which(positions$cluster==i)[[1]]}) %>% unlist
highlight.message3 <- lapply(7:9, function(i){which(positions$cluster==i)[[1]]}) %>% unlist

positions.message1 <- positions[highlight.message1,]
positions.message2 <- positions[highlight.message2,]
positions.message3 <- positions[highlight.message3,]




dark.colors <- c('#637b88','#6a8552','#1e601a','#965c5b','#880f10','#977242','#796a80','#6a3d9a','#582c14')
light.colors <- c('#e300ff','#b0ff00','#00ffd2','	#fdff00')

p <- plot_ly() %>%
  add_trace(data=positions, type='scatter3d', mode = 'markers',x=~X1, y=~X2, z=~X3, 
            color=~cluster,opacity=.2,colors=colors,
            marker=list(size=5, symbol = 'circle'))  %>%
  layout(title = "Current Conversations", scene = scene)
p <- p %>% add_markers(data=positions.message1, x=~X1, y=~X2, z=~X3, color=~cluster,opacity=1, colors=colors,
                  marker=list(size=6, line=list(color='black', width=3), symbol='square'))
p <- p %>%  add_markers(data=positions.message2, x=~X1, y=~X2, z=~X3, color=~cluster,opacity=1, colors=colors,
                        marker=list(size=6, line=list(color='blue', width=3), symbol='diamond'))
p <- p %>%  add_markers(data=positions.message3, x=~X1, y=~X2, z=~X3, color=~cluster,opacity=1, colors=colors,
                        marker=list(size=6, line=list(color='purple', width=3), symbol='cross'))
p
##############
##simulate within group time series
baseline.message1 <- .003
baseline.message2 <- .006
baseline.message3 <- .005

message1.performance <- c(c(-1, 0, 1), rep(0,6))
message2.performance <- c(rep(0,3),c(-1, 0, 1), rep(0,3))
message3.performance <- c(rep(0,6),c(-1, 0, 1))

message1.performance <- (logit(baseline.message1)+message1.performance) %>% inv.logit
message2.performance <- (logit(baseline.message2)+message2.performance) %>% inv.logit
message3.performance <- (logit(baseline.message3)+message3.performance) %>% inv.logit


positions$prob1 <- message1.performance[positions$cluster %>% as.integer]
positions$prob2 <- message2.performance[(positions$cluster %>% as.integer)]
positions$prob3 <- message3.performance[(positions$cluster %>% as.integer)]

#for each position, figure out what step it flips at
positions$trial <- NA
trial1 <- lapply(positions$prob1[positions$cluster %in% 1:3], function(x){
  views <- which(rbinom(20,1, x)==1)
  if(length(views)==0){
    views=0
  }else{
    views <- views[[1]]
  }
})
positions$trial[positions$cluster %in% 1:3] <- trial1 %>% unlist

trial2 <- lapply(positions$prob1[positions$cluster %in% 4:6], function(x){
  views <- which(rbinom(20,1, x)==1)
  if(length(views)==0){
    views=0
  }else{
    views <- views[[1]]
  }
})
positions$trial[positions$cluster %in% 4:6] <- trial2 %>% unlist

trial3 <- lapply(positions$prob1[positions$cluster %in% 7:9], function(x){
  views <- which(rbinom(20,1, x)==1)
  if(length(views)==0){
    views=0
  }else{
    views <- views[[1]]
  }
})
positions$trial[positions$cluster %in% 7:9] <- trial3 %>% unlist

#######
#animate the views
message.spread <- positions
message.spread <- message.spread[message.spread$trial>0,]
message.spread$message <- 1
message.spread$message[message.spread$cluster %in% 4:6] <- 2
message.spread$message[message.spread$cluster %in% 7:9] <- 3

message.spread <- message.spread[order(message.spread$trial),]
message.spread <- split(message.spread, message.spread$trial)
message.spread <- lapply(message.spread, function(x){
  x <- lapply(x$trial[[1]]:20, function(i){
    y <- x
    y$trial <- i
    return(y)
  })
  x <- do.call(rbind, x)
})
message.spread <- do.call(rbind, message.spread)
message.spread <- message.spread[order(message.spread$trial),]
message.spread <- unique(message.spread)
message.spread$message <- message.spread$message %>% as.factor()




message.spread$line.color <- c('black','blue','purple')[message.spread$message]
message.spread$fill.color <- colors[message.spread$cluster %>% as.integer]
message.spread$shape <- c('square','diamond','cross')[message.spread$message]

p <- p %>%  add_markers(data=message.spread, x=~X1, y=~X2, z=~X3, opacity=1,frame=~trial,  marker=list(size=6, 
                          color=message.spread$fill.color %>% as.character,
                          line=list(color = message.spread$line.color %>% as.character,
                                       width = 3), 
                          symbol=message.spread$shape))  
p

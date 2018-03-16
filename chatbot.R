# library(twitteR)
# library(ROAuth)
# library(httr)
library(dplyr)
library(stringr)
library(keras)
# Set API Keys
# api_key <- "wZoojHCppjHwEinKDh2oooOrO"
# api_secret <- "0wc1p2ScFdMt0m89rbxlOqP3KFCqtUCGGsDgjcusSVqufbJ3LD"
# access_token <- "965226645912281088-qgd8P8kW2NRnfNOBMBcj0bczhhd9ZD5"
# access_token_secret <- "FmEdFUh0fVJVe2OafixQKnpnwIPynHQH67NsXUGR4BvMQ"
# setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
# 
# bernie <- searchTwitter('from:BernieSanders',   retryOnRateLimit=10)

## Bernie Sanders' Account
bernie <- read.csv("https://raw.githubusercontent.com/ryanburge/pls2003_sp17/master/twitter/bernie.csv")

## Paul Ryan's Account
ryan <- read.csv("~/Documents/twitter/ryan_fromtrumparchive.csv", fill=T, quote='', stringsAsFactors = F)

## McCain
mccain <- read.csv("~/Documents/twitter/mccain_fromtrumparchive.csv", fill=T, quote='', stringsAsFactors = F)

## Elizabeth Warren's Account
warren <- read.csv("https://raw.githubusercontent.com/ryanburge/pls2003_sp17/master/twitter/warren.csv")

## Ted Crus
cruz <- read.csv("~/Documents/twitter/cruz_fromtrumparchive.csv", fill=T, quote='', stringsAsFactors = F)

## Hillary Clinton
clinton <- read.csv("~/Documents/twitter/clinton_fromtrumparchive", fill=T, quote='', stringsAsFactors = F)

####stack the tweets
ryan <- ryan$text %>% data.frame
ryan$text <- ryan$. %>% as.character
ryan$y <- 0

mccain <- mccain$text %>% data.frame
mccain$text <- mccain$. %>% as.character
mccain$y <- 1

cruz <- cruz$text %>% data.frame
cruz$text <- cruz$. %>% as.character
cruz$y <- 2

clinton <- clinton$text %>% data.frame
clinton$text <- clinton$. %>% as.character
clinton$y <- 3

all.data <- rbind(ryan, mccain, cruz, clinton)
set.seed(1)
splittter <- runif(nrow(all.data))
all.data <- all.data[order(splittter),]

#eliminate self-references
all.data$text <- str_replace_all(all.data$text, regex('cruz|clinton|hillary|trump|ted|donald|john|mccain|paul|ryan',ignore_case=T),'')
all.data$text  <- str_replace_all(all.data$text , regex('@.\\w+',ignore_case=T),'')
all.data$text  <- str_replace_all(all.data$text , regex('http\\:.*\b',ignore_case=T),'')
all.data$text  <- str_replace_all(all.data$text , regex('(https?:\\/)([A-Z0-9\\.-\\/]+)?',ignore_case=T),'')

all.data$text <- str_replace_all(all.data$text, regex(paste0(state.abb, collapse='|'),ignore_case=F),'')
all.data$text <- str_replace_all(all.data$text, regex('TEXANS',ignore_case=T),'')

all.data$text <- str_replace_all(all.data$text, '[0-9]','')
all.data$text <- str_replace_all(all.data$text, ' w/ ',' with ')
all.data$text <- str_replace_all(all.data$text, '&amp','and')


all.data$text <- str_replace_all(all.data$text, '”','')
all.data$text <- str_replace_all(all.data$text, '“','')
all.data$text <- str_replace_all(all.data$text, '…','')
all.data$text <- str_replace_all(all.data$text, '→','')
all.data$text <- str_replace_all(all.data$text, '\\\"','')
all.data$text <- str_replace_all(all.data$text, '—','')
all.data$text <- str_replace_all(all.data$text, "’s",'')
all.data$text <- str_replace_all(all.data$text, "\\'s",'')

all.data$text <- str_replace_all(all.data$text, '\\b[s|t|u|h|vo]\\b','')


#amp, rt, w,—, "”", "→","…""s","u","t",'—
#txsen, sen, 

##############
####process into words
num_words=5000
EMBEDDING_DIM=100
tk <- text_tokenizer(num_words = num_words)
tk <- fit_text_tokenizer(tk, all.data$text)

train.text <- texts_to_sequences(tk, all.data$text[splittter<.7])
#some of these end up empty after they've been cleaned
ls <- train.text %>% lapply(length) %>% unlist
train.text <- train.text[ls>=2]

####add START and END flags
# train.text <- lapply(train.text, function(x){
#   x <- c(num_words+1, x)
#   x <- c( x,num_words+2)
# })


#pad them out
#train.text <- pad_sequences(train.text, 31,value = 0)

#get pretrained embeddings
embeddings_index <- new.env(parent = emptyenv())
lines <- readLines(file.path('~/Research/glove.6B.100d.txt'))
for (line in lines) {
  values <- strsplit(line, ' ', fixed = TRUE)[[1]]
  word <- values[[1]]
  coefs <- as.numeric(values[-1])
  embeddings_index[[word]] <- coefs
}

cat(sprintf('Found %s word vectors.\n', length(embeddings_index)))
word_index <- tk$word_index
gc()
prepare_embedding_matrix <- function() {
  embedding_matrix <- matrix(0L, nrow = num_words+1, ncol = EMBEDDING_DIM)
  for (word in names(word_index)) {
    index <- word_index[[word]]
    if (index >= num_words)
      next
    embedding_vector <- embeddings_index[[word]]
    if (!is.null(embedding_vector)) {
      # words not found in embedding index will be all-zeros.
      embedding_matrix[index+1,] <- embedding_vector
    }
  }
  embedding_matrix
}
gc()
embedding_matrix <- prepare_embedding_matrix()


#######
##architect a generative model
########
#https://github.com/oswaldoludwig/Seq2seq-Chatbot-for-Keras



train.splits <- lapply(train.text, function(x){
  xpart = lapply(1:(length(x)-1), function(i){x[1:i]})
  xpart = pad_sequences(xpart, 15)
  # -1 to zero index
  ypart = to_categorical(x[2:length(x)]-1, num_classes = num_words)
  return(list(xpart, ypart))
})

r <- runif(length(train.splits))
train.splits <- train.splits[order(r)]

xs <- lapply(train.splits, function(x){x[[1]]})
#I need to repeat the author layer for as many xs as there are.
all.y <- all.data$y[splittter<.7][ls>=2]
all.y  <- all.y[order(r)]
all.y <- lapply(1:length(xs), function(i){rep(all.y[[i]], nrow(xs[[i]]))})
all.y <- all.y %>% unlist

xs <- do.call(rbind, xs)

ys <- lapply(train.splits, function(x){x[[2]]})
ys <- do.call(rbind, ys)
gc()

maxlen_input=15
maxlen_output=15

author <- to_categorical(all.y,num_classes = 4)
##############
###conversation model
##############

#this is the author
input_context = layer_input(shape=(4),  name='the_context_text')
#this is the front text
input_answer = layer_input(shape=(maxlen_input), dtype='int32', name='the_answer_text_up_to_the_current_token')


answer_embedding = input_answer %>% 
  layer_embedding(output_dim=100, input_dim=num_words+1, input_length=maxlen_input, name='Shared', trainable=T, mask_zero=T) %>%
  layer_gru(300,  kernel_initializer= 'lecun_uniform', name='Encode_answer_up_to_the_current_token', return_sequences=F) 
  #layer_conv_1d(filters = 128, kernel_size = 5, activation = 'relu') %>% 
  #layer_max_pooling_1d(pool_size = 5) %>% 
#layer_flatten()  %>%
  #layer_dense(round(num_words/2), activation="relu", name='relu_activation') 

author_expansion = input_context %>%
  layer_activation('tanh')
  

merge_layer = layer_concatenate(list(author_expansion, answer_embedding), axis=1, name='concatenate_the_embeddings_of_the_context_and_the_answer_up_to_current_token') %>%
  layer_dense(round(num_words/2), activation="relu", name='relu_activation') %>%
  layer_dense(num_words, activation="softmax", name='likelihood_of_the_current_token_using_softmax_activation')

model = keras_model(inputs=list(input_context, input_answer), outputs = merge_layer)


model %>% compile(
    loss='categorical_crossentropy', optimizer=optimizer_adam(lr=0.00005, epsilon = .1)
)


model <- load_model_hdf5('~/Desktop/tmpmodel_3.keras')
history <- model %>% fit(x = list(author, xs), 
                         y=ys, batch_size = 512,epochs = 1, validation_split = .1, shuffle=T)

save_model_hdf5(model, '~/Desktop/tmpmodel_3.keras')
#######
###try a prediction
########
tmp <- tk$word_index
tmp <- tmp %>% unlist %>% data.frame
tmp$word <- row.names(tmp)
tmp <- tmp[order(tmp$.),]
tmp <- tmp[tmp$.<=num_words,]

word='america'

test.y <- matrix(c(0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0), nrow=4)
test.word <- matrix(rep(c(which(tmp$word==word)), 4) , nrow=4,byrow = T)
test.word <- pad_sequences(test.word, 15, padding = 'pre',truncating = 'pre')

for(i in 1:25){
  p <- model %>% predict(list(test.y, test.word[,(ncol(test.word)-14):ncol(test.word)]))
  new.word <- apply(p, 1, function(row){
    index <- sample(1:length(row), 1, prob = row)
  })
  test.word <- cbind(test.word, new.word)
}


paste0("Clinton:",paste0(tmp$word[test.word[1,]], collapse=' '))
paste0("Cruz:",paste0(tmp$word[test.word[2,]], collapse=' '))
paste0("McCain:",paste0(tmp$word[test.word[3,]], collapse=' '))
paste0("Ryan:",paste0(tmp$word[test.word[4,]], collapse=' '))

cruz.quotes <- c("we have a proven record of fighting for liberty",
                 "this november is coming in our money bomb",
                 "txsen teaparty tcot txsen teaparty tcot",
                 "new radio ad in the runoff please find your polling place and choose sen crew and join the crew here sen teaparty tcot the texas",
                 "obama and a texas military and priorities for a proven leader",
                 "texas friends voting for a leader and defender of constitution",
                 "can you just early vote for every find polling place bring friends and choose sen"
)

mccain.quotes <- c("staff defense budget only gt rt when held up legislation at senate cmte hearing",
                   "syria policy is definition of doing same and as hands and other iraqi air base positive",
                   "vlad has the opportunity to use a nuclear weapon on a basic challenge to our country",
                   "today mexico former first lady de fox for convo on human trafficking",
                   "america armed forces in afghanistan deserve leadership from washington worthy of their service and sacrifice to reform in this country",
                   "turkey agrees to let us strike is from air base positive step forward in the transparency and special counsel",
                   "senator says we should wait for the senate to use russia sanctions burma to punish russia for attacking election house must quickly do"
                   )

ryan.quotes <- c("reasonable caps to lower health care costs",
                 "make sure injured patients recover full medical costs and an education for their sacrifice",
                 "a special relationship between you and all across the nation who serve on our own terms",
                 "spending cuts need to washington budget deficit",
                 "to learn more about the roadmap and veterans to the country serve betterway for america",
                 "betterway is all needed ideas",
                 "the house passed bipartisan legislation to help ensure our safety net programs we need new administration"
                 )

war <- list(cruz='war on texas', mccain='war games', ryan='war games')
we <- list(cruz='we need to protect the constitutional rights of our constitution',
           mccain='we met a guy yesterday on the water',
           ryan='we are starting today on our work')
america <- list(cruz='america stands shoulder to shoulder with to bring to the terrorists and keep our promise together we must lead the world and the white house into caucus',
                mccain='america armed forces in afghanistan deserve leadership from washington worthy of their service and sacrifice to reform and better serve our nation',
                ryan="america is the last thing our tax code is all about that is what we're giving us back to our work cut ← and cut taxes")
jobs <- list(cruz='jobs and education in the next year for millions of families',
                mccain='jobs and american air live free or die but they do not arm syria rebels',
                ryan="jobs and education bills are full ahead")
economic <- list(cruz='economic growth under this historic is not cutting economic taxes',
                 mccain='economic recovery of from new of arizona',
                 ryan="economic growth rate cuts and jobs by republican policies like to achieve financial independence")
change <- list(cruz='change is coming but there is hope for a fullrepeal',
               mccain='economic recovery of from new of arizona',
               ryan="change can we do so much for american families")
democrats <- list(cruz='democrats attacked org that defends the bill', 
                  mccain='democrats warm to defense budget', 
                  ryan='democrats just can’ defend the separation of powers')

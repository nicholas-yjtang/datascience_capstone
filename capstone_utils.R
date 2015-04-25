library(doParallel)
library(tau)
library(qdap)
library(dplyr)

clean_text <- function (original_text, swear_words) {
  new_text <- original_text  
  grep_string <- paste0("\\b[[:alnum:]]*(", paste(swear_words, collapse="|"), ")[[:alnum:]]*\\b")
  #grep_string <- paste0("(^|[[:space:]]+)([[:alnum:]])*(", paste(swear_words, collapse="|"), ")([[:alnum:]]*)([[:space:]]+|$)")  
  #print(grep_string)
  new_text <- new_text[!grepl(grep_string, new_text, ignore.case=T)] 
  new_text  
}

clean_text2 <- function (original_text, swear_words) {
  
}

fix_text <- function(original_text) {  
  new_text <- (original_text)
  #new_text <- gsub("#([[:alnum:]]+)", "\\1", new_text)
  new_text <- gsub("[\u0085]", "...", new_text)
  new_text <- gsub("[\u0093]", "\"", new_text)  
  new_text <- gsub("`", "'", new_text)
  new_text <- gsub("[\u201c]", "\"", new_text)
  new_text <- gsub("[\u2019]", "'", new_text)
  new_text <- gsub ("&#39;", "'", new_text)
  new_text <- gsub("\xe9", "e", new_text)
  new_text <- gsub ("\u00E8", "e", new_text)
  new_text <- gsub ("\u00F8", "o", new_text)
  new_text <- gsub ("\u00E6", "ae", new_text)
  new_text <- gsub ("\u00F1", "n", new_text)
  new_text <- gsub ("\u00EE", "i", new_text)
  new_text <- gsub ("\u00E4", "a", new_text)
  new_text <- gsub ("\u00F3", "a", new_text)
  new_text <- gsub ("\u00E2", "a", new_text)
  new_text <- gsub ("\u00E7", "c", new_text)
  new_text <- gsub ("\u00E1", "a", new_text)
  new_text <- gsub ("\u00EB", "e", new_text)
  #new_text <- gsub ("\xe7a", "c", new_text)
  new_text <- gsub("\xde", "", new_text)
  #new_text <- gsub("'([^[:space:]]+?)'", "\\1", new_text)    
  #new_text <- gsub("@[[:alnum:]]+", "", new_text)
  new_text <- gsub("(^rt[[:space:]]*[:]*)|([[:space:]]+rt[[:space:]]+[:]*)|rt[[:space:]]*[:]*$", " retweet ", new_text, ignore.case=T) 
  new_text <- gsub("^re:|( re: )|re:$", " retweet ", new_text, ignore.case=T)   
  new_text <- gsub("\\brt\\b", " retweet ", new_text, ignore.case=T)
  new_text <- gsub ("\\bs/o", "shout out", new_text, ignore.case=T)
  new_text <- gsub("(^|[[:space:]]+)P\\.S\\.", " PS ", new_text, ignore.case=T)
  new_text <- gsub ("(^|[[:space:]]+)b[\\/]w", " between ", new_text, ignore.case=T)
  new_text <- gsub ("(^|[[:space:]]+)b[\\/]c", " because ", new_text, ignore.case=T)  
  new_text <- gsub ("(^|[[:space:]]+)w[\\/]o", " without ", new_text, ignore.case=T)    
  new_text <- gsub("\\b(n)\\b", "and", new_text, ignore.case=T)
  new_text <- gsub("\\b(r)\\b", "are", new_text, ignore.case=T)
  new_text <- gsub("\\bw/", "with ", new_text, ignore.case=T)
  new_text <- gsub("p\\.m\\.", "pm", new_text, ignore.case=T)
  new_text <- gsub("a\\.m\\.", "am", new_text, ignore.case=T)
  new_text <- gsub("u\\.s[\\.]{0,1}", "united states", new_text, ignore.case=T)
  new_text <- gsub("\\b(u)\\b", "you", new_text, ignore.case=T)
  
  
  
  #new_text <- gsub ("shout out", "shoutout", new_text, ignore.case=T)
  #new_text <- gsub("^[_-]^|<3|\\(:|\\(;|\\(c\\)", " ", new_text)  
  #new_text <- gsub(" [-]+ ", " ", new_text)  
  new_text <- gsub("[:;8][`]*[-]*([(|)Oo\\/xXDTPp]|\\]|\\[)([[:space:]]+|$)", " ", new_text)
  new_text <- gsub("(^|[[:space:]]+)[Oo0].[Oo0]([[:space:]]+|$)", " ", new_text)
  new_text <- gsub("[d]*-[_]+-[b]*", " ", new_text)
  new_text <- gsub("\\bx(\\]|\\))", " ", new_text)  
  new_text <- gsub("\\bc[;:]", " ", new_text)  
  
  new_text <- gsub("[[:space:]]+&[[:space:]]+", " and ", new_text)
  
  #new_text <- gsub("([0-9]*),([0-9]+)", "\\1\\2", new_text)
  #new_text <- gsub("\\.[\\.]+", " ", new_text)
  #new_text <- gsub("\\[\\]+", " ", new_text)
  new_text <- gsub("[[:digit:]]+/[[:digit:]]+/[[:digit:]]+", " ", new_text)
  new_text <- gsub("/[/]+", " ", new_text)    
  new_text <- gsub("[^[:space:]]*www\\.[^[:space:]]*", " ", new_text, ignore.case=T)
  new_text <- gsub("[^[:space:]]*\\.(com|net|org|biz)", " ", new_text, ignore.case=T)
  #new_text <- gsub("([^[:alnum:]])(\\.[[:space:]]$)", "with", new_text)      
  #new_text <- gsub("[@!?\"*,<>]", " ", new_text)
  new_text <- gsub("\\bi[[:space:]]+m\\b", " i'm ", new_text, ignore.case=T)
  new_text <- gsub("\\b[[:digit:]]+[[:alpha:]]*\\b", "", new_text)
  #new_text <- gsub("[[:alnum:]]+:", "", new_text) #conversation?
  #new_texst <- gsub("(^|[[:space:]]+)([-]+([[:space:]]+|$))+", " ", new_text)
  #remove most of the punctuation
  new_text <- gsub("[^[:alnum:][:space:]'\\.!?,]", " ", new_text)
  new_text <- gsub("([\\.!?,]+)([[:alnum:]]+)", "\\1 \\2", new_text)
  new_text <- gsub("([[:alnum:]]+)([\\.!?,]+)", "\\1 \\2", new_text)  
  new_text <- gsub("^('s)+|[[:space:]]+('s)+", " ", new_text)
  new_text <- gsub("^[']+", " ", new_text)
  new_text <- gsub("[[:space:]]+'(.*?)'", " \\1", new_text)
  #new_text <- gsub ("(^|[[:space:]]+)'([[:space:]]+|$)", " ", new_text)  
  #new_text <- gsub ("[']+([[:alnum:]]*)", "\\1", new_text)
  new_text <- gsub("(^|[[:space:]]+)[']+([^[:space:]']*)", "\\1\\2", new_text)
  new_text <- gsub("\\b([^[:space:]]*[[:digit:]]+[^[:space:]]*)\\b", "", new_text)  
  new_text <- gsub("([[:alnum:]]+)'(ve|m|s)\\b", "\\1", new_text)  #remove the 's, 'm, 've etc
  new_text <- tolower(new_text)
  new_text
}

read_lines <- function(fname) {
  conn = file(fname, open="rb", encoding="UTF-8")
  lines = readLines(conn, skipNul=T, encoding="UTF-8")
  close(conn)
  lines
}

get_swear_words <- function () {
  conn <- file("data/swear.txt", open="rb")
  swear_words <- readLines(conn)
  close(conn)
  swear_words
}

sampling <- function (sample_size, sample_rate) {
  set.seed(12345)
  which(rbinom(sample_size, 1, sample_rate) %in% c(1))  
}

data_processing <- function (fname, sample_rate = 0.05) {
  original <- read_lines(fname)
  sample_index = sampling(length(original), sample_rate) #which (rbinom (length(original),1,sample_rate) %in% c(1)) 
  swear_words = get_swear_words()
  cleaned_data <- parallel_cleaning(original[sample_index], swear_words)
  cleaned_data 
}

parallel_cleaning <- function(original_text, swear_words) {
  print("start cluster")
  num_clusters = 6
  cl <- makeCluster(num_clusters)
  registerDoParallel(cl)
  results = NULL
  try(
  results <- foreach(i=1:num_clusters, .combine='c', .export=c("clean_text", "fix_text")) %dopar% {
    start_index = (i-1)/num_clusters * length(original_text) + 1
    end_index = i/num_clusters * length(original_text)
    fix_text(clean_text(original_text[start_index:end_index], swear_words))
  }) 
  stopCluster(cl)
  print("end cluster")  
  results  
}

write_data <- function (data, fname) {
  conn = file(fname, open="wb", encoding="UTF-8")
  write(data, file=conn)
  close(conn)
}



unique_words_percentage <- function(df, percentage) {
  total_count = sum(df$count) * percentage
  current_count = 0
  unique_words = 0
  for(i in 1:nrow(df)) {
    if (current_count < total_count) {
      current_count = current_count + df[i,"counts"]
      unique_words = unique_words + 1
    }
    else {
      break
    }
  }  
  unique_words
}

remove_stopwords_single <- function (original_text, sep=FALSE) {
  rm_stopwords(original_text, tm::stopwords("english"), separate=sep, strip=TRUE)
}

remove_stopwords <- function(original_text, sep=FALSE) {
  print("start remove_stopwords cluster")
  num_clusters = 6
  cl <- makeCluster(num_clusters)
  registerDoParallel(cl)
  results = NULL
  try(  
    results <- foreach(i=1:num_clusters, .combine='c', .export=c("rm_stopwords")) %dopar% {
      start_index = (i-1)/num_clusters * length(original_text) + 1
      end_index = i/num_clusters * length(original_text)
      rm_stopwords(original_text[start_index:end_index], tm::stopwords("english"), separate=sep, strip=TRUE)      
    })
  stopCluster(cl)
  print("end remove_stopwords cluster")    
  results
}

calculate_probability_1 <- function (ngram_df, word1, word2) {  
  #P(word2|word1)
  total_word1 <- sum(ngram_df[grepl(paste0("^", word1," .*$"), ngram_df$word), "counts"])
  #print(sprintf("total word1=", total_word1))
  total_word1nword2 <- sum(ngram_df[grepl(paste0("^", word1," ", word2, "$"), ngram_df$word), "counts"])
  #pword2 <- sum(ngram_df[grep])
  print(sprintf("P(%s|%s) = %f", word2, word1, total_word1nword2/total_word1))
  #print(total_word1nword2)
  total_word1nword2/total_word1
}

calculate_probability_2<- function (ngram_df, word1, word2, word3) {  
  #P(word3|word1 and word2)
  print(sprintf("word1=%s,word2=%s", word1, word2))
  total_word1 <- sum(ngram_df[grepl(paste0("^", word1," ",word2, " .*$"), ngram_df$word), "counts"])
  #print(sprintf("total word1=", total_word1))
  total_word1nword2 <- sum(ngram_df[grepl(paste0("^", word1," ", word2, "$"), ngram_df$word), "counts"])
  print(sprintf("P(%s|%s)=%f", word1, word2, total_word1nword2/total_word1))
  #print(total_word1nword2)
  total_word1nword2/total_word1
}


predict_words_1 <- function (ngram_df, phrase, choices) {
  history = remove_stopwords(phrase, sep=T)
  #print(history)
  size = length(history[[1]])
  choice_prob = 0
  while (sum(choice_prob) == 0 && size != 0) {
      word1 = history[[1]][size]
      choice_prob = sapply (choices, FUN=calculate_probability_1, ngram_df=ngram_df, word1=word1)
      size = size - 1
  }
}

predict_words_2 <- function (ngram_df, phrase, choices) {
  history = remove_stopwords(phrase, sep=T)
  #print(history)
  size = length(history[[1]])
  choice_prob = 0
  while (sum(choice_prob) == 0 && size != 1) {
    word1 = history[[1]][size]
    word2 = history[[1]][size-1]
    choice_prob = sapply (choices, FUN=calculate_probability_2, ngram_df=ngram_df, word1=word1, word2=word2)
    size = size - 1
  }
}

paste_token_till <- function (text_tokens, num) {
  return_token = paste(text_tokens[1:num], collapse=" ")
  #print(return_token)
  return_token
}

get_token_at <- function (text_tokens, num) {
  #print(text_tokens)
  return_token = text_tokens[num]
  #print(return_token)
  return_token
}

text_count <- function (text_data, ngram_count) {
  split_string="[[:space:]]+"
  count = textcnt(text_data,method="string",n=ngram_count,split=split_string)
  #output_word = rep(1:length(text_data))
  original_words = names(unclass(count))
  counts = as.integer(unclass(count))
  tokens = strsplit(original_words, "[[:space:]]+")
  token_length = length(tokens)
  #print(token_length)
  input_word = rep("", token_length)
  if (ngram_count>1) {
    input_word = unlist(lapply(tokens, paste_token_till, num=(ngram_count-1)))
  }
  output_word = unlist(lapply (tokens, get_token_at,num=ngram_count))
  df = data.frame(counts=counts, word=original_words, input_word=input_word, output_word=output_word, stringsAsFactors=F)
  df
}

create_ngram <- function(text_data, ngram_count) {
  list_count = lapply(text_data, text_count, ngram_count=ngram_count)
  final_df = rbind_all(list_count)
  summarise(group_by(final_df, word,input_word, output_word), counts=sum(counts)) #%>% arrange(desc(counts))
}

construct_all_ngram <- function(data, ngram_num=4, vocab = NULL, isRemoveStopwords = FALSE) {
  data_clean = data
  sapply (1:ngram_num, construct_ngram, original_data=data_clean, vocab = vocab, isRemoveStopwords=isRemoveStopwords)
}

construct_vocab <- function (original_data, isRemoveStopwords = FALSE) {
  if (isRemoveStopwords) {
    data = remove_stopwords(original_data)
  }
  results = construct_ngram_parallel (original_data, 1)
  filter(results, !grepl("[\\.,!?]", word))  %>% group_by(word)   %>% summarise(counts=sum(counts))
  #summarise(group_by(results, word), counts=sum(counts)) %>% filter(!grepl("[\\.,!?]", word))
}

construct_ngram_parallel <- function (data, ngram_num) {
  print("start construct_ngram cluster")
  num_clusters = 6
  cl <- makeCluster(num_clusters)
  registerDoParallel(cl)
  results = NULL
  try(  
    results <- foreach(i=1:num_clusters, .combine='rbind_list', .export=c("create_ngram","summarise", "paste_token_till", "get_token_at", "group_by", "text_count", "rbind_all", "rm_stopwords", "textcnt")) %dopar% {
      start_index = (i-1)/num_clusters * length(data) + 1
      end_index = i/num_clusters * length(data)
      create_ngram(data[start_index:end_index], ngram_num)      
    })
  stopCluster(cl)
  print("end construct_ngram cluster")
  as.data.frame(results)
}

replace_ngram_with_vocab <- function (original_data, ngram_num, vocab) {
  
  original_data
  
}

find_index <- function (vocab, word) {
  #print(word)
  index = which(vocab$word == word)
  if (length(index)==0) {
    index = 0
  }
  index
}

find_index_tokens <- function (vocab, word) {
  tokens = unlist(strsplit(word, "[[:space:]]+"))  
  as.vector(sapply (tokens, find_index, vocab=vocab))
}

combine_tokens <- function (text_token, num) {  
  unlist(lapply(1:length(text_token), FUN=function(x){text_token[[x]][num]} ))
}

mutate_dataframe <- function (num, results, list) {
  col_name = paste0("word_index_", num)
  #print(col_name)
  #print(list)
  results[,col_name] = list
  results
}

construct_index <- function(results, vocab, ngram_num) {
  if (!is.null(vocab)) {
    tokens = strsplit(results$input_word, "[[:space:]]+")
    #print(unlist(tokens))
    list = lapply(tokens, find_index_tokens, vocab=vocab)
    #print(list)
    for (i in 1:(ngram_num-1)) {
      results = mutate_dataframe(i, results, combine_tokens(list,i))
    }
    results[,"output_word_index"] = find_index_tokens(vocab, results$output_word)
    results = as.data.frame(results)
    results = results %>% select (-word,-input_word, -output_word)
  }  
  results
}

construct_index_parallel <- function (sub_results, vocab,ngram_num) {
  
  print("start construct_index_parallel cluster")
  num_clusters = 6
  cl <- makeCluster(num_clusters)
  registerDoParallel(cl)
  results = NULL
  try(  
    results <- foreach(i=1:num_clusters, .combine='rbind_list', .export=c("select", "%>%", "find_index", "find_index_tokens", "construct_index", "mutate_dataframe", "strsplit", "combine_tokens")) %dopar% {
      start_index = (i-1)/num_clusters * nrow(sub_results) + 1
      end_index = i/num_clusters * nrow(sub_results)
      construct_index(sub_results[start_index:end_index,], vocab, ngram_num)     
    })
  stopCluster(cl)
  print("end construct_index_parallel cluster")    
  results
  
  
}

construct_ngram <- function (original_data, ngram_num, vocab = NULL, isRemoveStopwords = FALSE) {
  
  data = original_data
  if (isRemoveStopwords) {
    data = remove_stopwords(original_data)
  }

  #if (ngram_num>1) {
  #  data = paste(rep("<s>", ngram_num-1), original_data, rep("</s>", ngram_num-1))
  #}
  results = construct_ngram_parallel(original_data, ngram_num)
  results = filter(results, !grepl("[\\.,!?]", word)) %>% group_by(word, input_word, output_word) %>% summarise(counts=sum(counts)) %>% arrange(desc(counts)) 
  results = results %>% group_by(input_word) %>% filter(counts==max(counts)) %>% distinct()
  print("results done!")
  if (!is.null(vocab)) {
    results = construct_index_parallel(results, vocab, ngram_num)
  }
  as.data.frame(results)
}

predict_next_word_recusive <- function (ngram_data, ngram_no, text_tokens) {
  tokens = c()
  results = ""  
  num_tokens = length(text_tokens)
  #print(num_tokens)
  if (num_tokens == 0) {
    results = ngram_data["word", 1][[1]][1] #return the most frequent word
  }
  else if (num_tokens >= ngram_no-1) { 
    #print(ngram_no)
    for(i in 1:(ngram_no-1)) {
      tokens = c(text_tokens[num_tokens-i+1],tokens)        
    }
    grepstring = paste0("^", paste(tokens,collapse=" "), " ")
    #print(grepstring)
    #print(ngram_no)
    results_row = ngram_data["word", ngram_no][[1]][grepl(grepstring, ngram_data["word",ngram_no][[1]])][1]
    if (is.na(results_row)) {
      if (ngram_no != 1) {
        results = predict_next_word_recusive(ngram_data, ngram_no-1, text_tokens)      
      }
    }
    else {
      results = gsub(grepstring,"", results_row)
    }
  }
  else {
   results = predict_next_word_recusive(ngram_data, ngram_no-1, text_tokens)
  }
  results  
}

s_filter = function(.data, ...) {
  eval.string.dplyr(.data,"filter", ...)
}

eval.string.dplyr = function(.data, .fun.name, ...) {
  args = list(...)
  args = unlist(args)
  code = paste0(.fun.name,"(.data,", paste0(args, collapse=","), ")")
  df = eval(parse(text=code,srcfile=NULL))
  df  
}

predict_next_word_recusive_index <- function (ngram_data, ngram_no, text_tokens, vocab) {
  tokens = c()
  results = ""  
  num_tokens = length(text_tokens)
  #print(num_tokens)
  if (num_tokens == 0 || ngram_no == 1) {
    #print("finally")
    results = vocab$word[ngram_data[[1]]$output_word_index] #return the most frequent word
  }
  else if (num_tokens >= ngram_no-1) { 
    #print(ngram_no)
    filter_vector = c()
    for(i in 1:(ngram_no-1)) {
      current_token = text_tokens[num_tokens-i+1]
      current_index = find_index(vocab=vocab, word = current_token)
      #tokens = c(text_tokens[num_tokens-i+1],tokens)        
      filter_vector = c(filter_vector, paste0("word_index_", ngram_no -i, "==", current_index))
    }
    result_search_string = paste0(filter_vector, collapse="&")
    print(result_search_string)
    results_row = s_filter (ngram_data[[ngram_no]], result_search_string)   
    #print(results_row)
    #  ngram_data["word", ngram_no][[1]][grepl(grepstring, ngram_data["word",ngram_no][[1]])][1]
    if (nrow(results_row)==0) {      
      if (ngram_no != 1) {
        results = predict_next_word_recusive_index(ngram_data, ngram_no-1, text_tokens, vocab)      
      }
    }
    else {
      print("getting the results")
      results = vocab$word[results_row[1,"output_word_index"]]
    }
  }
  else {
    results = predict_next_word_recusive_index(ngram_data, ngram_no-1, text_tokens, vocab)
  }
  results  
}

predict_next_word<- function (ngram_data, text, ngram_num=4, vocab=NULL, isRemoveStopwords = FALSE) {
  test_text <- (fix_text(text))
  if (isRemoveStopwords) {
    test_text = remove_stopwords_single(test_text)
  }
  split_string="[[:space:]$,.;:\"]+"
  text_tokens = strsplit(test_text, split_string)[[1]]
  if (is.null(vocab)) {
    results = predict_next_word_recusive(ngram_data, ngram_num, text_tokens)
  }
  else {
    results = predict_next_word_recusive_index(ngram_data, ngram_num, text_tokens, vocab)
  }
  #print(results)
  results
}


predict_next_word_accuracy <- function (ngram_data, original_text,vocab=NULL) {
  new_text = gsub("[^[:alnum:][:space:]']", "", original_text)
  split_string="[[:space:]]+"
  text_tokens = strsplit(new_text, split_string)[[1]]
  tokens_length = length(text_tokens)
  text = paste0(text_tokens[1:(tokens_length-1)], collapse=" ")
  #print (text)
  actual_result = text_tokens[tokens_length]
  #print(actual_result)
  result = predict_next_word (ngram_data, text,vocab=vocab)
  if (result == actual_result) {
    print("correct with " )
  }
  (result == actual_result)  
}

total_correct <- function (ngram_data, text, vocab=NULL) {
  print("total_correct started")
  sum(sapply(text, predict_next_word_accuracy, ngram_data=ngram_data, vocab=vocab))
}

total_accuracy <- function (ngram_data, text) {
  print("start total_accuracy_parallel cluster")
  num_clusters = 2
  cl <- makeCluster(num_clusters)
  registerDoParallel(cl)
  results = NULL
  try(  
    results <- foreach(i=1:num_clusters, .combine='rbind_list', .export=c("total_correct","predict_next_word_accuracy","predict_next_word", "predict_next_word_recusive","remove_stopwords_single", "fix_text", "rm_stopwords", "textcnt")) %dopar% {
      start_index = (i-1)/num_clusters * length(text) + 1
      end_index = i/num_clusters * length(text)
      total_correct(ngram_data, text[start_index:end_index])      
    })
  stopCluster(cl)
  print("end total_accuracy_parallel cluster")  
  results/length(text)
}


NgramTree <- setClass(
    "NgramTree",
    slots = c(
      word = "character",
      ngram_1_freq = "numeric",
      ngram_2_freq = "numeric",
      ngram_3_freq = "numeric",
      children = "list"
      )    
)

setGeneric(name="addChildren",
           def=function(theObject,children)
           {
             standardGeneric("addChildren")
           }
)

setMethod(f="addChildren",
          signature="NgramTree",
          definition=function(theObject,children)
          {            
            theObject@children
          }
)


createNgram <- function (current_node, children) {
  node_children = current_node$children
  
}

createNgramTree <- function (text_data) {
  split_string="[[:space:]]+"
  text_tokens = strsplit(new_text, split_string)  
  root = NgramTree("root")
  
}

calculate_probability <- function (unigram, bigram, text) {
  new_text <- paste("<s>", fix_text(text), "</s>")
  split_string="[[:space:]]+"
  text_tokens = strsplit(new_text, split_string)
  
}

predict_next_word_ngram <- function (ngram_data, ngram_no, text, isRemoveStopwords = FALSE) {
  new_text = fix_text(text)
  if (isRemoveStopwords) {
    new_text = remove_stopwords_single(new_text)
  }
  split_string="[[:space:]]+"
  text_tokens = strsplit(new_text, split_string)[[1]]
  num_tokens = length(text_tokens)
  tokens = c()
  for(i in 1:(ngram_no-1)) {
    print(num_tokens-i+1)    
    tokens = c(text_tokens[(num_tokens-i+1)],tokens)        
  }    
  print(tokens)
  grepstring = paste0("^", paste(tokens,collapse=" "), " ")
  print(grepstring)
  results_row = ngram_data[grepl(grepstring, ngram_data$word),]
  print(results_row)
  results_row %>% arrange(desc(counts))
}

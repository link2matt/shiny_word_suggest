##Shiny Lecture 2
##Script 3
##Submit Action Button

library(xtable)

load(file = "data/bi.smooth.df.RData")
load(file = "data/tri.smooth.df.RData")
load(file = "data/uni.smooth.df.RData")
load(file = "data/uni.counts.smooth.RData")

# top.five <- as.data.frame(c("the", "and", "to", "a", "of"))
top.five <- as.data.frame(c(0.04572683, 0.02639678, 0.02318224,  0.02284628 , 0.01917777 ))
top.five[,2] <- c("the","to", "and",  "a", "of")
colnames(top.five)  <- c("uni.prob", "unigram")
##


clean.text <- function(text.input){
  text.input <- tolower(text.input)
  # text.input <- gsub(pattern = " ", " ", x = text.input)
  text.input <- gsub(pattern = "\\." , " ", x = text.input)
  text.input <- gsub(pattern = ",", " ", x = text.input)
  text.input <- gsub(pattern = "\\!", " ", x = text.input)
  text.input <- gsub(pattern = "-", " ", x = text.input)
  text.input <- gsub(pattern = "\\?", " ", x = text.input)
  #text.input <- gsub(pattern = "* ", " ", x = text.input)
  text.input <- gsub(pattern = "[^a-zA-Z ]", "", x = text.input)
  word.two <- paste0(tail(strsplit(text.input,split=" ")[[1]],2))
  if(!is.na(word.two[2])) { output <- paste0(word.two[1], "_", word.two[2]) }
     else { output <- word.two[1] }
  output
}

## Backoff with just leftore over probability
##
backoff.alpha.bi <- function(bigram.input){
  backoff <- (1 - (sum(tri.smooth.df[(tri.smooth.df$bigram1 == bigram.input),3], na.rm = T))) 
  if (backoff == 0) {backoff <- 1}
  backoff
}


## PREDICT FROM TRIGRAMS

## PREDICT FUNCTION WITH 2 WORD INPUT

bi.predict <- function(bigram.input) {
        trigram.match <- NULL
        word.minus.one <- sub(pattern = "_.*", replacement = "", x = bigram.input)
        word.last <- sub(pattern = ".*_", replacement = "", x = bigram.input)
        trigram.match <- tri.smooth.df[tri.smooth.df$bigram1 == bigram.input,]
        trigram.match <- trigram.match[order(-trigram.match$probGT),]
        finals <- NULL
        finals.matrix <- matrix(data = NA, nrow = 3, ncol = 3)
        words.picked <- 0
        potentials <- NULL
        if(!is.na(trigram.match[1,1])) {
                for (i in 1:nrow(trigram.match)) {
                        if (words.picked < 3) {
                                potentials <- rbind(potentials, trigram.match[i, ])
                                ## next.row <- 
                                if(is.na(trigram.match[i + 1, 3]) | trigram.match[i,3] > trigram.match[i + 1, 3]) {
                                        potent2 <- potentials
                                        potentials <- NULL
                                        colnames(potent2) <- c("tri.gram", "bigram1", "probGT")
                                        potent2 <- potent2[order(-potent2$probGT), ]
                                        
                                        for (j in 1:nrow(potent2)){
                                                if (words.picked <3 ){
                                                        finals <- c(finals, sub(pattern = ".*_", replacement = "", x = potent2[j, 1]))
                                                        words.picked <- words.picked + 1
                                                        finals.matrix[words.picked, 1] <- sub(pattern = ".*_", replacement = "", x = potent2[j, 1])
                                                        finals.matrix[words.picked, 2] <- "trigram"
                                                        finals.matrix[words.picked, 3] <- potent2[j, 3]
                                                }
                                        }
                                }
                        }
                } ##End of checking trigrams returned loop
        } ##End of trigram.match conditional
        
        if (words.picked < 3){
                ## What happens if first word not in dataset?
                if(!is.na(uni.counts.smooth[word.minus.one][[1]])) {
                        backoff.alpha.first <- uni.smooth.df[uni.smooth.df$unigram == word.minus.one, 3]
                } else {backoff.alpha.first <- 1}
                bigram.match <- bi.smooth.df[bi.smooth.df$uni.x == word.last,]
                bigram.match <- bigram.match[order(-bigram.match$probGT), ]
                potent3 <- NULL
                if(!is.na(bigram.match[1,1])) {
                        for (i in 1:nrow(bigram.match)){
                                if (words.picked < 3){
                                        potent3 <- rbind(potent3, bigram.match[i, ])
                                        if(is.na(bigram.match[i + 1, 4]) | bigram.match[i,4] > bigram.match[i + 1, 4]) {
                                                potent4 <- potent3
                                                potent3 <- NULL
                                                potentials <- NULL
                                                for (k in 1:nrow(potent4)){
                                                        i
                                                        potent4[k, 5] <- uni.smooth.df[uni.smooth.df$unigram == potent4[k, 3], 1]
                                                }
                                                colnames(potent4) <-c( "bi.gram","uni.x","uni.y","probGT","y.prob")
                                                potent4 <- potent4[order(-potent4$y.prob), ]

                                                
                                                for (j in 1:nrow(potent4)){
                                                        if (words.picked <3 & !is.element( sub(pattern = ".*_", replacement = "", x = potent4[j, 1]), finals)){
                                                                
                                                                finals <- c(finals, sub(pattern = ".*_", replacement = "", x = potent4[j, 1]))
                                                                words.picked <- words.picked + 1
                                                                finals.matrix[words.picked, 1] <- sub(pattern = ".*_", replacement = "", x = potent4[j, 1])
                                                                finals.matrix[words.picked, 2] <- "bigram"
                                                                finals.matrix[words.picked, 3] <- potent4[j, 4] * backoff.alpha.first

                                                        }          
                                                        
                                                }
                                        }
                                }
                        }
                } ## of IF there are bigram matches 
        } ## End of looking for matching bigrams
        
        if (words.picked < 3){
                backoff.alpha.bigram <- backoff.alpha.bi(bigram.input)
                for(k in 1:5){
                        if (words.picked <3 & !is.element(top.five[k,2], finals)){
                                finals <- c(finals, top.five[k,2])
                                words.picked <- words.picked + 1
                                finals.matrix[words.picked, 1] <- top.five[k,2]
                                finals.matrix[words.picked, 2] <- "unigram"
                                finals.matrix[words.picked, 3] <- top.five[k,1] * backoff.alpha.bigram

                                
                        }
                }
        } ## END OF TOP FIVE FOR LOOP
        colnames(finals.matrix) <- c("Suggestion", "Match", "Probability")
        finals.matrix[,3] <- round( as.numeric(finals.matrix[,3]), digits =  5)
        finals.matrix
} ## End of bi.predict function



#
#
# E N D *** E N D *** E N D ****
## End of Verbose Function

#
#





shinyServer(
  function(input, output) {
    output$text1 <- renderText({input$text1})
    rv <- reactiveValues (data1 = "startabcdefg")


  #    input$goButton ##command below is conditional on button being pressed
   #   isolate(bi.predict(paste0(input$text1, input$text2, sep = "_"))[1,1]) ##isolate keeps this command from running until button
   #         isolate ( data1 <- reactive ({ bi.predict(input$text1) })  ) ##isolate keeps this command from running until button

#     output$matrix <- renderUI({
# 
#             M <- print(xtable(bi.predict(paste0(input$text1, sep = "_", input$text2)), digits = 5), floating=F, tabular.environment="array", comment=F, print.results=F)
#             html <- paste0("$$", M, "$$")
#             list(
#                     withMathJax(HTML(html))
#             )
#   }
# )
#     
    observeEvent (input$goButton, { rv$data1 <- clean.text(input$text1) })
     output$matrix1 <- renderTable(expr = bi.predict(rv$data1) )
  }
)
    
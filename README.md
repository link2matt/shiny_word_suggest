# shiny_word_suggest
Coursera Data Science Capstone Project

## The Problem of Word Suggestion

What's the best way to recommend words to assist in writing? As part of the Coursera Data Science Capstone course, I researched natural language processing to create an app that suggested words.

I used a dataset of English from blogs, news articles and tweets, supplied for the class. I ended up using approximately 10% of the dataset to build a model of English, which would be used to predict word occurrence.

## The Model

My model was based on examining n-grams, combinations of one or more words. I ended up calculating the occurrence of unigrams, bigrams and trigrams in the dataset.

My dataset had over 10 million words in it with a vocabulary of approximately 200,000 words. In order to minimize the size of model in both computer storage and memory, bigrams or trigrams which occurred less than 5 times were removed from my dataset.

## Prediction

The principle of maximum likelihood estimation (MLE) was used to analyze the dataset. With language, the concept is that the preceding word or words can be use to generate a probability of what words would come next.

In general, the more words you can analyze in sequence the better your chances or predicting the next word. However, sometimes prediction has to be generated on novel words that haven’t been analyzed before. In order to facilitate the prediction of novel words or word combinations, I used a Katz backoff function.

## The App

Please visit my app, hosted on shinyapps.io. It’s easy to use. By entering English words, the app offers three suggestions for a following word. The suggestions have a probability for how likely they are to follow the sequence of words provided, as well whether the suggested word comes from a match from my model of trigrams, bigrams or unigrams.

The code used to run the app as well as the data objects that represent my model are on Github.

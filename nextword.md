Next Word Prediction Shinyapp
========================================================
author: Tianzi Harrison
date: August 17, 2015
transition: rotate
css: united.css

About the App
========================================================

This is a simple next word prediction that tries to predict the next word after the user inputs a sentence or a phrase in English. 

The prediction is triggered when the user inputs some text and click the **Go** button. 

***

![myapp] (myapp.png)


It [runs](https://tianzih.shinyapps.io/nextword) on shinyapps.io by RStudio. 

Under the Hood
========================================================



Input text gets parsed into words first. 

For instance, `Reason #5 - There is an outmoded (and wholly inaccurate) belief `  becomes  "***reason there's an out-moded and wholly inaccurate belief*** ".

Any numbers and punctuations are removed, except for intra-word apostrophes and dashes. This then gets fed into my predictive function. 

Under the Hood - Continued
========================================================

The predictive function predicts on the last three words of the text. 

It goes into an ngram library that's pre-built using sampled Twitter, news, and blogs text from [HC Corpora](http://www.corpora.heliohost.org/). The library consists of a dictionary, a bigram, a trigram, and a fourgram lookup table. 

A simple backup method is implemented for the function to find the most frequent fourgrams using three words, then the most frequent trigrams using two words, and so on. ***Counts are converted into probabilities for each lookup word using the formula on the next slide.***

Performance
========================================================

![myformula] (myformula.jpg)

Here is how my predictive function performs using [a benchmark test](https://github.com/hfoffani/dsci-benchmark) created by a former TA of the class.

![mybenchmark](mybenchmark.png)

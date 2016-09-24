
# User interface code for Next-texT

library(shiny)

shinyUI(fluidPage(

   h1("Next-texT - The Next Word Predictor",style = "color:blue"),
  #img(src = "logo.png", height = 72, width = 72),
  p("Welcome to Next-texT the application which predicts the next word you are going to type in a sentence."),
  p("Type your words into the text box below and click submit. 
     Next-texT then predicts the top five words with the highest probability of occuring after
    your text. The highest probability words are often 'stop-words' like 'the', 'to' and 'I'.
    A novel aspect of Next-texT is that it also predicts the next five highest probability words which 
    are not 'stop-words'. So ten predictions are provided in total."),
  textInput("text", "Please enter your text below and press submit to get your prediction:", value = "Predicting text with Next-texT is so much",width = '100%'),
  submitButton("Submit"),
  br(),
  p("Top 5 predictions:"),
  verbatimTextOutput("text1"),
  p("Next 5 'non-stop word' predictions:"),
  verbatimTextOutput("text2"),
  br(),
  h3("About Next-texT",style = "color:blue"),
  p("Next-texT in based on an n-gram model comprising 1,2,3 and 4 ngrams derived from a publically available corpus. For an understanding
    of Ngram models there are excellent lecture notes available from the University of Illinois at this link:"),
  a("http://l2r.cs.uiuc.edu/~danr/Teaching/CS546-09/Lectures/Lec5-Stat-09-ext.pdf"),
  br(),
  p("Next-texT uses a 'back off' algorithm to handle words you type in that it does not have stored probabilities for. 
    In tests on 200,000 test sentences the model acheived an average prediction accuracy rate of 29%,
    taking on average less then half a second computation time per prediction. The application is written in the R
    programming language and all of the code to reproduce this application, as well as a short presentation
    on how it works, is available at:"),
  a("https://github.com/ermckinnon/CapstoneProject"),
  h3("Sources/ Attributions",style = "color:blue"),
  p("Next-texT has been trained and tested on a corpus called HC Corpora (www.corpora.heliohost.org) which is available  
    at the following link:"),
  a("http://www.corpora.heliohost.org/aboutcorpus.html"),
  p("In addition Next-texT has been enhanced with word occurance probabilities derived from an additional publically available Corpus
    at the following link:"),
  a("http://corpus.byu.edu/full-text/")
))

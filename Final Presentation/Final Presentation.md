<style>
.small-codevpre code {
  font-size: lem;
}
</style>
Data Science Capstone Project
========================================================
![alt text](logo.png)

Text Prediction Application in Shiny - [Next-texT] (https://ermckinnon.shinyapps.io/NEXT-TEXT/)   
Ewen McKinnon, October 2016   


1. Project Background
========================================================
type: sub-section
## Challenge <small>  
- Develop a text prediction application which predicts the next word in a sentence of text typed in by a user.  
- The app must offer a choice of predictions.  
- It should provide results in a reasonable time and operate within the typical memory constraints of mobile phones.</small>    

***

## Approach <small> 
- Use freely available corpus [HC Corpora] (http://www.corpora.heliohost.org/aboutcorpus.html) which includes:  
-- 2.4 million lines of tweets    
-- 77 thousand lines of news  
-- 899 thousand lines of blogs
- Use 70% of corpus for training and 30% for testing  
- Compare models on prediction accuracy, prediction time and memory usage   
- Implement the best model as a [shiny] (http://shiny.rstudio.com) application </small>

2. Next-texT explained
========================================================
type: sub-section
- [Next-texT] (https://ermckinnon.shinyapps.io/NEXT-TEXT/) predicts text based on stored n-gram tables (see box below). The n-gram tables are developed from frequency counts within the training corpus.  
![alt text](modeldiagram.png)

3. Next-texT Performance
========================================================
type: sub-section

- Models were tested on 200 runs of 100 predictions  
- Graphs show performance vs two of the many other models tested A and B  
- [Next-texT] (https://ermckinnon.shinyapps.io/NEXT-TEXT/) has an average accuracy of 29.23%, takes 0.5secs per prediction, and uses 232 Mbytes of memory
![alt text](modelperformance.png)

4. Using the Application
========================================================
type: sub-section
- The application can be found here: [Next-texT] (https://ermckinnon.shinyapps.io/NEXT-TEXT/)  
- The user interface is shown below:  
![alt text](userinterface.png)


5. Lessons and References
========================================================
### Project Lessons
- Developing the n-gram tables required 16GBytes of RAM to process half of the Corpus - it is memory intensive!   
- Time & memory performance is enhanced by cutting low frequency n-grams from tables - Next-texT uses n-grams that occur >20 times in the training Corpus   
- Next-Text could be enhanced with i. further tuning of back-off weights, ii. start-of-sentence identifiers  

### References
- [Large Language Models in Machine Translation (2007)](http://www.aclweb.org/anthology/D07-1090.pdf)  
- ngram tables are enhanced by [BYU Corpus] (http://corpus.byu.edu/full-text/) probabilities
- All code is available at this [github](https://github.com/ermckinnon/CapstoneProject) link

# Project NLP

Project NLP or **PNLP** will be my playground to learn more about text mining and machine learning (ML) in Natural Language Processing (NLP). My background before this project was mainly R language, and some text mining abilities derived from my previous work in Bond, analyzing social media comments. I chose the [Quora Question Pair](https://www.kaggle.com/c/quora-question-pairs) competition in Kaggle as my dataset to experiment and learn. 

# Timeline and experimentation

The first 2 months of the competition I was interested in learning more about text mining and feature extraction in R. I was  able to come up with a few size difference feature related to difference in character and word length using stopwords or not. I also took time in doing text cleaning and transformation. However, text vectorization was not practical with this "big" data set. Only, toy examples were my playground. Fabricating features was extremely slow in training and didn't dare to jump to the test set. Nearing the end of the competition everything changes with [text2vec](http://text2vec.org). 

### Text2Vec (May 2017)

Text2Vec is an amazing package recently developed by Dmitry Selivanov, who by the description in his page is "obsessed about efficiency", which was great for me. Text2Vec seamlessly vectorizes your text, produces super fast DTMs and allows you to do a bunch of things raging from Glove implementation to extract word vectors, topic modelling and document similarity (cosine, Euclidian and jaccard). It also includes TF-IDF transformations, and normalizations. With these package alone I could come up with a total of 20 features combining stopwords and other text processing criteria. These package made my feature extraction processes much much shorter and practical for experimentation. 

I also discovered Abhishek quora project (link below) and its python implementation of fuzzywuzzy derived features. I was stuck trying to implement it in R, so for the sake of reducing time I copy and pasted that chunk of code in a python script and ran it on the training to produce those features. 

### MLR (June 2017)

I already had around 26 features that I could start experimenting with ML algorithms. However, I felt and disadvantage with scikit-learn in python. All ML pipeline can be done with that library. Everything I knew in R was almost custom made, learner by learner. I read about Kaggle winners who thousands of models, and ensemble or stacked them. For me it was almost science fiction. So trying to find something similar in R I discovered MLR (link below). Its exactly what I needed, at first I was going with Caret package but after reading and testing MLR I fell for it. Everything is thought out, and since it has al least 3 years of development, most of the bugs have been caught and many features have been added. Make sure to check it if you area an R lover, it has the whole data and ML pipeline, feature selection, tuning, nested resampling, learning curves, benchmarking, a whole pack of visualization of the results, parallelization and more. 

With these package I produce my first basic model. The floor model, nothing can be worst than this. The "*classif.featureless*", a learner from mlr does exactly that. It fits a model, which gives me a constant probability of a duplicate questions. Its taking the percent of duplicated in the training set. This gave me a logloss value of **0.55411**. Other test with top 20 selected features (by information gain) gave me:

- Logistic Regression Learner: 0.47292
- Random Forest Learner: 0.42220
- First place Kaggler: 0.11580

For sake of learning, I decided to throw every learner that MLR had integrated (~56 for classification tasks). This gave me headaches, because is was extremely computational intensive. My first approach was to do a **learning curve**, just to see how much did the classifiers learn with increasing percent of the training set and how much time it took. Since everything was computational expensive, my idea was to find an optimal subset of the data for which I could use to do experiments faster but without sacrificing too much model performance. This task consumed my month, most of my experiments ended with a R session crash, or just taking forever. When trying to parallelize and obliviated my RAM. I turn my experiments to AWS instances. I learn how to set up one, which was extremely easy because of community provided AMIs, one of them was a ready-to-go Rstudio. I experimented up to c4.8xlarge instances (payed), however my poor knowledge in parallelization and high performance computing (HPC) didn't help at all understand why it still crashed. Eventually, I return to my Macbook Pro and solved the issue with the following criteria:

- Training all models using 10 percent of data.
- Long training time models where eliminated (*"classif.boosting", "classif.evtree", "classif.gausspr", "classif.bartMachine", "classif.cforest"*)
- Those that threw errors or consumed too much memory were eliminated (*"classif.ada", "classif.qda", "classif.bartMachine", "classif.lqa", "classif.JRip", "classif.neuralnet"*)
- Training the rest on 10, 20, 40, 70, 100 percent subsets, with 3 CV and measuring them with AUC, Logloss and Brier score. 

Although I had many problems, I ran some of the learning curves sequentially and others in parallel. It took some weeks the whole process but the results came through and were saved in the results folder as *LearningCurve_all.csv*. 

### Benchmarking (July 2017)

The models were benchmarked on 40 percent of the training set using 10 CV. Performance problems were present all the time, specially because I consumed all my RAM even though I avoided saving the models or predictions. Later, I realized that some models were not yet working well with parallelization (*"classif.extraTrees", "classif.PART", "classif.h2o.randomForest", "classif.h2o.deeplearning",  "classif.h2o.gbm"*), h2o learners, for example, crashed when multiple CV instances were trying to connect to the h2o API. I learned more about HPC and a super package called [BatchTools](https://github.com/mllg/batchtools), which for ever eliminated my performance problems and crashes. The result of benchmark experiments were save in the results folder as plots that start with *bmrBase*. Most of the trees derived learners performed very well on the training set. I selected the best by logloss and trained it on the whole training set and resulted in:

- Extra Trees Model: 0.59630 (Worst than base)

This probably was due to overfitting. 

# Useful Githubs and tutorials

- bradleypallen's quora project [Link](https://github.com/bradleypallen/keras-quora-question-pairs)
- Abhishek quora project. [Link](https://github.com/abhishekkrthakur/is_that_a_duplicate_quora_question)
- 24th place solution. [Link](https://github.com/aerdem4/kaggle-quora-dup)
- MLR tutorial. [Link](https://mlr-org.github.io/mlr-tutorial/release/html/index.html)
- MLR Example Kernel. [Link](https://www.kaggle.com/xanderhorn/train-r-ml-models-efficiently-with-mlr/notebook)
- Ensemble learning with feature selection for Alzheimer?s disease prediction. [Link](http://www.rpubs.com/jamin567/ad1 )
- Estimate distribution of data in LB? [Link](https://www.kaggle.com/badat0202/estimate-distribution-of-data-in-lb)

# Useful articles

- Is That a Duplicate Quora Question? by Abhishek Thakur. [Link](https://www.linkedin.com/pulse/duplicate-quora-question-abhishek-thakur)
- Quora's Semantic Question Matching with Deep Learning. [Link](https://engineering.quora.com/Semantic-Question-Matching-with-Deep-Learning)
- First Quora Dataset Release Question Pairs. [Link](https://data.quora.com/First-Quora-Dataset-Release-Question-Pairs)
- Collection of Kaggle's Quora competition solutions. [Link](https://www.kaggle.com/c/quora-question-pairs/discussion/34325)
- MLR example practice. [Link](https://www.analyticsvidhya.com/blog/2016/08/practicing-machine-learning-techniques-in-r-with-mlr-package/)
- Cross entropy and training-test class imbalance. [Link](https://swarbrickjones.wordpress.com/2017/03/28/cross-entropy-and-training-test-class-imbalance/)
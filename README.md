# Detecting-Fake-News
Differentiating between fake and real news by Topic Modeling

Data set : The data was taken from Kaggle ( https://www.kaggle.com/rtatman/nlp-in-r-topic-modelling-workbook/data)

The dataset contains text and metadata from 244 websites and represents 12,999 posts in total from the past 30 days. 
The data was pulled using the webhose.io API; because it's coming from their crawler, not all websites identified by the BS 
Detector are present in this dataset. Each website was labeled according to the BS Detector as documented here. Data sources 
that were missing a label were simply assigned a label of "bs". There are (ostensibly) no genuine, reliable, or trustworthy 
news sources represented in this dataset (so far), so don't trust anything you read.

Method Used : I have used a particularly famous Latent Dirichlet allocation (LDA) method for Unsupervised topic modeling and a 
              supervised model using TF-IDF, which stands for "term frequency-inverse document frequency".

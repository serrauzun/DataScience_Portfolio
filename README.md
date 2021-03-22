# Academic Portfolio 
### Selected Work Samples

This repository consists of Serra Uzun's academic projects completed between Fall 2019 and Spring 2021 throughout the Master of Science in Data Science program at Northwestern University. The work samples include Python and R coding samples, supervised and unsupervised models and their associated final analysis reports. 
Please click on the course name for overview and access the folder with work samples. 

- Mathematics
- Introduction to Data Science
- Database Systems and Data Preparation
- Decision Analytics
- Project Management
- [Practical Machine Learning](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/blob/main/README.md#practical-machine-learning)
- [Unsupervised Learning](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/blob/main/README.md#unsupervised-learning)
- [Supervised Learning](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/blob/main/README.md#supervised-learning)
- [Marketing Analytics](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/blob/main/README.md#marketing-analytics)
- [Applied Statistics with R](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/blob/main/README.md#applied-statistics-with-r)
- [Python for Data Science](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/blob/main/README.md#python-for-data-science)
- Capstone Project (in progress)

## Portfolio Overview
### [Practical Machine Learning](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Practical%20Machine%20Learning)
#### Home Equity Line of Credit Default and Loss Predictions
The HMEQ project involved conducting unsupervised learning method like Principal Component Analysis and k-Means Clustering, followed by numerous supervised learning models such as Decision Tree, Random Forest, Gradient Boosting, Linear and Logistic Regression and Neural Networks (Tensor Flow) to predict the probability of default credit and loss amount assuming loan defaults. Finally ran a ROC comparison between three based, regression and NN to determine optimal model. Achieved over 95% accuracy in loan default and loss amount predictions.

![Practical](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/blob/main/images/practical1.png)

#### [Image Recognition with Fashion MNIST data](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Practical%20Machine%20Learning/TensorFlow)
Fashion-MNIST is a dataset of Zalando's article images—consisting of a training set of 60,000 examples and a test set of 10,000 examples. Each example is a 28x28 grayscale image, associated with a label from 10 classes. Each image is 28 pixels in height and 28 pixels in width, for a total of 784 pixels in total. The project required to build a computer vision model using keras that will predict the label class of the item of clothing through the image with approximately 90% accuracy. 

---

### [Unsupervised Learning Models](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Unsupervised%20Learning)
#### [Dimension Reduction The Stock Portfolio Data](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Unsupervised%20Learning/PCA%20and%20t-SNE)
The Stock Portfolio project involves the use of Principal Component Analysis (PCA) and Stochastic Neighbor Embedding (t-SNE) as methods of dimension reduction for the stock portfolio dataset that consists of daily closing stock prices for twenty stocks and a large-cap index fund from Vanguard (VV). The aim was to use PCA as a remedial measure for multicollinearity while using t-SNE to achieve a dimensional reduction in a non-linear mechanism.  

#### [Exploratory Factor Analysis on Synthetic Aperture Personality Assessment Data](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Unsupervised%20Learning/Exploratory%20Factor%20Analysis)
Exploratory Factor Analysis method used on Synthetic Aperture Personality Assessment survey responses obtained from 2800 subjects. The model was taken through both varimax and promax rotations and examined various number of factors to obtain the best model for determining possible variable banding and underlying correlations.

#### [k-Means and Hierachical Clustering with EU Employment Data](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Unsupervised%20Learning/Hierarchical%20and%20k-Means%20Clustering)
This project includes hierarchical and k-means clustering analysis on Employment information on various industry segments reported as a percent for thirty European nations. Assessed various numbers of clusters and their associated Within Sum of Squares and Between Sum of Squares were assessed in the process of determining the optimum number of clusters and best model performance. 

![US2](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/blob/main/images/unsupervised%202.png)

#### [Final Project: Ames Iowa Housing](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Unsupervised%20Learning/AmesHousing_FinalProject)
The final course project includes PCA and EFA being performed on the Ames Iowa Housing data to determine multicollinearity, underlying patterns, and strong association between variables. SalePrice was picked as the dependent variable; finally, the k-Means clustering model was introduced to determine possible clusters within the dataset that indicate similarities and common characteristics between independent variables that would shape the supervised learning models that will follow this analysis.

![USF](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/blob/main/images/unsupervised%20final.png)

---

### [Supervised Learning](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Supervised%20Learning)
#### [Ames Iowa Housing](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Supervised%20Learning/Regression%20Modeling%20with%20Variable%20Selection)
For the Ames Iowa Housing SalePrice Prediction project, I performed Simple Linear Regression, Multiple Linear Regression, and Regression model with transformed response variable were performed on the Ames Iowa Housing dataset with SalePrice being the dependent variables. In addition, ran variables selection (forward, backward and stepwise) in order to determine the most significant independent variables and conduct generalized linear regression models to predict SalePrice.

![Ames](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/blob/main/images/Ames1.png)

#### [Binary Classification for Personal Loan Predictions](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Supervised%20Learning/Binary%20Classification)
Using the Universal Bank dataset provided by NU, the project involved building logistic regression models for binary classification, with having Personal Loan as the response variable. In-sample and out-sample sets were evaluated using valid metrics and compared all models to select the optimal approach. 

#### [Count Modeling with Medical Office Visit Data](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Supervised%20Learning/Count%20Data%20Modeling)
Poisson (with and without dispersion), Negative Binomial, Hurdle and Zero-Inflated Regression models were conducted on the medical care dataset obtained from the journal article: Demand For Medical Care By the Elderly: A Finite Mixture Approach by Deb and Trivedi, to predict the number of future office visits by patients.

---

### [Marketing Analytics](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Marketing%20Analytics)
#### [Market Segmentation for App Happy](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Marketing%20Analytics/Market%20Segmentation%20(Clustering))
The App Happy Company wants to better understand what they believe is the market for a new social entertainment app they are developing. App Happy hired the Consumer Spy Corporation (CSC) to survey consumers in the market. CSC collected data from a sample of consumers and provided App Happy with a dataset of their responses. The project involved taking the survey responses and conducting clustering to perform a general attitudinal post hoc segmentation analysis and develop a segmentation scheme. The scheme's segments were profiled, and recommendations were made about the product opportunities through the outcome of the analysis. 

#### [Conjoint Measurement and Choice Modeling for Star Technology Company](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Marketing%20Analytics/Conjoint%20Measurement%20and%20Choice%20Modeling%20(MCMC))
The survey results from the choice-based conjoint (CBC) task we used to conduct Hierarchical Bayes (HB) Multinomial Logit (MNL) and Markov Chain Monte Carlo Simulation to estimate the effects of attributes in order to make product recommendations to the product development team on the remote control model they should go to market with. 

![solo2](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/blob/main/images/solo2.png)

#### [Customer Segmentation and Target Marketting](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Marketing%20Analytics/Customer%20Segmentation%20and%20Target%20Marketing%20(LR%26RF))
The project involved using data from the past 16 marketing campaigns and purchased demographic and geographic data to come-up with a targeting strategy for the next mail campaign. Through feature engineering and Random Forest and Logistic regression models, customer segmentation and targeted marketing approach were outlined to improve the mail campaign response rate.  

![solo3](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/blob/main/images/solo3.png)

---

### [Applied Statistics with R](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Applied%20Statistics)
The coursework includes evaluating statistical information, performing data analyses, and interpreting and communicating analytical results. Topics covered include descriptive statistics, central tendency, exploratory data analysis, probability theory, discrete and continuous distributions, statistical inference, correlation, multiple linear regression, contingency tables, and chi-square tests. Selected contemporary statistical concepts, such as bootstrapping, are introduced to supplement traditional statistical methods.

---

### [Python for Data Science](https://github.com/serrauzun/NorthwesternMSDS_Portfolio/tree/main/Python%20for%20DS)
The coursework includes demonstration of data representation strategies, showing how data structures are implemented in Python and demonstrating tools for data science and software engineering. Working on data analysis problems, students employ various programming paradigms, including functional programming, object-oriented programming, and data stream processing. 

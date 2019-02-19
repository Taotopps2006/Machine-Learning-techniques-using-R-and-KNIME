# Machine-Learning-techniques-using-R-and-KNIME
Analysis of the Adult dataset using supervised and unsupervised machine learning techniques.

# Introduction
In the present day, data is created constantly from several sources for example through sensor devices and social media, and growing at an unprecedented rate in different forms (volume, velocity, variety) due to technology, and challenges are being faced on how to handle, analyse and make sense from these huge data in order to turn them into useful information and knowledge.

Extracting knowledge from these huge datasets follows a seven-step process called Knowledge Discovery from Data (KDD). Data Mining is an integral part where intelligent techniques are applied in order to extract patterns (Han and Kamber 2006). In this report, we have chosen to explore the Adult dataset / US census dataset using two data mining techniques namely classification algorithm and association rule mining. In addition, two software tools were used for data analysis; **R** and **KNIME**.

The remainder of this report is structured as follows: In Section 2, we present a description of the chosen dataset. In Section 3, we performed exploratory data analysis and, in Section 4 and 5, data mining techniques were applied on the pre-processed data set. Section 6 rounds off the report and presents future research directions.

# Data Choice
* Name of Dataset and Source: 
The data set used in this report is called the Adult dataset (also known as "Census Income" dataset) sourced from the University of California Irvine machine-learning repository (UCI 1996).
* Dataset Description: 
The adult dataset contains forty-eight thousand, eight hundred and forty-two (48842) instances of census information taken from the United States (US) Census bureau database. It was donated into the UCI repository on 01/05/1996 (UCI 1994). The adult dataset contains a mix of nominal and continuous variables.
* Dataset Choice Motivation: 
The dataset described in above was not just chosen at random. It was the preferred dataset choice for the Data Mining module because of the following reasons:
  *  It is a large, messy and a challenging dataset to work with.
  *  It is sufficient for illustrating all steps in the KDD process.
  *  The dataset has been used by lots of researcher to perform different data mining algorithms (UCI 1996).
  *  It has missing values, which enables one to demonstrate the skills gained during the data mining class lectures and laboratory exercises on how to handle missing values during the data pre-processing stage.
  *  It is a good dataset for performing all forms of data mining techniques like classification, association etc.
  *  It has many attributes which makes it suitable for better data exploration/analysis or in-depth data analysis
  *  Since it has been partitioned randomly by default using ML++ GenCVFiles, comparison of the accuracy of further partitioning methods can therefore be compared.
* Data Mining Objectives: 
The data-mining objectives are to:
  *  Perform an in-depth data pre-processing to clean and prepare the large and messy dataset for further data analysis.
  *  Create a classification model, which can determine whether a person makes over fifty thousand United States dollar by mining nameless census data containing information such as age, work class, marital status, occupation, sex and education level.
  *  Select the optimal or most accurate classification model out of a number of models created.
  *  Perform Association rule mining to find frequent patterns that exists in the dataset and determine which values of the dataset attributes determine high income or low income.

# Data Analysis
* R will be used in exploring and pre-processing the data. R will also be used for Association rule mining
* KNIME will be used to examine several Classification algorithms

# References
* Han, J., and Kamber, M. (2006) Data mining: concepts and techniques. Elsevier.
* UCI (1996) Adult Data Set [Online] Available at: https://archive.ics.uci.edu/ml/datasets/Adult Accessed: 5th February 2017.

To request for a pdf report of this project, send an email to taotopps2006@gmail.com

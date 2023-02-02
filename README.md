# Loan Eligibility Bayesian Prediction
This Repository refers to the final project of the course Statistical Methods in Data Science and Laboratory II (SDS2) at University Sapienza of Rome 2021/2022.

![Cover](dataset-cover.jpg)


# Abstract
Today there are many people and businesses that apply for bank loans. The main activity of
every bank is the distribution of loans, so its goal must be to give money to people who will pay it
back. But for the verification process they take a long time. To predict if a client may be eligible
for a loan I applied three models 1) Bayesian Logistic Regression, 2) Bayesian Cloglog and 3)
Frequentist Logistic Regression. In this way, using certain characteristics of loan applicants, the
models were able to predict an applicant’s eligibility quite well, as affirmed by the high values of
the calculated metrics.

# Introduction
Our banking systems have many products to sell, but it’s know that the main profit comes
directly from the loan’s interest. The banks are central in the modern economy, they have to
decide if a costumer is a good(non-defaulter) or bad(defaulter) one before giving the loans to
the borrowers. This type of prediction is a very difficult and time consuming task for any bank
or organization.
Although considering the delicacy in dealing with this topic, in fact, if a bank lends money
to many individuals who cannot repay the debt, this will have a powerful economic effect, my
goal is to try to automate and speed up the process of verifying the requirements for obtaining
a loan.I used Bayesian and Frequentist Logist regression to predict the outcome of the elegibilty
for a loan.
In addition, I created interesting new features on which I applied statistical models, evaluated
through numerous metrics. Finally, in the conclusion are presented some recommendations and
salient points that I found during the analysis.

# Related Works
A lot of researcher have worked on how to build predictive models to automate the process of
targeting the right applicants. Professor Amruta Sankh and his students of the Atharva College
of Engineering in Mumbai use different machine learning models such as Random Forest, Naive
Bayes, Decision Tree and logistic regression. In a similar way, many other data scientist treated
of this problem. For example 26 Kaggle users have uploaded very interesting codes on numerous
statistical models, using the same data that I have used on these pages. Moreover, an article
about this theme has been publishing in the famous site Towards Data Science.

#  Dataset and Benchmark
To conduct the analysis presented before i have used the ”Loan Eligible Dataset” from Kaggle,
it contains 614 observation of thirteen variables.
About the performance of the models built on this dataset,from the results that i saw on
Kaggle and in almost all cited papers I can say that the accuracy of logistic regression is pretty
high, in mean that metric is about 80%. in addition, almost all the other models, even the more
complex ones, also performed well by always having rather high metric values.
Even in my case, with the models reported in this paper the results were quite satisfactory,
on average every metric shown has values higher than 80%

# Comparing Results
In my problem set the most important metric trought it evaluating the models is the Recall.
This metric is very usefull when there is a high cost associated with False Negative, In fact, if
a model classifies many people who will not pay the loan as eligible to get it, this will create
serious risks for the bank. This metric is always higher than 90%.
The difference between the various models, holding the random seed generator constant, is low. Therefore, the three models can be expected to act similarly.


![c](Comparison.png)

# coursera-getting-and-cleaning-data-project
Project for Coursera Getting and cleaning data

Introduction

This repository contains the work for the project part of the Coursera course "Getting and Cleaning data", part of the Data Science specialization. 

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project.

You are required to submit:

a tidy data set as described below
a link to a Github repository with your script for performing the analysis, and
a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This file explains how all of the scripts work and how they are connected.


About the raw data

The features (561 of them) are unlabeled and can be found in the x_test.txt. The activity labels are in the y_test.txt file. The test subjects are in the subject_test.txt file.

The same holds for the training set.
About the script and the tidy dataset

I created a script called run_analysis.R which will merge the test and training sets together. Prerequisites for this script:

the UCI HAR Dataset must be extracted and..
the UCI HAR Dataset must be available in a directory called "UCI HAR Dataset"

After merging testing and training, labels are added and only columns that have to do with mean and standard deviation are kept.

Lastly, the script will create a tidy data set containing the means of all the columns per test subject and per activity. This tidy dataset will be written to a tab-delimited file called tidy.txt, which can also be found in this repository.
About the Code Book

The CodeBook.md file explains the transformations performed and the resulting data and variables.

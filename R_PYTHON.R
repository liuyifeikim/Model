library(reticulate)

py_available(initialize = TRUE)
conda_create('r-reticulate')
conda_install('r-reticulate')

#用R的方式导入包
os <- import('os')
os$getcwd()
os$listdir()

#切换到python环境
repl_python() #切换环境

import pandas as pd
import numpy as np

glass_py = pd.read_csv('glass.csv')
adult_py = pd.read_csv('adult.csv')
glass_py.columns
glass_py.shape
adult_py.columns
adult_py.shape

exit #退出python环境, 要在console输入

#用R计算python对象
summary(py$glass_py) #要用py$python对象
summary(py$adult_py)

#用Python计算R对象
glass_r <- read.csv('glass.csv')
adult_r <- read.csv('adult.csv')

repl_python()
r.adult_r.columns #要用r.+r对象
r.adult_r.shape
pd.value_counts(r.adult_r['sex'])

from FinalProject.DataLoader import DataLoader
import pandas as pd
from FinalProject.LanguageModel.Preprocessor import Preprocessor
from FinalProject.LanguageModel.TfIdf import Vectorizer
from FinalProject.Learners.SVM import SVM

## set hyperparameters to tune the model
alpha_list = [1e-4, 5e-5, 1e-5, 5e-6, 1e-6]
alpha_list1 = [4e-5, 3.5e-5, 3e-5, 2.5e-5, 2e-5]
alpha_list2 = [2.4e-5, 2.2e-5, 1.8e-5, 1.6e-5]
alpha_list3 = [1.95e-5, 1.9e-5, 1.85e-5, 1.8e-5, 1.75e-5, 1.7e-5, 1.75e-5]

'''## load the dataset
dataloader = DataLoader('/uufs/chpc.utah.edu/common/HIPAA/u1318593/Downloads/SafeUT/FINAL_ANONYMIZED_SAFEUT.xlsx')
# dataloader.plot()
dataloader.to_engagement(5)
print('passed 1')'''

train = pd.read_csv('../../Downloads/SafeUT/ghosting_inspection/train_ghosting_v2.csv')
dev = pd.read_csv('../../Downloads/SafeUT/ghosting_inspection/dev_ghosting_v2.csv')
test = pd.read_csv('../../Downloads/SafeUT/ghosting_inspection/test_ghosting_v2.csv')

## preprocess data and convert to numeric vectors
preprocessor = Preprocessor(lowercase=True, lemma=True, remove_punc=True, remove_stopwords=False)
vectorizer = Vectorizer(ngram=(1, 2), nmessage=3, preprocessor=preprocessor)
# vectorizer.load(dataloader.data)
vectorizer.load(train, dev)
vectorizer.text2vec()
print('passed 2')

## train the model with vectorized data and tune the hyperparameter
for alpha in alpha_list3:
    print(f'Now using alpha={alpha}')
    svm = SVM(alpha=alpha)
    svm.kfold(n_splits=10)
    svm.evaluate(vectorizer.train_X, vectorizer.train_y, vectorizer.test_X, vectorizer.test_y)
print('all passed')
# best performance at alpha=1.7e-05

## test with the trained model
vectorizer_test = Vectorizer(ngram=(1, 2), nmessage=3, preprocessor=preprocessor)
vectorizer_test.load(train, test)
vectorizer_test.text2vec()
print('passed 2')

svm_test = SVM(alpha=1.7e-5)
svm_test.evaluate(vectorizer_test.train_X, vectorizer_test.train_y, vectorizer_test.test_X, vectorizer_test.test_y)
print('all passed')

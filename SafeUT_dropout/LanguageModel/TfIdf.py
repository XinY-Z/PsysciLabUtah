import re
import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer


class Vectorizer(TfidfVectorizer):

    def __init__(self, ngram, nmessage, preprocessor):
        super().__init__()
        self.__nmessage = nmessage
        self.__ngram = ngram
        self.preprocessor = preprocessor
        self.input = None
        self.dev = None
        self.train_doclist = []
        self.test_doclist = []
        self.train_X = None
        self.train_y = None
        self.test_X = None
        self.test_y = None

    ## load data
    def load(self, data1, data2):
        self.input = data1
        self.test = data2

    ## select first x messages in each encounter as predictor (x=3)
    ## transform data structure to fit sklearn tf-idf vectorizer
    def preprocess(self):

        '''## select first three messages and convert to lists
        data_sel = self.input.groupby('encounterId').head(self.__nmessage)
        data_sel = data_sel.loc[:, ['messageId', 'encounterId', 'originator', 'message', 'engagement']]
        ## add speaker type as part of the data
        # data_sel['originator'] = data_sel['originator'].apply(lambda x: str(x))
        data_sel['message'] = data_sel['message'].apply(lambda x: str(x))
        # data_sel['ori_message'] = '[#' + data_sel['originator'] + '] ' + data_sel['message']
        doclist = data_sel.groupby('encounterId')['message'].apply(list).to_list()
        doclist = [' '.join(doc) for doc in doclist]
        ## remove redacted parts
        doclist = [re.sub('\\[(.*?)]', '', doc) for doc in doclist]'''
        train_doclist = [re.sub('\\[(.*?)]', '', doc) for doc in self.input['talk_turns']]
        test_doclist = [re.sub('\\[(.*?)]', '', doc) for doc in self.test['talk_turns']]
        self.train_doclist = train_doclist
        self.test_doclist = test_doclist

    ## Convert texts to vectors using tf-idf
    def text2vec(self):
        self.preprocess()

        ## instantiate tf-idf vectorizer
        if isinstance(self.__ngram, int):
            tfidf = TfidfVectorizer(ngram_range=(self.__ngram, self.__ngram),
                                    preprocessor=self.preprocessor.preprocess)
        elif isinstance(self.__ngram, tuple):
            tfidf = TfidfVectorizer(ngram_range=(self.__ngram[0], self.__ngram[1]),
                                    preprocessor=self.preprocessor.preprocess)
        else:
            raise TypeError('Argument "ngram" must be int or tuple.')

        ## Vectorize the texts
        train_tf_idf = tfidf.fit_transform(self.train_doclist)
        test_tf_idf = tfidf.transform(self.test_doclist)
        self.train_X = train_tf_idf
        self.test_X = test_tf_idf

        ## Create new dataset in which texts are converted to vectors
        # engagement_list = self.input.groupby('encounterId').head(1)['engagement'].to_list()
        train_engagement_list = self.input['label'].to_list()
        test_engagement_list = self.dev['label'].to_list()
        self.train_y = np.array(train_engagement_list)
        self.test_y = np.array(test_engagement_list)

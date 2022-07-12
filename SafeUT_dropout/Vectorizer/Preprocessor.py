import spacy

nlp = spacy.load('en_core_web_sm')


class Preprocessor:
    def __init__(self, lowercase=True, lemma=True, remove_punc=True, remove_stopwords=False):
        super().__init__()
        self.__lowercase = lowercase
        self.__lemma = lemma
        self.__remove_punc = remove_punc
        self.__remove_stopwords = remove_stopwords

    # remove punctuation and stopwords, lowercase the words and lemmatize
    def preprocess(self, string):
        if self.__lowercase:
            string = string.lower()
        doc = nlp(string)

        if self.__remove_punc:
            doc = [token for token in doc if not token.is_punct]
        if self.__remove_stopwords:
            doc = [token for token in doc if not token.is_stop]

        if self.__lemma:
            doc = [token.lemma_ for token in doc]
        else:
            doc = [token.text for token in doc]

        return ' '.join(doc)

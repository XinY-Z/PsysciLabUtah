from sklearn.linear_model import SGDClassifier
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import f1_score, precision_score, recall_score, confusion_matrix, roc_auc_score
from sklearn.calibration import CalibratedClassifierCV
import numpy as np


class SVM(SGDClassifier, StratifiedKFold):

    def __init__(self, alpha):
        super(SGDClassifier).__init__()
        super(StratifiedKFold).__init__()
        self.clf = SGDClassifier(loss='hinge', random_state=1, alpha=alpha)
        self.skf = None

    ## set k-fold stratified cross validation
    def kfold(self, n_splits):
        self.skf = StratifiedKFold(n_splits=n_splits)

    ## fit and evaluate the model.
    '''def evaluate(self, X, y):

        dataset_x = X
        dataset_y = y

        ## Use stratified k-fold cross validation to split dataset
        ## train SVM and return performance metrics
        train_errors, base_rates, accuracies, precisions, recalls, f1_scores, confusions = \
            [], [], [], [], [], [], []
        for train_index, test_index in self.skf.split(dataset_x, dataset_y):
            train_x, test_x = dataset_x[train_index], dataset_x[test_index]
            train_y, test_y = dataset_y[train_index], dataset_y[test_index]

            self.clf.fit(train_x, train_y)
            base_rate = test_y.mean()
            train_error = 1 - self.clf.score(train_x, train_y)
            accuracy = self.clf.score(test_x, test_y)

            predictions = self.clf.predict(test_x)
            precision = precision_score(test_y, predictions)
            recall = recall_score(test_y, predictions)
            f1 = f1_score(test_y, predictions)
            conf_mat = confusion_matrix(test_y, predictions)

            base_rates.append(base_rate)
            train_errors.append(train_error)
            accuracies.append(accuracy)
            precisions.append(precision)
            recalls.append(recall)
            f1_scores.append(f1)
            confusions.append(conf_mat)

        print(f'Training error: {np.mean(train_errors)}')
        print(f'Base rate: {np.mean(base_rates)}')
        print(f'Accuracy: {np.mean(accuracies)}')
        print(f'Precision: {np.mean(precisions)}')
        print(f'Recall: {np.mean(recalls)}')
        print(f'F1 score: {np.mean(f1_scores)}')
        print(f'Confusion matrix: {np.mean(confusions, axis=0)}')'''
    def evaluate(self, train_x, train_y, test_x, test_y):
        self.clf.fit(train_x, train_y)
        base_rate = test_y.mean()
        train_error = 1 - self.clf.score(train_x, train_y)
        accuracy = self.clf.score(test_x, test_y)

        predictions = self.clf.predict(test_x)
        precision = precision_score(test_y, predictions)
        recall = recall_score(test_y, predictions)
        f1_baseline = f1_score(test_y, [1] * len(test_y))
        f1 = f1_score(test_y, predictions)
        conf_mat = confusion_matrix(test_y, predictions)

        calibrator = CalibratedClassifierCV(self.clf, cv='prefit')
        model_ = calibrator.fit(train_x, train_y)
        y_probs = [prob[1] for prob in model_.predict_proba(test_x)]

        print(f'Training error: {train_error}')
        print(f'Base rate: {base_rate}')
        print(f'Baseline F1 : {f1_baseline}')
        print(f'Accuracy: {accuracy}')
        print(f'Precision: {precision}')
        print(f'Recall: {recall}')
        print(f'F1 score: {f1}')
        print(f'ROC AUC: {roc_auc_score(test_y, y_probs)}')
        print(f'Confusion matrix: {conf_mat}')

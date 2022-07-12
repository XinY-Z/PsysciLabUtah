import pandas as pd
import numpy as np
from matplotlib import pyplot as plt


class DataLoader:

    ## initiate and import data
    def __init__(self, path):
        self.data_path = path
        data = pd.read_excel(self.data_path, sheet_name=['Message 1', 'Message 2'], engine='openpyxl')
        self.data = pd.concat(data)
        tip = pd.read_excel(self.data_path, 'Encounter', engine='openpyxl')
        tip_list = tip.loc[tip['encounterType'] == 'Tip', 'encounterId'].unique()
        self.data = self.data[~self.data['encounterId'].isin(tip_list)]
        self.data = self.data.sort_values(['encounterId', 'Unnamed: 0'])
        self.data = self.data[self.data['originator'] != 'Auto']

    ## Randomly select a small portion of encounters for testing
    def random_select(self, n=500):
        np.random.seed(123)
        encounterId = self.data['encounterId'].unique()
        max = self.data['encounterId'].nunique()
        if n > max:
            raise ValueError('Number of selected datapoints exceeds the maximum number of the entire dataset.')
        else:
            rand_inds = np.random.choice(encounterId, n, replace=False)
            self.data = self.data[self.data['encounterId'].isin(rand_inds)]

    ## Plot data to inspect distribution
    def plot(self):
        sizes = self.data.groupby('encounterId').size()
        plt.hist(sizes, bins=range(100))
        plt.xlim(0, 100)
        plt.xticks(range(0, 101, 10))
        plt.xlabel('Number of Messages')
        plt.title('Distribution of Lengths of Conversations, <= 100-message shown')
        plt.show()

    ## Create and assign outcome values
    def to_engagement(self, cutoff):
        sizes = self.data.groupby('encounterId').size()
        sizes.name = 'encounterLen'
        self.data = self.data.join(sizes, on='encounterId')
        self.data['engagement'] = self.data['encounterLen'].apply(lambda x: 1 if x <= cutoff else 0)

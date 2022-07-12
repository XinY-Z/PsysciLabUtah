import pandas as pd
from FinalProject import DataLoader
from matplotlib import pyplot as plt
# from collections import Counter
import spacy
import re

test22 = pd.read_csv('../../Downloads/SafeUT/engagement_new_datasets_csv/test_dev_datasets_with_predictions'
                     '/test_talkturn_2_and_2_w_pred_seed42.csv')
test24 = pd.read_csv('../../Downloads/SafeUT/engagement_new_datasets_csv/test_dev_datasets_with_predictions'
                     '/test_talkturn_2_and_4_w_pred_seed22.csv')
test26 = pd.read_csv('../../Downloads/SafeUT/engagement_new_datasets_csv/test_dev_datasets_with_predictions'
                     '/test_talkturn_2_and_6_w_pred_seed22.csv')
test28 = pd.read_csv('../../Downloads/SafeUT/engagement_new_datasets_csv/test_dev_datasets_with_predictions'
                     '/test_talkturn_2_and_8_w_pred_seed12.csv')
test5 = pd.read_csv('../../Downloads/SafeUT/engagement_new_datasets_csv/test_dev_datasets_with_predictions'
                    '/test_by_message_w_pred_seed22.csv')
fulldata = DataLoader.DataLoader('../../Downloads/SafeUT/FINAL_ANONYMIZED_SAFEUT.xlsx')
fulldata.data = fulldata.data[fulldata.data['encounterId'] > 94]

## Select true positive and true negative subsets
pos_index22 = test22.loc[test22['confusion'] == 'TruePos', 'id']
neg_index22 = test22.loc[test22['confusion'] == 'TrueNeg', 'id']
pos_data22 = fulldata.data[fulldata.data['encounterId'].isin(pos_index22)]
neg_data22 = fulldata.data[fulldata.data['encounterId'].isin(neg_index22)]

pos_index24 = test24.loc[test24['confusion'] == 'TruePos', 'id']
neg_index24 = test24.loc[test24['confusion'] == 'TrueNeg', 'id']
pos_data24 = fulldata.data[fulldata.data['encounterId'].isin(pos_index24)]
neg_data24 = fulldata.data[fulldata.data['encounterId'].isin(neg_index24)]

pos_index26 = test26.loc[test26['confusion'] == 'TruePos', 'id']
neg_index26 = test26.loc[test26['confusion'] == 'TrueNeg', 'id']
pos_data26 = fulldata.data[fulldata.data['encounterId'].isin(pos_index26)]
neg_data26 = fulldata.data[fulldata.data['encounterId'].isin(neg_index26)]

pos_index28 = test28.loc[test28['confusion'] == 'TruePos', 'id']
neg_index28 = test28.loc[test28['confusion'] == 'TrueNeg', 'id']
pos_data28 = fulldata.data[fulldata.data['encounterId'].isin(pos_index28)]
neg_data28 = fulldata.data[fulldata.data['encounterId'].isin(neg_index28)]

'''pos_index5 = test5.loc[test5['confusion'] == 'TruePos', 'id']
neg_index5 = test5.loc[test5['confusion'] == 'TrueNeg', 'id']
pos_data5 = fulldata.data[fulldata.data['encounterId'].isin(pos_index5)]
neg_data5 = fulldata.data[fulldata.data['encounterId'].isin(neg_index5)]'''

## Inspect utterance distribution in encounters in talk turn models
plt.hist(pos_data22.groupby('encounterId').size(), bins=range(10))
plt.xlabel('Number of Utterances')

'''## Justfiy the use of 3 messages in message models
pred_seq5 = pos_data5.groupby('encounterId').head(3)
pred_seq5 = pred_seq5.groupby('encounterId')['originator'].apply(' '.join).reset_index().rename(
    columns={'originator': 'ori_seq'})
pred_seq5['ori_seq'].value_counts()'''

## Word analysis
en = spacy.load('en_core_web_sm')
stopwords = en.Defaults.stop_words

'''pred_word5 = pos_data5.groupby('encounterId').head(3)
uttlist = pred_word5['message'].apply(str)
wordlist = []
for x in uttlist:
    words = x.split(' ')
    wordlist.extend(words)
counts = Counter(wordlist)
for stop in stopwords:
    [counts.pop(stop, None) for stop in stopwords]'''

## Inspect the potential length issue in message models
'''plt.hist(pos_data5.groupby('encounterId').size(), bins=range(10))
plt.xlabel('Number of utterances')'''

## Look at speaker sequence in the entire encounter
pos_seq22 = pos_data22.groupby('encounterId')['originator'].apply(' '.join).reset_index().rename(
    columns={'originator': 'ori_seq'})
# pos_data22 = pos_data22.join(pos_seq22.set_index('encounterId'), on='encounterId', how='right')
pos_seq22['ori_seq'].value_counts(normalize=True)

pos_seq24 = pos_data24.groupby('encounterId')['originator'].apply(' '.join).reset_index().rename(
    columns={'originator': 'ori_seq'})
# pos_data24 = pos_data24.join(pos_seq24.set_index('encounterId'), on='encounterId', how='right')
pos_seq24['ori_seq'].value_counts(normalize=True)

pos_seq26 = pos_data26.groupby('encounterId')['originator'].apply(' '.join).reset_index().rename(
    columns={'originator': 'ori_seq'})
# pos_data26 = pos_data26.join(pos_seq26.set_index('encounterId'), on='encounterId', how='right')
pos_seq26['ori_seq'].value_counts(normalize=True)

pos_seq28 = pos_data28.groupby('encounterId')['originator'].apply(' '.join).reset_index().rename(
    columns={'originator': 'ori_seq'})
# pos_data28 = pos_data28.join(pos_seq28.set_index('encounterId'), on='encounterId', how='right')
pos_seq28['ori_seq'].value_counts(normalize=True)

'''pos_seq5 = pos_data5.groupby('encounterId')['originator'].apply(' '.join).reset_index().rename(
    columns={'originator': 'ori_seq'})
# pos_data5 = pos_data5.join(all_seq5.set_index('encounterId'), on='encounterId', how='right')
pos_seq5['ori_seq'].value_counts()'''

## Inspect encounters that contain "ghosting messages"
## First inspect true positive using speaker sequence
pos_index22_seq = pos_seq22[pos_seq22['ori_seq'].isin(['Mobile Admin Admin Admin', 'Mobile Admin Admin'])]
pos_data22_seq = fulldata.data[fulldata.data['encounterId'].isin(pos_index22_seq['encounterId'])]

pos_data22_seq.loc[:, ['messageId', 'encounterId', 'message', 'originator']].to_csv('InspectGhostingMessage_2tktrn.csv',
                                                                                    index=False)

## -------------------------------------------------------------------- ##
## Add talk turn index to each encounter
b0 = fulldata.data['originator']
b1 = fulldata.data.groupby('encounterId')['originator'].shift()
fulldata.data['ind2'] = b0.ne(b1)
fulldata.data['tlktrn_ind'] = fulldata.data.groupby('encounterId')['ind2'].cumsum()
del fulldata.data['ind2']

## Inspect ghosting messages using modified Meghan's patterns
ghost_list = [
    'still(\\s\\w+){,4} (chat|talk|text)(\\s\\w+){,3}\\?',
    'still (there|here)\\?',
    'close(\\s\\w+){,2} chat',
    'close this (out|for now)',
    't heard(\\sback)? from you',
    'if you(\\s\\w+){,2} chat later',
    "noticed(\\sthat)? you(\\s\\w+)? (haven't|havent|have not) respond",
    'still here if you want to chat',
    '^are you there(\\?)*',
    '^you there\\?',
    'would you like to (still|keep|continue) (chat|talk)',
    'still there, wanting to chat\\?$'
]

## Find messages that match the pattern and label them as 1
fulldata.data['ghost_ind'] = fulldata.data.loc[fulldata.data['originator'] == 'Admin', 'message'].apply(
    lambda msg: 1 if any(re.search(ptrn, str(msg), re.IGNORECASE) for ptrn in ghost_list) else 0
)
size = fulldata.data.groupby('encounterId').size()
size.name = 'msg_length'  # number of messages in an encounter
fulldata.data = fulldata.data.join(size, on='encounterId')
fulldata.data['msg_ind'] = fulldata.data.reset_index().index
ghost_msg = fulldata.data.loc[
    fulldata.data['ghost_ind'] == 1,
    ['encounterId', 'messageId', 'originator', 'message', 'msg_length', 'msg_ind', 'tkltrn_ind']
]

## Check distribution of ghosting messages by # of message in encounters
ghost_enc_msg = ghost_msg.drop_duplicates('encounterId', keep=False)
print(ghost_enc_msg.value_counts('msg_length', normalize=True))

## Check distribution of ghosting messages by # of talk turns in encounters
ghost_enc_id = ghost_msg['encounterId'].unique()
ghost_enc_tlktrn = fulldata.data.loc[fulldata.data['encounterId'].isin(ghost_enc_id)]
ghost_tlktrn_dist = ghost_enc_tlktrn.groupby('encounterId')['tkltrn_ind'].apply(max)
print(ghost_tlktrn_dist.value_counts(normalize=True))

## Check for false negatives
noghost_tlktrn_id = fulldata.data.loc[~fulldata.data['encounterId'].isin(ghost_enc_id), 'encounterId'].unique()
noghost_tlktrn = fulldata.data.loc[
    fulldata.data['encounterId'].isin(noghost_tlktrn_id),
    ['encounterId', 'originator', 'message', 'tkltrn_ind', 'ghost_ind']
]

## Compare the distribution of ghosting message and full data
plt.clf()
plt.hist(ghost_tlktrn_dist, bins=100, range=(1, 100), alpha=0.5, density=True, label='Ghost')
plt.hist(fulldata.data.groupby('encounterId')['tkltrn_ind'].apply(max), bins=100, range=(1, 100), alpha=0.5,
         density=True, label='Full Data')
plt.xlabel('Talk Turns')
plt.legend(loc='upper right')
plt.show()

## Create new data with encounter ids and an indicator to indicate if the encounter contains ghosting messages \
## and an indicator to indicate if the counselor is ghosted

## find index of the last ghost message
a = fulldata.data.loc[fulldata.data['ghost_ind'] == 1, ['encounterId', 'msg_ind']]
a = a.groupby('encounterId')['msg_ind'].last().reset_index().rename(columns={'msg_ind': 'ghost_loc_ind'})
## find index of the last message in each encounter
b = fulldata.data.groupby('encounterId')['msg_ind'].last().reset_index().rename(columns={'msg_ind': 'last_ind'})
b = b.loc[b['encounterId'].isin(a['encounterId'])]
## combine the two for easier use
z = a.join(b.set_index('encounterId'), on='encounterId')
## check if all the speakers between the two indices within each encounter is Admin. If it is, return the encounter id
fulldata.data.reset_index(drop=True, inplace=True)
real_ghost_encounterId = []
for i in z['encounterId']:
    jj = fulldata.data.loc[fulldata.data['encounterId'] == i]
    kk = z.loc[z['encounterId'] == i]
    jj = jj.loc[kk['ghost_loc_ind'].values[0]:kk['last_ind'].values[0]]
    ll = 1 if all(o == 'Admin' for o in jj['originator']) else 0
    if ll == 1:
        real_ghost_encounterId.append(i)

## attach result back to the original dataset
real_ghost_enc = pd.DataFrame(
    {'encounterId': real_ghost_encounterId,
     'ghost_ind2': 1}
)
real_ghost_enc.set_index('encounterId', inplace=True)
fulldata.data = fulldata.data.join(real_ghost_enc, on='encounterId')
fulldata.data.loc[fulldata.data['ghost_ind2'].isnull(), 'ghost_ind2'] = 0

## export the indicators
ghost_msg_id = fulldata.data.loc[
    fulldata.data['ghost_ind'] == 1,
    ['encounterId', 'msg_ind']
] \
    .groupby('encounterId')['msg_ind'].apply(list)

ghost_tlktrn = pd.DataFrame(
    {'encounterId': fulldata.data['encounterId'].unique(),
     'ghost_ind': fulldata.data.groupby('encounterId')['ghost_ind'].apply(lambda ind: 1 if any(ind == 1) else 0),
     'real_ghost_ind': fulldata.data.groupby('encounterId').head(1)['ghost_ind2'].to_list(),
     'ghost_msg_id': [[] if enc_id not in ghost_msg_id.index
                      else [fulldata.data.loc[i, 'messageId'] for i in ghost_msg_id[enc_id]]
                      for enc_id in fulldata.data['encounterId'].unique()],
     'tlktrn_length': fulldata.data.groupby('encounterId')['tkltrn_ind'].apply(max)}
)

## Inspect the distribution of real ghosting encounters
ghost_enc_tlktrn = fulldata.data.loc[fulldata.data['encounterId'].isin(real_ghost_encounterId)]
ghost_tlktrn_dist = ghost_enc_tlktrn.groupby('encounterId')['tkltrn_ind'].apply(max)
print(ghost_tlktrn_dist.value_counts(normalize=True))

## Compare the distribution of real ghosting message and full data
plt.clf()
plt.hist(ghost_tlktrn_dist, bins=100, range=(1, 100), alpha=0.5, density=True, label='Ghost')
plt.hist(fulldata.data.groupby('encounterId')['tkltrn_ind'].apply(max), bins=100, range=(1, 100), alpha=0.5,
         density=True, label='Full Data')
plt.xlabel('Talk Turns')
plt.legend(loc='upper right')
plt.show()

## Compare distribution of positive and negative in ground truth
plt.clf()
temp = fulldata.data.loc[fulldata.data['encounterId'].isin(test.loc[test['label'] == 1, 'id'])].groupby('encounterId') \
    ['tkltrn_ind'].max()
plt.hist(temp, bins=100, range=(1, 100), alpha=0.5, density=True, label='Pos')
temp = fulldata.data.loc[fulldata.data['encounterId'].isin(test.loc[test['label'] == 0, 'id'])].groupby('encounterId') \
    ['tkltrn_ind'].max()
plt.hist(temp, bins=100, range=(1, 100), alpha=0.5, density=True, label='Neg')
plt.title('Test')
plt.xlabel('Talk Turns')
plt.legend(loc='upper right')
plt.show()

## check auc score
from sklearn.calibration import CalibratedClassifierCV
from sklearn.metrics import roc_auc_score

calibrator = CalibratedClassifierCV(svm.clf, cv='prefit')
model_ = calibrator.fit(vectorizer.train_X, vectorizer.train_y)
y_probs = [prob[1] for prob in model_.predict_proba(vectorizer.test_X)]
print('roc auc score: ', roc_auc_score(vectorizer.test_y, y_probs))


## plot confusion matrix
def print_confusion_matrix(conf_matrix):
    fig, ax = plt.subplots(figsize=(3.5, 3.5))
    ax.matshow(conf_matrix, cmap=plt.cm.Reds, alpha=0.3)
    for i in range(conf_matrix.shape[0]):
        for j in range(conf_matrix.shape[1]):
            ax.text(x=j, y=i, s=conf_matrix[i, j], va='center', ha='center', size='large')

            plt.xlabel('Predictions', fontsize=10)
            plt.ylabel('Actuals', fontsize=10)
            # plt.title('Confusion Matrix', fontsize=10)
            plt.xticks(ticks=[0, 1], labels=['No-dropout', 'dropout'])
            plt.yticks(ticks=[0, 1], labels=['No-dropout', 'dropout'], rotation='vertical', va='center')
    plt.show()

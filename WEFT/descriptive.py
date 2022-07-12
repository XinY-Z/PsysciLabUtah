import pandas as pd

file_path = r'C:\Users\XinZ\OneDrive\Curricula\Utah\My Research\WEFT'
ids = pd.read_csv(file_path + r'\backlink_map.csv')
data = pd.read_csv(file_path + r'\crowdsourcing_prolific_052222.csv')
data_part1 = data.loc[2:, 'QID2_1':'QID919_3']

ids_ = ids.iloc[:, [1, 3, 4, 5]]
ids_.loc[:, 'Choice A Resp Num'] = ids_['Choice A Resp Num'].astype(str) + '_' + ids_['Prompt Number'].astype(str)
ids_.loc[:, 'Choice B Resp Num'] = ids_['Choice B Resp Num'].astype(str) + '_' + ids_['Prompt Number'].astype(str)
ids_.loc[:, 'Choice C Resp Num'] = ids_['Choice C Resp Num'].astype(str) + '_' + ids_['Prompt Number'].astype(str)
del ids_['Prompt Number']
new_index = ids_.to_numpy().flatten()
data_part1.set_axis(new_index, axis=1, inplace=True)

with open('descriptives.csv', 'w') as f:
    print('ID,Prompt,Mean,Median,SD,N', file=f)
    for ID in data_part1.columns.unique():
        if isinstance(data_part1.loc[:, ID], pd.core.series.Series):
            subpart = data_part1.loc[:, ID].dropna().apply(int)
        else:
            subpart = data_part1.loc[:, ID].stack().apply(int)
        print(f'{ID.split("_")[0]},{ID.split("_")[1]},{subpart.mean()},{subpart.median()},{subpart.std()},'
              f'{subpart.count().sum()}', file=f)

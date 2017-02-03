# Python code to make multiple files on the basis of Call_Type and Day_Type

import pandas as pd
from joblib import Parallel, delayed
import multiprocessing

train_df =pd.read_csv('/home/dsuser/palash/code/coord/train.csv', sep=',')

#train_df['MISSING_DATA'] = train_df['MISSING_DATA'].apply(lambda x: x.lower())
_train_df = train_df[train_df['MISSING_DATA']==False]
_train_df['CALL_DAY'] = train_df['CALL_TYPE'] + '_'+train_df['DAY_TYPE']
del _train_df['MISSING_DATA']
del _train_df['CALL_TYPE']
del _train_df['DAY_TYPE']
del _train_df['ORIGIN_CALL']

# _train_df.head(10)
#_train_df_Grouped=_train_df.groupby('CALL_DAY')
#for name, group in _train_df_Grouped.groups.iteritems():
#    print name, len(_train_df_Grouped.get_group(name))

def mytmpFunc_df_to_csv(df):
    grp_name = list(set(df['CALL_DAY']))[0]
    grp_len = len(df)	
    grp_filename = '/tmp/tempproject/%s_%d.csv'%(grp_name, grp_len)							# file name
    print "%d records for CALL_DAY %s stored at %s"%(grp_len, grp_name,grp_filename)		# printing the entries
    del df['CALL_DAY']																		# This will remove the column of CALL_DAY
    df.to_csv(grp_filename, index=False)
    return 

def applyParallel(dfGrouped, func):
    retLst = Parallel(n_jobs=multiprocessing.cpu_count())(delayed(func)(group) for name, group in dfGrouped)
    return #pd.concat(retLst)

applyParallel(_train_df.groupby('CALL_DAY'), mytmpFunc_df_to_csv)

aa_df = pd.read_csv('/tmp/tempproject/A_A_364769.csv')			# Storing the count as well
ba_df = pd.read_csv('/tmp/tempproject/B_A_817878.csv')			
ca_df = pd.read_csv('/tmp/tempproject/C_A_528013.csv')			


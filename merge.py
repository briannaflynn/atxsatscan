#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np


# In[10]:


nhgis_df = pd.read_csv("./nhgis.csv", delimiter=",")

#print(nhgis_df["ZCTA5A"])
working_df = pd.read_csv("./zip_codes.csv", delimiter=",")
#print(working_df)

df_final = nhgis_df.merge(working_df, on="ZCTA5A", how="inner")
print(df_final)


df_final.to_csv("Filtered_NHGIS.csv", encoding="utf-8", index=False)


# In[ ]:





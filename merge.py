#!/usr/bin/env python
# coding: utf-8

import pandas as pd
import numpy as np

nhgis_df = pd.read_csv("./nhgis.csv", delimiter=",")

working_df = pd.read_csv("./zip_codes.csv", delimiter=",")

df_final = nhgis_df.merge(working_df, on="ZCTA5A", how="inner")
print(df_final)


df_final.to_csv("Filtered_NHGIS.csv", encoding="utf-8", index=False)






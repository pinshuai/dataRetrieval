#!/usr/bin/env python
# coding: utf-8

# In[9]:


import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt


# In[20]:


fig_dir = "~/Dropbox/Conferences_and_Meetings/PI meetings/PI meeting 2020/figures/"


# In[2]:


# data = pd.read_csv("~/Dropbox/Conferences_and_Meetings/PI meetings/PI meeting 2020/data/USGS_12510500_NO3.csv")


# In[32]:


data = pd.read_csv("./USGS-12505450_NO3.csv")


# In[33]:


data


# In[14]:


data['startDateTime'] = pd.to_datetime(data['startDateTime'])


# In[19]:


fig, ax = plt.subplots(1,1, figsize = (8, 6))

sns.lineplot(x = "startDateTime", y = "result_va", marker = "o",
             data = data)
ax.set_ylabel('Nitrate (mg/L)', fontsize = 18)
ax.set_xlabel('')
ax.set_title('USGS-12510500 (Yakima River at Kiona, WA)', fontsize = 24)

ax.tick_params(axis='both', which='major', labelsize=16)


# In[30]:


# fig.savefig(fig_dir + "USGS-12510500.png", dpi=300)
fig.savefig("./USGS-12510500.png", dpi=300)
plt.close(fig)


# ## from WQP station

# In[34]:


data = pd.read_csv("./USGS-12505450_NO3.csv")


# In[35]:


data


# In[37]:


data.columns


# In[36]:


data['ActivityStartDateTime'] = pd.to_datetime(data['ActivityStartDateTime'])


# In[40]:


fig, ax = plt.subplots(1,1, figsize = (8, 6))

sns.lineplot(x = "ActivityStartDateTime", y = "ResultMeasureValue", marker = "o",
             data = data)
ax.set_ylabel('Nitrate (mg/L)', fontsize = 18)
ax.set_xlabel('')
ax.set_title('USGS-12505450 (GRANGER DRAIN AT GRANGER, WA)', fontsize = 20, pad = 20)

ax.tick_params(axis='both', which='major', labelsize=16)


# In[41]:


# fig.savefig(fig_dir + "USGS-12510500.png", dpi=300)
fig.savefig("./USGS-12505450.png", dpi=300)
plt.close(fig)


# In[ ]:





import pandas as pd
import numpy as np
import json
import re

#Plotting
import matplotlib.pyplot as plt
import FCPython 

#Statistical fitting of models
import statsmodels.api as sm
import statsmodels.formula.api as smf

import os
import glob


pd.set_option('display.max_columns', None)


path_to_json = os.path.join(os.getcwd(), 'Wyscout_18_19/events/')

json_pattern = os.path.join(path_to_json,'*.json')

file_list = glob.glob(json_pattern)
#file_list = ['Wyscout_18_19/events/events_Germany.json']
    
dfs = pd.DataFrame()
df_pen = pd.DataFrame()
df_set_piece = pd.DataFrame()

for file in file_list:
    print(file)
    with open(file) as f:
        data = json.load(f)
    #dfs.append(data)
    #data = pd.read_json(file, lines=True) # read data frame from json file
    data = pd.DataFrame(data)# append the data frame to the list
    data_pen = data[data['subEventName'] == 'Penalty']
    data_set_piece = data[data['subEventName'].isin(['Free kick shot'])]
    data = data[data['subEventName'] == 'Shot']
    
    dfs = pd.concat([dfs, data], ignore_index=True)
    df_pen = pd.concat([df_pen, data_pen], ignore_index=True)
    df_set_piece = pd.concat([df_set_piece, data_set_piece])

print(dfs.shape)
dfs.to_csv("data_shots.csv")


shots=dfs[dfs['subEventName']=='Shot']
shots_model=pd.DataFrame(columns=['Goal','X','Y'])
header_model = pd.DataFrame(columns=['Goal', 'X', 'Y'])
penalty_model = pd.DataFrame(columns=['Goal', 'X', 'Y'])
set_piece_model = pd.DataFrame(columns=['Goal', 'X', 'Y'])

# Go through the dataframe and calculate X, Y co-ordinates.
# Distance from a line in the centre
# Shot angle.
# positions: the origin and destination positions associated with the event. Each position is a pair of coordinates (x,y.)
# The x and y coordinates are always in the range [0, 100] and indicate the percentage of the field from the perspective of the attacking team. 
# In particular, the value of the x coordinate indicates the event’s nearness (in percentage) to the opponent’s goal, 
# while the value of the y coordinates indicates the event’s nearness (in percentage) to the right side of the field;


for i,shot in shots.iterrows():
    
    header=0
    for shottags in shot['tags']:
        if shottags['id']==403:
            header_model.at[i,'X']=100-shot['positions'][0]['x']
            header_model.at[i,'Y']=shot['positions'][0]['y']
            header_model.at[i,'C']=abs(shot['positions'][0]['y']-50)
            header_model.at[i, 'matchId'] = shot['matchId']
            header_model.at[i, 'teamId'] = shot['teamId']
            header_model.at[i, 'eventSec'] = shot['eventSec']
            #Distance in metres and shot angle in radians.
            x=header_model.at[i,'X']*100/100 # ursprünglich *105/100 aber Sinn nicht gesehen bei Feldlänge Hälfte 50m 
            y=header_model.at[i,'C']*65/100
            header_model.at[i,'Distance']=np.sqrt(x**2 + y**2)
            a = np.arctan(7.32 *x /(x**2 + y**2 - (7.32/2)**2)) #Erklärung hierzu auf separater Notiz; folgt aus RR für Tangens
            if a<0:
                a=np.pi+a
            header_model.at[i,'Angle'] =a
            
            #Was it a goal
            header_model.at[i,'Goal']=0
            for shottags in shot['tags']:
                    #Tags contain that its a goal
                    if shottags['id']==101:
                        header_model.at[i,'Goal']=1
            header=1
    #Only include non-headers        
    if not(header):        
        shots_model.at[i,'X']=100-shot['positions'][0]['x']
        shots_model.at[i,'Y']=shot['positions'][0]['y']
        shots_model.at[i,'C']=abs(shot['positions'][0]['y']-50)
        shots_model.at[i, 'matchId'] = shot['matchId']
        shots_model.at[i, 'teamId'] = shot['teamId']
        shots_model.at[i, 'eventSec'] = shot['eventSec']
        #Distance in metres and shot angle in radians.
        x=shots_model.at[i,'X']*100/100 # ursprünglich *105/100 aber Sinn nicht gesehen bei Feldlänge Hälfte 50m 
        y=shots_model.at[i,'C']*65/100
        shots_model.at[i,'Distance']=np.sqrt(x**2 + y**2)
        a = np.arctan(7.32 *x /(x**2 + y**2 - (7.32/2)**2)) #Erklärung hierzu auf separater Notiz; folgt aus RR für Tangens
        if a<0:
            a=np.pi+a
        shots_model.at[i,'Angle'] =a
    
        #Was it a goal
        shots_model.at[i,'Goal']=0
        for shottags in shot['tags']:
                #Tags contain that its a goal
                if shottags['id']==101:
                    shots_model.at[i,'Goal']=1



for i, shot in df_pen.iterrows():
    penalty_model.at[i,'X']=100-shot['positions'][0]['x']
    penalty_model.at[i,'Y']=shot['positions'][0]['y']
    penalty_model.at[i,'C']=abs(shot['positions'][0]['y']-50)
    penalty_model.at[i, 'matchId'] = shot['matchId']
    penalty_model.at[i, 'teamId'] = shot['teamId']
    penalty_model.at[i, 'eventSec'] = shot['eventSec'] 
    
    #Distance in metres and shot angle in radians.
    x=penalty_model.at[i,'X']*100/100 # ursprünglich *105/100 aber Sinn nicht gesehen bei Feldlänge Hälfte 50m 
    y=penalty_model.at[i,'C']*65/100
    penalty_model.at[i,'Distance']=np.sqrt(x**2 + y**2)
    a = np.arctan(7.32 *x /(x**2 + y**2 - (7.32/2)**2)) #Erklärung hierzu auf separater Notiz; folgt aus RR für Tangens
    if a<0:
        a=np.pi+a
    penalty_model.at[i,'Angle'] =a

    #Was it a goal
    penalty_model.at[i,'Goal']=0
    for shottags in shot['tags']:
            #Tags contain that its a goal
            if shottags['id']==101:
                penalty_model.at[i,'Goal']=1
                
for i, shot in df_set_piece.iterrows():
    set_piece_model.at[i,'X']=100-shot['positions'][0]['x']
    set_piece_model.at[i,'Y']=shot['positions'][0]['y']
    set_piece_model.at[i,'C']=abs(shot['positions'][0]['y']-50)
    set_piece_model.at[i, 'matchId'] = shot['matchId']
    set_piece_model.at[i, 'teamId'] = shot['teamId']
    set_piece_model.at[i, 'eventSec'] = shot['eventSec'] 
    
    #Distance in metres and shot angle in radians.
    x=set_piece_model.at[i,'X']*100/100 # ursprünglich *105/100 aber Sinn nicht gesehen bei Feldlänge Hälfte 50m 
    y=set_piece_model.at[i,'C']*65/100
    set_piece_model.at[i,'Distance']=np.sqrt(x**2 + y**2)
    a = np.arctan(7.32 *x /(x**2 + y**2 - (7.32/2)**2)) #Erklärung hierzu auf separater Notiz; folgt aus RR für Tangens
    if a<0:
        a=np.pi+a
    set_piece_model.at[i,'Angle'] =a

    #Was it a goal
    set_piece_model.at[i,'Goal']=0
    for shottags in shot['tags']:
            #Tags contain that its a goal
            if shottags['id']==101:
                set_piece_model.at[i,'Goal']=1

                    
shots_model = shots_model.reset_index(drop=True)
header_model = header_model.reset_index(drop=True)
penalty_model = penalty_model.reset_index(drop=True)
set_piece_model = set_piece_model.reset_index(drop=True)


print(shots_model.shape)
print(header_model.shape)
print(penalty_model.shape)
print(set_piece_model.shape)
                    
shots_model.to_csv('shots_model.csv')
header_model.to_csv('header_model.csv')
penalty_model.to_csv('penalty_model.csv')
set_piece_model.to_csv('set_piece_model.csv')

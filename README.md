# Krag_MA
## Programmiercode Masterarbeit Colin Krag 
## Statistische Paarvergleichsmodelle für Spielstärken im Fußball unter Einbeziehung von Expected Goals: Modellanpassung und Prädiktion
Info: Alles aus 14/15 war ursprünglich zum Testen und ist nicht eingegangen in die Arbeit 
Daten aus Wyscout 18/19. Als Ordner 'Wyscout_18_19' eigenständig hinzuzufügen.
Hier: 
'shots_model.csv' enthält alle Schüsse, 'header_model.csv' alle Kopfbälle, etc. -> siehe 'shot_analysis.ipynb'. Dadurch Datenquelle hier nicht explizit benötigt.


- 'FCPython.py' zur Erstellung der Spielfelder aus Datenquelle FriendsOfTracking, siehe Quellen. 

Aufstellen eigenes Expected Goals Modell: 
- 'grab_data.py'
    Erstellt die 'shots_model.csv' etc. aus Ordner 'Wyscout_18_19'.
    Dazu X und Y Koordinaten jeder Aktion kalkulieren zB.. 

- 'get_matches_with_goal_count.ipynb'
    - erzeugt Datei 'matches_ger_with_goals_count.csv'   
- 'shot_analysis.ipynb': 
    - Input: 'shots_model.csv' + vglw. für Kopfbälle etc. mit Daten aller Schüsse aus Datensatz von 'Wyscout_18_19' aus Saison 17/18 plus WM und EM 
    - Output:
        - 'Output/NumberOfShots.pdf' Schüsse pro Cluster Plot 
        - 'Output/NumberOfGoals.pdf' Tore pro Cluster Plot 
        - 'Output/ProbabilityOfScoring.pdf' empirische Torwahrscheinlichkeit pro Cluster 
        - 'Output/ProbabilityOfScoringAngleFit.pdf' Modell fit für Winkel 
        - 'Output/ProbabilityOfScoringDistance.pdf' Modell fit für Distanz 
        - 'Output/ProbabilityOfScoringDistanceSquared.pdf' Modell fit mit quadrierter Distanz 
        - 'Output/goalprobfor_' + model  + '.pdf' für gegebenes Modell 'model' Torwahrscheinlichkeiten 2D Map
        - 'Output/NumberOfHeaders.pdf' s.o. mit Kopfbällen 
        - 'Output/NumberOfGoalsHeaders.pdf' s.o. mit Kopfbällen 
        - 'Output/ProbabilityOfScoringHeaders.pdf' s.o. 
        - 'Output/goalprobfor_' + model  + 'OfHeaders.pdf' s.o. für Kopfbälle 
        - 'Output/NumberOfSetPieces.pdf' mit Standardsituationen noch 
        - 'Output/NumberOfGoalsSetPieces.pdf' s.o.
        - 'Output/ProbabilityOfScoringSetPieces.pdf' s.o. 
        - 'Output/goalprobfor_' + model  + 'OfSetPieces.pdf' s.o. 
        - 'xG_shot.csv' berechnete xG für Schüsse 
        - 'xG_pen.csv' berechnete xG für Elfmeter 
        - 'xG_set_pieces.csv' berechnete xG für Standards 
        - 'xG_head.csv' ... für Kopfbälle
        - 'xG_full.csv' mit allem zur weiteren Verabeitung

    
    Berechnet die xGs für ggb. Schüsse/Kopfbälle/Standards/Elfmeter plus Analyse 

- 'create_xG_scores.ipynb'
    - Erstellt für gegebene Matches in GER und die verschiedenen Abschlüsse die Alternativergebnisse nach xG 'matches_ger_with_goals_count.csv'
    - Erzeugen der xG Beispiele aus Arbeit zwischen Leverkusen und Bayern 
    - kleinere Tests, die es nicht in die Arbeit geschafft haben 
    - Erzeugen der ordinalskalierten Response pro Partie und Exportieren der Dateien nach reinen Ergebnissen 'df_scores.csv', ordrespxG (kein Unentschieden - später weggelassen) und 
        ordroundxG als 'df_scores_xG_2.csv' aus 'xG_full.csv'

- 'bt.ipynb'
    - Erstellt Design Matrizen und Response für BT Kalkulationen in R 
    - kleinere Experimente, die auch tlw. benutzt in Arbeit mit den Ergebnissen aus R Kalkulationen
    - Zur Beachtung: Alles aus Saison 14/15 war ursprünglich ein Test 
    - Korrelationsanalyse für Siegpunkte 


- 'xG_ext.ipynb' 
    - Erstellt Alternativergebnisse für xG-Werte von Opta 
    - bereit für Import in R 
    
- 'odds.ipynb'
    - Erstellt den Quotenvektor für die Quoten von Betfair, der später in R importiert wird 


- 'bt.R'
    - komplette Anwendung der BT Modelle plus Validierung/Prädiktion 
    - wichtigstes File mit Ergebnissen 

- 'requirements.txt'
    - installierte Pakete; Installation über '.venv' möglich 


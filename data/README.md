## Daten Überblick

| Variable | vorhanden? | von | bis | day ahead? | Kurzkommentar | Variablendimension |
| -------- | ---------- | ---- | ---- | ---- | -------------------- | ---- |
| PUN | DE ja AT vill  | 2011 | 2018 | ja | Noch überprüfen ob DE/AT | €/MWh |
| DMD | DE ja AT ja  | 2015 | 2018 | ja | inkl. Luxemburg | MW(?) |
| SOLAR | DE ja AT ja  | 2015 | 2018 | ja | - | MW |
| WIND  | DE ja AT ja  | 2015 | 2018 | ja | <2015 inkomp. mit AT | MW |
| GAS_Price | DE nein AT nein  | - | 2018 | - | 3 Preise? | - |


## PUN (Strom - Day-Ahead Preis )
#### https://www.energidataservice.dk/en/dataset/elspotprices/resource_extract/c86859d2-942e-4029-aec1-32d56f1a2e5d
**Data description:** Daten aus Dänemark, sehen aber sehr gut aus und gehen bis 2010.


## DMD (DE/AT/LU Day Ahead)
#### https://transparency.entsoe.eu/load-domain/r2/totalLoadR2/show?name=&defaultValue=false&viewType=TABLE&areaType=BZN&atch=false&dateTime.dateTime=18.05.2018+00:00|CET|DAY&biddingZone.values=CTY|10Y1001A1001A83F!BZN|10Y1001A1001A63L&dateTime.timezone=CET_CEST&dateTime.timezone_input=CET+(UTC+1)+/+CEST+(UTC+2)

## WIND+SOLAR DE (Day Ahead Prognose, seit 2015):
#### https://transparency.entsoe.eu/generation/r2/dayAheadGenerationForecastWindAndSolar/show?name=&defaultValue=false&viewType=TABLE&areaType=CTY&atch=false&dateTime.dateTime=01.05.2015+00:00|CET|DAYTIMERANGE&dateTime.endDateTime=01.05.2015+00:00|CET|DAYTIMERANGE&area.values=CTY|10Y1001A1001A83F!CTY|10Y1001A1001A83F&productionType.values=B16&productionType.values=B18&productionType.values=B19&processType.values=A18&processType.values=A01&processType.values=A40&dateTime.timezone=CET_CEST&dateTime.timezone_input=CET+(UTC+1)+/+CEST+(UTC+2)

#### WIND DE (Day Ahead Prognose, seit 2012):
###### https://www.netztransparenz.de/Weitere-Veroeffentlichungen/Windenergie-Prognose
Hier ist Direktvermarktung nicht mit drin, daher sind die Daten unvollständig.

#### WIND DE (Day Ahead Gehandelt, seit 2012):
###### https://www.netztransparenz.de/Erneuerbare-Energien-Gesetz/Transparenzanforderungen/Vermarktung-1h-Auktion-Windenergie

## SOLAR DE (Day Ahead Prognose, seit 2012):
#### https://www.netztransparenz.de/Weitere-Veroeffentlichungen/Solarenergie-Prognose

#### SOLAR DE (Day Ahead Gehandelt, seit 2012) 
###### https://www.netztransparenz.de/Weitere-Veroeffentlichungen/Solarenergie-Prognose

## WIND+SOLAR AT (Day Ahead Prognose, seit 2012):
#### https://www.apg.at/de/markt/Markttransparenz/erzeugung/erzeugungsprognose
**Data description:** Aus Gründen der Konsistenz mit den nach der Verordnung (EU) Nr. 543/2013 veröffentlichten Daten wird seit 01.01.2015 in der folgenden Darstellung die prognostizierte Leistung aller in die Regelzone APG einspeisenden Windkraftanlagen berücksichtigt. Die historischen Werte bis einschließlich 31.12.2014 errechnen sich weiterhin aus allen in die Ökobilanzgruppe der Regelzone APG einspeisenden Windkraftanlagen.
Die Wind- und Solarerzeugungsprognose wird in der Regel bis ca. 11:00 Uhr für den Folgetag bereitgestellt. Ab diesem Zeitpunkt stehen im Normalfall auch indikative Werte für den übernächsten Tag zur Verfügung.
Eine Aktualisierung der Wind- und Solarerzeugungsprognose wird täglich um ca. 08:00 Uhr für den laufenden Tag durchgeführt.
Die gesamte Erzeugungsprognose aller Erzeugungs-Typen wird üblicherweise um  18:00 Uhr für den Folgetag bereitgestellt.
Seit 2011 ist das Tiroler Netzgebiet in der veröffentlichten Erzeugungsprognose inkludiert. Seit 2012 umfasst die Regelzone APG ganz Österreich mit Ausnahme eines Korridors in Vorarlberg.

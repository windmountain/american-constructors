module Data exposing (csvData)


csvData : String
csvData =
    """Id,Section,Name,Deps on (1),Deps on (2),Deps on (3),Deps on (4),Estimate,Low Estimate,High Estimate,Effective End,Weather-dependent,Can Expedite,Date,Duration,ES,EF,LF,LS,Slack
T0,terrace,terrace start,P0,,,,,,,,,,,0,0,0,16.5,16.5,16.5
T1,terrace,window surrounds,T0,,,,8,,,,,,,8,0,8,24.5,16.5,16.5
T2,terrace,waterproofing,T1,,,,7,,,,,,,7,8,15,31.5,24.5,16.5
T3,terrace,insulation,T2,,,,3,,,,,,,3,15,18,34.5,31.5,16.5
T4,terrace,deck concrete,T3,,,,,5,10,,yes,,,7.5,18,25.5,42,34.5,16.5
T5,terrace,stairs concrete,T4,,,,10,,,,,yes,,10,25.5,35.5,52,42,16.5
T6,terrace,aluminum rails,T4,,,,5,,,,,,,5,25.5,30.5,52,47,21.5
T7,terrace,masonry,T6,T5,,,10,,,,,,,10,35.5,45.5,62,52,16.5
T8,terrace,cleanup and ACI punch list,P12,,,,1,,,,,,,1,45.5,46.5,63,62,16.5
B0,bookstore,bookstore start,P0,,,,,,,,,,,0,0,0,2,2,2
B1,bookstore,drywall,B0,,,,,16,18,,,,,17,0,17,19,2,2
B2,bookstore,hard tile,B1,,,,10,,,,,,,10,17,27,39,29,12
B3,bookstore,stone columns,B1,,,,5,,,,,,,5,17,22,39,34,17
B4,bookstore,millwork,B1,,,,,15,21,,,yes,,18,17,35,37,19,2
B5,bookstore,casework,B4,,,,5,,,,,,,5,35,40,42,37,2
B6,bookstore,flooring,B5,,,,10,,,,,,,10,40,50,52,42,2
B7,bookstore,glass installation,B3,B2,,,3,,,,,,,3,27,30,42,39,12
B8,bookstore,painting,B7,,,,10,,,,,,,10,30,40,52,42,12
B11,bookstore,cleanup and ACI punch list,P14,,,,1,,,,,,,1,60,61,63,62,2
B9,bookstore,doors and hardware,B8,B6,,,,5,8,,,,,6.5,50,56.5,62,55.5,5.5
B10,bookstore,MEP,B8,B6,,,10,,,,,,,10,50,60,62,52,2
S0,sanctuary,sanctuary start,P0,,,,,,,,,,,0,0,0,0,0,0
S1,sanctuary,drywall,S0,,,,16,,,,,,,16,0,16,42,26,26
S2,sanctuary,core drill for rails,S0,,,,2,,,,,,,2,0,2,2,0,0
S3,sanctuary,install rails,S2,,,,5,,,,,,,5,2,7,57,52,50
S4,sanctuary,install carpeting at seats,S2,,,,15,,,,,,,15,2,17,17,2,0
S5,sanctuary,carpeting at rails,S3,,,,5,,,,,,,5,7,12,62,57,50
S6,sanctuary,"wood paneling, trim and stage",S4,,,,25,,,,,,,25,17,42,42,17,0
S7,sanctuary,painting,S10,S8,S4,,20,,,,,,,20,17,37,55.5,35.5,18.5
S8,sanctuary,concrete floor staining,S10,,,,5,,,,,,,5,7,12,35.5,30.5,23.5
S9,sanctuary,installation of seats,S1,S6,,,20,,,,,,,20,42,62,62,42,0
S10,sanctuary,wood stage steps,S2,,,,5,,,,,,,5,2,7,30.5,25.5,23.5
S11,sanctuary,"install carpet (steps, flats, aisles)",S10,,,,5,,,,,,,5,7,12,62,57,50
S12,sanctuary,install doors and hardware,S7,S6,,,,5,8,,,,,6.5,42,48.5,62,55.5,13.5
S13,sanctuary,cleanup and ACI punch list,P11,,,,1,,,,,,,1,62,63,63,62,0
L0,lobby,lobby start,P0,,,,,,,,,,,0,0,0,41.75,41.75,41.75
L1,lobby,millwork for reception,L0,,,,3,,,,,,,3,0,3,44.75,41.75,41.75
L2,lobby,millwork for walls and rails,L0,,,,10,,,,,,,10,0,10,60.25,50.25,50.25
L3,lobby,hard ceiling,L0,,,,15,,,,,,,15,0,15,60.25,45.25,45.25
L4,lobby,install drywall,L0,,,,15,,,,,,,15,0,15,60.25,45.25,45.25
L5,lobby,painting,L4,L3,L2,,5,,,,,,,5,15,20,65.25,60.25,45.25
L6,lobby,concrete for carpet areas,L1,,,,,5,8,,,,,6.5,3,9.5,51.25,44.75,41.75
L7,lobby,hard tile,L4,,,,5,,,,,,,5,15,20,80.25,75.25,60.25
L8,lobby,wood flooring install,L6,,,,,20,28,,,"yes, formalize acclimatization precisely",,24,9.5,33.5,75.25,51.25,41.75
L9,lobby,"other floors, carpeting",L8,,,,5,,,,,,,5,33.5,38.5,80.25,75.25,41.75
L10,lobby,public restrooms,L3,L4,,,9,,,,,,,9,15,24,70.25,61.25,46.25
L11,lobby,glass and chandeliers,L3,L4,,,3,,,,,,,3,15,18,80.25,77.25,62.25
L12,lobby,ceiling tiles,L5,,,,5,,,,,,,5,20,25,70.25,65.25,45.25
L13,lobby,doors and hardware,L12,L10,,,10,,,,,,,10,25,35,80.25,70.25,45.25
L14,lobby,MEP,L12,L10,,,10,,,,,,,10,25,35,80.25,70.25,45.25
L15,lobby,cleanup and ACI punch list,P14,,,,1,,,,,,,1,60,61,63,62,2
P0,overall,Now,,,,,,,,,,,09/24/2009,0,0,0,0,0,0
P2,overall,Architect’s punch list tasks,P5,,,,,5,10,,,,,7.5,72,79.5,79.5,72,0
P3,overall,Inform architect of close out responsibilities,P0,,,,,,,,,,,0,0,0,46,46,46
P4,overall,Items completed by architecture firm,P3,,,,,14,28,,,,,21,0,21,67,46,46
P5,overall,fire marshal’s inspection,P9,P4,,,5,,,,,,,5,67,72,72,67,0
P6,overall,sign general guarantee and warranty,P2,,,,,0.5,1,,,,,0.75,79.5,80.25,80.25,79.5,0
P7,overall,sign final release of lien,P2,,,,,,,,,,,0,79.5,79.5,80.25,80.25,0.75
P9,overall,cleanup and ACI punch list,P15,,,,4,,,,,,,4,63,67,67,63,0
P10,overall,lobby done except cleanup and ACI punch list,L14,L13,L9,,,,,,,,,0,38.5,38.5,80.25,80.25,41.75
P11,overall,sanctuary done except cleanup and ACI punch list,S11,S12,S9,S5,,,,,,,,0,62,62,62,62,0
P12,overall,terrace done except cleanup and ACI punch list,T7,,,,,,,,,,,0,45.5,45.5,62,62,16.5
P14,overall,bookstore done except cleanup and ACI punch list,B9,B10,,,,,,,,,,0,60,60,62,62,2
P15,overall,all sections done,L15,S13,B11,T8,,,,,,,,0,63,63,63,63,0
P16,overall,occupancy permitted,P5,,,,,,,yes,,,,0,72,72,80.25,80.25,8.25"""

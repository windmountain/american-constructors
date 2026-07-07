module Data exposing (csvData)


csvData : String
csvData =
    """Id,Section,Name,Deps on (1),Deps on (2),Deps on (3),Deps on (4),Estimate,Low Estimate,High Estimate,Effective End,Weather-dependent,Can Expedite,Date,Duration,ES,EF,LF,LS,Slack
T0,terrace,terrace start,P0,,,,,,,,,,,0,0,0,0,0,0
T1,terrace,window surrounds,T0,,,,8,,,,,,,8,0,8,8,0,0
T2,terrace,waterproofing,T1,,,,5,,,,,,,5,8,13,13,8,0
T3,terrace,insulation,T2,,,,3,,,,,,,3,13,16,16,13,0
T4,terrace,deck concrete,T3,,,,,5,10,,yes,,,7.5,16,23.5,23.5,16,0
T5,terrace,stairs concrete,T4,,,,10,,,,,yes,,10,23.5,33.5,33.5,23.5,0
T6,terrace,aluminum rails,T4,,,,5,,,,,,,5,23.5,28.5,33.5,28.5,5
T7,terrace,masonry,T6,T5,,,10,,,,,,,10,33.5,43.5,43.5,33.5,0
T8,terrace,cleanup and ACI punch list,P12,,,,1,,,,,,,1,43.5,44.5,44.5,43.5,0
B0,bookstore,bookstore start,P0,,,,,,,,,,,0,0,0,2.5,2.5,2.5
B1,bookstore,drywall,B0,,,,,2,4,,,,,3,0,3,5.5,2.5,2.5
B2,bookstore,hard tile,B1,,,,10,,,,,,,10,3,13,20.5,10.5,7.5
B3,bookstore,stone columns,B1,,,,5,,,,,,,5,3,8,20.5,15.5,12.5
B4,bookstore,millwork,B1,,,,,10,16,,,yes,,13,3,16,18.5,5.5,2.5
B5,bookstore,casework,B4,,,,5,,,,,,,5,16,21,23.5,18.5,2.5
B6,bookstore,flooring,B5,,,,10,,,,,,,10,21,31,33.5,23.5,2.5
B7,bookstore,glass installation,B3,B2,,,3,,,,,,,3,13,16,23.5,20.5,7.5
B8,bookstore,painting,B7,,,,10,,,,,,,10,16,26,33.5,23.5,7.5
B11,bookstore,cleanup and ACI punch list,P14,,,,1,,,,,,,1,41,42,44.5,43.5,2.5
B9,bookstore,doors and hardware,B8,B6,,,,5,8,,,,,6.5,31,37.5,43.5,37,6
B10,bookstore,MEP,B8,B6,,,10,,,,,,,10,31,41,43.5,33.5,2.5
S0,sanctuary,sanctuary start,P0,,,,,,,,,,,0,0,0,6,6,6
S1,sanctuary,drywall,S0,,,,16,,,,,,,16,0,16,33.5,17.5,17.5
S2.1,sanctuary,core drill for rails (left side),S0,,,,1,,,,,,,1,0,1,9.5,8.5,8.5
S2.2,sanctuary,core drill for rails (right side),S0,,,,1,,,,,,,1,0,1,7,6,6
S3.1,sanctuary,install rails (left side),S2.1,,,,2.5,,,,,,,2.5,1,3.5,38.5,36,35
S3.2,sanctuary,install rails (right side),S2.2,,,,2.5,,,,,,,2.5,1,3.5,38.5,36,35
S4.1,sanctuary,install carpeting at seats (left side),S2.1,,,,7.5,,,,,,,7.5,1,8.5,17,9.5,8.5
S4.2,sanctuary,install carpeting at seats (right side),S2.2,,,,7.5,,,,,,,7.5,1,8.5,17,9.5,8.5
S5,sanctuary,carpeting at rails,S3.1,S3.2,,,5,,,,,,,5,3.5,8.5,43.5,38.5,35
S6.1,sanctuary,"wood paneling, trim and stage (left side)",S4.1,,,,12.5,,,,,,,12.5,8.5,21,51.75,39.25,30.75
S6.2,sanctuary,"wood paneling, trim and stage (right side)",S4.2,,,,12.5,,,,,,,12.5,8.5,21,33.5,21,12.5
S7,sanctuary,painting,S10,S8,S4.1,S4.2,20,,,,,,,20,11,31,37,17,6
S8,sanctuary,concrete floor staining,S10,,,,5,,,,,,,5,6,11,17,12,6
S9.1,sanctuary,installation of seats (left side),S1,S6.1,,,10,,,,,,,10,21,31,61.75,51.75,30.75
S9.2,sanctuary,installation of seats (right side),S1,S6.2,,,10,,,,,,,10,21,31,43.5,33.5,12.5
S10,sanctuary,wood stage steps,S2.2,,,,5,,,,,,,5,1,6,12,7,6
S11,sanctuary,"install carpet (steps, flats, aisles)",S10,,,,5,,,,,,,5,6,11,43.5,38.5,32.5
S12,sanctuary,install doors and hardware,S7,S6.2,,,,5,8,,,,,6.5,31,37.5,43.5,37,6
S13,sanctuary,cleanup and ACI punch list,P11,,,,1,,,,,,,1,37.5,38.5,44.5,43.5,6
L0,lobby,lobby start,P0,,,,,,,,,,,0,0,0,5,5,5
L1,lobby,millwork for reception,L0,,,,3,,,,,,,3,0,3,8,5,5
L2,lobby,millwork for walls and rails,L0,,,,10,,,,,,,10,0,10,23.5,13.5,13.5
L3,lobby,hard ceiling,L0,,,,15,,,,,,,15,0,15,23.5,8.5,8.5
L4,lobby,install drywall,L0,,,,15,,,,,,,15,0,15,23.5,8.5,8.5
L5,lobby,painting,L4,L3,L2,,5,,,,,,,5,15,20,28.5,23.5,8.5
L6,lobby,concrete for carpet areas,L1,,,,,5,8,,,,,6.5,3,9.5,14.5,8,5
L7,lobby,hard tile,L4,,,,5,,,,,,,5,15,20,61.75,56.75,41.75
L8,lobby,wood flooring install,L6,,,,,20,28,,,"yes, formalize acclimatization precisely",,24,9.5,33.5,38.5,14.5,5
L9,lobby,"other floors, carpeting",L8,,,,5,,,,,,,5,33.5,38.5,43.5,38.5,5
L10,lobby,public restrooms,L3,L4,,,9,,,,,,,9,15,24,33.5,24.5,9.5
L11,lobby,glass and chandeliers,L3,L4,,,3,,,,,,,3,15,18,61.75,58.75,43.75
L12,lobby,ceiling tiles,L5,,,,5,,,,,,,5,20,25,33.5,28.5,8.5
L13,lobby,doors and hardware,L12,L10,,,10,,,,,,,10,25,35,43.5,33.5,8.5
L14,lobby,MEP,L12,L10,,,10,,,,,,,10,25,35,43.5,33.5,8.5
L15,lobby,cleanup and ACI punch list,P10,,,,1,,,,,,,1,38.5,39.5,44.5,43.5,5
P0,overall,Now,,,,,,,,,,,09/24/2009,0,0,0,0,0,0
P2,overall,Architect’s punch list tasks,P5,,,,,5,10,,,,,7.5,53.5,61,61,53.5,0
P3,overall,Inform architect of close out responsibilities,P0,,,,,,,,,,,0,0,0,27.5,27.5,27.5
P4,overall,Items completed by architecture firm,P3,,,,,14,28,,,,,21,0,21,48.5,27.5,27.5
P5,overall,fire marshal’s inspection,P9,P4,,,5,,,,,,,5,48.5,53.5,53.5,48.5,0
P6,overall,sign general guarantee and warranty,P2,,,,,0.5,1,,,,,0.75,61,61.75,61.75,61,0
P7,overall,sign final release of lien,P2,,,,,,,,,,,0,61,61,61.75,61.75,0.75
P9,overall,cleanup and ACI punch list,P15,,,,4,,,,,,,4,44.5,48.5,48.5,44.5,0
P10,overall,lobby done except cleanup and ACI punch list,L14,L13,L9,,,,,,,,,0,38.5,38.5,43.5,43.5,5
P11,overall,sanctuary done except cleanup and ACI punch list,S11,S12,S9.2,S5,,,,,,,,0,37.5,37.5,43.5,43.5,6
P12,overall,terrace done except cleanup and ACI punch list,T7,,,,,,,,,,,0,43.5,43.5,43.5,43.5,0
P14,overall,bookstore done except cleanup and ACI punch list,B9,B10,,,,,,,,,,0,41,41,43.5,43.5,2.5
P15,overall,all sections done,L15,S13,B11,T8,,,,,,,,0,44.5,44.5,44.5,44.5,0
P16,overall,occupancy permitted,P5,,,,,,,yes,,,,0,53.5,53.5,61.75,61.75,8.25"""

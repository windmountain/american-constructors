module Data exposing (csvData)


csvData : String
csvData =
    """Id,Section,Name,Deps on (1),Deps on (2),Deps on (3),Deps on (4),Estimate,Low Estimate,High Estimate,Weather-dependent,Can Expedite,Date,Duration,ES,EF,LF,LS,Slack
T0,terrace,terrace start,P0,,,,,,,,,,0,0,0,19.5,19.5,19.5
T1,terrace,window surrounds,T0,,,,8,,,,,,8,0,8,27.5,19.5,19.5
T2,terrace,waterproofing,T1,,,,7,,,,,,7,8,15,34.5,27.5,19.5
T3,terrace,insulation,T2,,,,3,,,,,,3,15,18,37.5,34.5,19.5
T4,terrace,deck concrete,T3,,,,,5,10,yes,,,7.5,18,25.5,45,37.5,19.5
T5,terrace,stairs concrete,T4,,,,10,,,,yes,,10,25.5,35.5,55,45,19.5
T6,terrace,aluminum rails,T4,,,,5,,,,,,5,25.5,30.5,55,50,24.5
T7,terrace,masonry,T6,T5,,,10,,,,,,10,35.5,45.5,65,55,19.5
B0,b/c/k,bookstore start,P0,,,,,,,,,,0,0,0,0,0,0
B1,b/c/k,drywall,B0,,,,,21,23,,,,22,0,22,22,0,0
B2,b/c/k,hard tile,B1,,,,10,,,,,,10,22,32,42,32,10
B3,b/c/k,stone columns,B1,,,,5,,,,,,5,22,27,42,37,15
B4,b/c/k,millwork,B1,,,,,15,21,,yes,,18,22,40,40,22,0
B5,b/c/k,casework,B4,,,,5,,,,,,5,40,45,45,40,0
B6,b/c/k,flooring,B5,,,,10,,,,,,10,45,55,55,45,0
B7,b/c/k,glass installation,B3,B2,,,3,,,,,,3,32,35,45,42,10
B8,b/c/k,painting,B7,,,,10,,,,,,10,35,45,55,45,10
B9,b/c/k,doors and hardware,B8,B6,,,,5,8,,,,6.5,55,61.5,65,58.5,3.5
B10,b/c/k,MEP,B8,B6,,,10,,,,,,10,55,65,65,55,0
S0,sanctuary,sanctuary start,P0,,,,,,,,,,0,0,0,3,3,3
S1,sanctuary,drywall,S0,,,,16,,,,,,16,0,16,45,29,29
S2,sanctuary,core drill for rails,S0,,,,2,,,,,,2,0,2,5,3,3
S3,sanctuary,install rails,S2,,,,5,,,,,,5,2,7,60,55,53
S4,sanctuary,install carpeting at seats,S2,,,,15,,,,,,15,2,17,20,5,3
S5,sanctuary,carpeting at rails,S3,,,,5,,,,,,5,7,12,65,60,53
S6,sanctuary,"wood paneling, trim and stage",S4,,,,25,,,,,,25,17,42,45,20,3
S7,sanctuary,painting,S10,S8,S4,,20,,,,,,20,17,37,58.5,38.5,21.5
S8,sanctuary,concrete floor staining,S10,,,,5,,,,,,5,7,12,38.5,33.5,26.5
S9,sanctuary,installation of seats,S1,S6,,,20,,,,,,20,42,62,65,45,3
S10,sanctuary,wood stage steps,S2,,,,5,,,,,,5,2,7,33.5,28.5,26.5
S11,sanctuary,"install carpet (steps, flats, aisles)",S10,,,,5,,,,,,5,7,12,65,60,53
S12,sanctuary,install doors and hardware,S7,S6,,,,5,8,,,,6.5,42,48.5,65,58.5,16.5
L0,lobby,lobby start,P0,,,,,,,,,,0,0,0,26.5,26.5,26.5
L1,lobby,millwork for reception,L0,,,,3,,,,,,3,0,3,29.5,26.5,26.5
L2,lobby,millwork for walls and rails,L0,,,,10,,,,,,10,0,10,45,35,35
L3,lobby,hard ceiling,L0,,,,15,,,,,,15,0,15,45,30,30
L4,lobby,install drywall,L0,,,,15,,,,,,15,0,15,45,30,30
L5,lobby,painting,L4,L3,L2,,5,,,,,,5,15,20,50,45,30
L6,lobby,concrete for carpet areas,L1,,,,,5,8,,,,6.5,3,9.5,36,29.5,26.5
L7,lobby,hard tile,L4,,,,5,,,,,,5,15,20,438.25,433.25,418.25
L8,lobby,wood flooring install,L6,,,,,20,28,,"yes, formalize acclimatization precisely",,24,9.5,33.5,60,36,26.5
L9,lobby,"other floors, carpeting",L8,,,,5,,,,,,5,33.5,38.5,65,60,26.5
L10,lobby,public restrooms,L3,L4,,,9,,,,,,9,15,24,55,46,31
L11,lobby,glass and chandeliers,L3,L4,,,3,,,,,,3,15,18,438.25,435.25,420.25
L12,lobby,ceiling tiles,L5,,,,5,,,,,,5,20,25,55,50,30
L13,lobby,doors and hardware,L12,L10,,,10,,,,,,10,25,35,65,55,30
L14,lobby,MEP,L12,L10,,,10,,,,,,10,25,35,65,55,30
P0,overall,Now,,,,,,,,,,09/24/2009,0,0,0,0,0,0
P1,overall,Nominal finish date,P15,,,,,,,,,12/14/2009,0,65,65,65,65,0
P2,overall,Architect’s punch list tasks,P1,,,,,5,10,,,,7.5,65,72.5,72.5,65,0
P3,overall,Inform architect of close out responsibilities,P0,,,,,,,,,,0,0,0,413.25,413.25,413.25
P4,overall,Items completed by architecture firm,P3,,,,,14,28,,,,21,0,21,434.25,413.25,413.25
P5,overall,fire marshal’s inspection,P2,P4,,,,,,,,,0,72.5,72.5,434.25,434.25,361.75
P6,overall,sign general guarantee and warranty,P2,,,,,0.5,1,,,,0.75,72.5,73.25,73.25,72.5,0
P7,overall,sign final release of lien,P2,,,,,,,,,,0,72.5,72.5,438.25,438.25,365.75
P8,overall,ACL relinquished of responsibilities,P6,,,,365,,,,,,365,73.25,438.25,438.25,73.25,0
P9,overall,cleanup and ACL internal punch list,P5,,,,4,,,,,,4,72.5,76.5,438.25,434.25,361.75
P10,overall,lobby done,L14,L13,L9,,,,,,,,0,38.5,38.5,65,65,26.5
P11,overall,sanctuary done,S11,S12,S9,S5,,,,,,,0,62,62,65,65,3
P12,overall,terrace done,T7,,,,,,,,,,0,45.5,45.5,65,65,19.5
P14,overall,b/c/k done,B9,B10,,,,,,,,,0,65,65,65,65,0
P15,overall,all sections done,P14,P11,P10,P12,,,,,,,0,65,65,65,65,0"""

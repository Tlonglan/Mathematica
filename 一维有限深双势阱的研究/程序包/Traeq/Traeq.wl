(* ::Package:: *)

BeginPackage["Traeq`"]

Traeq::usage="Traeq[hv1,hv3,hv4,hv5,lwei,mwei,rwei,toler]\:ff1a\:56fe\:89e3\:8d85\:8d8a\:65b9\:7a0b\:ff0c\:524d4\:4e2a\:4e3a\:52bf\:9631\:4ece\:5de6\:5230\:53f3\:7684\:52bf\:9631\:9ad8\:5ea6\:ff0c\:7b2c5~7\:4e2a\:4e3a\:5de6\:4e2d\:53f3\:52bf\:5792\:5bbd\:ff0c\:6700\:540e\:4e00\:4e2a\:4e3a\:884c\:5316\:7b80\:7684\:5141\:8bb8\:8bef\:5dee"

Begin["`Traeq`"]

Traeq[hv1_,hv3_,hv4_,hv5_,
lwei_,mwei_,rwei_,toler_]:=Moudle[
{fw,a,b,c,\[Alpha],\[Beta],\[Gamma],\[Eta],\[Delta],
t,J,me,hb,
a1,b1,c1,
V01,V03,V04,V05,Vm,
\[Alpha]1,\[Beta]1,\[Gamma]1,\[Eta]1,\[Delta]1,
nf01,pnf,Extc,Ex},
fw=Function[{a,b,c,\[Alpha],\[Beta],\[Gamma],\[Eta],\[Delta]},(2 \[Beta] Cos[(a-b) \[Beta]] (\[Delta] (-(\[Alpha]-\[Gamma]) (\[Gamma]-\[Eta])-E^(4 a \[Gamma]) (\[Alpha]+\[Gamma]) (\[Gamma]+\[Eta])) Cos[(a-c) \[Delta]]+(E^(4 a \[Gamma]) (\[Alpha]+\[Gamma]) (-\[Delta]^2+\[Gamma] \[Eta])+(\[Alpha]-\[Gamma]) (\[Delta]^2+\[Gamma] \[Eta])) Sin[(a-c) \[Delta]])+2 Sin[(a-b) \[Beta]] (\[Delta] (-(\[Beta]^2+\[Alpha] \[Gamma]) (\[Gamma]-\[Eta])+E^(4 a \[Gamma]) (-\[Beta]^2+\[Alpha] \[Gamma]) (\[Gamma]+\[Eta])) Cos[(a-c) \[Delta]]+(E^(4 a \[Gamma]) (\[Beta]^2-\[Alpha] \[Gamma]) (-\[Delta]^2+\[Gamma] \[Eta])+(\[Beta]^2+\[Alpha] \[Gamma]) (\[Delta]^2+\[Gamma] \[Eta])) Sin[(a-c) \[Delta]]))];
Clear[t,J,hb,me];
Clear[a1,b1,c1,\[Alpha]1,\[Beta]1,\[Gamma]1,\[Delta]1,\[Eta]1];
Clear[V01,V03,V04,V05];
t=10^(-9);(*1\:7eb3\:7c73*)
J=1.6*10^(-22);(* 1\:6beb\:7535\:5b50\:4f0f\:7279*)
me=9.1*10^(-31);(*\:5355\:4f4d\:ff1akg*)
hb=6.626*10^(-34)/(2*Pi)(*\:5355\:4f4d J*s *);


(*=======\:8c03\:6574\:52bf\:9631\:53c2\:6570======*)
a1=t*(mwei/2);
b1=t*(lwei+mwei/2);
c1=t*(rwei+mwei/2);

V01=J*hv1;
V03=J*hv3;
V04=J*hv4;
V05=J*hv5;

Clear[Vm]
Vm=Min[Select[{V01,V03,V04,V05},#>0&]];


\[Alpha]1=Sqrt[2*me*(V01-e)/hb^2];
\[Beta]1=Sqrt[2*me*(e)/hb^2];
\[Gamma]1=Sqrt[2*me*(V03-e)/hb^2];
If[V04!=0,\[Delta]1=Sqrt[2*me*(V04-e)/hb^2],V04==0,\[Delta]1=Sqrt[2*me*e/hb^2]];
\[Eta]1=Sqrt[2*me*(V05-e)/hb^2];

(*===========================\:7ed8\:5236 - \:8d85\:8d8a\:65b9\:7a0b\:56fe\:50cf\:89e3================================*)
Clear[nf01,pnf];
nf01=fw[a1,b1,c1,\[Alpha]1,\[Beta]1,\[Gamma]1,\[Eta]1,\[Delta]1];
pnf=Plot[nf01,{e,-1*10^-18,Vm},PlotRange->{-3*10^39,2*10^39},PlotStyle->Red,PlotPoints->800];

Print["\:80fd\:91cf\:6781\:9650\:ff1a",Vm," J"," \:6216 ",Vm/J," meV"];

Clear[Extc];
Extc=NSolve[nf01==0 && 0<e && e<Vm,e,5]
Clear[Ex];
Ex=Table[Exe,0];


If[Length[Extc]==1,Ex=Append[Ex,Extc[[1,1,2]]]];(*\:5982\:679c\:53ea\:6709\:4e00\:4e2a\:89e3\:ff0c\:5219\:76f4\:63a5\:63d0\:53d6\:89e3\:5e76\:5b58\:653e*)
(*\:5982\:679c\:67092\:4e2a\:62162\:4e2a\:4ee5\:4e0a\:7684\:89e3\:ff0c\:5219\:ff1a*)
For[i=1,i<Length[Extc],i++,
Which[
(*\:6b63\:5e38\:60c5\:51b5\:ff1a\:5224\:65ad\:524d\:540e\:7684\:89e3\:662f\:5426\:8fc7\:4e8e\:63a5\:8fd1\:7684\:60c5\:51b5\:ff0c\:589e\:5e45\:5c0f\:4e8e10^-4\:7684\:5219\:89c6\:4e3a\:540c\:4e00\:4e2a\:89e3*)
i<(Length[Extc]-1)&&Abs[Extc[[i+1,1,2]]-Extc[[i,1,2]]]/Extc[[i,1,2]]>=10^-4,
Ex=Append[Ex,Extc[[i,1,2]]],
(*\:589e\:5e45\:8fc7\:5c0f\:7684\:89e3\:53d6\:4e24\:8005\:7684\:5747\:503c*)
i>1&& Abs[Extc[[i+1,1,2]]-Extc[[i,1,2]]]/Extc[[i,1,2]]<10^-4,
Ex=Append[Ex,(Extc[[i,1,2]]+Extc[[i+1,1,2]])/2];i=i+1,
(*\:8865\:5145\:4e0a\:4e00\:4e2a\:6761\:4ef6\:5904\:7406\:4e0d\:4e86\:7684\:6570\:636e\:6570\:91cf\:7684\:8fb9\:754c\:70b9\:5bf9\:5e94\:7684\:89e3*)
i==(Length[Extc]-1)&&Abs[Extc[[i+1,1,2]]-Extc[[i,1,2]]]/Extc[[i,1,2]]>=10^-4,
Ex=Append[Ex,Extc[[i,1,2]]];Ex=Append[Ex,Extc[[i+1,1,2]]]];
];
Print["\:675f\:7f1a\:6001\:6570\:91cf\:ff1a",Length[Ex]];
Ex;
Print["\:675f\:7f1a\:6001\:80fd\:91cf\:ff1a",Ex/J,"(meV)"];
];

End[]
EndPackage[]




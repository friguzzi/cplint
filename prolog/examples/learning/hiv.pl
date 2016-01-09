/* HIV dataset from
Beerenwinkel N, RahnenfÂ¨uhrer J, DÂ¨aumer M, Hoffmann D, Kaiser R
Selbig J, Lengauer T (2005) 
Learning multiple evolutionary pathways from cross-sectional data. 
Journal of Computational Biology 12:584-598

Prolog version kindly provided by Wannes Meert from 
Meert, W., Struyf, J., and Blockeel, H. 2008. 
Learning ground CP-Logic theories by leveraging Bayesian network learning 
techniques. Fundamenta Informaticae 89, 131-160

Used in
Nicola Di Mauro, Elena Bellodi, and Fabrizio Riguzzi. Bandit-based Monte-Carlo 
structure learning of probabilistic logic programs. 
Machine Learning, 100(1):127-156, July 2015
Elena Bellodi and Fabrizio Riguzzi. Structure learning of probabilistic logic 
programs by searching the clause space. 
Theory and Practice of Logic Programming, 15(2):169-212, 2015

*/


/** <examples>
?- induce([1,2,3,4],[5],P,LL,AUCROC,ROC,AUCPR,PR).
?- induce_par([1,2,3,4],[5],P,LL,AUCROC,ROC,AUCPR,PR).
?- induce([1,2,3,4,5],P).
?- induce_par([1,2,3,4,5],P).
*/
:- use_module(library(slipcover)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(lpad).
:- endif.

:-sc.

:- set_sc(depth_bound,true).
:- set_sc(specialization,mode).
:- set_sc(verbosity,1).

bg([]).

in([]).

fold(1,[m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30,m31,m32,m33,m34,m35,m36,m37,m38,m39,m40,m41,m42,m43,m44,m45,m46,m47,m48,m49,m50,m51,m52,m53,m54,m55,m56,m57,m58,m59,m60,m61,m62,m63,m64,m65,m66,m67,m68,m69,m70,m71,m72,m73]).

fold(2,[m74,m75,m76,m77,m78,m79,m80,m81,m82,m83,m84,m85,m86,m87,m88,m89,m90,m91,m92,m93,m94,m95,m96,m97,m98,m99,m100,m101,m102,m103,m104,m105,m106,m107,m108,m109,m110,m111,m112,m113,m114,m115,m116,m117,m118,m119,m120,m121,m122,m123,m124,m125,m126,m127,m128,m129,m130,m131,m132,m133,m134,m135,m136,m137,m138,m139,m140,m141,m142,m143,m144,m145,m146]).

fold(3,[m147,m148,m149,m150,m151,m152,m153,m154,m155,m156,m157,m158,m159,m160,m161,m162,m163,m164,m165,m166,m167,m168,m169,m170,m171,m172,m173,m174,m175,m176,m177,m178,m179,m180,m181,m182,m183,m184,m185,m186,m187,m188,m189,m190,m191,m192,m193,m194,m195,m196,m197,m198,m199,m200,m201,m202,m203,m204,m205,m206,m207,m208,m209,m210,m211,m212,m213,m214,m215,m216,m217,m218,m219]).

fold(4,[m220,m221,m222,m223,m224,m225,m226,m227,m228,m229,m230,m231,m232,m233,m234,m235,m236,m237,m238,m239,m240,m241,m242,m243,m244,m245,m246,m247,m248,m249,m250,m251,m252,m253,m254,m255,m256,m257,m258,m259,m260,m261,m262,m263,m264,m265,m266,m267,m268,m269,m270,m271,m272,m273,m274,m275,m276,m277,m278,m279,m280,m281,m282,m283,m284,m285,m286,m287,m288,m289,m290,m291,m292]).

fold(5,[m293,m294,m295,m296,m297,m298,m299,m300,m301,m302,m303,m304,m305,m306,m307,m308,m309,m310,m311,m312,m313,m314,m315,m316,m317,m318,m319,m320,m321,m322,m323,m324,m325,m326,m327,m328,m329,m330,m331,m332,m333,m334,m335,m336,m337,m338,m339,m340,m341,m342,m343,m344,m345,m346,m347,m348,m349,m350,m351,m352,m353,m354,m355,m356,m357,m358,m359,m360,m361,m362,m363,m364]).

output('41L'/0).

output('67N'/0).

output('70R'/0).

output('210W'/0).

output('215FY'/0).

output('219EQ'/0).

input('41L'/0).

input('67N'/0).

input('70R'/0).

input('210W'/0).

input('215FY'/0).

input('219EQ'/0).

modeh(*,'41L'). 
modeh(*,'67N'). 
modeh(*,'70R'). 
modeh(*,'210W'). 
modeh(*,'215FY'). 
modeh(*,'219EQ'). 

determination('41L'/0,'67N'/0).
determination('41L'/0,'70R'/0).
determination('41L'/0,'210W'/0).
determination('41L'/0,'215FY'/0).
determination('41L'/0,'219EQ'/0).

determination('67N'/0,'41L'/0).
determination('67N'/0,'70R'/0).
determination('67N'/0,'210W'/0).
determination('67N'/0,'215FY'/0).
determination('67N'/0,'219EQ'/0).

determination('70R'/0,'67N'/0).
determination('70R'/0,'41L'/0).
determination('70R'/0,'210W'/0).
determination('70R'/0,'215FY'/0).
determination('70R'/0,'219EQ'/0).

determination('210W'/0,'67N'/0).
determination('210W'/0,'70R'/0).
determination('210W'/0,'41L'/0).
determination('210W'/0,'215FY'/0).
determination('210W'/0,'219EQ'/0).

determination('215FY'/0,'67N'/0).
determination('215FY'/0,'70R'/0).
determination('215FY'/0,'210W'/0).
determination('215FY'/0,'41L'/0).
determination('215FY'/0,'219EQ'/0).

determination('219EQ'/0,'67N'/0).
determination('219EQ'/0,'70R'/0).
determination('219EQ'/0,'210W'/0).
determination('219EQ'/0,'215FY'/0).
determination('219EQ'/0,'41L'/0).

modeb(*,'41L'). 
modeb(*,'67N'). 
modeb(*,'70R'). 
modeb(*,'210W'). 
modeb(*,'215FY'). 
modeb(*,'219EQ'). 





begin(model(m1)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m1)).

begin(model(m2)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m2)).

begin(model(m3)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m3)).

begin(model(m4)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m4)).

begin(model(m5)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m5)).

begin(model(m6)).
'wildtype'.
neg('41L').
'67N'.
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m6)).

begin(model(m7)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m7)).

begin(model(m8)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m8)).

begin(model(m9)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m9)).

begin(model(m10)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m10)).

begin(model(m11)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m11)).

begin(model(m12)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m12)).

begin(model(m13)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m13)).

begin(model(m14)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m14)).

begin(model(m15)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m15)).

begin(model(m16)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m16)).

begin(model(m17)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m17)).

begin(model(m18)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m18)).

begin(model(m19)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m19)).

begin(model(m20)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m20)).

begin(model(m21)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m21)).

begin(model(m22)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m22)).

begin(model(m23)).
'wildtype'.
'41L'.
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m23)).

begin(model(m24)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m24)).

begin(model(m25)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m25)).

begin(model(m26)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m26)).

begin(model(m27)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m27)).

begin(model(m28)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m28)).

begin(model(m29)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m29)).

begin(model(m30)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m30)).

begin(model(m31)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m31)).

begin(model(m32)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m32)).

begin(model(m33)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m33)).

begin(model(m34)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m34)).

begin(model(m35)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m35)).

begin(model(m36)).
'wildtype'.
'41L'.
'67N'.
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m36)).

begin(model(m37)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m37)).

begin(model(m38)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m38)).

begin(model(m39)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m39)).

begin(model(m40)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m40)).

begin(model(m41)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m41)).

begin(model(m42)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m42)).

begin(model(m43)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m43)).

begin(model(m44)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m44)).

begin(model(m45)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m45)).

begin(model(m46)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m46)).

begin(model(m47)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m47)).

begin(model(m48)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m48)).

begin(model(m49)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m49)).

begin(model(m50)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m50)).

begin(model(m51)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m51)).

begin(model(m52)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m52)).

begin(model(m53)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m53)).

begin(model(m54)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m54)).

begin(model(m55)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m55)).

begin(model(m56)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m56)).

begin(model(m57)).
'wildtype'.
'41L'.
neg('67N').
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m57)).

begin(model(m58)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m58)).

begin(model(m59)).
'wildtype'.
neg('41L').
'67N'.
neg('70R').
neg('210W').
'215FY'.
'219EQ'.
end(model(m59)).

begin(model(m60)).
'wildtype'.
'41L'.
'67N'.
'70R'.
'210W'.
'215FY'.
neg('219EQ').
end(model(m60)).

begin(model(m61)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m61)).

begin(model(m62)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m62)).

begin(model(m63)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m63)).

begin(model(m64)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m64)).

begin(model(m65)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m65)).

begin(model(m66)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m66)).

begin(model(m67)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m67)).

begin(model(m68)).
'wildtype'.
'41L'.
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m68)).

begin(model(m69)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m69)).

begin(model(m70)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m70)).

begin(model(m71)).
'wildtype'.
'41L'.
'67N'.
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m71)).

begin(model(m72)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m72)).

begin(model(m73)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m73)).
%73 modelli
begin(model(m74)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m74)).

begin(model(m75)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m75)).

begin(model(m76)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m76)).

begin(model(m77)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m77)).

begin(model(m78)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m78)).

begin(model(m79)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m79)).

begin(model(m80)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m80)).

begin(model(m81)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m81)).

begin(model(m82)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m82)).

begin(model(m83)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m83)).

begin(model(m84)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m84)).

begin(model(m85)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m85)).

begin(model(m86)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m86)).

begin(model(m87)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m87)).

begin(model(m88)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m88)).

begin(model(m89)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m89)).

begin(model(m90)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m90)).

begin(model(m91)).
'wildtype'.
'41L'.
'67N'.
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m91)).

begin(model(m92)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m92)).

begin(model(m93)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m93)).

begin(model(m94)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m94)).

begin(model(m95)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m95)).

begin(model(m96)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m96)).

begin(model(m97)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m97)).

begin(model(m98)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m98)).

begin(model(m99)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m99)).

begin(model(m100)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m100)).

begin(model(m101)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m101)).

begin(model(m102)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m102)).

begin(model(m103)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m103)).

begin(model(m104)).
'wildtype'.
'41L'.
'67N'.
'70R'.
'210W'.
'215FY'.
neg('219EQ').
end(model(m104)).

begin(model(m105)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m105)).

begin(model(m106)).
'wildtype'.
'41L'.
'67N'.
neg('70R').
'210W'.
'215FY'.
'219EQ'.
end(model(m106)).

begin(model(m107)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m107)).

begin(model(m108)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m108)).

begin(model(m109)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m109)).

begin(model(m110)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m110)).

begin(model(m111)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m111)).

begin(model(m112)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m112)).

begin(model(m113)).
'wildtype'.
'41L'.
'67N'.
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m113)).

begin(model(m114)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m114)).

begin(model(m115)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m115)).

begin(model(m116)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m116)).

begin(model(m117)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m117)).

begin(model(m118)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m118)).

begin(model(m119)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m119)).

begin(model(m120)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m120)).

begin(model(m121)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m121)).

begin(model(m122)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m122)).

begin(model(m123)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m123)).

begin(model(m124)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m124)).

begin(model(m125)).
'wildtype'.
'41L'.
'67N'.
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m125)).

begin(model(m126)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m126)).

begin(model(m127)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m127)).

begin(model(m128)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m128)).

begin(model(m129)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m129)).

begin(model(m130)).
'wildtype'.
'41L'.
'67N'.
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m130)).

begin(model(m131)).
'wildtype'.
'41L'.
'67N'.
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m131)).

begin(model(m132)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m132)).

begin(model(m133)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m133)).

begin(model(m134)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m134)).

begin(model(m135)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m135)).

begin(model(m136)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m136)).

begin(model(m137)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m137)).

begin(model(m138)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m138)).

begin(model(m139)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m139)).

begin(model(m140)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m140)).

begin(model(m141)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m141)).

begin(model(m142)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m142)).

begin(model(m143)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m143)).

begin(model(m144)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m144)).

begin(model(m145)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m145)).

begin(model(m146)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m146)).
%73 modelli
begin(model(m147)).
'wildtype'.
neg('41L').
'67N'.
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m147)).

begin(model(m148)).
'wildtype'.
'41L'.
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m148)).

begin(model(m149)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m149)).

begin(model(m150)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m150)).

begin(model(m151)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m151)).

begin(model(m152)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m152)).

begin(model(m153)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m153)).

begin(model(m154)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m154)).

begin(model(m155)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m155)).

begin(model(m156)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m156)).

begin(model(m157)).
'wildtype'.
'41L'.
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m157)).

begin(model(m158)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m158)).

begin(model(m159)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m159)).

begin(model(m160)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m160)).

begin(model(m161)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m161)).

begin(model(m162)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m162)).

begin(model(m163)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m163)).

begin(model(m164)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m164)).

begin(model(m165)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m165)).

begin(model(m166)).
'wildtype'.
neg('41L').
'67N'.
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m166)).

begin(model(m167)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m167)).

begin(model(m168)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m168)).

begin(model(m169)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
'219EQ'.
end(model(m169)).

begin(model(m170)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m170)).

begin(model(m171)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m171)).

begin(model(m172)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m172)).

begin(model(m173)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m173)).

begin(model(m174)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m174)).

begin(model(m175)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m175)).

begin(model(m176)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m176)).

begin(model(m177)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m177)).

begin(model(m178)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m178)).

begin(model(m179)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m179)).

begin(model(m180)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m180)).

begin(model(m181)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m181)).

begin(model(m182)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m182)).

begin(model(m183)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m183)).

begin(model(m184)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m184)).

begin(model(m185)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m185)).

begin(model(m186)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m186)).

begin(model(m187)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m187)).

begin(model(m188)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m188)).

begin(model(m189)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m189)).

begin(model(m190)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m190)).

begin(model(m191)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m191)).

begin(model(m192)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m192)).

begin(model(m193)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m193)).

begin(model(m194)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m194)).

begin(model(m195)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m195)).

begin(model(m196)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m196)).

begin(model(m197)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m197)).

begin(model(m198)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m198)).

begin(model(m199)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m199)).

begin(model(m200)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m200)).

begin(model(m201)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m201)).

begin(model(m202)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m202)).

begin(model(m203)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
'210W'.
'215FY'.
'219EQ'.
end(model(m203)).

begin(model(m204)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m204)).

begin(model(m205)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m205)).

begin(model(m206)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m206)).

begin(model(m207)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m207)).

begin(model(m208)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m208)).

begin(model(m209)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m209)).

begin(model(m210)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m210)).

begin(model(m211)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m211)).

begin(model(m212)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m212)).

begin(model(m213)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m213)).

begin(model(m214)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m214)).

begin(model(m215)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m215)).

begin(model(m216)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m216)).

begin(model(m217)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m217)).

begin(model(m218)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m218)).

begin(model(m219)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m219)).
%73 modelli
begin(model(m220)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m220)).

begin(model(m221)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m221)).

begin(model(m222)).
'wildtype'.
'41L'.
'67N'.
neg('70R').
'210W'.
'215FY'.
'219EQ'.
end(model(m222)).

begin(model(m223)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m223)).

begin(model(m224)).
'wildtype'.
'41L'.
'67N'.
'70R'.
'210W'.
'215FY'.
neg('219EQ').
end(model(m224)).

begin(model(m225)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m225)).

begin(model(m226)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m226)).

begin(model(m227)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m227)).

begin(model(m228)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m228)).

begin(model(m229)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m229)).

begin(model(m230)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m230)).

begin(model(m231)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m231)).

begin(model(m232)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m232)).

begin(model(m233)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m233)).

begin(model(m234)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m234)).

begin(model(m235)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m235)).

begin(model(m236)).
'wildtype'.
'41L'.
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m236)).

begin(model(m237)).
'wildtype'.
neg('41L').
'67N'.
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m237)).

begin(model(m238)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m238)).

begin(model(m239)).
'wildtype'.
'41L'.
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m239)).

begin(model(m240)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m240)).

begin(model(m241)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m241)).

begin(model(m242)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m242)).

begin(model(m243)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
'210W'.
'215FY'.
'219EQ'.
end(model(m243)).

begin(model(m244)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m244)).

begin(model(m245)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m245)).

begin(model(m246)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m246)).

begin(model(m247)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m247)).

begin(model(m248)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m248)).

begin(model(m249)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m249)).

begin(model(m250)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m250)).

begin(model(m251)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m251)).

begin(model(m252)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m252)).

begin(model(m253)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m253)).

begin(model(m254)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
'219EQ'.
end(model(m254)).

begin(model(m255)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m255)).

begin(model(m256)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m256)).

begin(model(m257)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m257)).

begin(model(m258)).
'wildtype'.
'41L'.
'67N'.
'70R'.
'210W'.
'215FY'.
neg('219EQ').
end(model(m258)).

begin(model(m259)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m259)).

begin(model(m260)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
'219EQ'.
end(model(m260)).

begin(model(m261)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m261)).

begin(model(m262)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m262)).

begin(model(m263)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m263)).

begin(model(m264)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m264)).

begin(model(m265)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m265)).

begin(model(m266)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m266)).

begin(model(m267)).
'wildtype'.
'41L'.
'67N'.
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m267)).

begin(model(m268)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m268)).

begin(model(m269)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m269)).

begin(model(m270)).
'wildtype'.
'41L'.
'67N'.
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m270)).

begin(model(m271)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m271)).

begin(model(m272)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m272)).

begin(model(m273)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m273)).

begin(model(m274)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m274)).

begin(model(m275)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m275)).

begin(model(m276)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m276)).

begin(model(m277)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m277)).

begin(model(m278)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m278)).

begin(model(m279)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m279)).

begin(model(m280)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m280)).

begin(model(m281)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m281)).

begin(model(m282)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m282)).

begin(model(m283)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m283)).

begin(model(m284)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m284)).

begin(model(m285)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m285)).

begin(model(m286)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m286)).

begin(model(m287)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m287)).

begin(model(m288)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m288)).

begin(model(m289)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m289)).

begin(model(m290)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m290)).

begin(model(m291)).
'wildtype'.
neg('41L').
'67N'.
neg('70R').
neg('210W').
neg('215FY').
'219EQ'.
end(model(m291)).

begin(model(m292)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m292)).
%73 modelli
begin(model(m293)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m293)).

begin(model(m294)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m294)).

begin(model(m295)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m295)).

begin(model(m296)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m296)).

begin(model(m297)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m297)).

begin(model(m298)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m298)).

begin(model(m299)).
'wildtype'.
'41L'.
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m299)).

begin(model(m300)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m300)).

begin(model(m301)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m301)).

begin(model(m302)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m302)).

begin(model(m303)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m303)).

begin(model(m304)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m304)).

begin(model(m305)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m305)).

begin(model(m306)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m306)).

begin(model(m307)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m307)).

begin(model(m308)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m308)).

begin(model(m309)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m309)).

begin(model(m310)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m310)).

begin(model(m311)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m311)).

begin(model(m312)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m312)).

begin(model(m313)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
'210W'.
'215FY'.
'219EQ'.
end(model(m313)).

begin(model(m314)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m314)).

begin(model(m315)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m315)).

begin(model(m316)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m316)).

begin(model(m317)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m317)).

begin(model(m318)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m318)).

begin(model(m319)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m319)).

begin(model(m320)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m320)).

begin(model(m321)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m321)).

begin(model(m322)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m322)).

begin(model(m323)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m323)).

begin(model(m324)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m324)).

begin(model(m325)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m325)).

begin(model(m326)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m326)).

begin(model(m327)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m327)).

begin(model(m328)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m328)).

begin(model(m329)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m329)).

begin(model(m330)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m330)).

begin(model(m331)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m331)).

begin(model(m332)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m332)).

begin(model(m333)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m333)).

begin(model(m334)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m334)).

begin(model(m335)).
'wildtype'.
'41L'.
'67N'.
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m335)).

begin(model(m336)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m336)).

begin(model(m337)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m337)).

begin(model(m338)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m338)).

begin(model(m339)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m339)).

begin(model(m340)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m340)).

begin(model(m341)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m341)).

begin(model(m342)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m342)).

begin(model(m343)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m343)).

begin(model(m344)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m344)).

begin(model(m345)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m345)).

begin(model(m346)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m346)).

begin(model(m347)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
'219EQ'.
end(model(m347)).

begin(model(m348)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
neg('215FY').
neg('219EQ').
end(model(m348)).

begin(model(m349)).
'wildtype'.
'41L'.
neg('67N').
'70R'.
neg('210W').
'215FY'.
neg('219EQ').
end(model(m349)).

begin(model(m350)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m350)).

begin(model(m351)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m351)).

begin(model(m352)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
'210W'.
'215FY'.
neg('219EQ').
end(model(m352)).

begin(model(m353)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m353)).

begin(model(m354)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m354)).

begin(model(m355)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m355)).

begin(model(m356)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m356)).

begin(model(m357)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m357)).

begin(model(m358)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
'215FY'.
'219EQ'.
end(model(m358)).

begin(model(m359)).
'wildtype'.
neg('41L').
neg('67N').
neg('70R').
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m359)).

begin(model(m360)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m360)).

begin(model(m361)).
'wildtype'.
'41L'.
neg('67N').
neg('70R').
neg('210W').
'215FY'.
neg('219EQ').
end(model(m361)).

begin(model(m362)).
'wildtype'.
neg('41L').
'67N'.
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m362)).

begin(model(m363)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m363)).

begin(model(m364)).
'wildtype'.
neg('41L').
neg('67N').
'70R'.
neg('210W').
neg('215FY').
neg('219EQ').
end(model(m364)).
%72 modelli

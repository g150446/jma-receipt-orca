name	orca71;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGE01";
bind	"E01"		"OpenCOBOL"	"ORCGE01";
bind	"E02"		"OpenCOBOL"	"ORCGE02";
bind	"EERR"		"OpenCOBOL"	"ORCGEERR";
bind	"EID1"		"OpenCOBOL"	"ORCGEID1";
bind	"EID2"		"OpenCOBOL"	"ORCGEID2";
bind	"F01"		"OpenCOBOL"	"ORCGF01";
bind	"FERR"		"OpenCOBOL"	"ORCGFERR";
bind	"FID1"		"OpenCOBOL"	"ORCGFID1";
bind	"FID2"		"OpenCOBOL"	"ORCGFID2";
bindapi "onshicond"	"OpenCOBOL" "ORAPIONCONDR1V2";
bindapi "onlineimg1"    "OpenCOBOL" "ORAPION011R1V2";
bindapi "onlinemedical1"	"OpenCOBOL" "ORAPION010R1V2";
bindapi "onlinemedical2"	"OpenCOBOL" "ORAPION012R1V2";
bindapi "onlineaidlstreq1"	"OpenCOBOL" "ORAPION013R1V2";
bindapi	"onlinequaapp3"	"OpenCOBOL"	"ORAPION014R1V2";


multiplex_group	orca71;



data	{
	spa	orcaspa;
	window	{
		E01;
		E02;
		EERR;
		EID1;
		EID2;
		F01;
		FERR;
		FID1;
		FID2;
		onshicond;
		onlineimg1;
		onlinemedical1;
		onlinemedical2;
		onlineaidlstreq1;
		onlinequaapp3;

	};
};

db "xml2" {
 xml_onshicondreq;
 xml_onshicondres;
 xml_onlinequares1;
 xml_onlineimgreq1;
 xml_onlineaidlstreq1;
 xml_onlinequaappres1;

};

db "msgarray" {
  xml_onlinemedicalreq1;
  xml_onlinemedicalreq2;
  xml_onlinequaappreq3;
};
/*
db	{
	tbl_jobkanri;
	tbl_jyurrk;
	tbl_jyurrk_prv;
	tbl_ptnum;
	tbl_syskanri;
};
*/

#include "orcadb.inc"
#include "orcadbh.inc"

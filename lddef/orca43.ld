name	orca43;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGH01";
bind	"H01"		"OpenCOBOL"	"ORCGH01";
bind	"H02"		"OpenCOBOL"	"ORCGH02";
bind	"H03"		"OpenCOBOL"	"ORCGH03";
bind	"H04"		"OpenCOBOL"	"ORCGH04";
bind	"H05"		"OpenCOBOL"	"ORCGH05";
bind	"H98"		"OpenCOBOL"	"ORCGH98";
bind	"H99"		"OpenCOBOL"	"ORCGH99";
bind	"HERR"		"OpenCOBOL"	"ORCGHERR";
bind	"HID1"		"OpenCOBOL"	"ORCGHID1";
bind	"HID2"		"OpenCOBOL"	"ORCGHID2";
bindapi	"claimedmanagementv3"	"OpenCOBOL"	"ORAPI043R1V3";

multiplex_group	orca43;


data	{
	spa	orcaspa;
	window	{
		H01;
		H02;
		H03;
		H04;
		H05;
		H98;
		H99;
		HERR;
		HID1;
		HID2;
		claimedmanagementv3;
	};
};

db "xml2" {
 xml_claimed_managementv3req;
 xml_claimed_managementv3res;
};

db	{
	tbl_syskanri;
	tbl_ptnum;
	tbl_labor_sio;
	tbl_lock;
	tbl_ldmap;
	tbl_monthlynum;
	tbl_seikyu_main;
	tbl_seikyu_koh;
	tbl_seikyu_etc;
	tbl_seikyu_rsi;
	tbl_kohsky;
	tbl_ptinf;
	tbl_ptkohinf;
	tbl_henrei_kanri;
	tbl_henrei_body;
	tbl_receden;
	tbl_jibai_prt;
	tbl_ptrsiinf;
	tbl_sysuser;
	tbl_sysbase;
	tbl_api_para;
	tbl_dbcatalog;
	tbl_jobkanri;
	tbl_pthkninf;

};

/* #include "orcadb.inc" */
#include "orcadbh.inc"

name	orca07;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGXG01";
bind	"XG01"		"OpenCOBOL"	"ORCGXG01";
bind	"XG02"		"OpenCOBOL"	"ORCGXG02";
bind	"XG03"		"OpenCOBOL"	"ORCGXG03";
bind	"XG04"		"OpenCOBOL"	"ORCGXG04";
bind	"XG99"		"OpenCOBOL"	"ORCGXG99";
bind	"XGERR"		"OpenCOBOL"	"ORCGXGERR";
bind	"XGID1"		"OpenCOBOL"	"ORCGXGID1";
bindapi "statisticsdatav3"	"OpenCOBOL"	"ORAPI007R1V3";

multiplex_group	orca07;


data	{
	spa	orcaspa;
	window	{
		XG01;
		XG02;
		XG03;
		XG04;
		XG99;
		XGERR;
		XGID1;
		statisticsdatav3;
	};
};

db "xml2" {
	xml_statistics_datav3req;
	xml_statistics_datav3res;
};

db "monblob" {
 monblob;
};


db	{
	tbl_file_info;
	tbl_jobkanri;
	tbl_syskanri;
	tbl_toukeicsv_h;
	tbl_toukeicsv_b;
	tbl_sysuser;
	tbl_ldmap;
	tbl_ptinf;
	tbl_ptnum;
	tbl_mnote_h;
	tbl_mnote_b;
	tbl_file_mis;
	tbl_dbcatalog;
	tbl_sysbase;
};

/* #include "orcadb.inc" */
#include "orcadbh.inc"

name	orca14;

arraysize	20;
textsize	400;

bind	""		"OpenCOBOL"	"ORCGY01";
bind	"Y01"		"OpenCOBOL"	"ORCGY01";
bind	"Y011"		"OpenCOBOL"	"ORCGY011";
bind	"Y012"		"OpenCOBOL"	"ORCGY012";
bind	"Y03"		"OpenCOBOL"	"ORCGY03";
bind	"Y04"		"OpenCOBOL"	"ORCGY04";
bind	"Y05"		"OpenCOBOL"	"ORCGY05";
bind	"Y06"		"OpenCOBOL"	"ORCGY06";
bind	"YERR"		"OpenCOBOL"	"ORCGYERR";
bind	"YID1"		"OpenCOBOL"	"ORCGYID1";
bindapi "appointmodv2"	"OpenCOBOL"	"ORAPI014S1V2";
bindapi "onlinequa1"	"OpenCOBOL" "ORAPION001R1V2";
bindapi "onlinequa2"	"OpenCOBOL" "ORAPION002R1V2";
bindapi "onlinequa3"	"OpenCOBOL" "ORAPION003R1V2";
bindapi "onlinedrug1"	"OpenCOBOL" "ORAPION004R1V2";
bindapi "onlinespec1"	"OpenCOBOL" "ORAPION005R1V2";
bindapi "onlinerefall1"	"OpenCOBOL" "ORAPION006R1V2";
bindapi "onlinequa4"	"OpenCOBOL" "ORAPION007R1V2";
bindapi "onlinequaapp1"	"OpenCOBOL" "ORAPION008R1V2";
bindapi "onlinequaapp2"	"OpenCOBOL" "ORAPION009R1V2";

multiplex_group	orca14;


data	{
	spa	orcaspa;
	window	{
		Y01;
		Y011;
		Y012;
		Y03;
		Y04;
		Y05;
		Y06;
		YERR;
		YID1;
		appointmodv2;
		onlinequa1;
		onlinequa2;
		onlinequa3;
		onlinedrug1;
		onlinespec1;
		onlinerefall1;
		onlinequa4;
		onlinequaapp1;
		onlinequaapp2;
	};
};


db "xml2" {
 xml_appointv2req;
 xml_appointv2res;
 fm_appointment_form;
 fm_appointment_list;
 fm_appointment_list_patient;
 xml_onlinequareq1;
 xml_onlinequares1;
 xml_onlinequareq2;
 xml_onlinedrugreq1;
 xml_onlinespecreq1;
 xml_onlinerefallreq1;
 xml_onlinerefallres1;
 xml_onlinequareq3;
 xml_onlinequaappreq1;
 xml_onlinequaappreq2;
 xml_onlinequaappres1;
};

db "push" {
        push_onlinequa;
};

#include "orcadb.inc"
#include "orcadbh.inc"


\set ON_ERROR_STOP
--
-- »ñ³Ê³ÎÇ§ÆÃÄê·ò¿Ç¾ðÊó³ÊÇ¼¥Æ¡¼¥Ö¥ë£±
--
-- Create Date : 2021/2/1
--

create	table	tbl_onshi_kenshin_main	(
	hospnum		smallint DEFAULT 0 NOT NULL,
	ptid		bigint DEFAULT 0  NOT NULL,
	jisshiymd	character varying(8) NOT NULL,
	hknjanum	character varying(8),
	kigo		character varying(80),
	num		character varying(80),
	edaban		character varying(2),
	termid		character varying(64),
	opid		character varying(16),
	creymd		character(8),
	crehms		character(6),
	upymd		character(8),
	uphms		character(6)
);
ALTER TABLE ONLY tbl_onshi_kenshin_main
    ADD CONSTRAINT tbl_onshi_kenshin_main_primary_key PRIMARY KEY (hospnum,ptid,jisshiymd);


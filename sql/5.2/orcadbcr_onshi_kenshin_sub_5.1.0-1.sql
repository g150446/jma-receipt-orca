\set ON_ERROR_STOP
--
-- 資格確認特定健診情報格納テーブル2
--
-- Create Date : 2021/1/269
--

create	table	tbl_onshi_kenshin_sub	(
	hospnum		smallint DEFAULT 0 NOT NULL,
	ptid		bigint DEFAULT 0  NOT NULL,
	jisshiymd	character varying(8) NOT NULL,
	rennum		smallint DEFAULT 0 NOT NULL,
	komokucd	character varying(17),
	komokuname	character varying(80),
	data_type	character varying(2),
	data_value	character varying(256),
	data_tani	character varying(26),
	termid		character varying(64),
	opid		character varying(16),
	creymd		character(8),
	crehms		character(6),
	upymd		character(8),
	uphms		character(6)
);
ALTER TABLE ONLY tbl_onshi_kenshin_sub
    ADD CONSTRAINT tbl_onshi_kenshin_sub_primary_key PRIMARY KEY (hospnum,ptid,jisshiymd,rennum);

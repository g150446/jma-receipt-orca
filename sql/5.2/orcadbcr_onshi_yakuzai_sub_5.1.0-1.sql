\set ON_ERROR_STOP
--
-- »ñ³Ê³ÎÇ§ÌôºÞ¾ðÊó³ÊÇ¼¥Æ¡¼¥Ö¥ë£²¥µ¥Ö
--
-- Create Date : 2021/1/26
--

CREATE TABLE tbl_onshi_yakuzai_sub (
    hospnum smallint DEFAULT 0 NOT NULL,
    ptid bigint DEFAULT 0,
    sryym character varying(6) NOT NULL,
    nyugaikbn character varying(1) NOT NULL,
    srydd character varying(2)  NOT NULL,
    shoho_hakkoymd character varying(8)  NOT NULL,
    rennum smallint DEFAULT 0,
    srykbn character varying(2),
    yohocd character varying(3),
    yohoname character varying(30),
    shiji character varying(80),
    srycd character varying(9),
    yakuzainame character varying(64),
    seibunname character varying(200),
    taniname character varying(12),
    suryo	 numeric(10,5) DEFAULT 0,
    yoryo	 numeric(10,5) DEFAULT 0,
    kaisu	 smallint DEFAULT 0,
    termid character varying(64),
    opid character varying(16),
    creymd character(8),
    crehms character(6),
    upymd character(8),
    uphms character(6)
);
ALTER TABLE ONLY tbl_onshi_yakuzai_sub
    ADD CONSTRAINT tbl_onshi_yakuzai_sub_primary_key PRIMARY KEY (hospnum, ptid, sryym, nyugaikbn,srydd,shoho_hakkoymd,rennum);

--
--  CREATE INDEX idx_onshi_yakuzai_main_ptid ON tbl_onshi_yakuzai_main USING btree (ptid); 
--


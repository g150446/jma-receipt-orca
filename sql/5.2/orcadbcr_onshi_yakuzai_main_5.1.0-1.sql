\set ON_ERROR_STOP
--
-- »ñ³Ê³ÎÇ§ÌôºÞ¾ðÊó³ÊÇ¼¥Æ¡¼¥Ö¥ë£±
--
-- Create Date : 2020/9/9
--

CREATE TABLE tbl_onshi_yakuzai_main (
    hospnum smallint DEFAULT 0 NOT NULL,
    ptid bigint DEFAULT 0,
    sryym character varying(6) NOT NULL,
    nyugaikbn character varying(1) NOT NULL,
    hknjanum character varying(8),
    kigo  character varying(80),
    num   character varying(80),
    edaban  character varying(2),
    chozai_seqnum smallint DEFAULT 0,
    chozai_kbn character varying(1),
    shoho_seqnum smallint DEFAULT 0,
    shoho_kbn character varying(1),
    srydd_1 character varying(2),
    srydd_2 character varying(2),
    srydd_3 character varying(2),
    srydd_4 character varying(2),
    srydd_5 character varying(2),
    srydd_6 character varying(2),
    srydd_7 character varying(2),
    srydd_8 character varying(2),
    srydd_9 character varying(2),
    srydd_10 character varying(2),
    srydd_11 character varying(2),
    srydd_12 character varying(2),
    srydd_13 character varying(2),
    srydd_14 character varying(2),
    srydd_15 character varying(2),
    srydd_16 character varying(2),
    srydd_17 character varying(2),
    srydd_18 character varying(2),
    srydd_19 character varying(2),
    srydd_20 character varying(2),
    srydd_21 character varying(2),
    srydd_22 character varying(2),
    srydd_23 character varying(2),
    srydd_24 character varying(2),
    srydd_25 character varying(2),
    srydd_26 character varying(2),
    srydd_27 character varying(2),
    srydd_28 character varying(2),
    srydd_29 character varying(2),
    srydd_30 character varying(2),
    srydd_31 character varying(2),
    termid character varying(64),
    opid character varying(16),
    creymd character(8),
    crehms character(6),
    upymd character(8),
    uphms character(6)
);
ALTER TABLE ONLY tbl_onshi_yakuzai_main
    ADD CONSTRAINT tbl_onshi_yakuzai_main_primary_key PRIMARY KEY (hospnum, ptid, sryym, nyugaikbn);

--
--  CREATE INDEX idx_onshi_yakuzai_main_ptid ON tbl_onshi_yakuzai_main USING btree (ptid); 
--


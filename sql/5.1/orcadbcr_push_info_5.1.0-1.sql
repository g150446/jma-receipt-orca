\set ON_ERROR_STOP
--
-- PUSH通知情報格納用
--
-- Create Date : 2019/9/28
--

create	table	tbl_push_info	(
	HOSPNUM	smallint DEFAULT 0 NOT NULL,
	PUSH_UID	varchar(37),
	EVENT	  	varchar(50),
	PATIENT_MODE	varchar(10),
	PTNUM		varchar(20),
	PTID		bigint,
	INFORMATION_DATE	varchar(10),
	INFORMATION_TIME	varchar(8),
	PERFORM_DATE	varchar(10),
	PUSH_INFO	varchar(50),
	SRYYMD  	varchar(8),
	OPID    	varchar(16),
	CREYMD   	varchar(8),
	UPHMS		varchar(6)
);

ALTER TABLE ONLY tbl_push_info
    ADD CONSTRAINT tbl_push_info_primary_key PRIMARY KEY (hospnum,push_uid);
    CREATE INDEX idx_push_info_ptid ON tbl_push_info USING btree (ptnum,ptid	);


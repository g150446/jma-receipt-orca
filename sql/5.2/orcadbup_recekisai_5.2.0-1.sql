\set ON_ERROR_STOP

DROP VIEW IF EXISTS public.tbl_recekisai;

ALTER TABLE master.tbl_recekisai
 ADD nyugaikbn character varying(1),
 ADD kaisu smallint default 0;
UPDATE master.tbl_recekisai SET nyugaikbn='0',kaisu=0;

CREATE VIEW public.tbl_recekisai AS SELECT * FROM master.tbl_recekisai;

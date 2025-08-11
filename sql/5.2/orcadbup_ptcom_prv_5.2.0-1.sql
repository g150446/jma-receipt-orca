\set ON_ERROR_STOP

alter table tbl_ptcom_prv
  alter column inputcoment type character varying(140),
  alter column inputchi1 type character varying(10),
  alter column inputchi2 type character varying(10),
  alter column inputchi3 type character varying(10),
  alter column inputchi4 type character varying(10),
  alter column inputchi5 type character varying(10);

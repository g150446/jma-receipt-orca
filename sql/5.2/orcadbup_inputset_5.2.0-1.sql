\set ON_ERROR_STOP

alter table tbl_inputset
  alter column coment type character varying(140),
  alter column atai1 type character varying(10),
  alter column atai2 type character varying(10),
  alter column atai3 type character varying(10),
  alter column atai4 type character varying(10),
  alter column atai5 type character varying(10);

\set ON_ERROR_STOP

alter table tbl_ptcom add column inputstring character varying(200);
update tbl_ptcom set inputstring = '';

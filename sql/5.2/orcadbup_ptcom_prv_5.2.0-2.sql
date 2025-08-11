\set ON_ERROR_STOP

alter table tbl_ptcom_prv add column inputstring character varying(200);
update tbl_ptcom_prv set inputstring = '';

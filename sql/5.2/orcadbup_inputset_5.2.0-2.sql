\set ON_ERROR_STOP

alter table tbl_inputset add column inputstring character varying(200);
update tbl_inputset set inputstring = '';

\set ON_ERROR_STOP

alter table tbl_pthkninf add column edaban character varying(2);
update tbl_pthkninf set edaban = '';

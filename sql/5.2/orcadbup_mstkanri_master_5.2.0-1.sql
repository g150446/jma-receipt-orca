\set ON_ERROR_STOP

alter table master.tbl_mstkanri_master
 add patch_ymd character varying(8);
update master.tbl_mstkanri_master set patch_ymd='';

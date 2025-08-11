\set ON_ERROR_STOP

alter table tbl_mstkanri_public
 add patch_ymd character varying(8);
update tbl_mstkanri_public set patch_ymd='';

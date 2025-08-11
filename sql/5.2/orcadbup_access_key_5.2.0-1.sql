\set ON_ERROR_STOP

alter table tbl_access_key
 add kado_ym character varying(6),
 add kado_level smallint default 0,
 add contract_cond smallint default 0,
 add contract_ymd character varying(8),
 add inquire_ymd character varying(8);
update tbl_access_key set kado_ym='',contract_ymd='',inquire_ymd='';

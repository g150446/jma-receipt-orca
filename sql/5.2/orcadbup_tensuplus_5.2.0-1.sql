\set ON_ERROR_STOP

alter table tbl_tensuplus
 add usertanicd character varying(3),
 add usertaniname character varying(24);
update tbl_tensuplus set usertanicd='',usertaniname='';

\set ON_ERROR_STOP

alter table tbl_wksryact
  add column inputstring_1 character varying(200),
  add column inputstring_2 character varying(200),
  add column inputstring_3 character varying(200),
  add column inputstring_4 character varying(200),
  add column inputstring_5 character varying(200);
update tbl_wksryact set
  inputstring_1 = '',
  inputstring_2 = '',
  inputstring_3 = '',
  inputstring_4 = '',
  inputstring_5 = '';

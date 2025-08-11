\set ON_ERROR_STOP

CREATE INDEX idx_pthkninf_hoken ON tbl_pthkninf USING btree (hknjanum,kigo,num);

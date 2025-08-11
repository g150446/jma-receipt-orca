-- 点数テーブル(TBL_TENSU_MASTER)
-- 令和２年４月改定対応
--
\set ON_ERROR_STOP

ALTER TABLE master.tbl_tensu_master
  ADD COLUMN sogaikoteiksn smallint default 0,
  ADD COLUMN chpsessakuksn smallint default 0,
  ADD COLUMN sasinjiheisikbn smallint default 0;

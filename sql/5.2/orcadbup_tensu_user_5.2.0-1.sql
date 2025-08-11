-- 点数テーブル(TBL_TENSU_USER)
-- 令和２年４月改定対応
--
\set ON_ERROR_STOP

ALTER TABLE public.tbl_tensu_user
  ADD COLUMN sogaikoteiksn smallint default 0,
  ADD COLUMN chpsessakuksn smallint default 0,
  ADD COLUMN sasinjiheisikbn smallint default 0;

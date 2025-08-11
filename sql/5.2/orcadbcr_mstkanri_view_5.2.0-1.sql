\set ON_ERROR_STOP
--
-- マスタ管理ビュー
--
CREATE OR REPLACE VIEW
  public.tbl_mstkanri AS
(
  SELECT
    *
  FROM public.tbl_mstkanri_public
  UNION ALL
  SELECT
    *
  FROM master.tbl_mstkanri_master
  ORDER BY
    hospnum,
    kanricd
);

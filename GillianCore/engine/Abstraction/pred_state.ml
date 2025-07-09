type 'a t = {
  state : 'a;
  preds : Preds.t;
  wands : Wands.t;
  pred_defs : MP.preds_tbl_t;
}

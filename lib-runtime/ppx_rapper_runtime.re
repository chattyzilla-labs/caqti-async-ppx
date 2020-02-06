module type CUSTOM = {
  type t;

  let t: Caqti_type.t(t);
};

module Dynparam = {
  type t =
    | Pack(Caqti_type.t('a), 'a): t;

  let empty =  Pack(Caqti_type.unit, ());

  let add = (t, x,  Pack(t', x')) =>
     Pack(Caqti_type.tup2(t', t), (x', x));
};

module type CUSTOM = {
  type t;

  let t: Caqti_type.t(t);
};

module Dynparam: {
  type t =
    | Pack(Caqti_type.t('a), 'a): t;

  let empty: t;

  let add: (Caqti_type.t('a), 'a, t) => t;
};

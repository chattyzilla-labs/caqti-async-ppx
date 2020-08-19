module type CUSTOM = {
  type t;

  let t: Caqti_type.t(t);
};

let load_many:
  (
    ('row => 'parent, 'parent => 'key),
    list(('row => 'child, ('parent, list('child)) => 'parent)),
    list('row)
  ) =>
  list('parent);

module Internal: {
  module Dynparam: {
    type t =
      | Pack(Caqti_type.t('a), 'a): t;

    let empty: t;

    let add: (Caqti_type.t('a), 'a, t) => t;
  };
};

module type CUSTOM = {
  type t;

  let t: Caqti_type.t(t);
};

/* Derived from implementation of List.group in Base.
   Original code is copyright (c) 2016--2020 Jane Street Group, LLC opensource@janestreet.com
   and the following permission notice applies to it:
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. */
let list_group = (l, ~break) => {
  let groups =
    List.fold_left(
      (acc, x) =>
        switch (acc) {
        | [] => [[x]]
        | [current_group, ...tl] =>
          if (break(List.hd(current_group), x)) {
            [[x], current_group, ...tl];
          } else {
            /* start new group */
            [[x, ...current_group], ...tl];
          }
        },
      [],
      l,
    );
  /* extend current group */

  switch (groups) {
  | [] => []
  | l => List.rev_map(List.rev, l)
  };
};

let load_many =
    ((get_parent, parent_key), children_getters_and_setters, data) =>
  data
  |> list_group(~break=(x, y) =>
       parent_key(get_parent(x)) != parent_key(get_parent(y))
     )
  |> List.map(group => {
    let (parents, children) = (
      List.map(get_parent, group),
      List.map(
        ((getter, setter)) => (setter, List.map(getter, group)),
        children_getters_and_setters,
      ),
    );

    let parent = List.hd(parents);
    List.fold_left(
      (current_parent, (setter, children)) =>
        setter(current_parent, children),
      parent,
      children,
    );
    });

module Internal = {
  module Dynparam = {
    type t =
      | Pack(Caqti_type.t('a), 'a): t;

    let empty = Pack(Caqti_type.unit, ());

    let add = (t, x, Pack(t', x')) =>
     Pack(Caqti_type.tup2(t', t), (x', x));
  };
};

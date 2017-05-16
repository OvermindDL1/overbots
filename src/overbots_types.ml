

type msg =
  | NothingYet
[@@bs.deriving {accessors}]

type model = {
  notUsedYet : int;
}

open Notty

type terminal = {
  grid : ((uchar * Notty.attr) option) array array;
  curs : (int * int) option
}

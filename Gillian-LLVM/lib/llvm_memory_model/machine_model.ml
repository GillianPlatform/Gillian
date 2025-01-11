type t = {
  alignment : int;
  bool_width : int;
  char_is_unsigned : bool;
  char_width : int;
  double_width : int;
  int_width : int;
  is_big_endian : bool;
  long_double_width : int;
  long_int_width : int;
  long_long_int_width : int;
  memory_operand_size : int;
  null_is_zero : bool;
  pointer_width : int;
  short_int_width : int;
  single_width : int;
  wchar_t_is_unsigned : bool;
  wchar_t_width : int;
  word_size : int;
}
[@@deriving eq, show { with_path = false }]

let archi64 =
  {
    alignment = 1;
    bool_width = 8;
    char_is_unsigned = false;
    char_width = 8;
    double_width = 64;
    int_width = 32;
    is_big_endian = false;
    long_double_width = 128;
    long_int_width = 64;
    long_long_int_width = 64;
    memory_operand_size = 4;
    null_is_zero = true;
    pointer_width = 64;
    short_int_width = 16;
    single_width = 32;
    wchar_t_width = 32;
    word_size = 32;
    wchar_t_is_unsigned = false;
  }

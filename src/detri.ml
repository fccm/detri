#load "unix.cma"

let lang = "en"  (* language: English *)

let default_detri_dir = ".detri"
let default_detri_dir = "../mga-planning"  (* devel-dir *)

let default_event_color = "black_on_bold_cyan"
let default_day_label_color = "bold_cyan"
let default_month_label_color = "black_on_cyan"

let event_color (event_color, _, _) = (event_color)
let day_label_color (_, day_label_color, _) =  (day_label_color)
let month_label_color (_, _, month_label_color) = (month_label_color)


let usage ec =
  Printf.printf "Usage:\n %s
    --year <year>
    --detri-dir <detri_dir>
    --event-color <event_color>
    --day-label-color <day_label_color>
    --month-label-color <month_label_color>
    --print-avail-colors
    --help | -help | -h
  " Sys.argv.(0);
  exit ec


let month_langs = [
  "en", [|
    "January"; "February"; "March"; "April";
    "May"; "June"; "July"; "August"; "September";
    "October"; "November"; "December";
  |];
  "fr", [|
    "janvier"; "février"; "mars"; "avril"; "mai";
    "juin"; "juillet"; "août"; "septembre";
    "octobre"; "novembre"; "décembre";
  |];
  "de", [|
    "Januar"; "Februar"; "März"; "April"; "Mai";
    "Juni"; "Juli"; "August"; "September";
    "Oktober"; "November"; "Dezember";
  |];
  "es", [|
    "enero"; "febrero"; "marzo"; "abril";
    "mayo"; "junio"; "julio"; "agosto";
    "septiembre"; "octubre"; "noviembre"; "diciembre";
  |];
  "it", [|
    "gennaio"; "febbraio"; "marzo"; "aprile";
    "maggio"; "giugno"; "luglio"; "agosto";
    "settembre"; "ottobre"; "novembre"; "dicembre";
  |];
  "nl", [|
    "januari"; "februari"; "maart"; "april";
    "mei"; "juni"; "juli"; "augustus";
    "september"; "oktober"; "november"; "december";
  |];
  "da", [|
    "januar"; "februar"; "marts"; "april";
    "maj"; "juni"; "juli"; "august";
    "september"; "oktober"; "november"; "december";
  |];
  "id", [|
    "Januari"; "Februari"; "Maret"; "April"; "Mei";
    "Juni"; "Juli"; "Agustus"; "September";
    "Oktober"; "November"; "Desember";
  |];
  "pt", [|
    "janeiro"; "fevereiro"; "março"; "abril";
    "maio"; "junho"; "julho"; "agosto";
    "setembro"; "outubro"; "novembro"; "dezembro";
  |];
]

let days_lang = [
  "en", [| "Monday"; "Tuesday"; "Wednesday";
    "Thursday"; "Friday"; "Saturday"; "Sunday" |];
  "fr", [| "lundi"; "mardi"; "mercredi";
    "jeudi"; "vendredi"; "samedi"; "dimanche" |];
  "de", [| "Montag"; "Dienstag"; "Mittwoch";
    "Donnerstag"; "Freitag"; "Samstag"; "Sonntag" |];
  "es", [| "lunes"; "martes"; "miércoles";
    "jueves"; "viernes"; "sábado"; "domingo" |];
  "it", [| "lunedì"; "martedì"; "mercoledì";
    "giovedì"; "venerdì"; "sabato"; "domenica" |];
  "nl", [| "maandag"; "dinsdag"; "woensdag";
    "donderdag"; "vrijdag"; "zaterdag"; "zondag" |];
  "da", [| "mandag"; "tirsdag"; "onsdag";
    "torsdag"; "fredag"; "lørdag"; "søndag" |];
  "id", [| "Senin"; "Selasa"; "Rabu";
    "Kamis"; "Jumat"; "Sabtu"; "Minggu" |];
  "pt", [| "segunda-feira"; "terça-feira"; "quarta-feira";
    "quinta-feira"; "sexta-feira"; "sábado"; "domingo" |];
]

let days = List.assoc lang days_lang
let month = List.assoc lang month_langs

let monday_first = 6, [| 0; 1; 2; 3; 4; 5; 6 |]
let sunday_first = 0, [| 6; 0; 1; 2; 3; 4; 5 |]

let off, days_order = sunday_first
let off, days_order = monday_first

let num_c3 s =
  let n = String.length s in
  let num = ref 0 in
  for i = 0 to pred n do
    let c = String.unsafe_get s i in
    if c = '\xC3' then incr num
  done;
  (!num)

let str_len s =
  (String.length s - num_c3 s)

let shorten n s =
  let len = String.length s in
  if n >= len then s else
    let n = if s.[n-1] = '\xC3' then n+1 else n in
    if n >= len then s else
      (String.sub s 0 n)

let rand_take arr =
  let n = Array.length arr in
  let i = Random.int n in
  (Array.unsafe_get arr i)

let pad size c s =
  let len = str_len s in
  let n1 = (size - len) / 2 in
  let n2 = size - len - n1 in
  String.make n1 c ^ s ^
  String.make n2 c

let padl size c s =
  let len = str_len s in
  let n = size - len in
  String.make n c ^ s

let list_assoc_replace k v lst =
  let rec aux acc = function
  | [] -> failwith "list_assoc_replace"
  | ((x,_) as p)::tl ->
      if x = k then List.rev_append acc ((x,v) :: tl)
      else aux (p::acc) tl
  in
  aux [] lst

let color color_name s =
  let r = "\027[00m" in  (* reset *)
  match color_name with
  | `normal         -> s
  | `reset          -> "\027[m" ^ s ^ r
  | `bold           -> "\027[1m" ^ s ^ r
  | `red            -> "\027[31m" ^ s ^ r
  | `green          -> "\027[32m" ^ s ^ r
  | `yellow         -> "\027[33m" ^ s ^ r
  | `blue           -> "\027[34m" ^ s ^ r
  | `magenta        -> "\027[35m" ^ s ^ r
  | `cyan           -> "\027[36m" ^ s ^ r
  | `bold_red       -> "\027[1;31m" ^ s ^ r
  | `bold_green     -> "\027[1;32m" ^ s ^ r
  | `bold_yellow    -> "\027[1;33m" ^ s ^ r
  | `bold_blue      -> "\027[1;34m" ^ s ^ r
  | `bold_magenta   -> "\027[1;35m" ^ s ^ r
  | `bold_cyan      -> "\027[1;36m" ^ s ^ r
  | `bg_red         -> "\027[41m" ^ s ^ r
  | `bg_green       -> "\027[42m" ^ s ^ r
  | `bg_yellow      -> "\027[43m" ^ s ^ r
  | `bg_blue        -> "\027[44m" ^ s ^ r
  | `bg_magenta     -> "\027[45m" ^ s ^ r
  | `bg_cyan        -> "\027[46m" ^ s ^ r

  | `black_on_white     -> "\027[30;47m" ^ s ^ r
  | `black_on_red       -> "\027[30;41m" ^ s ^ r
  | `black_on_green     -> "\027[30;42m" ^ s ^ r
  | `black_on_yellow    -> "\027[30;43m" ^ s ^ r
  | `black_on_blue      -> "\027[30;44m" ^ s ^ r
  | `black_on_magenta   -> "\027[30;45m" ^ s ^ r
  | `black_on_cyan      -> "\027[30;46m" ^ s ^ r

  | `black_on_bold_red       -> "\027[30;101m" ^ s ^ r
  | `black_on_bold_green     -> "\027[30;102m" ^ s ^ r
  | `black_on_bold_yellow    -> "\027[30;103m" ^ s ^ r
  | `black_on_bold_blue      -> "\027[30;104m" ^ s ^ r
  | `black_on_bold_magenta   -> "\027[30;105m" ^ s ^ r
  | `black_on_bold_cyan      -> "\027[30;106m" ^ s ^ r


let color_rand () =
  rand_take [|
    `red;
    `green;
    `yellow;
    `blue;
    `magenta;
    `cyan;
  |]

let bold_rand () =
  rand_take [|
    `bold_red;
    `bold_green;
    `bold_yellow;
    `bold_blue;
    `bold_magenta;
    `bold_cyan;
  |]

let bg_rand () =
  rand_take [|
    `bg_red;
    `bg_green;
    `bg_yellow;
    `bg_blue;
    `bg_magenta;
    `bg_cyan;
  |]

let black_on_rand () =
  rand_take [|
    `black_on_white;
    `black_on_red;
    `black_on_green;
    `black_on_yellow;
    `black_on_blue;
    `black_on_magenta;
    `black_on_cyan;
  |]

let black_on_bold_rand () =
  rand_take [|
    `black_on_bold_red;
    `black_on_bold_green;
    `black_on_bold_yellow;
    `black_on_bold_blue;
    `black_on_bold_magenta;
    `black_on_bold_cyan;
  |]


let color_of_string = function
  | "normal"        -> `normal
  | "reset"         -> `reset
  | "bold"          -> `bold
  | "red"           -> `red
  | "green"         -> `green
  | "yellow"        -> `yellow
  | "blue"          -> `blue
  | "magenta"       -> `magenta
  | "cyan"          -> `cyan
  | "bold_red"      -> `bold_red
  | "bold_green"    -> `bold_green
  | "bold_yellow"   -> `bold_yellow
  | "bold_blue"     -> `bold_blue
  | "bold_magenta"  -> `bold_magenta
  | "bold_cyan"     -> `bold_cyan
  | "bg_red"        -> `bg_red
  | "bg_green"      -> `bg_green
  | "bg_yellow"     -> `bg_yellow
  | "bg_blue"       -> `bg_blue
  | "bg_magenta"    -> `bg_magenta
  | "bg_cyan"       -> `bg_cyan
  | "black_on_white"    -> `black_on_white
  | "black_on_red"      -> `black_on_red
  | "black_on_green"    -> `black_on_green
  | "black_on_yellow"   -> `black_on_yellow
  | "black_on_blue"     -> `black_on_blue
  | "black_on_magenta"  -> `black_on_magenta
  | "black_on_cyan"     -> `black_on_cyan
  | "black_on_bold_red"      -> `black_on_bold_red
  | "black_on_bold_green"    -> `black_on_bold_green
  | "black_on_bold_yellow"   -> `black_on_bold_yellow
  | "black_on_bold_blue"     -> `black_on_bold_blue
  | "black_on_bold_magenta"  -> `black_on_bold_magenta
  | "black_on_bold_cyan"     -> `black_on_bold_cyan
  | "rand"          -> color_rand ()
  | "bold_rand"     -> bold_rand ()
  | "bg_rand"       -> bg_rand ()
  | "black_on_rand" -> black_on_rand ()
  | "black_on_bold_rand"  -> black_on_bold_rand ()
  | _ -> invalid_arg "color_of_string"


let color_s color_name s =
  color (color_of_string color_name) s


let print_avail_colors ec =
  print_endline "
  The base colors are:
    red, green, yellow, blue, magenta, cyan
    (and rand for random)

  With these colors, here is what's available:
  * bg_<color>
  * bold_<color>
  * black_on_<color>
  * black_on_bold_<color>

  (bg stands for background)
";
  exit ec


let chop_extension s =
  try Filename.chop_extension s
  with Invalid_argument _ -> s

let days = Array.map (shorten 2) days

let indices ofs =
  (ofs / 7, ofs mod 7)

let t_same t1 t2 =
  ( t1.Unix.tm_year = t2.Unix.tm_year &&
    t1.Unix.tm_mon  = t2.Unix.tm_mon &&
    t1.Unix.tm_mday = t2.Unix.tm_mday )

let to_string t =
  Printf.sprintf "%d-%02d-%02d"
    (t.Unix.tm_year + 1900)
    (t.Unix.tm_mon + 1)
    (t.Unix.tm_mday)

let dt_of_string s =
  let year, month, day =
    Scanf.sscanf s "%d-%d-%d" (fun y m d -> y, m, d)
  in
  (year, month, day)

let du_of_string s =
  let year, month, day = dt_of_string s in
  { (Unix.gmtime 0.0) with
    Unix.tm_year = year - 1900;
    Unix.tm_mon = month - 1;
    Unix.tm_mday = day;
  }

let of_range_d s =
  let year, month, day1, day2 =
    Scanf.sscanf s "%d-%d-%d--%d" (fun y m d1 d2 -> y, m, d1, d2)
  in
  let t = Unix.gmtime 0.0 in
  { t with
    Unix.tm_year = year - 1900;
    Unix.tm_mon = month - 1;
    Unix.tm_mday = day1;
  },
  { t with
    Unix.tm_year = year - 1900;
    Unix.tm_mon = month - 1;
    Unix.tm_mday = day2;
  }

let of_range_m s =
  let year, month1, month2, day1, day2 =
    Scanf.sscanf s "%d-%d-%d--%d-%d" (fun y m1 d1 m2 d2 -> y, m1, m2, d1, d2)
  in
  let t = Unix.gmtime 0.0 in
  { t with
    Unix.tm_year = year - 1900;
    Unix.tm_mon = month1 - 1;
    Unix.tm_mday = day1;
  },
  { t with
    Unix.tm_year = year - 1900;
    Unix.tm_mon = month2 - 1;
    Unix.tm_mday = day2;
  }

let of_range_y s =
  let year1, year2, month1, month2, day1, day2 =
    Scanf.sscanf s "%d-%d-%d--%d-%d-%d" (fun y1 m1 d1 y2 m2 d2 ->
      y1, y2, m1, m2, d1, d2)
  in
  let t = Unix.gmtime 0.0 in
  { t with
    Unix.tm_year = year1 - 1900;
    Unix.tm_mon = month1 - 1;
    Unix.tm_mday = day1;
  },
  { t with
    Unix.tm_year = year2 - 1900;
    Unix.tm_mon = month2 - 1;
    Unix.tm_mday = day2;
  }

let of_range s =
  try of_range_y s
  with _ ->
    try of_range_m s
    with _ ->
      try of_range_d s
      with _ ->
        invalid_arg "of_range"

let current_year () =
  let t = Unix.localtime (Unix.time ()) in
  string_of_int (t.Unix.tm_year  + 1900)

type day = {
  d: int;
  ev: string option;
}

let there_event evs tm =
  try Some (List.assoc (to_string tm) evs)
  with _ -> None

let make_month t evs year month =
  let empty_day = { d = 0; ev = None } in
  let m = Array.make_matrix 6 7 empty_day in
  let ofs = ref 0 in
  for day = 1 to 31 do
    let tm =
      { t with
        Unix.tm_year = year - 1900;
        Unix.tm_mon = month;
        Unix.tm_mday = day;
      }
    in
    let _, this = Unix.mktime tm in
    if !ofs = 0 then ofs := (this.Unix.tm_wday + off) mod 7;
    if t_same this tm then
      let i, j = indices !ofs in
      m.(i).(j) <- { d = day; ev = there_event evs tm };
    incr ofs;
  done;
  (m)

let cal evs ~year =
  let empty = [| [| |] |] in
  let months = Array.make 12 empty in
  let t = Unix.gmtime 0.0 in
  let y = int_of_string year in
  for mon = 0 to 11 do
    months.(mon) <- make_month t evs y mon;
  done;
  (months)

let print_month_label is cs =
  List.iter (fun i ->
    let mon = pad 20 ' ' month.(i) in
    Printf.printf " %s  " (color_s (month_label_color cs) mon)
  ) is;
  print_newline ()

let print_day_label is cs =
  List.iter (fun _ ->
    Array.iter (fun i ->
      Printf.printf " %s" (color_s (day_label_color cs) days.(i))
    ) days_order
    ; print_string "  "
  ) is;
  print_newline ()

let print_mon m is cs =
  print_month_label is cs;
  print_day_label is cs;
  for w = 0 to pred 6 do
    List.iter (fun i ->
      for d = 0 to pred 7 do
        match m.(i).(w).(d) with
        | { d = 0; ev = None } ->
            print_string "   "
        | { d = 0; ev = Some _ } ->
            Printf.printf " %s" (color `red "XX")
        | { d; ev = Some _ } ->
            Printf.printf " %s"
              (color_s (event_color cs) (padl 2 ' ' (string_of_int d)))
        | { d; ev = None } ->
            Printf.printf " %2d" d
      done
      ; print_string "  "
    ) is
    ; print_newline ()
  done

let print_cal cs m =
  List.iter (fun mon_row ->
    print_mon m mon_row cs
  ) [
      [ 0;  1;  2;  3 ];
      [ 4;  5;  6;  7 ];
      [ 8;  9; 10; 11 ];

      (*
      [  0;  1;  2 ];
      [  3;  4;  5 ];
      [  6;  7;  8 ];
      [  9; 10; 11 ];

      [ 0;  3;  6;  9 ];
      [ 1;  4;  7; 10 ];
      [ 2;  5;  8; 11 ];
      *)
    ]

let load_file fn =
  let ic = open_in fn in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  (Bytes.to_string s)

let read_events dir =
  let ev_files = Array.to_list (Sys.readdir dir) in
  let f ev_file =
    let date = chop_extension ev_file in
    let content = load_file (Filename.concat dir ev_file) in
    (date, String.trim content)
  in
  (List.map f ev_files)

let read_events dir =
  try read_events dir
  with Sys_error _ -> []


let print_events cs evs ~year =
  let _year = int_of_string year in
  let filter_date (date, _) =
    let y, _, _ = dt_of_string date in
    (y = _year)
  in
  let compare_date (date1, _) (date2, _) = compare date1 date2 in
  let evs = List.filter filter_date evs in
  let evs = List.sort compare_date evs in
  List.iter (fun (date, content) ->
    Printf.printf "%s: %s\n"
      (color_s (event_color cs) date)
      (color `normal content)
  ) evs

let print_year_header cs ~year =
  let year_lbl = pad (23*4-3) ' ' year in
  Printf.printf " %s\n" (color_s (month_label_color cs) year_lbl);
  print_newline ()


let default_params = [
  ("detri_dir", default_detri_dir);
  ("year", current_year ());
  ("event_color", default_event_color);
  ("day_label_color", default_day_label_color);
  ("month_label_color", default_month_label_color);
]


let rec parse_args params = function
  | "--year" :: year :: args ->
      let params = list_assoc_replace "year" year params in
      parse_args params args

  | "--detri-dir" :: detri_dir :: args ->
      let params = list_assoc_replace "detri_dir" detri_dir params in
      parse_args params args

  | "--event-color" :: event_color :: args ->
      let params = list_assoc_replace "event_color" event_color params in
      parse_args params args

  | "--day-label-color" :: day_label_color :: args ->
      let params =
        list_assoc_replace "day_label_color" day_label_color params
      in
      parse_args params args

  | "--month-label-color" :: month_label_color :: args ->
      let params =
        list_assoc_replace "month_label_color" month_label_color params
      in
      parse_args params args

  | "--print-avail-colors" :: _ ->
      print_avail_colors 0

  | "--help"::[] | "-help"::[] | "-h"::[] -> usage 0

  | [] -> params

  | _ -> usage 1


let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let params = parse_args default_params args in
  let get_param param = List.assoc param params in
  Random.self_init ();

  let year = get_param "year" in
  let detri_dir = get_param "detri_dir" in

  let event_color = get_param "event_color" in
  let day_label_color = get_param "day_label_color" in
  let month_label_color = get_param "month_label_color" in
  let cs = (event_color, day_label_color, month_label_color) in

  let evs = read_events detri_dir in
  print_year_header cs ~year;
  print_cal cs (cal evs ~year);
  print_events cs evs ~year;
;;


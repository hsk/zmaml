(* ZMA（HRA! さんの Z80/R800 クロスアセンブラ、https://github.com/hra1129/zma ）の書き方を読み、機械語にする。

   3 つに分ける。どれも、前の結果だけから次を作る関数の組み合わせで、書き換える変数はほとんどない。
   1. 命令の組み立て: 命令を「ニーモニックとオペランドの並び」というデータ（Ast.line）で持ち、
      機械語か ZMA の記法のテキストにする。逆アセンブラ（dis.ml）もこのデータを作る。
   2. 展開（expand）: 行のリスト -> 行のリスト。MACRO、REPEAT、IF、SCOPE、INCLUDE、BINARY_LINK を
      文字の置き換えで済ませる。
   3. 読み込み（parse_line）: 1 行 -> Ast.line。式と命令の形を読む。

   読むものと、まだ読まないものは docs/zma.md にまとめてある。 *)

(* ---- 命令のデータ（AST）と、それを ZMA の記法のテキストにする show ---- *)
module Ast = struct
  type e =                         (* アセンブル時に決まる式 *)
    | Num of int
    | Sym of string                (* ラベルの番地 *)
    | Bop of string * e * e        (* 二項演算（ZMA の式）: + - * / % << >> & | ^ == != < > <= >= && || *)
    | Uop of string * e            (* 単項演算: - ~ ! *)
    | Here                         (* $: いま組み立てている行の番地 *)

  type arg =
    | R of string                  (* レジスタ名か条件名: a hl nz ... *)
    | I of e                       (* 即値 *)
    | M of arg                     (* (…) メモリ参照 *)
    | X of string * e              (* ix+d。M の中に置く *)

  type line =
    | Ins of (string * arg list)
    | Label of string
    | Db of e list
    | Dw of e list
    | Ds of int                    (* 0 で埋める *)
    | Align of int                 (* 番地がこの数の倍数になるまで 0 で埋める *)
    | Equ of string * e            (* 名前に値を付ける *)

  let rec show_e = function
    | Num n -> if n < 10 then string_of_int n else Printf.sprintf "0x%x" n
    | Sym s -> s
    | Bop (o, a, b) -> Printf.sprintf "(%s %s %s)" (show_e a) o (show_e b)
    | Uop (o, a) -> Printf.sprintf "(%s%s)" o (show_e a)
    | Here -> "$"

  (* ZMA の記法: メモリ参照は [..]、定数は :=、データは defb / defw / space *)
  let rec show_arg = function
    | R r -> r
    | I e -> show_e e
    | M a -> "[" ^ show_arg a ^ "]"
    | X (r, Num d) when d < 0 -> Printf.sprintf "%s-%d" r (-d)
    | X (r, e) -> r ^ "+" ^ show_e e

  let show_ins (op, args) =
    if args = [] then op else op ^ " " ^ String.concat "," (List.map show_arg args)

  let show = function
    | Ins (op, args) -> "    " ^ show_ins (op, args)
    | Label l -> l ^ ":"
    | Db es -> "    defb " ^ String.concat "," (List.map show_e es)
    | Dw es -> "    defw " ^ String.concat "," (List.map show_e es)
    | Ds n -> Printf.sprintf "    space %d" n
    | Align n -> Printf.sprintf "    align %d" n
    | Equ (l, e) -> Printf.sprintf "%s := %s" l (show_e e)
end

open Ast

exception Error of string
let err fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

(* ---- 1 命令を機械語にする。外に見せるのは enc と、逆アセンブラも使う命令名の表だけ ---- *)
module Encoder : sig
  val alu : string list
  val rot : string list
  val plain : (string * int list) list
  (* enc v pc 命令: v は式を数にする関数、pc はその命令の番地。機械語のバイト列を返す *)
  val enc : (e -> int) -> int -> string * arg list -> int list
end = struct
  (* ---- オペランドの分類。番号は命令コードの y, z, p にそのまま入る ---- *)

  let index x l =
    let rec go i = function
      | [] -> None
      | y :: t -> if x = y then Some i else go (i + 1) t
    in
    go 0 l

  let r8 = function                (* b c d e h l (hl) a *)
    | M (R "hl") -> Some 6
    | R r -> index r [ "b"; "c"; "d"; "e"; "h"; "l"; ""; "a" ]
    | _ -> None

  let pair names = function R r -> index r names | _ -> None
  let rp = pair [ "bc"; "de"; "hl"; "sp" ]
  let rp2 = pair [ "bc"; "de"; "hl"; "af" ]
  let cc = pair [ "nz"; "z"; "nc"; "c"; "po"; "pe"; "p"; "m" ]

  let alu = [ "add"; "adc"; "sub"; "sbc"; "and"; "xor"; "or"; "cp" ]
  let rot = [ "rlc"; "rrc"; "rl"; "rr"; "sla"; "sra"; "sll"; "srl" ]

  let plain = [                    (* オペランドのない命令 *)
    "nop", [ 0x00 ]; "rlca", [ 0x07 ]; "rrca", [ 0x0f ]; "rla", [ 0x17 ];
    "rra", [ 0x1f ]; "daa", [ 0x27 ]; "cpl", [ 0x2f ]; "scf", [ 0x37 ];
    "ccf", [ 0x3f ]; "halt", [ 0x76 ]; "ret", [ 0xc9 ]; "exx", [ 0xd9 ];
    "di", [ 0xf3 ]; "ei", [ 0xfb ]; "neg", [ 0xed; 0x44 ]; "retn", [ 0xed; 0x45 ];
    "reti", [ 0xed; 0x4d ]; "rrd", [ 0xed; 0x67 ]; "rld", [ 0xed; 0x6f ];
    "ldi", [ 0xed; 0xa0 ]; "cpi", [ 0xed; 0xa1 ]; "ini", [ 0xed; 0xa2 ];
    "outi", [ 0xed; 0xa3 ]; "ldd", [ 0xed; 0xa8 ]; "cpd", [ 0xed; 0xa9 ];
    "ind", [ 0xed; 0xaa ]; "outd", [ 0xed; 0xab ]; "ldir", [ 0xed; 0xb0 ];
    "cpir", [ 0xed; 0xb1 ]; "inir", [ 0xed; 0xb2 ]; "otir", [ 0xed; 0xb3 ];
    "lddr", [ 0xed; 0xb8 ]; "cpdr", [ 0xed; 0xb9 ]; "indr", [ 0xed; 0xba ];
    "otdr", [ 0xed; 0xbb ];
  ]

  (* ---- 1 命令を機械語にする ----
     v は式を数にする関数、pc はこの命令の番地（相対ジャンプに使う）。
     IX/IY の命令は「HL の命令に前置バイト（dd / fd）と変位 d を足したもの」なので、
     ここでは HL の命令だけを表にする。IX/IY は下の enc が hl に読み替えて、この関数に渡す。 *)

  let enc_hl v pc (op, args) =
    let n e = v e land 0xff in
    let w e = let x = v e in [ x land 0xff; x lsr 8 land 0xff ] in
    let rel e =
      let d = v e - pc - 2 in
      if d < -128 || d > 127 then err "相対ジャンプが届かない: %s" (show_ins (op, args));
      d land 0xff
    in
    let ( ! ) o = Option.get o in
    let is o = o <> None in
    match op, args with
    | _, [] when List.mem_assoc op plain -> List.assoc op plain
    (* 8bit ロード *)
    | "ld", [ R "a"; M (R "bc") ] -> [ 0x0a ]
    | "ld", [ R "a"; M (R "de") ] -> [ 0x1a ]
    | "ld", [ M (R "bc"); R "a" ] -> [ 0x02 ]
    | "ld", [ M (R "de"); R "a" ] -> [ 0x12 ]
    | "ld", [ R "a"; M (I e) ] -> 0x3a :: w e
    | "ld", [ M (I e); R "a" ] -> 0x32 :: w e
    | "ld", [ R "i"; R "a" ] -> [ 0xed; 0x47 ]
    | "ld", [ R "a"; R "i" ] -> [ 0xed; 0x57 ]
    | "ld", [ R "r"; R "a" ] -> [ 0xed; 0x4f ]
    | "ld", [ R "a"; R "r" ] -> [ 0xed; 0x5f ]
    (* 16bit ロード *)
    | "ld", [ R "hl"; M (I e) ] -> 0x2a :: w e
    | "ld", [ M (I e); R "hl" ] -> 0x22 :: w e
    | "ld", [ R "sp"; R "hl" ] -> [ 0xf9 ]
    | "ld", [ q; M (I e) ] when is (rp q) -> 0xed :: (0x4b + (16 * !(rp q))) :: w e
    | "ld", [ M (I e); q ] when is (rp q) -> 0xed :: (0x43 + (16 * !(rp q))) :: w e
    | "ld", [ q; I e ] when is (rp q) -> (0x01 + (16 * !(rp q))) :: w e
    | "ld", [ x; I e ] when is (r8 x) -> [ 0x06 + (8 * !(r8 x)); n e ]
    | "ld", [ x; y ] when is (r8 x) && is (r8 y) && (x, y) <> (M (R "hl"), M (R "hl")) ->
      [ 0x40 + (8 * !(r8 x)) + !(r8 y) ]
    (* 増減 *)
    | "inc", [ x ] when is (r8 x) -> [ 0x04 + (8 * !(r8 x)) ]
    | "dec", [ x ] when is (r8 x) -> [ 0x05 + (8 * !(r8 x)) ]
    | "inc", [ q ] when is (rp q) -> [ 0x03 + (16 * !(rp q)) ]
    | "dec", [ q ] when is (rp q) -> [ 0x0b + (16 * !(rp q)) ]
    (* 16bit 演算 *)
    | "add", [ R "hl"; q ] when is (rp q) -> [ 0x09 + (16 * !(rp q)) ]
    | "adc", [ R "hl"; q ] when is (rp q) -> [ 0xed; 0x4a + (16 * !(rp q)) ]
    | "sbc", [ R "hl"; q ] when is (rp q) -> [ 0xed; 0x42 + (16 * !(rp q)) ]
    (* R800 の掛け算。mulub a,r: HL = A * r（r は b c d e）。muluw hl,bc: DE:HL = HL * BC（どちらも符号なし） *)
    | "mulub", [ R "a"; (R ("b" | "c" | "d" | "e") as x) ] -> [ 0xed; 0xc1 + (8 * !(r8 x)) ]
    | "muluw", [ R "hl"; (R ("bc" | "sp") as q) ] -> [ 0xed; 0xc3 + (16 * !(rp q)) ]
    (* 8bit 演算。add a,b と sub b の両方の書き方を受ける *)
    | _, ([ R "a"; x ] | [ x ]) when List.mem op alu && is (r8 x) ->
      [ 0x80 + (8 * !(index op alu)) + !(r8 x) ]
    | _, ([ R "a"; I e ] | [ I e ]) when List.mem op alu ->
      [ 0xc6 + (8 * !(index op alu)); n e ]
    (* 回転・シフトとビット操作 *)
    | _, [ x ] when List.mem op rot && is (r8 x) -> [ 0xcb; (8 * !(index op rot)) + !(r8 x) ]
    | ("bit" | "res" | "set"), [ I b; x ] when is (r8 x) ->
      let b = v b in
      if b < 0 || b > 7 then err "ビット番号が範囲外: %s" (show_ins (op, args));
      [ 0xcb; (0x40 * (1 + !(index op [ "bit"; "res"; "set" ]))) + (8 * b) + !(r8 x) ]
    (* 分岐 *)
    | "jp", [ I e ] -> 0xc3 :: w e
    | "jp", [ c; I e ] when is (cc c) -> (0xc2 + (8 * !(cc c))) :: w e
    | "jp", [ M (R "hl") ] -> [ 0xe9 ]
    | "jr", [ I e ] -> [ 0x18; rel e ]
    | "jr", [ c; I e ] when is (cc c) && !(cc c) < 4 -> [ 0x20 + (8 * !(cc c)); rel e ]
    | "djnz", [ I e ] -> [ 0x10; rel e ]
    | "call", [ I e ] -> 0xcd :: w e
    | "call", [ c; I e ] when is (cc c) -> (0xc4 + (8 * !(cc c))) :: w e
    | "ret", [ c ] when is (cc c) -> [ 0xc0 + (8 * !(cc c)) ]
    | "rst", [ I e ] -> [ 0xc7 + v e ]
    (* スタックと交換 *)
    | "push", [ q ] when is (rp2 q) -> [ 0xc5 + (16 * !(rp2 q)) ]
    | "pop", [ q ] when is (rp2 q) -> [ 0xc1 + (16 * !(rp2 q)) ]
    | "ex", [ R "de"; R "hl" ] -> [ 0xeb ]
    | "ex", [ R "af"; R "af'" ] -> [ 0x08 ]
    | "ex", [ M (R "sp"); R "hl" ] -> [ 0xe3 ]
    (* 入出力と割り込み *)
    | "in", [ R "a"; M (I e) ] -> [ 0xdb; n e ]
    | "out", [ M (I e); R "a" ] -> [ 0xd3; n e ]
    | "in", [ x; M (R "c") ] when is (r8 x) -> [ 0xed; 0x40 + (8 * !(r8 x)) ]
    | "out", [ M (R "c"); x ] when is (r8 x) -> [ 0xed; 0x41 + (8 * !(r8 x)) ]
    | "im", [ I e ] -> [ 0xed; [| 0x46; 0x56; 0x5e |].(v e) ]
    | _ -> err "命令が組み立てられない: %s" (show_ins (op, args))

  (* IX/IY は「HL の読み替え」。HL の命令を作り、前置バイトと変位 d を差し込む。 *)
  let enc v pc (op, args) =
    let pre = ref None and d = ref None in
    let set p =
      if !pre <> None && !pre <> Some p then err "IX と IY は混ぜられない: %s" (show_ins (op, args));
      pre := Some p
    in
    let code = function "ix" -> 0xdd | _ -> 0xfd in
    let sub = function
      | R (("ix" | "iy") as r) -> set (code r); R "hl"
      | M (R (("ix" | "iy") as r)) -> set (code r); (if op <> "jp" then d := Some 0); M (R "hl")
      | M (X (r, e)) -> set (code r); d := Some (v e land 0xff); M (R "hl")
      | a -> a
    in
    let args = List.map sub args in
    match !pre, !d, enc_hl v pc (op, args) with
    | None, _, b -> b
    | Some p, None, b -> p :: b
    | Some p, Some d, [ 0xcb; o ] -> [ p; 0xcb; d; o ]
    | Some p, Some d, o :: rest -> p :: o :: d :: rest
    | _ -> assert false
end

(* ---- 式の値 ---- *)
module Evaluator : sig
  (* eval look pc 式: look はラベルの番地を返す関数、pc は $ の値 *)
  val eval : (string -> int) -> int -> e -> int
end = struct
  let rec eval look pc = function
    | Num n -> n
    | Sym s -> look s
    | Here -> pc
    | Uop (o, a) ->
      let x = eval look pc a in
      (match o with "-" -> -x | "~" -> lnot x | _ -> Bool.to_int (x = 0))
    | Bop (o, a, b) ->
      let x = eval look pc a and y = eval look pc b in
      let t c = Bool.to_int c in
      (match o with
       | "+" -> x + y | "-" -> x - y | "*" -> x * y
       | "/" | "%" -> if y = 0 then err "0 で割った" else if o = "/" then x / y else x mod y
       | "<<" -> x lsl y | ">>" -> x asr y | "&" -> x land y | "|" -> x lor y | "^" -> x lxor y
       | "==" -> t (x = y) | "!=" -> t (x <> y) | "<" -> t (x < y) | ">" -> t (x > y) | "<=" -> t (x <= y) | ">=" -> t (x >= y)
       | "&&" -> t (x <> 0 && y <> 0) | "||" -> t (x <> 0 || y <> 0)
       | _ -> err "知らない演算: %s" o)
end

(* 全体を組み立てる。2 回なめる: 1 回目で番地を決め、2 回目で値を埋める。
   戻り値は (機械語, ラベルの表) *)
module Builder : sig
  val build : ?org:int -> line list -> string * (string * int) list
end = struct
  let build ?(org = 0) lines =
    let syms = Hashtbl.create 64 in
    let pass final =
      let buf = Buffer.create 1024 in
      let pc () = org + Buffer.length buf in
      let look s =
        match Hashtbl.find_opt syms s with
        | Some a -> a
        | None -> if final then err "未定義のラベル: %s" s else pc ()
      in
      let at = ref org in                                   (* 行の先頭の番地。式の $ になる *)
      let v e = Evaluator.eval look !at e in
      let byte b = Buffer.add_char buf (Char.chr (b land 0xff)) in
      List.iter
        (fun l ->
          at := pc ();
          match l with
          | Ins (op, args) -> List.iter byte (Encoder.enc v (pc ()) (op, args))
          | Label l ->
            if (not final) && Hashtbl.mem syms l then err "ラベルが重複: %s" l;
            Hashtbl.replace syms l (pc ())
          | Db es -> List.iter (fun e -> byte (v e)) es
          | Dw es -> List.iter (fun e -> let x = v e in byte x; byte (x lsr 8)) es
          | Ds n -> for _ = 1 to n do byte 0 done
          | Align n -> while pc () mod n <> 0 do byte 0 done
          | Equ (l, e) -> Hashtbl.replace syms l (v e))
        lines;
      Buffer.contents buf
    in
    ignore (pass false);
    let bin = pass true in
    bin, Hashtbl.fold (fun k a l -> (k, a) :: l) syms []
end

(* ======== ZMA の記法の読み込み ========
   外に見せるのは parse だけ。字句、式、展開などの部品は、この中に閉じておく。 *)
module Parser : sig
  (* 戻り値は (org 指示があればその番地, 行) *)
  val parse : ?read:(string -> string) -> ?defs:(string * int) list -> string -> int option * line list
end = struct
  let err ln fmt = Printf.ksprintf (fun s -> raise (Error (Printf.sprintf "%d 行目: %s" ln s))) fmt

  (* ---- 字句: 文字の並びを、コメント・カンマ・語・名前の置き換えで切り分ける ---- *)
  module Lexer = struct
    let is_id c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '.' || c = '@'
    let is_digit c = c >= '0' && c <= '9'
    let is_tok c = is_id c || c = '$'

    (* 'c' の形の文字定数が i から始まるか（af' の ' とは区別する） *)
    let is_char s i = i + 2 < String.length s && s.[i] = '\'' && s.[i + 2] = '\'' && (i = 0 || not (is_id s.[i - 1]))

    (* コメント（; から後ろ）を捨てて、タブを空白にして、前後の空白を取る。文字列と文字定数の中の ; は残す *)
    let norm s =
      let s = String.map (fun c -> if c = '\t' || c = '\r' then ' ' else c) s in
      let n = String.length s in
      let rec go i q =
        if i >= n then s
        else if s.[i] = ';' && not q then String.sub s 0 i
        else if s.[i] = '"' then go (i + 1) (not q)
        else if (not q) && is_char s i then go (i + 3) q
        else go (i + 1) q
      in
      String.trim (go 0 false)

    (* 括弧の外のカンマで分ける *)
    let split s =
      let n = String.length s in
      let part a b = String.trim (String.sub s a (b - a)) in
      let rec go i from depth q acc =
        if i >= n then List.rev (part from n :: acc)
        else if s.[i] = '"' then go (i + 1) from depth (not q) acc
        else if q then go (i + 1) from depth q acc
        else if is_char s i then go (i + 3) from depth q acc
        else match s.[i] with
          | '(' | '[' -> go (i + 1) from (depth + 1) q acc
          | ')' | ']' -> go (i + 1) from (depth - 1) q acc
          | ',' when depth = 0 -> go (i + 1) (i + 1) depth q (part from i :: acc)
          | _ -> go (i + 1) from depth q acc
      in
      if s = "" then [] else go 0 0 0 false []

    (* 最初の語（小文字にする）と、残り *)
    let word s =
      match String.index_opt s ' ' with
      | Some i -> String.lowercase_ascii (String.sub s 0 i), String.trim (String.sub s i (String.length s - i))
      | None -> String.lowercase_ascii s, ""

    (* 名前の部分を tbl で置き換える。文字列と文字定数の中は触らない *)
    let subst tbl s =
      if tbl = [] then s
      else begin
        let n = String.length s and b = Buffer.create 64 in
        let rec go i q =
          if i < n then
            if s.[i] = '"' then (Buffer.add_char b '"'; go (i + 1) (not q))
            else if q then (Buffer.add_char b s.[i]; go (i + 1) q)
            else if is_char s i then (Buffer.add_string b (String.sub s i 3); go (i + 3) q)
            else if is_tok s.[i] then begin
              let j = ref i in
              while !j < n && is_tok s.[!j] do incr j done;
              let t = String.sub s i (!j - i) in
              Buffer.add_string b (Option.value ~default:t (List.assoc_opt t tbl));
              go !j q
            end
            else (Buffer.add_char b s.[i]; go (i + 1) q)
        in
        go 0 false;
        Buffer.contents b
      end
  end

  (* ---- 式 ----
     ZMA の優先順位（ゆるい順）: || && | ^ & ==,!= < > <= >= << >> + - * / %。単項は + - ! ~。 *)

  let levels = [ [ "||" ]; [ "&&" ]; [ "|" ]; [ "^" ]; [ "&" ]; [ "=="; "!=" ]; [ "<="; ">="; "<"; ">" ];
                 [ "<<"; ">>" ]; [ "+"; "-" ]; [ "*"; "/"; "%" ] ]

  (* 数だけの式は、その場で計算する *)
  let fold ln e =
    match e with
    | Bop (_, Num _, Num _) | Uop (_, Num _) -> (try Num (Evaluator.eval (fun _ -> 0) 0 e) with Error m -> err ln "%s" m)
    | e -> e

  let number ln t =
    let t' = String.concat "" (String.split_on_char '_' t) in
    let m = String.length t' in
    let hex b = int_of_string_opt ("0x" ^ b) in
    let all p b = b <> "" && String.for_all p b in
    let is_hex c = Lexer.is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') in
    let r =
      if m > 1 && t'.[0] = '$' then hex (String.sub t' 1 (m - 1))
      else if m > 2 && (String.sub t' 0 2 = "0x" || String.sub t' 0 2 = "0X") then hex (String.sub t' 2 (m - 2))
      else if m > 1 && (t'.[m - 1] = 'h' || t'.[m - 1] = 'H') && all is_hex (String.sub t' 0 (m - 1)) then hex (String.sub t' 0 (m - 1))
      else if m > 2 && (String.sub t' 0 2 = "0b" || String.sub t' 0 2 = "0B") then int_of_string_opt ("0b" ^ String.sub t' 2 (m - 2))
      else if m > 1 && t'.[0] = '0' then int_of_string_opt ("0o" ^ String.sub t' 1 (m - 1))    (* 先頭の 0 は 8 進数 *)
      else int_of_string_opt t'
    in
    match r with Some v -> Num v | None -> err ln "数が読めない: %s" t

  let expr ln s =
    let n = String.length s and i = ref 0 in
    let peek k = if !i + k < n then s.[!i + k] else '\000' in
    let skip () = while peek 0 = ' ' do incr i done in
    (* 演算子 o が、ここにあるか（&& の途中の & などは除く） *)
    let at o =
      let k = String.length o in
      !i + k <= n && String.sub s !i k = o
      && (match o with
          | "&" -> peek 1 <> '&' | "|" -> peek 1 <> '|'
          | "<" | ">" -> peek 1 <> '=' && peek 1 <> peek 0 | _ -> true)
    in
    let rec bin = function
      | [] -> unary ()
      | ops :: tighter ->
        let l = ref (bin tighter) in
        let rec loop () =
          skip ();
          match List.find_opt at ops with
          | Some o -> i := !i + String.length o; l := fold ln (Bop (o, !l, bin tighter)); loop ()
          | None -> ()
        in
        loop ();
        !l
    and unary () =
      skip ();
      match peek 0 with
      | ('+' | '-' | '!' | '~') as c ->
        incr i;
        let a = unary () in
        if c = '+' then a else fold ln (Uop (String.make 1 c, a))
      | _ -> prim ()
    and prim () =
      skip ();
      match peek 0 with
      | '(' ->
        incr i;
        let e = bin levels in
        skip ();
        if peek 0 <> ')' then err ln "閉じ括弧がない: %s" s;
        incr i;
        e
      | '\'' when Lexer.is_char s !i -> let c = s.[!i + 1] in i := !i + 3; Num (Char.code c)
      | _ ->
        let a = !i in
        while !i < n && Lexer.is_tok s.[!i] do incr i done;
        let t = String.sub s a (!i - a) in
        if t = "" then err ln "式が読めない: %s" s
        else if t = "$" then Here
        else if Lexer.is_digit t.[0] || (t.[0] = '$' && String.length t > 1) then number ln t
        else Sym t
    in
    let e = bin levels in
    skip ();
    if !i < n then err ln "式が読めない: %s" s;
    e

  (* ---- 命令 ---- *)

  let regs = [ "a"; "b"; "c"; "d"; "e"; "h"; "l"; "i"; "r"; "af"; "af'"; "bc"; "de"; "hl"; "sp"; "ix"; "iy";
               "nz"; "z"; "nc"; "po"; "pe"; "p"; "m" ]

  (* 先頭の括弧に対応する閉じ括弧の位置 *)
  let close s =
    let rec go i d =
      if i >= String.length s then -1
      else match s.[i] with
        | '(' | '[' -> go (i + 1) (d + 1)
        | ')' | ']' -> if d = 1 then i else go (i + 1) (d - 1)
        | _ -> go (i + 1) d
    in
    go 0 0

  (* メモリ参照の中身 *)
  let mem ln s =
    let l = String.lowercase_ascii s in
    let idx = String.length l >= 2 && (String.sub l 0 2 = "ix" || String.sub l 0 2 = "iy") in
    let rest = if idx then String.trim (String.sub l 2 (String.length l - 2)) else "" in
    if List.mem l [ "bc"; "de"; "hl"; "sp"; "c" ] then M (R l)
    else if idx && rest = "" then M (X (String.sub l 0 2, Num 0))
    else if idx && (rest.[0] = '+' || rest.[0] = '-') then M (X (String.sub l 0 2, expr ln (String.sub s 2 (String.length s - 2))))
    else M (I (expr ln s))

  (* [..] はメモリ参照。( ) は式のまとまりで、ld a,(1+2) は定数 3 *)
  let operand ln s =
    let n = String.length s and l = String.lowercase_ascii s in
    if List.mem l regs then R l
    else if n >= 2 && s.[0] = '[' && close s = n - 1 then mem ln (String.trim (String.sub s 1 (n - 2)))
    else I (expr ln s)

  (* jp hl / jp [ix] は、ジャンプ先ではなくレジスタの値へ飛ぶ *)
  let fix op args =
    match op, args with
    | "jp", [ R (("hl" | "ix" | "iy") as r) ] | "jp", [ M (X (("ix" | "iy") as r, Num 0)) ] -> [ M (R r) ]
    | _ -> args

  (* 行頭の「名前」と、そのあとの印（:: 大域、:= 大域の定数、: 局所、= 局所の定数）と、残り *)
  type kind = Global | Const_g | Local | Const

  let head s =
    let n = String.length s in
    let i = ref 0 in
    while !i < n && Lexer.is_id s.[!i] do incr i done;
    if !i = 0 || Lexer.is_digit s.[0] then None
    else begin
      let name = String.sub s 0 !i and r = String.trim (String.sub s !i (n - !i)) in
      let has p = String.starts_with ~prefix:p r in
      let cut k = String.trim (String.sub r k (String.length r - k)) in
      if has "::" then Some (Global, name, cut 2)
      else if has ":=" then Some (Const_g, name, cut 2)
      else if has ":" then Some (Local, name, cut 1)
      else if has "=" && not (has "==") then Some (Const, name, cut 1)
      else None
    end

  (* 展開の済んだ 1 行 -> 組み立ての行（ラベルと命令が一緒にあれば 2 つ） *)
  let parse_line ln s : line list =
    let lab, s = match head s with
      | Some ((Global | Local), n, rest) -> [ Label n ], rest
      | Some ((Const | Const_g), n, rest) -> [ Equ (n, expr ln rest) ], ""
      | None -> [], s
    in
    let op, rest = Lexer.word s in
    let args = Lexer.split rest in
    let num a = match expr ln a with Num v -> v | _ -> err ln "%s の引数は数" op in
    let data one = List.concat_map (fun a ->
        let k = String.length a in
        if k >= 2 && a.[0] = '"' && a.[k - 1] = '"' then List.init (k - 2) (fun j -> Num (Char.code a.[j + 1])) else one (expr ln a)) args in
    let le k e = List.init k (fun j -> fold ln (Bop ("&", fold ln (Bop (">>", e, Num (8 * j))), Num 255))) in
    lab @
    match op with
    | "" -> []
    | "defb" -> [ Db (data (fun e -> [ e ])) ]
    | "defw" -> [ Dw (List.map (expr ln) args) ]
    | "defd" -> [ Db (List.concat_map (fun a -> le 4 (expr ln a)) args) ]
    | "space" ->
      (match args with
       | [ n ] | [ n; "0" ] -> [ Ds (num n) ]
       | [ n; f ] -> [ Db (List.init (num n) (fun _ -> expr ln f)) ]
       | _ -> err ln "%s は 個数 [, 埋める値]" op)
    | "align" -> [ Align (num rest) ]
    | _ ->
      let ins = op, fix op (List.map (operand ln) args) in
      (try ignore (Encoder.enc (fun _ -> 0) 0 ins)
       with Error m when String.starts_with ~prefix:"命令が組み立てられない" m -> err ln "%s" m | Error _ -> ());
      [ Ins ins ]

  (* ---- 展開 ---- *)

  type env = {
    vars : (string * string) list;                         (* マクロの引数と REPEAT の変数 -> 置き換える文字 *)
    consts : (string * int) list;                          (* 値が決まっている定数（IF と REPEAT の判断に使う） *)
    macros : (string * (string list * (int * string) list)) list;   (* マクロ名（小文字） -> (引数の名前, 本体) *)
    depth : int;
  }

  let has_here = let rec f = function Here -> true | Bop (_, a, b) -> f a || f b
                                    | Uop (_, a) -> f a | Num _ | Sym _ -> false in f

  (* いま値が決まる式の値。ラベルや $ が入っていれば None *)
  let value env ln s =
    let e = expr ln s in
    if has_here e then None
    else try Some (Evaluator.eval (fun x -> match List.assoc_opt x env.consts with Some v -> v | None -> raise Not_found) 0 e)
      with Not_found -> None

  let need env ln s =
    match value env ln s with Some v -> v | None -> err ln "この時点で値が決まらない（ラベルは使えない）: %s" s

  (* 先頭から、対応する close までを (中身, 残り) に分ける。opens は入れ子を作る開始の語 *)
  let block ln opens close lines =
    let rec go d acc = function
      | [] -> err ln "%s がない" close
      | ((_, t) as l) :: rest ->
        let k = fst (Lexer.word t) in
        if k = close && d = 0 then List.rev acc, rest
        else go (if List.mem k opens then d + 1 else if k = close then d - 1 else d) (l :: acc) rest
    in
    go 0 [] lines

  (* IF の本体を ELSEIF / ELSE で分ける: (条件の式, 行) の並び *)
  let branches c0 body =
    let rec go cond acc d out = function
      | [] -> List.rev ((cond, List.rev acc) :: out)
      | ((_, t) as x) :: rest ->
        let k, r = Lexer.word t in
        if d = 0 && k = "elseif" then go r [] 0 ((cond, List.rev acc) :: out) rest
        else if d = 0 && k = "else" then go "1" [] 0 ((cond, List.rev acc) :: out) rest
        else go cond (x :: acc) (if k = "if" then d + 1 else if k = "endif" then d - 1 else d) out rest
    in
    go c0 [] 0 [] body

  (* 局所のラベルと定数に、他と重ならない名前を付ける（SCOPE と REPEAT の 1 回ぶん） *)
  let scoped ctr lines =
    let locals = List.filter_map (fun (_, t) -> match head t with Some ((Local | Const), n, _) -> Some n | _ -> None) lines in
    if locals = [] then lines
    else begin
      incr ctr;
      let tbl = List.map (fun n -> n, Printf.sprintf "%s@%d" n !ctr) locals in
      List.map (fun (l, t) -> l, Lexer.subst tbl t) lines
    end

  let unquote ln s =
    let n = String.length s in
    if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2) else err ln "文字列（\"...\"）が要る: %s" s

  let lines_of text = List.filter (fun (_, t) -> t <> "") (List.mapi (fun i s -> i + 1, Lexer.norm s) (String.split_on_char '\n' text))

  (* rd: ファイル名 -> 中身。戻り値は (定数などを引き継いだ env, 展開した行) *)
  let rec go rd ctr env = function
    | [] -> env, []
    | (ln, t) :: rest ->
      let w, r = Lexer.word t in
      let w2, r2 = Lexer.word r in
      let more env' ls = let e, tl = go rd ctr { env' with vars = env.vars } rest in e, ls @ tl in
      if env.depth > 64 then err ln "展開が深すぎる（マクロが自分を呼んでいないか）"
      else if w2 = "macro" then begin
        let body, rest = block ln [] "endm" rest in
        go rd ctr { env with macros = (w, (Lexer.split r2, body)) :: env.macros } rest
      end
      else if w = "repeat" then begin
        (* 変数の名前は置き換えない。回数だけ、外側の変数で置き換える *)
        let v, n = match Lexer.split r with [ v; n ] -> v, need env ln (Lexer.subst env.vars n) | _ -> err ln "repeat は 変数, 回数" in
        let body, rest = block ln [ "repeat" ] "endr" rest in
        let env', ls =
          List.fold_left
            (fun (e, acc) i ->
              let e', ls = go rd ctr { e with vars = (v, string_of_int i) :: env.vars; depth = env.depth + 1 } body in
              e', acc @ scoped ctr ls)
            (env, []) (List.init (max n 0) Fun.id)
        in
        let e, tl = go rd ctr { env' with vars = env.vars } rest in
        e, ls @ tl
      end
      else begin
        let t = Lexer.subst env.vars t in
        let w, r = Lexer.word t in
        match w with
        | "if" ->
          let body, rest = block ln [ "if" ] "endif" rest in
          let chosen = List.find_opt (fun (c, _) -> need env ln (Lexer.subst env.vars c) <> 0) (branches r body) in
          let env', ls = match chosen with Some (_, b) -> go rd ctr { env with depth = env.depth + 1 } b | None -> env, [] in
          let e, tl = go rd ctr { env' with vars = env.vars } rest in
          e, ls @ tl
        | "scope" ->
          let body, rest = block ln [ "scope" ] "endscope" rest in
          let env', ls = go rd ctr { env with depth = env.depth + 1 } body in
          let e, tl = go rd ctr { env' with vars = env.vars } rest in
          e, scoped ctr ls @ tl
        | "include" -> go rd ctr env (lines_of (rd (unquote ln r)) @ rest)
        | "binary_link" ->
          let bytes = rd (unquote ln r) in
          let row k = "defb " ^ String.concat "," (List.init (min 16 (String.length bytes - k)) (fun j -> string_of_int (Char.code bytes.[k + j]))) in
          go rd ctr env (List.init ((String.length bytes + 15) / 16) (fun j -> ln, row (16 * j)) @ rest)
        | "message" -> prerr_endline (unquote ln r); go rd ctr env rest
        | "error" -> err ln "%s" (unquote ln r)
        | "endm" | "endr" | "endif" | "endscope" | "else" | "elseif" -> err ln "対応する開始がない: %s" w
        | "chg_char_set" | "mapping_char" | "link" | "endl" | "add_include_path" -> err ln "ZMA の %s には未対応" w
        | _ ->
          (match List.assoc_opt w env.macros with
           | Some (params, body) ->
             let args = Lexer.split r in
             if List.length args <> List.length params then err ln "%s の引数は %d 個" w (List.length params);
             let env', ls = go rd ctr { env with vars = List.combine params args; depth = env.depth + 1 } body in
             more env' ls
           | None ->
             (* name = 値 の定数は、決まっていれば覚える（あとの IF や REPEAT で使える） *)
             let env = match head t with
               | Some ((Const | Const_g), n, e) ->
                 (match value env ln e with Some v -> { env with consts = (n, v) :: env.consts } | None -> env)
               | _ -> env
             in
             let e, tl = go rd ctr env rest in
             e, (ln, t) :: tl)
      end

  let no_file name = raise (Error ("INCLUDE や BINARY_LINK のファイルを読む手段がない: " ^ name))

  (* 戻り値は (org 指示があればその番地, 行) *)
  let parse ?(read = no_file) ?(defs = []) src =
    let env = { vars = []; consts = defs; macros = []; depth = 0 } in
    let _, ls = go read (ref 0) env (lines_of src) in
    match ls with
    | (ln, t) :: rest when fst (Lexer.word t) = "org" ->
      let r = snd (Lexer.word t) in
      (match value env ln r with
       | Some v -> Some v, List.concat_map (fun (l, t) -> parse_line l t) rest
       | None -> err ln "org は数")
    | _ ->
      List.iter (fun (ln, t) -> if fst (Lexer.word t) = "org" then err ln "org は先頭に 1 つだけ（途中の org には未対応）") ls;
      None, List.concat_map (fun (l, t) -> parse_line l t) ls
end

(* 組み立てる。戻り値は (org, 機械語, ラベルの表) *)
let assemble ?(org = 0) ?read ?defs src =
  let o, lines = Parser.parse ?read ?defs src in
  let org = Option.value o ~default:org in
  let bin, syms = Builder.build ~org lines in
  org, bin, syms

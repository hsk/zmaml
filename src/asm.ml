(* Z80 / R800 アセンブラ。命令は「ニーモニックとオペランドの並び」というデータで持つ。
   Zma（読み込み）と Dis（逆アセンブル）がこのデータを作り、ここで機械語か ZMA の書き方のテキストにする。 *)

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

exception Error of string
let err fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

(* ---- テキストにする ---- *)

let rec show_e = function
  | Num n -> if n < 10 then string_of_int n else Printf.sprintf "0x%x" n
  | Sym s -> s
  | Bop (o, a, b) -> Printf.sprintf "(%s %s %s)" (show_e a) o (show_e b)
  | Uop (o, a) -> Printf.sprintf "(%s%s)" o (show_e a)
  | Here -> "$"

(* ZMA の書き方: メモリ参照は [..]、定数は :=、データは defb / defw / space *)
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
   v は式の値、pc はこの命令の番地。IX/IY は別に扱うので、ここでは HL だけ考える。 *)

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

(* ---- 全体を組み立てる。2 回なめる: 1 回目で番地を決め、2 回目で値を埋める ---- *)

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

let assemble ?(org = 0) lines =
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
    let v e = eval look !at e in
    let byte b = Buffer.add_char buf (Char.chr (b land 0xff)) in
    List.iter
      (fun l ->
        at := pc ();
        match l with
        | Ins (op, args) -> List.iter byte (enc v (pc ()) (op, args))
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

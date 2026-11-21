(* Z80 / R800 逆アセンブラ。命令コードを x y z p q に分けて読み、Asm.Ast.line の命令データに戻して ZMA の記法で出力する。
   アセンブラの逆関数なので、「逆アセンブルして組み立て直すと元に戻る」ことで両方を検査できる。 *)

open Asm
open Ast

exception Bad                    (* 公式の命令ではない *)

let sx v = if v > 127 then v - 256 else v

(* 番地 pc にある 1 命令を逆アセンブルする（Encoder.enc の逆）。
   rd は「番地を渡すとその番地のバイトを返す関数」。
   戻り値は (その命令, 命令のバイト数)。例: 3e 64 なら (ld a,100, 2)。
   命令として読めないバイトは、1 バイトの defb とみなして (defb そのバイト, 1) を返す。 *)
let dis rd pc =
  let at = ref pc in
  let next () = let b = rd !at in incr at; b in
  let ix = ref None and used = ref false in           (* IX/IY 前置と、それが効いたか *)
  let hl () = match !ix with None -> R "hl" | Some r -> used := true; R r in
  let mem () =                                        (* (HL) か (IX+d) *)
    match !ix with
    | None -> M (R "hl")
    | Some r -> used := true; M (X (r, Num (sx (next ()))))
  in
  let n () = I (Num (next ())) in
  let nn () = let l = next () in I (Num (l lor (next () lsl 8))) in
  let rel () = let d = sx (next ()) in I (Num ((!at + d) land 0xffff)) in
  let r8 i = if i = 6 then mem () else R [| "b"; "c"; "d"; "e"; "h"; "l"; ""; "a" |].(i) in
  let rp p = if p = 2 then hl () else R [| "bc"; "de"; ""; "sp" |].(p) in
  let rp2 p = if p = 3 then R "af" else rp p in
  let cc y = R [| "nz"; "z"; "nc"; "c"; "po"; "pe"; "p"; "m" |].(y) in
  let field op = op lsr 6, op lsr 3 land 7, op land 7 in
  (* add a,x / adc a,x / sbc a,x だが sub x / and x / ... *)
  let alu_ins y x = List.nth Encoder.alu y, if y < 4 && y <> 2 then [ R "a"; x ] else [ x ] in
  let cb () =
    let m = if !ix <> None then Some (mem ()) else None in   (* d は命令コードより前 *)
    let x, y, z = field (next ()) in
    let a = match m with Some a -> if z <> 6 then raise Bad; a | None -> r8 z in
    if x = 0 && y = 6 then raise Bad;                 (* sll は非公式 *)
    if x = 0 then List.nth Encoder.rot y, [ a ]
    else [| ""; "bit"; "res"; "set" |].(x), [ I (Num y); a ]
  in
  let ed () =
    if !ix <> None then raise Bad;
    let op = next () in
    let x, y, z = field op in
    let p = y lsr 1 and q = y land 1 in
    match x, z with
    | 1, 0 when y <> 6 -> "in", [ r8 y; M (R "c") ]
    | 1, 1 when y <> 6 -> "out", [ M (R "c"); r8 y ]
    | 1, 2 -> (if q = 0 then "sbc" else "adc"), [ R "hl"; rp p ]
    | 1, 3 when p <> 2 -> let a = M (nn ()) in "ld", if q = 0 then [ a; rp p ] else [ rp p; a ]
    | 1, 6 when y = 0 || y = 2 || y = 3 -> "im", [ I (Num (max 0 (y - 1))) ]
    | 3, 1 when y < 4 -> "mulub", [ R "a"; r8 y ]                       (* R800: mulub a,b/c/d/e *)
    | 3, 3 when q = 0 && (p = 0 || p = 3) -> "muluw", [ R "hl"; rp p ]  (* muluw hl,bc / hl,sp *)
    | 1, 7 when y < 4 ->
      "ld", List.nth [ [ R "i"; R "a" ]; [ R "r"; R "a" ]; [ R "a"; R "i" ]; [ R "a"; R "r" ] ] y
    | _ ->
      (match List.find_opt (fun (_, b) -> b = [ 0xed; op ]) Encoder.plain with
       | Some (name, _) -> name, []
       | None -> raise Bad)
  in
  let rec main () =
    let op = next () in
    let x, y, z = field op in
    let p = y lsr 1 and q = y land 1 in
    match x, z with
    | 0, 0 when y = 1 -> "ex", [ R "af"; R "af'" ]
    | 0, 0 when y = 2 -> "djnz", [ rel () ]
    | 0, 0 when y = 3 -> "jr", [ rel () ]
    | 0, 0 when y > 3 -> "jr", [ cc (y - 4); rel () ]
    | 0, 1 -> if q = 0 then (let r = rp p in "ld", [ r; nn () ]) else (let h = hl () in "add", [ h; rp p ])
    | 0, 2 when p < 2 ->
      let m = M (R (if p = 0 then "bc" else "de")) in
      "ld", if q = 0 then [ m; R "a" ] else [ R "a"; m ]
    | 0, 2 ->
      let m = M (nn ()) and r = if p = 2 then hl () else R "a" in
      "ld", if q = 0 then [ m; r ] else [ r; m ]
    | 0, 3 -> (if q = 0 then "inc" else "dec"), [ rp p ]
    | 0, 4 -> "inc", [ r8 y ]
    | 0, 5 -> "dec", [ r8 y ]
    | 0, 6 -> let r = r8 y in "ld", [ r; n () ]
    | 1, _ when op <> 0x76 ->
      (* (IX+d) を使うとき、相手の H, L はそのまま *)
      if y = 6 || z = 6 then (let m = mem () in ix := None; "ld", if y = 6 then [ m; r8 z ] else [ r8 y; m ])
      else "ld", [ r8 y; r8 z ]
    | 2, _ -> alu_ins y (r8 z)
    | 3, 0 -> "ret", [ cc y ]
    | 3, 1 when q = 0 -> "pop", [ rp2 p ]
    | 3, 1 when p = 2 -> "jp", [ M (hl ()) ]
    | 3, 1 when p = 3 -> "ld", [ R "sp"; hl () ]
    | 3, 2 -> "jp", [ cc y; nn () ]
    | 3, 3 when y = 0 -> "jp", [ nn () ]
    | 3, 3 when y = 1 -> cb ()
    | 3, 3 when y = 2 -> "out", [ M (n ()); R "a" ]
    | 3, 3 when y = 3 -> "in", [ R "a"; M (n ()) ]
    | 3, 3 when y = 4 -> "ex", [ M (R "sp"); hl () ]
    | 3, 3 when y = 5 -> "ex", [ R "de"; R "hl" ]
    | 3, 4 -> "call", [ cc y; nn () ]
    | 3, 5 when q = 0 -> "push", [ rp2 p ]
    | 3, 5 when p = 0 -> "call", [ nn () ]
    | 3, 5 when p = 2 -> ed ()
    | 3, 5 ->
      if !ix <> None then raise Bad;
      ix := Some (if p = 1 then "ix" else "iy");
      let i = main () in
      if not !used then raise Bad;                    (* 前置が効かない命令 *)
      i
    | 3, 6 -> alu_ins y (n ())
    | 3, 7 -> "rst", [ I (Num (y * 8)) ]
    | _ ->
      (match List.find_opt (fun (_, b) -> b = [ op ]) Encoder.plain with
       | Some (name, _) -> name, []
       | None -> raise Bad)
  in
  match main () with
  | op, args -> Ins (op, args), !at - pc
  | exception Bad -> Db [ Num (rd pc) ], 1

(* ---- 逆アセンブルの出力。組み立て直せる形（Asm.assemble が読める）で、コメントに番地と機械語を付ける ---- *)

(* image の外を呼ぶ・飛ぶときの名前（MSX の BIOS）と、ポートの名前 *)
let bios = [ 0x47, "wrtvdp"; 0x4a, "rdvrm"; 0x4d, "wrtvrm"; 0x56, "filvrm"; 0x5c, "ldirvm"; 0x5f, "chgmod"; 0x62, "chgclr";
             0x90, "gicini"; 0x93, "wrtpsg"; 0x96, "rdpsg"; 0xa2, "chput"; 0xc6, "posit"; 0xd5, "gtstck"; 0xd8, "gttrig"; 0x141, "snsmat" ]
let ports = [ 0x98, "MSX VDP data"; 0x99, "MSX VDP ctrl"; 0xa0, "MSX PSG addr"; 0xa1, "MSX PSG data"; 0xa8, "MSX slot";
              0xbe, "SMS VDP data"; 0xbf, "SMS VDP ctrl"; 0x7f, "SMS PSG"; 0xdc, "SMS pad 1" ]

(* data: 命令にせず defb にする範囲 (始め, 終わり)。データの表を命令として読まないために使う *)
let disassemble ?(org = 0) ?(data = []) bin =
  let ends = org + String.length bin in
  let rd a = Char.code bin.[a - org] in
  let in_data a = List.exists (fun (lo, hi) -> a >= lo && a < hi) data in
  let db pc = Db [ Num (rd pc) ], 1 in
  let rec scan pc =
    if pc >= ends then []
    else begin
      let line, len =
        if in_data pc then begin                      (* 範囲の中は 8 バイトずつ defb *)
          let hi = List.fold_left (fun m (lo, hi) -> if pc >= lo && pc < hi then hi else m) pc data in
          let k = min 8 (min hi ends - pc) in
          Db (List.init k (fun i -> Num (rd (pc + i)))), k
        end else
          match dis rd pc with
          | (Ins _, len) as r when not (List.exists in_data (List.init (len - 1) (fun i -> pc + i + 1))) -> r
          | _ -> db pc
          | exception Invalid_argument _ -> db pc
      in
      (pc, line, len) :: scan (pc + len)
    end
  in
  let code = scan org in
  let start = Hashtbl.create 64 in
  List.iter (fun (pc, _, _) -> Hashtbl.replace start pc ()) code;
  let name t = Printf.sprintf "L%04x" t in
  let jumps = [ "jp"; "jr"; "call"; "djnz" ] in
  let target = function
    | Ins (op, args) when List.mem op jumps ->
      (match List.rev args with I (Num t) :: _ when t >= org && t < ends && Hashtbl.mem start t -> Some t | _ -> None)
    | _ -> None
  in
  let labelled = Hashtbl.create 64 in
  List.iter (fun (_, l, _) -> Option.iter (fun t -> Hashtbl.replace labelled t ()) (target l)) code;
  let b = Buffer.create 4096 in
  Printf.bprintf b "; 逆アセンブル（ZMA の記法）（%d バイト、番地 0x%04x〜0x%04x）。\n; 行末のコメントは「番地: 機械語」。L で始まる名前は、飛び先から付けたラベル。\n    org 0x%x\n" (String.length bin) org (ends - 1) org;
  List.iter
    (fun (pc, line, len) ->
      if Hashtbl.mem labelled pc then Printf.bprintf b "%s:\n" (name pc);
      let line = match line, target line with
        | Ins (op, args), Some t -> Ins (op, List.rev (match List.rev args with _ :: r -> I (Sym (name t)) :: r | [] -> []))
        | l, _ -> l
      in
      let hex = String.concat " " (List.init len (fun i -> Printf.sprintf "%02x" (rd (pc + i)))) in
      let hint =
        match line with
        | Ins (("call" | "jp"), [ I (Num t) ]) -> Option.map (fun n -> "BIOS " ^ n) (List.assoc_opt t bios)
        | Ins (("in" | "out"), args) ->
          List.find_map (function M (I (Num p)) -> List.assoc_opt p ports | _ -> None) args
        | _ -> None
      in
      Printf.bprintf b "%-32s; %04x: %s%s\n" (show line) pc hex (match hint with Some h -> "  " ^ h | None -> ""))
    code;
  Buffer.contents b

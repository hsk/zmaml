(* ZMA の読み込みと、逆アセンブルのテスト。
   1. ZMA の取扱説明書の例（マクロ、REPEAT の入れ子、IF、SCOPE、数の書き方）が、手で確かめた機械語になる。
   2. 逆アセンブルして組み立て直すと、元の機械語に戻る。 *)

open Zmaml

let hex s = String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let spec = {|
        org 0x4000
VDP_PORT = 0x99
HOGE    macro
        ld d, h
        ld e, l
        endm
MOGE    MACRO n
        LD A, n
        ENDM
LDM     MACRO D, S
        if D == 1
          ld bc, S
        else
          ld de, S
        endif
        ENDM
start::
        MOGE 100
        HOGE
        ld a, [hl]
        ld [ix+3], a
        ld a, [ix]
        out [VDP_PORT], a
        jp hl
        LDM 1, 0x1234
        LDM 2, 10*(2+1)
        repeat I, 3
          defb I*5 + 1
        endr
        repeat I, 2
          repeat J, 3
            defb I*5 + J
          endr
        endr
        repeat I, 3
          repeat I, I+1
            defb I
          endr
        endr
        scope HOGE2
        loop:
          djnz loop
        endscope
loop:   jr loop
        defw 0DEADh, $BEEF, 0xAA55
        defb 0b1010_1010, 0777, 1_234 & 255, "hi", 'x'
        space 3
        space 2, 0xff
        align 16
data:   defd 0x12345678
        defb (data - start) & 255, $ - start
|}

let () =
  let fails = ref 0 in
  let check what ok = if not ok then (incr fails; Printf.printf "NG zma: %s\n" what) in
  (* 取扱説明書の例 *)
  (match Asm.assemble spec with
   | org, bin, syms ->
     check "org" (org = 0x4000);
     (* 命令、マクロ、REPEAT（入れ子は 0,0,1,0,1,2）、SCOPE、数の書き方、space / align / defd、$ を順に並べたもの *)
     check "機械語"
       (hex bin = "3e64545d7edd7703dd7e00d399e9" ^ "013412" ^ "111e00" ^ "01060b" ^ "000102050607" ^ "000001000102"
                  ^ "10fe18fe" ^ "addeefbe55aa" ^ "aaffd2686978" ^ "000000" ^ "ffff" ^ "0000000000000000" ^ "78563412" ^ "4044");
     check "start は大域ラベル" (List.assoc "start" syms = 0x4000)
   | exception Asm.Error m -> check ("組み立てられない: " ^ m) false);
  (* 1 つずつ確かめる（上の連結比較より読みやすい形） *)
  let one src want =
    match Asm.assemble src with
    | _, bin, _ -> check (Printf.sprintf "%S → %s（%s だった）" src want (hex bin)) (hex bin = want)
    | exception Asm.Error m -> check (src ^ ": " ^ m) false
  in
  one "MOGE macro n\n ld a, n\n endm\n MOGE 100" "3e64";
  one "ld a,[hl]\nld [ix+3],a\nld a,[ix]\nout [0x99],a\njp hl\njp [ix]" "7edd7703dd7e00d399e9dde9";
  one "defw 0DEADh, $BEEF, 0xAA55" "addeefbe55aa";
  one "defb 0b1010_1010, 0777, 1_234 & 255, \"hi\", 'x'" "aaffd2686978";
  one "space 3\nspace 2, 0xff\ndefd 0x12345678" "000000ffff78563412";
  one "repeat I, 3\n repeat I, I+1\n  defb I\n endr\nendr" "000001000102";
  one "repeat I, 2\n repeat J, 3\n  defb I*5 + J\n endr\nendr" "000102050607";
  one "scope A\n x: djnz x\nendscope\nx: jr x" "10fe18fe";
  one "n = 3\nif n == 3\n defb 1\nelseif n == 4\n defb 2\nelse\n defb 3\nendif" "01";
  one "a: defb $ - a, $ - a" "0000";               (* $ は、その行の先頭の番地 *)
  one "ld a,(1+2)*3 ; 式" "3e09";
  one "ld a,(1+2)" "3e03";          (* ( ) は式のまとまり。メモリ参照は [ ] *)
  one "defb 2 + 3 * 4 == 14 && 1 | 6 == 7, -1 >> 1, !0, ~0 & 0xf, 7 % 4, 1 << 3" "01ff010f0308";
  (* エラーは行番号つきで出る *)
  (match Asm.assemble "nop\nld a,\nfoo bar" with
   | _ -> check "エラーになるはず" false
   | exception Asm.Error m -> check ("行番号: " ^ m) (String.starts_with ~prefix:"2 行目" m || String.starts_with ~prefix:"3 行目" m));
  (match Asm.assemble "if lab == 1\nendif\nlab: nop" with
   | _ -> check "IF にラベルは使えないはず" false
   | exception Asm.Error _ -> ());
  (* 逆アセンブルして組み立て直すと元に戻る（命令にならないバイトは defb になる） *)
  let seed = ref 12345 in
  let rnd () = seed := (!seed * 1103515245 + 12345) land 0x7fffffff; (!seed lsr 16) land 0xff in
  let image = String.init 20000 (fun _ -> Char.chr (rnd ())) in
  (match Asm.assemble (Dis.disassemble ~org:0x4000 image) with
   | _, bin, _ -> check "逆アセンブルの往復" (bin = image)
   | exception Asm.Error m -> check ("逆アセンブルの結果が組み立てられない: " ^ m) false);
  if !fails > 0 then exit 1

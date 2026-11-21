(* zmaml: ZMA 形式の Z80 アセンブラ。
     zmaml file.zma [--org N] [-Dname=値] [-Ipath] [-o out.bin]   組み立てて .bin を作る
     zmaml file.bin --dis [--org N] [--db 始め-終わり]            逆アセンブル *)

open Zmaml

let slurp f =
  let ic = open_in_bin f in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let () =
  let file = ref "" and dis = ref false and org = ref None and out = ref "" in
  let data = ref [] and defs = ref [] and incs = ref [] in
  let rec args = function
    | [] -> ()
    | "--dis" :: r -> dis := true; args r
    | "--org" :: v :: r -> org := Some (int_of_string v); args r
    | "-o" :: v :: r -> out := v; args r
    | "--db" :: v :: r -> Scanf.sscanf v "%i-%i" (fun a b -> data := (a, b) :: !data); args r
    | a :: r when String.starts_with ~prefix:"-D" a ->
      Scanf.sscanf (String.sub a 2 (String.length a - 2)) "%[^=]=%i" (fun k v -> defs := (k, v) :: !defs); args r
    | a :: r when String.starts_with ~prefix:"-I" a -> incs := String.sub a 2 (String.length a - 2) :: !incs; args r
    | a :: r -> file := a; args r
  in
  args (List.tl (Array.to_list Sys.argv));
  if !file = "" then (prerr_endline "使い方: zmaml file.zma [-o out.bin] | zmaml file.bin --dis"; exit 2);
  if !dis then (print_string (Dis.zma ~org:(Option.value !org ~default:0) ~data:!data (slurp !file)); exit 0);
  let dirs = Filename.dirname !file :: List.rev !incs in
  let rd f =
    match List.find_opt Sys.file_exists (List.map (fun d -> Filename.concat d f) dirs) with
    | Some p -> slurp p
    | None -> raise (Asm.Error ("ファイルがない: " ^ f))
  in
  match Zma.assemble ?org:!org ~read:rd ~defs:!defs (slurp !file) with
  | exception Asm.Error m -> Printf.eprintf "%s: %s\n" !file m; exit 1
  | org, bin, _ ->
    let o = if !out = "" then Filename.remove_extension !file ^ ".bin" else !out in
    let oc = open_out_bin o in
    output_string oc bin;
    close_out oc;
    Printf.printf "%s: %d バイト（番地 0x%x から）\n" o (String.length bin) org

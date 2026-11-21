# zmaml

OCaml で書いた Z80 / R800 クロスアセンブラ。HRA! さんの [ZMA](https://github.com/hra1129/zma) の書き方を読む。
[hlang](../hlang) から、アセンブラの部分だけを取り出したもの。

```sh
dune build
dune test
dune exec zmaml -- file.zma [--org 0x4000] [-DMODE=1] [-Iinc] [-o out.bin]
dune exec zmaml -- file.bin --dis [--org N] [--db 0x120-0x140]
```

- `src/zma.ml` — 展開（MACRO / REPEAT / IF / SCOPE / INCLUDE）と 1 行の読み込み
- `src/asm.ml` — 命令の機械語への組み立て
- `src/dis.ml` — 逆アセンブル（ZMA の書き方で出す）
- `docs/zma.md` — 読めるもの、ZMA との違い、まだ読まないもの

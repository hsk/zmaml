# zmaml

C++ の OOP で垂直分割して書かれた Z80 のマクロアセンブラ [ZMA](https://github.com/hra1129/zma)（HRA! さん作）を、
OCaml で水平分割して関数的に書き直しました。ZMA と互換性がある記法（MACRO / REPEAT / IF / SCOPE / INCLUDE など）を読み込み、Z80 / R800 の機械語を出力します。

- 垂直分割: 命令ごと・機能ごとにクラスを作り、それぞれが解析も出力も抱える
- 水平分割: 処理の段階ごとに分ける（展開 → 1 行の読み込み → 組み立て）。各段階は値を受け取って値を返す関数の集まり

## ビルド方法

```sh
dune build
dune test
dune exec zmaml -- file.zma [--org 0x4000] [-DMODE=1] [-Iinc] [-o out.bin]
dune exec zmaml -- file.bin --dis [--org N] [--db 0x120-0x140]
```

## ソースファイル一覧

- `src/zma.ml` — 命令の組み立て、展開（MACRO / REPEAT / IF / SCOPE / INCLUDE）、1 行の読み込み
- `src/dis.ml` — 逆アセンブル（ZMA の書き方で出す）
- `docs/zma.md` — 読めるもの、ZMA との違い、まだ読まないもの

## ライセンス

MIT

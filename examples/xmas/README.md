# xmas — SCC で鳴らすクリスマスソング

アドベントカレンダー用の例です。Konami SCC 音源で **Jingle Bells** と **We Wish You a Merry Christmas**（どちらも伝承曲）を、ハンドベルのような音で繰り返し鳴らし、SCREEN 1 でツリーの飾りを拍に合わせて光らせ、スプライトの雪を降らせます。

C-BIOS だけで動きます（使う BIOS 呼び出しは CHGMOD / CHGCLR / WRTVDP / LDIRVM / FILVRM / WRTVRM / SETWRT / RSLREG / ENASLT だけ）。

```sh
dune build
examples/xmas/run.sh      # 組み立てて、openMSX（C-BIOS）で起動する
```

`run.sh` は、openMSX の音声バッファ（`samples`）を標準の 1024 にして起動します（設定ファイルは別のものを使うので、ふだんの設定は書き換えません）。`samples` が極端に小さいと（67 など）、音が途切れて「ぶるるる」というノイズが出ます。

手で動かすなら:

```sh
_build/default/bin/zmaml.exe examples/xmas/xmas.zma -o xmas.bin
# xmas.bin（番地 0x4000 から）を 32KB に 0xFF で埋めて xmas.rom にする
openmsx -machine C-BIOS_MSX2+_JP -carta xmas.rom -romtype KonamiSCC
```

## 操作

- **スペース**: 音色を切り替える（HANDBELL → ORGAN → FLUTE → MUSIC BOX → CHIPTUNE）。今の音色は画面の「SOUND」の右に出る。
- **1〜4**: そのチャンネルのミュートを切り替える。状態は地面に `1:on` / `1:off` で出る。1 = メロディ、2 = アルペジオ、3 = ベース、4 = 1 オクターブ上。ノイズがどのチャンネルから出ているかを切り分けるのに使える。

## しくみ

- **ROM**: 32KB の Konami SCC マッパー。先頭に `AB` のヘッダを置き、`start` から始まる。起動したら RSLREG / ENASLT でページ 2 も同じスロットにし、`0x9000` に `0x3F` を書いて `0x9800-` を SCC のレジスタにする。
- **音**: 4 チャンネルを使う。メロディ、和音のアルペジオ、ベース、メロディの 1 オクターブ上。波形は正弦波に倍音を少し足したもので、鳴らしたあとに音量を少しずつ減らして、ハンドベルの「ポーン」という響きにする。曲は（高さ, 長さ）の列で、`nt E4, T4` のようにマクロで書く。1 フレームごとに HALT で垂直帰線を待ち、各トラックを進める。音符の区切りで音量を 0 に落とすと「プツッ」というノイズが出るので、区切らず、新しい音で音量が戻ることで音の頭を出す。周波数は `3579545 / (32 x Hz) - 1`。
- **絵**: SCREEN 1。8 文字ごとの色を変えて飾りを光らせ、雪は 8x8 スプライト 12 個を 1 フレームごとに落として、左右に揺らす。

60Hz の機械向けです。50Hz では、少しゆっくりになります。

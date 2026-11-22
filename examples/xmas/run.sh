#!/bin/sh
# xmas.zma を組み立てて、C-BIOS の openMSX で鳴らす
set -e
cd "$(dirname "$0")"
dune build --root ../.. 2>/dev/null || true
ZMAML=../../_build/default/bin/zmaml.exe
$ZMAML xmas.zma -o xmas.bin
# 32KB（8KB x 4 バンク）にそろえて ROM にする
python3 - <<'PY'
d = open("xmas.bin", "rb").read()
open("xmas.rom", "wb").write(d + b"\xff" * (32768 - len(d)))
PY
# 音声バッファ（samples）が小さすぎると、音が途切れて「ぶるるる」というノイズになる。
# ふだんの設定ファイルは書き換えずに、この例だけ標準の 1024 で動かす。
SETTINGS="${TMPDIR:-/tmp}/xmas-openmsx-settings.xml"
[ -f "$SETTINGS" ] || cat > "$SETTINGS" <<'XML'
<!DOCTYPE settings SYSTEM 'settings.dtd'>
<settings>
  <settings/>
  <bindings/>
  <shortcuts/>
</settings>
XML
exec openmsx -setting "$SETTINGS" -command "set samples 1024" \
  -machine C-BIOS_MSX2+_JP -carta xmas.rom -romtype KonamiSCC

#!/bin/bash
# ビルドスクリプト

cd /workspace/oreore_exchange

# メインプログラムのビルド
alr build

# クライアントプログラムのビルド
gprbuild -p -P oreore_exchange.gpr src/client/client_main.adb -o bin/client

echo "ビルド完了！"
echo "サーバーとビルトインクライアント起動: ./bin/oreore_exchange"
echo "追加クライアント起動: ./bin/client --nick <ニックネーム>"

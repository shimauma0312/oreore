#!/bin/bash

# プロジェクトが初期化されていない場合のみ実行
if [ ! -f alire.toml ]; then
    echo "オレオレ証券取引所プロジェクトを初期化しています..."
    
    # Alireプロジェクトの初期化
    alr init --bin oreore_exchange
    
    # 必要な依存関係を追加
    cd oreore_exchange
    alr with gnat_native
    
    # ファイル構造をセットアップ
    mkdir -p src/client src/server src/common
    
    echo "プロジェクト初期化完了！"
else
    echo "プロジェクトはすでに初期化されています。"
fi

# オレオレ証券取引所

リアルタイム証券取引所シミュレータ - Ada タスク通信のデモ

### 環境構築手順

```bash
# 1. Dockerイメージをビルド
docker compose build

# 2. 開発用シェルを起動
docker compose run --rm exchange bash

# --- 以下、コンテナ内 ---
# 3. ビルドスクリプトを実行
chmod +x /workspace/build.sh
/workspace/build.sh

# 4. プログラム実行
./bin/oreore_exchange     # サーバー + 管理者クライアント起動
```

### 追加クライアントの起動

別のターミナルで以下のコマンドを実行して、追加のクライアントを起動できます：

```bash
# 新しいターミナルでクライアント追加
docker compose exec exchange ./bin/client --nick Alice
```

## 使い方

1. サーバーとデフォルトクライアントが起動すると、取引画面が表示されます
2. 画面の指示に従って注文を入力できます：
   - 1: 買い指値
   - 2: 買い成行
   - 3: 売り指値
   - 4: 売り成行
   - 0: 終了

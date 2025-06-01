FROM ubuntu:22.04

# タイムゾーンの設定を非対話的に行う
ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Asia/Tokyo

# 必要なパッケージのインストール
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    gpg \
    wget \
    unzip \
    libncursesw5-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Alire (Ada パッケージマネージャ) のインストール - GitHubから直接ダウンロード
WORKDIR /tmp
RUN wget -O alire.zip https://github.com/alire-project/alire/releases/download/v1.2.2/alr-1.2.2-bin-x86_64-linux.zip \
    && unzip alire.zip \
    && cp -f bin/alr /usr/local/bin/ \
    && chmod +x /usr/local/bin/alr \
    && rm -rf bin alire.zip

# GNAT Community Edition のインストール
WORKDIR /tmp
RUN wget -O gnat-linux.tar.gz https://github.com/alire-project/GNAT-FSF-builds/releases/download/gnat-13.2.0-1/gnat-x86_64-linux-13.2.0-1.tar.gz \
    && mkdir -p gnat-extract \
    && tar xf gnat-linux.tar.gz -C gnat-extract --strip-components=1 \
    && mv gnat-extract /opt/gnat \
    && rm gnat-linux.tar.gz

# GNAT をパスに追加
ENV PATH="/opt/gnat/bin:${PATH}"

# 作業ディレクトリの設定
WORKDIR /workspace

# Alire プロジェクト初期化スクリプト
COPY init_project.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/init_project.sh

# エントリーポイント
CMD ["/bin/bash"]


# ============================================
# 1. 基本パッケージ
# ============================================
log_info "基本パッケージをインストール中..."

sudo apt update && sudo apt upgrade -y

sudo apt install -y \
    build-essential \
    curl \
    wget \
    git \
    unzip \
    zip \
    tree \
    htop \
    make \
    libssl-dev \
    zlib1g-dev \
    libbz2-dev \
    libreadline-dev \
    libsqlite3-dev \
    llvm \
    libncurses5-dev \
    libncursesw5-dev \
    xz-utils \
    tk-dev \
    libffi-dev \
    liblzma-dev \
    software-properties-common \
    apt-transport-https \
    ca-certificates \
    gnupg \
    lsb-release \
    rlwrap

log_success "基本パッケージ完了"

# CYAN="0;36"
# START_COLOR="\[\e[${CYAN}m\]"
# END_COLOR="\[\e[0m\]"
# export PS1="[\h\w]\n[\u@${START_COLOR}syo-kudo${END_COLOR} \t]\$ "

include() {
    if [[ -f $1 ]]; then
        . "$1"
        return 0
    else
        dump YELLOW "(include) Not found: $1"
        return 1
    fi
}

# git管理の.bashrcをインクルードしてエイリアス等を設定する
include "/var/www/html/.bashrc"

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# nodeが使用するメモリ量を指定
export NODE_OPTIONS="--max-old-space-size=31448"

alias ls='ls -F --color=auto'
alias sl='ls'

export PATH=${HOME}/.local/bin:${PATH}

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH=$BUN_INSTALL/bin:$PATH

# pyenv
#export PYENV_ROOT="$HOME/.pyenv"
#[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
#eval "$(pyenv init -)"

# /var/www/htmlにcd
alias cdh='cd /var/www/html'

# rapid-yarn
alias ry="/usr/bin/python3 ~/rapid-yarn/rapid_yarn.py"

eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

# gitstatus
source ~/gitstatus/gitstatus.prompt.sh

# Enable bash completion
if [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi

alias log="git log --decorate --format=format:'%C(bold cyan)%as%C(reset) %C(bold blue)%h%C(reset) %C(white bold)%<(14,trunc)%an%C(reset) %C(green)%<(50,trunc)%s%C(reset) '"

export CDPATH="/var/www:/var/www/html/generator:${HOME}/Documents"

export NPM_TOKEN="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"

# git pull origin <remotebranch>
alias gpull='git pull origin $(git branch --show-current)'

# Git Aliases
alias gl='git log --simplify-by-decoration --oneline --decorate'
alias gs='git status -s -b'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gpl='gpull'

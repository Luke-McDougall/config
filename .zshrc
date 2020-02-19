autoload -U colors && colors
PS1="%{$fg[yellow]%}%~ % %{$reset_color%}> "
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

autoload -U compinit
zstyle ':completion:*' menu select 
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
setopt MENU_COMPLETE
zmodload zsh/complist
compinit
_comp_options+=(globdots)

alias rel='cargo run --release'
alias ls='exa  --sort=extension --group-directories-first --color=always'
alias c=clear
alias vim=nvim
alias build=./build
alias run=./run
alias f='nvim $(fzf)'
alias cf=". cf"
alias tis="zathura /home/luke/.local/share/Steam/steamapps/common/TIS-100/TIS-100\ Reference\ Manual.pdf &"
alias 4="~/4coder/./4ed &"
alias g="~/scripts/./games.sh"
alias gc="git clone https://github.com/Luke-McDougall/"

function i3c {
    vim ~/.config/i3/config
}

function mpdf {
    gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=$3 $1 $2
}

export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

export PATH="$PATH:/home/luke/scripts:/home/luke/.cargo/bin"
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border"
export FZF_DEFAULT_COMMAND="rg --files"
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

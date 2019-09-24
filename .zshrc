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

alias ls='ls --color=always'
alias c=clear
alias vim=nvim
alias f='nvim $(fzf)'
alias cf=". cf"
alias tis="zathura /home/luke/.local/share/Steam/steamapps/common/TIS-100/TIS-100\ Reference\ Manual.pdf &"
alias 4="~/4coder/./4ed &"

function tt {
    date | time_table $1
}

function mpdf {
    gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=$3 $1 $2
}

export PATH="$PATH:/home/luke/scripts:/home/luke/.cargo/bin"
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border"
export FZF_DEFAULT_COMMAND="rg --files"
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

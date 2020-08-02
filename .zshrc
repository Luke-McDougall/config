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

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char

function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}

zle -N zle-keymap-select

alias l='exa  --sort=extension --group-directories-first --color=always'
alias lt='exa  --sort=extension --group-directories-first --color=always -T'
alias c=clear
alias calc='python -i ~/python/calculator/calc.py'
alias vim=nvim
alias tis="mupdf /home/luke/.local/share/Steam/steamapps/common/TIS-100/TIS-100\ Reference\ Manual.pdf &"
alias g="~/scripts/games.sh"
alias pi="pacman -Slq | fzf -m --preview 'cat <(pacman -Si {1}) <(pacman -Fl {1} | awk \"{print \$2}\")' | xargs -ro sudo pacman -S"

function mpdf {
    gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=$3 $1 $2
}

function f {
    FILE=`fzf --prompt='Edit file: ' --preview 'head -n $LINES {}'`
    [ -f "$FILE" ] && nvim "$FILE"
}

# Video explaining the weird syntax incase I forget it.
# https://www.youtube.com/watch?v=QXineadwG4E
function cf {
    cd $HOME
    SUFFIX=`fzf --prompt='Change to dir containing: '`
    
    # Exit code of 130 means I didn't choose anything and therefore want to stay in my current directory
    if [ $? -eq 130 ]; then 
        cd - > /dev/null
        return
    fi

    # I need this intermediate variable so that switching to $HOME works
    # If you forget why this is necessary dude just trust me
    DIR="$HOME/$SUFFIX"
    cd ${DIR%/*}
}

export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

export PATH="/home/luke/.cargo/bin:/home/luke/.cask/bin:$PATH"
export TERMINAL="alacritty"
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border"
export FZF_DEFAULT_COMMAND="rg --files"
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

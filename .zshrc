autoload -U colors && colors
PS1="%{$fg[magenta]%}%~ % %{$reset_color%}$ "
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/cache/zsh/history

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
alias cf=". cf"
export PATH="$PATH:/home/luke/scripts:/home/luke/.cargo/bin"
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border"
export FZF_DEFAULT_COMMAND="rg --files"
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

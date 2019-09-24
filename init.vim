set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

" Plugin Zone
call plug#begin('~/.local/share/nvim/plugged')

" Cool status bar 
Plug 'itchyny/lightline.vim'

" Ctags my guy
Plug 'majutsushi/tagbar'

" Rust plugin
Plug 'rust-lang/rust.vim'

" Surround verb plugin. 
Plug 'https://github.com/tpope/vim-surround'

" Repeat plugin to repeat more stuff
Plug 'https://github.com/tpope/vim-repeat'

" Indent guides for being not confused
Plug 'Yggdroot/indentLine'

" fzf plugin
Plug '/usr/bin/fzf'

call plug#end()

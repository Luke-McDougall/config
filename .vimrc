let mapleader = "\<Space>"
syntax on
set tabstop=4
set softtabstop=4
set expandtab
set smarttab
set autoindent
set smartindent
set number relativenumber
set wildmenu
set nocompatible
set splitright
set splitbelow
set scrolloff=2
set guioptions-=T
set backspace=2 " Backspace over newlines
set lazyredraw
filetype plugin on
nnoremap ,s :setlocal spell! spelllang=en_au<CR>
set shiftwidth=4
:inoremap ( ()<Esc>i
:inoremap " ""<Esc>i
:inoremap [ []<Esc>i
:inoremap <C-j> <Esc>/[)}"'\]>]<CR>:nohl<CR>a
:inoremap {<CR> {<CR><BS>}<Esc>ko
:inoremap {} {}<Esc>i
:inoremap {; {};<Esc>hi<CR><Esc>O
" Sane move to end/start of line keys
map H ^
map L $
" Normal mode enter
nnoremap <CR> i<CR><Esc>
" Disable arrow keys
nnoremap <up> <nop>
nnoremap <down> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
" Left and right can switch buffers
nnoremap <left> :bp<CR>
nnoremap <right> :bn<CR>
" Center search result
nnoremap n nzz
" Vertical movement in long lines should work correctly now
nnoremap j gj
nnoremap k gk
" This doesn't work LOL
nnoremap <M-j> <C-d>
nnoremap <M-k> <C-u>
" Better split navigation keys
nnoremap <leader>h <C-w>h
nnoremap <leader>l <C-w>l
nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
" <leader><leader> toggles between buffers
nnoremap <leader><leader> <c-^>
" Set rustc as compiler
set makeprg=rustc
" Set commands for next and previous error functions
nnoremap <C-c> :cnext<CR>
nnoremap <C-x> :cprevious<CR>
" Compile current rust file with rustc and display any errors in quickfix
" window
nnoremap <F6> :make<CR>
" Run exe of current rust file
nnoremap <F5> :!./%:r<CR>
" Test current rust project to see if it will build using cargo
nnoremap <F7> :!cargo test<CR>
" Build and run current rust project using cargo
nnoremap <C-F7> :!cargo run<CR>
" Ctags thing button 
nnoremap <F8> :TagbarToggle<CR>
" Vertical split shortcut to open current file in both windows.
nnoremap <Leader>v :vsp %<CR>
" Quick save and quick savequit
nnoremap <leader>w :w<CR>
nnoremap <leader>q :wq<CR>
fu! OpenGroff()
    :w
    :! groff -e -t -ms % -T pdf > %:r.pdf
    :! zathura %:r.pdf & 
endfunction

fu! UpdateGroff()
    :w
    :! groff -e -t -ms % -T pdf > %:r.pdf
endfunction

nnoremap <Leader>o :call OpenGroff()<CR><CR>
nnoremap <Leader>u :call UpdateGroff()<CR><CR>

" Markdown syntax settings 
let g:markdown_fenced_languages = ['c']
let g:markdown_syntax_conceal = 0

let g:lightline = {
            \ 'colorscheme':'seoul256'
            \}

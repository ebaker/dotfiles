call pathogen#infect()

set encoding=utf-8
set ruler

"set number
syntax enable
filetype indent on
filetype plugin on

" when searching, search as I type, highlight matches, and only care about case
" if I have caps in my search phrase.
set hlsearch
set incsearch
set ignorecase
set smartcase

" tabstop shiftwidth expandtab for files
autocmd FileType html
 \ setlocal shiftwidth=2 |
 \ setlocal tabstop=2
 \ set expandtab
autocmd FileType coffee
 \ setlocal shiftwidth=2 |
 \ setlocal tabstop=2
 \ set expandtab
autocmd FileType js
 \ setlocal shiftwidth=2 |
 \ setlocal tabstop=2
 \ set expandtab
autocmd FileType jsx
 \ setlocal shiftwidth=2 |
 \ setlocal tabstop=2
 \ set expandtab
autocmd FileType styl
 \ setlocal shiftwidth=2 |
 \ setlocal tabstop=2
 \ set expandtab
autocmd FileType py
 \ setlocal shiftwidth=2 |
 \ setlocal tabstop=2
 \ set expandtab


" desert is a pretty good color scheme
colorscheme default

" tab navigating
map <F5> gT
map <F6> gt

map <Esc>1 1gt
map <Esc>2 2gt
map <Esc>3 3gt
map <Esc>4 4gt
map <Esc>5 5gt
map <Esc>6 6gt
map <Esc>7 7gt
map <Esc>8 8gt
map <Esc>9 9gt
map <Esc>0 10gt

imap <F5> <Esc>gTi
imap <F6> <Esc>gti

" i'm compulsive about saving
map <C-W> :w<CR>
imap <C-W> <C-O>:w<CR>

" i don't really know why i have this or if it's necessary
autocmd BufRead *.py set cinwords=if,elif,else,for,while,try,except,finally,def,class
autocmd BufRead *.cgi set cinwords=if,elif,else,for,while,try,except,finally,def,class

" javascript
let g:javascript_conceal_function   = "ƒ"
autocmd FileType javascript,css imap ;; <Esc>A;<Esc>o
autocmd FileType javascript,css map ;; A;<Esc>

" indent guides
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
" let g:indent_guides_auto_colors = 0
" autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=black

" cson files are coffee
:au BufNewFile,BufRead *.cson set filetype=coffee

" highlight everything over 80 chars in red
:au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)

" F2 makes pasting the clipboard not fuck up the indentation, F2 again turns it off.
" useful for pasting large blocks.
nnoremap <F2> :set invpaste paste?<CR>
imap <F2> <C-O><F2>
set pastetoggle=<F2>

" Up/down go visually instead of by physical lines (useful for long wraps)
" Interactive ones need to check whether we're in the autocomplete popup (which
" breaks if we remap to gk/gj)
map <up> gk
inoremap <up> <C-R>=pumvisible() ? "\<lt>up>" : "\<lt>C-o>gk"<Enter>
map <down> gj
inoremap <down> <C-R>=pumvisible() ? "\<lt>down>" : "\<lt>C-o>gj"<Enter>

" Map normal mode Enter to add a new line.
" Useful for adding spacing to a file while navigating.
nmap <Enter> o<Esc>

" vim -p *.py will open 100 files in tabs
" it's not really useful to have that many tabs, but it's better than having
" most of them not open in tabs at all
set tabpagemax=100

if has("autocmd")
aug vimrc
au!
" restore cursor position when the file has been read
au BufReadPost *
\ if line("'\"") > 0 && line("'\"") <= line("$") |
\ exe "norm g`\"" |
\ endif
aug ENG
endif

" markdown folding
let g:vim_markdown_folding_disabled=1

" templates
let g:templates_plugin_loaded = 0
let g:templates_no_autocmd = 0

" tmux mode allows cursor change in
if exists('$TMUX')
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" disable netrwhist
let g:netrw_dirhistmax=0

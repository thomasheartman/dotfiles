if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs htps://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
call plug#begin()
Plug 'airblade/vim-gitgutter'
" Plug 'benjifisher/matchit.zip'
Plug 'blueyed/vim-diminactive'
Plug 'bps/vim-textobj-python'
Plug 'bronson/vim-visual-star-search'
Plug 'christoomey/vim-tmux-navigator'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'davidhalter/jedi-vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'glts/vim-textobj-comment'
Plug 'honza/vim-snippets'
Plug 'jiangmiao/auto-pairs'
Plug 'joshdick/onedark.vim'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-function'
Plug 'kana/vim-textobj-user'
Plug 'kien/rainbow_parentheses.vim'
Plug 'majutsushi/tagbar'
Plug 'mileszs/ack.vim'
Plug 'mtth/cursorcross.vim'
" Plug 'myint/syntastic-extras'
" Plug 'OmniSharp/omnisharp-vim'
Plug 'rust-lang/rust.vim'
Plug 'schickling/vim-bufonly'
Plug 'scrooloose/nerdcommenter'
" Plug 'scrooloose/syntastic'
" Plug 'SirVer/ultisnips'
Plug 'terryma/vim-multiple-cursors'
Plug 'thinca/vim-textobj-function-javascript'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'tpope/tpope-vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
" Plug 'valloric/youcompleteme'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Plug 'vim-scripts/a.vim' hello
Plug 'wincent/command-t'
" Plug 'xolox/vim-easytags'
Plug 'xolox/vim-misc'
Plug 'easymotion/vim-easymotion'

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
call plug#end()

"additional packages
" packadd! matchit

" airline customisation
let g:airline_theme='distinguished'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" diminactive conf
let g:diminactive_enable_focus = 1

" disable extra cursoscross mappings
let g:cursorcross_mappings = 0

" use the silver searcher with Ack.vim
" let g:ackprg = 'ag --nogroup --nocolor --column'
" optional: let g:ackprg = 'ag --vimgrep'

syntax on " enable syntax highlighting
" colorscheme onedark " use onedark atom theme https://github.com/joshdick/onedark.vim
set backspace=indent,eol,start
set nocompatible " fixes some weird bugs in insert mode
set number " show line numbers
set incsearch " search as characters are entered
set hlsearch " highlight matches
set nrformats=
set history=200
set pastetoggle=<f5>
set autoread " set autoread to autoread files when they are changed on disk
set hidden " set hidden to allow switching from buffers with unsaved changes
set expandtab
set tabstop=4
set shiftwidth=4

hi Normal ctermbg=none
highlight NonText ctermbg=none

" remap the leader key
let mapleader=" "

" activate filetype plugins, allowing for language specific configs
filetype plugin on

" change directory to current file
command CD cd %:p:h
" change local dir to current file
command LCD lcd %:p:h

cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" " Remapping for line wrapping
" noremap  <buffer> <silent> k gk
" noremap  <buffer> <silent> j gj

" " to make dj still delete two lines
" onoremap <silent> j gj
" onoremap <silent> k gk

" Remapping of & to && for more consistent substitute output on repeats
nnoremap & :&&<CR>
xnoremap & :&&<CR>

" Disable cursorline in inactive windows
augroup CursorLineOnlyInActiveWindow
	autocmd!
	autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
	autocmd WinLeave * setlocal nocursorline
augroup END

" " syntastic config
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
" set signcolumn=yes

" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0
" let g:syntastic_aggregate_errors = 1
" let g:syntastic_auto_jump = 3 " jump tp errors if found in file

" " syntastic airline integration
" let g:airline#extensions#syntastic#stl_format_err = 1
" let g:airline#extensions#syntastic#stl_format_warn = 1

" let g:syntastic_error_symbol = '✗✗'
" let g:syntastic_style_error_symbol = '✠✠'
" let g:syntastic_warning_symbol = '∆∆'
" let g:syntastic_style_warning_symbol = '≈≈'
" " for using YouCompleteMe with Syntastic, but using
" " Syntastic's checkers for C, C++, Objective-C and Objective-C++
" " (c, cpp, objc, objcpp)
" " let g:ycm_show_diagnostics_ui = 0

" syntastic-extras
" let g:syntastic_gitcommit_checkers = ['language_check']


"" Omnisharp
"let g:OmniSharp_selector_ui = 'ctrlp'
"" let g:syntastic_cs_checkers = ['code_checker']
"" from docs: don't autoselect first item in omnicomplete, show if
"" only one item (for preview)
"" remove preview if you don't want to see any documentation
"set completeopt=longest,menuone ",preview
"augroup omnisharp_commands
"	autocmd!

"	" Builds can also run asynchronously with vim-dispatch installed
"	autocmd FileType cs nnoremap <leader>b :wa!<cr>:OmniSharpBuildAsync<cr>
"	" automatic syntax check on events (TextChanged requires Vim 7.4)
"	" autocmd BufEnter,TextChanged,InsertLeave *.cs SyntasticCheck

"	" Automatically add new cs files to the nearest project on save
"	autocmd BufWritePost *.cs call OmniSharp#AddToProject()

"	"show type information automatically when the cursor stops moving
"	autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()

"	"The following commands are contextual, based on the current cursor position.

"	autocmd FileType cs nnoremap gd :OmniSharpGotoDefinition<cr>
"	autocmd FileType cs nnoremap <leader>fi :OmniSharpFindImplementations<cr>
"	autocmd FileType cs nnoremap <leader>ft :OmniSharpFindType<cr>
"	autocmd FileType cs nnoremap <leader>fs :OmniSharpFindSymbol<cr>
"	autocmd FileType cs nnoremap <leader>fu :OmniSharpFindUsages<cr>
"	"finds members in the current buffer
"	autocmd FileType cs nnoremap <leader>fm :OmniSharpFindMembers<cr>
"	" cursor can be anywhere on the line containing an issue
"	autocmd FileType cs nnoremap <leader>x  :OmniSharpFixIssue<cr>
"	autocmd FileType cs nnoremap <leader>fx :OmniSharpFixUsings<cr>
"	autocmd FileType cs nnoremap <leader>tt :OmniSharpTypeLookup<cr>
"	autocmd FileType cs nnoremap <leader>dc :OmniSharpDocumentation<cr>
"	"navigate up by method/property/field
"	autocmd FileType cs nnoremap <C-K> :OmniSharpNavigateUp<cr>
"	"navigate down by method/property/field
"	autocmd FileType cs nnoremap <C-J> :OmniSharpNavigateDown<cr>
"    " set local omnifunc to C#
"    autocmd FileType cs setlocal omnifunc=OmniSharp#Complete

"augroup END

" this setting controls how long to wait (in ms) before fetching type / symbol information.
" set updatetime=500
" Remove 'Press Enter to continue' message when type information is longer than one line.
" set cmdheight=2

" " Contextual code actions (requires CtrlP or unite.vim)
" nnoremap <leader><space> :OmniSharpGetCodeActions<cr>
" " Run code actions with text selected in visual mode to extract method
" vnoremap <leader><space> :call OmniSharp#GetCodeActions('visual')<cr>

" " rename with dialog
" nnoremap <leader>nm :OmniSharpRename<cr>
" nnoremap <F2> :OmniSharpRename<cr>
" " rename without dialog - with cursor on the symbol to rename... ':Rename newname'
" command! -nargs=1 Rename :call OmniSharp#RenameTo("<args>")

" " Force OmniSharp to reload the solution. Useful when switching branches etc.
" nnoremap <leader>rl :OmniSharpReloadSolution<cr>
" nnoremap <leader>cf :OmniSharpCodeFormat<cr>
" " Load the current .cs file to the nearest project
" nnoremap <leader>tp :OmniSharpAddToProject<cr>

" " (Experimental - uses vim-dispatch or vimproc plugin) - Start the omnisharp server for the current solution
" nnoremap <leader>ss :OmniSharpStartServer<cr>
" nnoremap <leader>sp :OmniSharpStopServer<cr>

" " Add syntax highlighting for types and interfaces
" nnoremap <leader>th :OmniSharpHighlightTypes<cr>

" " Enable snippet completion, requires completeopt-=preview
" let g:OmniSharp_want_snippet = 1
" let g:OmniSharp_server_type = 'roslyn'



" tagbar
" nmap <F8> :TagbarToggle<CR>

" you complete me
" let g:ycm_collect_identifiers_from_tags_files = 1
" let g:ycm_min_num_of_chars_for_completion = 1

set encoding=utf-8

" tagbar config
" let g:tagbar_ctags_bin = 'opt/local/bin/ctags'

" don't move cursor back one space when exiting insert mode
" autocmd InsertEnter * let CursorColumnI = col('.')
" autocmd CursorMovedI * let CursorColumnI = col('.')
" autocmd InsertLeave * if col('.') != CursorColumnI | call cursor(0, col('.')+1) | endif

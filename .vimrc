set nocompatible               " be iMproved
filetype off                   " required for vundle

set rtp+=~/.vim/bundle/vundle
call vundle#begin()

" let Vundle manage Vundle
Plugin 'gmarik/vundle'

Plugin 'jiangmiao/auto-pairs'
Plugin 'kien/ctrlp.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-commentary'
Plugin 'vim-ruby/vim-ruby'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-bundler'
Plugin 'tpope/vim-rake'
" required by vim-snipmate
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'garbas/vim-snipmate'
Plugin 'honza/vim-snippets'
Plugin 'scrooloose/nerdtree'

" original repos on github
" Bundle 'tpope/vim-fugitive'
" vim-scripts repos
" Bundle 'L9'
" non github repos
" Bundle 'git://git.wincent.com/command-t.git'
" Bundle 'file:///Users/gmarik/path/to/plugin'

call vundle#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" BASIC EDITING CONFIGURATION
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype plugin indent on         " Turn on file type detection.
syntax enable                     " Turn on syntax highlighting.

runtime macros/matchit.vim        " Load the matchit plugin.

set showcmd                       " Display incomplete commands.
set showmode                      " Display the mode you're in.

set backspace=indent,eol,start    " Intuitive backspacing.

set hidden                        " Handle multiple buffers better.

set history=10000                 " remember more commands and search history

set wildmenu                      " Enhanced command line completion.
set wildmode=list:longest         " Complete files like a shell.

set ignorecase                    " Case-insensitive searching.
set smartcase                     " But case-sensitive if expression contains a capital letter.

set number                        " Show line numbers.
set ruler                         " Show cursor position.

set incsearch                     " Highlight matches as you type.
set hlsearch                      " Highlight matches.

set wrap                          " Turn on line wrapping.
set scrolloff=3                   " Show 3 lines of context around the cursor.

set title                         " Set the terminal's title

set visualbell                    " No beeping.

set nobackup                      " Don't make a backup before overwriting a file.
set nowritebackup                 " And again.
set directory=$HOME/.vim/tmp//,.  " Keep swap files in one location

set tabstop=2                    " Global tab width.
set shiftwidth=2                 " And again, related.
set expandtab                    " Use spaces instead of tabs

set autoindent

set laststatus=2                  " Show the status line all the time

set exrc                          " enable per-directory .vimrc files
set secure                        " disable unsafe commands in local .vimrc files

" Useful status information at bottom of screen
set statusline=[%n]\ %<%.99f\ %h%w%m%r%y\ %{exists('*CapsLockStatusline')?CapsLockStatusline():''}%=%-16(\ %l,%c-%v\ %)%P

" let vim download dictionary files when needed
let g:spellfile_URL = 'http://ftp.vim.org/vim/runtime/spell'

" map :W to :w
command! W :w

set winwidth=85

" Always show tab bar
set showtabline=2
" show whitespace
set list listchars=tab:\ \ ,trail:·"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ARROW KEYS ARE UNACCEPTABLE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
noremap <Left> <Nop>
noremap <Right> <Nop>
noremap <Up> <Nop>
noremap <Down> <Nop>

" Or use vividchalk
" colorscheme vividchalk
" colorscheme macvim
" set background=light
" :set background=dark
" :color grb256

let mapleader = ","             " Use , as leader

" Tab mappings.
noremap <leader>tt :tabnew<cr>
noremap <leader>te :tabedit
noremap <leader>tc :tabclose<cr>
noremap <leader>to :tabonly<cr>
noremap <leader>tn :tabnext<cr>
noremap <leader>tp :tabprevious<cr>
noremap <leader>tf :tabfirst<cr>
noremap <leader>tl :tablast<cr>
noremap <leader>tm :tabmove
noremap <f7> :tabprevious<cr>
noremap <f8> :tabnext<cr>

nnoremap <Left> :bprevious<CR>
nnoremap <Right> :bnext<CR>

nnoremap <cr> :nohlsearch<cr>

" reindent whole file and leave cursor where it was
nnoremap <leader>i mmgg=G`m

" remapping unimpaired.vim exchange commands to new shortcuts
nnoremap <C-k> [e
nnoremap <C-j> ]e

vnoremap <C-k> [egv
vnoremap <C-j> ]egv

" Map <c-p> to to º in insert mode
inoremap º <c-p>

" Jamis Buck's file opening plugin
" map <Leader>t :FuzzyFinderTextMate<Enter>
" nmap <C-S-T> <Leader>t

noremap <leader>ev :vs $MYVIMRC<cr>
noremap <leader>ss :vs ~/.vim/bundle/vim-snippets/snippets/<cr>
noremap <leader>p :split project_notes.txt<cr>

" map space to iterate buffer windows
nnoremap <space> <c-w>w

" spelling maps
nnoremap <F5> :setlocal nospell<cr>
nnoremap <F6> :setlocal spell spelllang=
nnoremap <F7> ]s
nnoremap <F8> z=

" associate capistrano files with ruby syntax
au BufRead,BufNewFile *.cap setfiletype ruby

augroup vimrc_acmd
  autocmd!
  " Source the vimrc file after saving it
  autocmd bufwritepost .vimrc source $MYVIMRC
  " Strip trailing whitespace
  autocmd BufWritePre * :%s/\s\+$//e
  " Jump to last cursor position unless it's invalid or in an event handler
  autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal g`\"" |
        \ endif
augroup END

:" Automatic fold settings for specific files. Uncomment to use.
:" autocmd FileType ruby setlocal foldmethod=syntax
:" autocmd FileType css  setlocal foldmethod=indent shiftwidth=2 tabstop=2

augroup md_acmd
  autocmd!
  " associate md extesion to markdown instead of modula2"
  autocmd BufRead,BufNewFile *.md set filetype=markdown
augroup END

augroup pl_acmd
  autocmd!
  " associate pl extesion to prolog instead of perl
  autocmd BufRead,BufNewFile *.pl set filetype=prolog
augroup END
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MULTIPURPOSE TAB KEY
" Indent if we're at the beginning of a line. Else, do completion.
" (from https://github.com/garybernhardt/dotfiles/blob/master/.vimrc)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" function! InsertTabWrapper()
"     let col = col('.') - 1
"     if !col || getline('.')[col - 1] !~ '\k'
"         return "\<tab>"
"       else
"         return "\<c-p>"
"     endif
" endfunction
" inoremap <tab> <c-r>=InsertTabWrapper()<cr>
" inoremap <s-tab> <c-n>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RENAME CURRENT FILE
" (from https://github.com/garybernhardt/dotfiles/blob/master/.vimrc)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', expand('%'), 'file')
  if new_name != '' && new_name != old_name
    exec ':saveas ' . new_name
    exec ':silent !rm ' . old_name
    redraw!
  endif
endfunction
noremap <leader>n :call RenameFile()<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RUNNING TESTS
" (from https://github.com/garybernhardt/dotfiles/blob/master/.vimrc)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

augroup ruby_test
  autocmd!
  autocmd FileType ruby noremap <leader>t :call RunTestFile()<cr>
  autocmd FileType cucumber noremap <leader>t :call RunTestFile()<cr>
  autocmd FileType ruby noremap <leader>T :call RunNearestTest()<cr>
  autocmd FileType ruby noremap <leader>a :call RunTests('')<cr>
  autocmd FileType ruby noremap <leader>c :w\|:!clear && bin/cucumber<cr>
  autocmd FileType ruby noremap <leader>w :w\|:!clear && bin/cucumber --profile wip<cr>
augroup END

function! RunTestFile(...)
  if a:0
    let command_suffix = a:1
  else
    let command_suffix = ""
  endif

  " Run the tests for the previously-marked file.
  let in_test_file = match(expand("%"), '\(_test.rb\|.feature\|_spec.rb\)$') != -1
  if in_test_file
    call SetTestFile()
  elseif !exists("t:actual_test_file")
    return
  end
  call RunTests(t:actual_test_file . command_suffix)
endfunction

function! RunNearestTest()
  let spec_line_number = line('.')
  call RunTestFile(":" . spec_line_number . " -b")
endfunction

function! SetTestFile()
  " Set the spec file that tests will be run for.
  let t:actual_test_file=@%
endfunction

function! RunTests(filename)
  " Write the file and run tests for the given filename
  :w
  :silent !clear
  if match(a:filename, '_test.rb$') != -1
    exec ":!ruby -Itest " . a:filename . " -v"
  elseif match(a:filename, '\.feature$') != -1
    exec ":!bin/cucumber " . a:filename
  else
    exec ":!bin/rspec " . a:filename
  end
endfunction

" Show syntax highlighting groups for word under cursor
nnoremap <leader>h :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

" Leader shortcuts for Rails commands
" https://github.com/ryanb/dotfiles/blob/master/vimrc

" noremap <Leader>m :Rmodel
" noremap <Leader>c :Rcontroller
" noremap <Leader>v :Rview
" noremap <Leader>u :Runittest
" noremap <Leader>f :Rfunctionaltest
" noremap <Leader>rm :RTmodel
" noremap <Leader>rc :RTcontroller
" noremap <Leader>rv :RTview
" noremap <Leader>ru :RTunittest
" noremap <Leader>rf :RTfunctionaltest
" noremap <Leader>sm :RSmodel
" noremap <Leader>sc :RScontroller
" noremap <Leader>sv :RSview
" noremap <Leader>su :RSunittest
" noremap <Leader>sf :RSfunctionaltest

noremap <leader>3 <c-\>s

noremap <Leader>b :NERDTreeToggle<cr>

let run_commands = {
      \'applescript': 'osascript',
      \'bash': 'bash',
      \'javascript': 'node',
      \'nodejs': 'node',
      \'perl': 'perl',
      \'php': 'php',
      \'prolog': 'swipl',
      \'python': 'python',
      \'ruby': 'ruby',
      \'sh': 'sh',
      \'r': 'Rscript',
      \}

augroup run_commands
  autocmd!
  for ft_name in keys(run_commands)
    execute 'autocmd Filetype ' . ft_name . ' nnoremap <buffer> <leader>r '
          \.'<Esc>:w<CR>:!clear<CR>:!' . run_commands[ft_name]
          \.' %<CR>'
  endfor
augroup END

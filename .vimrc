set nocompatible               " be iMproved
filetype off                   " required for vundle

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
Plugin 'VundleVim/Vundle.vim'

" Utility
Plugin 'scrooloose/nerdtree'
Plugin 'kien/ctrlp.vim'
" required by vim-snipmate
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'garbas/vim-snipmate'
Plugin 'honza/vim-snippets'

" Generic Programming
Plugin 'jiangmiao/auto-pairs'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'
Plugin 'godlygeek/tabular'
Plugin 'ervandew/supertab'
Plugin 'craigemery/vim-autotag'

" Git
Plugin 'tpope/vim-fugitive'

" Elixir
Plugin 'elixir-editors/vim-elixir'
Plugin 'slashmili/alchemist.vim'

" Javascript
Plugin 'mustache/vim-mustache-handlebars'

" Ruby
Plugin 'vim-ruby/vim-ruby'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-bundler'
Plugin 'tpope/vim-rake'
Plugin 'tpope/vim-cucumber'

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
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
                                  " ignore thhis files/folders by default

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

" autocomplete commands
autocmd FileType php setl ofu=phpcomplete#CompletePHP
autocmd FileType html,xhtml setl ofu=htmlcomplete#CompleteTags
autocmd FileType c setl ofu=ccomplete#CompleteCpp
autocmd FileType css setl ofu=csscomplete#CompleteCSS
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType ruby,eruby setl ofu=rubycomplete#Complete

set completeopt=preview,menuone

" supertab configuration
let g:SuperTabDefaultCompletionType = "context"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ARROW KEYS ARE UNACCEPTABLE
" use them to resize windows
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <Left>  :vertical resize -2<CR>
nnoremap <Right> :vertical resize +2<CR>
nnoremap <Up>    :resize -2<CR>
nnoremap <Down>  :resize +2<CR>

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

nnoremap <S-Left> :bprevious<CR>
nnoremap <S-Right> :bnext<CR>

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

" shortcuts for oppening special files
noremap <leader>ev :vs $MYVIMRC<cr>
noremap <leader>ss :vs ~/.vim/bundle/vim-snippets/snippets/<cr>
noremap <leader>pn :split project_notes.txt<cr>

" map space to iterate buffer windows
nnoremap <space> <c-w>w

" spelling maps
nnoremap <F5> :setlocal nospell<cr>
nnoremap <F6> :setlocal spell spelllang=
nnoremap <F7> ]s
nnoremap <F8> z=

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
"     else
"         return "\<c-p>"
"     endif
" endfunction
" inoremap <expr> <tab> InsertTabWrapper()
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
nnoremap <leader>t :call RunTestFile()<cr>
nnoremap <leader>a :call RunTests(' ')<cr>

function! RunTestFile(...)
    if a:0
        let command_suffix = a:1
    else
        let command_suffix = ""
    endif

    " Are we in a test file?
    let in_test_file = match(expand("%"), '\(_spec.rb\|_test.rb\|.feature\|_test.exs\|_test.js\)$') != -1

    " Run the tests for the previously-marked file (or the current file if
    " it's a test).
    if in_test_file
        call SetTestFile(command_suffix)
    elseif !exists("t:grb_test_file")
        return
    end
    call RunTests(t:grb_test_file)
endfunction

function! SetTestFile(command_suffix)
    " Set the spec file that tests will be run for.
    let t:grb_test_file=@% . a:command_suffix
endfunction

function! RunTests(filename)
    " Write the file and run tests for the given filename
    if expand("%") != ""
      :w
    end

    if filereadable("bin/cucumber") && a:filename =~ '.feature\>\| '
      exec ":!bin/cucumber " . a:filename
    end

    if filereadable("bin/rspec") && a:filename =~ '_spec.rb\| '
      exec ":!bin/rspec --color " . a:filename
    elseif filereadable("Gemfile") && strlen(glob("spec/**/*.rb"))
      exec ":!bundle exec rspec --color " . a:filename
    elseif filereadable("Gemfile") && strlen(glob("test/**/*.rb"))
      exec ":!bin/rails test " . a:filename
    elseif filereadable("mix.exs") && strlen(glob("test/**/*.exs"))
      exec ":!mix test " . a:filename
    elseif strlen(glob("**/*_test.exs"))
      exec ":!elixir *_test.exs"
    elseif filereadable("package.json") && strlen(glob("test/**/*_test.js"))
      exec ":!npm test " . a:filename
    end
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


" Show syntax highlighting groups for word under cursor
nnoremap <leader>h :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

noremap <leader>b :NERDTreeToggle<cr>
noremap <leader>p :CtrlP<cr>

let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']


let run_commands = {
      \'applescript': 'osascript',
      \'bash': 'bash',
      \'javascript': 'node',
      \'elixir': 'elixir',
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

augroup elixir_commands
  autocmd!
  autocmd FileType elixir nnoremap <leader>x <ESC>:w<CR>:!clear<CR>:!iex -S mix<CR>
augroup END


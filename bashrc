set -o vi
# custom prompt
export PS1='[\u@\h \W$(vcprompt -f " (%b%m)" -M "*")]$ '

export EDITOR=vim

# custom PATH
export PATH="$HOME/.elixir-ls/release:$HOME/bin:/usr/local/bin:/usr/local/sbin:$HOME/.cargo/bin:$HOME/.emacs.d/bin:$HOME/.emacses/doom/doom-emacs/bin:$PATH"
# needed for brew grep to replace system grep
export PATH="/usr/local/opt/grep/libexec/gnubin:$PATH"

# asdf version manager
source $(brew --prefix asdf)/libexec/asdf.sh
source /usr/local/etc/bash_completion.d/asdf.bash


# enable iex shell history
export ERL_AFLAGS="-kernel shell_history enabled"
export LC_ALL=en_US.UTF-8

# --files: List files that would be searched but do not search
# --no-ignore: Do not respect .gitignore, etc...
# --hidden: Search hidden files and folders
# --follow: Follow symlinks
# --glob: Additional conditions for search (in this case ignore everything in the .git/ folder)
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow'

#OSX exclusive
if hash osascript 2>/dev/null;then
  function cdf {
  target=`osascript -e 'tell application "Finder" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)'`

  if [ "$target" != "" ]; then
    cd "$target"; pwd
  else
    echo 'No Finder window found' >&2
  fi
}
fi

if [ -f /usr/local/share/bash-completion/bash_completion ]; then
  . /usr/local/share/bash-completion/bash_completion
fi

[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

# jsvu
export PATH="${HOME}/.jsvu:${PATH}"
export PATH="/usr/local/opt/sqlite/bin:$PATH"

# run emacs from the terminal
alias e='emacs -nw'
alias vi=nvim

swap_emacs () {
  if [ -f ~/.spacemacs ]; then
    mv ~/.{,no-}spacemacs
    mv ~/.{,spacemacs-}emacs.d
    mv ~/.{other-,}emacs.d
  else
    mv ~/.{,other-}emacs.d
    mv ~/.{no-,}spacemacs
    mv ~/.{spacemacs-,}emacs.d
  fi
}

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/isp/bin/google-cloud-sdk/path.bash.inc' ]; then . '/Users/isp/bin/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/isp/bin/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/isp/bin/google-cloud-sdk/completion.bash.inc'; fi

alias dokku='bash $HOME/.dokku/contrib/dokku_client.sh --rm'
alias android_emulator='$HOME/Library/Android/sdk/emulator/emulator'

# load direnv to  manage project specific env vars
eval "$(direnv hook bash)"

# to have erlang docs in iex
export KERL_BUILD_DOCS="yes"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

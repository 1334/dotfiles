set -o vi
# custom prompt
export PS1='[\u@\h \W$(vcprompt -f " (%b%m)" -M "*")]$ '

export EDITOR=vim

# asdf version manager
eval "$(/opt/homebrew/bin/brew shellenv)"
source $(brew --prefix asdf)/libexec/asdf.sh

# custom PATH
export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH:$HOME/Library/pnpm:$HOME/.elixir-ls/release:$HOME/bin:/usr/local/bin:/usr/local/sbin:$PATH"
export PATH="$HOME/.cargo/bin:$HOME/.emacs.d/bin:$HOME/.emacses/doom/doom-emacs/bin:$HOME/.local/bin:$PATH"
# needed for brew grep to replace system grep
# for intel chips
export PATH="/usr/local/opt/grep/libexec/gnubin:$PATH"
# for M1 chips
export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"

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

if type brew &>/dev/null
then
  HOMEBREW_PREFIX="$(brew --prefix)"
  if [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]]
  then
    source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
  else
    for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*
    do
      [[ -r "${COMPLETION}" ]] && source "${COMPLETION}"
    done
  fi
fi

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

[ -f ~/.bashrc-secrets ] && source ~/.bashrc-secrets

# hide macos deprectaion warning
export BASH_SILENCE_DEPRECATION_WARNING=1

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/bash/__tabtab.bash ] && . ~/.config/tabtab/bash/__tabtab.bash || true
# pnpm
export PNPM_HOME="/Users/isp/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
# bun
export BUN_INSTALL="$HOME/.bun"
export PATH=$BUN_INSTALL/bin:$PATH

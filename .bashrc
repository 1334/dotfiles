# set -o vi
# custom prompt
export PS1='[\u@\h \W$(vcprompt -f " (%b%m)" -M "*")]$ '

export EDITOR=vim

# custom PATH
export PATH="$HOME/bin:/usr/local/bin:/usr/local/sbin:$PATH"

# enable iex shell history
export ERL_AFLAGS="-kernel shell_history enabled"

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

# asdf version manager
source $HOME/.asdf/asdf.sh
source $HOME/.asdf/completions/asdf.bash

# jsvu
export PATH="${HOME}/.jsvu:${PATH}"
export PATH="/usr/local/opt/sqlite/bin:$PATH"

# run emacs from the terminal
alias e='emacs -nw'

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

cd () {
    builtin cd "$@"
    if [ -f .env ]; then
        source .env
    fi
}

cd .

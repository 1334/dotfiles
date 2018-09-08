set -o vi
# custom prompt
export PS1='[\u@\h \W$(vcprompt -f " (%b%m)" -M "*")]$ '

export EDITOR=vim

# custom PATH
export PATH="$HOME/bin:/usr/local/bin:$PATH"
export PATH="./bin:$PATH"

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

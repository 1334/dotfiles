set -o vi
# custom prompt
export PS1='[\u@\h \W$(vcprompt -f " (%b%m)" -M "*")]$ '

export EDITOR=vim

if hash brew 2>/dev/null; then
  export RBENV_ROOT=/usr/local/var/rbenv

  if [ -f $(brew --prefix)/etc/bash_completion  ]; then
    . $(brew --prefix)/etc/bash_completion
  fi
fi


# custom PATH
export PATH="bin:$HOME/.rbenv/bin:$HOME/bin:/usr/local/bin:$PATH"

eval "$(rbenv init -)"

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

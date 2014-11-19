# custom prompt
export PS1='[\u@\h \W$(vcprompt -f " (%b%m)" -M "*")]$ '

# custom PATH
export PATH="bin:$HOME/.rbenv/bin:$HOME/bin:/usr/local/bin:$PATH"

export EDITOR=vim

if hash brew 2>/dev/null; then
  export RBENV_ROOT=/usr/local/var/rbenv

  if [ -f $(brew --prefix)/etc/bash_completion  ]; then
    . $(brew --prefix)/etc/bash_completion
  fi
fi

eval "$(rbenv init -)"

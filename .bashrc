# custom prompt
export PS1='[\u@\h \W$(vcprompt -f " (%b%m)" -M "*")]$ '

# custom PATH
export PATH="bin:$HOME/.rbenv/bin:/usr/local/bin:~/bin:/usr/local/heroku/bin:$PATH"

export EDITOR=vim

if hash brew 2>/dev/null; then
  eval "$(rbenv init -)"
  export RBENV_ROOT=/usr/local/var/rbenv

  if [ -f $(brew --prefix)/etc/bash_completion  ]; then
    . $(brew --prefix)/etc/bash_completion
  fi
fi

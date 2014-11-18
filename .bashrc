# custom prompt
export PS1='[\u@\h \W$(vcprompt -f " (%b%m)" -M "*")]$ '

# custom PATH
export PATH="bin:$HOME/.rbenv/bin:/usr/local/bin:~/bin:/usr/local/heroku/bin:$PATH"

# To enable rbenv shims and autocompletion
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

export EDITOR=vim

if command -v brew 2>/dev/null; then
  if [ -f $(brew --prefix)/etc/bash_completion  ]; then
    . $(brew --prefix)/etc/bash_completion
  fi
fi

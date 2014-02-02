# custom prompt
export PS1='[\u@\h \W$(vcprompt -f " (%b%m)" -M "*")]$ '

# use homebrew's rbenv
export RBENV_ROOT=/usr/local/var/rbenv

# To enable rbenv shims and autocompletion
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# custom PATH
export PATH="bin:/usr/local/bin:~/bin:/usr/local/heroku/bin:$PATH"

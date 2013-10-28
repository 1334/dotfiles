# custom PATH
export PATH=/usr/local/bin:~/bin:$PATH

# custom prompt
export PS1='[\u@\h \W$(vcprompt -f " (%b%m)" -M "*")]$ '

# use homebrew's rbenv
export RBENV_ROOT=/usr/local/var/rbenv

# To enable rbenv shims and autocompletion
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

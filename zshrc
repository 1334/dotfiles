[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias e='emacs -nw'

alias python=python2

# custom prompt
setopt prompt_subst
# export PS1='[\u@\h \W$(vcprompt -f " (%b%m)")]$'
export PS1='%n:%~$(vcprompt -f " (%b%m)") $ '

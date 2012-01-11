autoload -U compinit promptinit
compinit
promptinit

autoload -U colors && colors

PROMPT="%{$fg[white]%}%n@%{$reset_color%}%{$fg[yellow]%}%1~%{$reset_color%}%#>"
RPROMPT="[%{$fg[yellow]%}%?%{$reset_color%}]"

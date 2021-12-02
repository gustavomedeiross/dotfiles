# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(
    git
    vi-mode
    shrink-path
    asdf
)

DISABLE_UNTRACKED_FILES_DIRTY="true"

source $ZSH/oh-my-zsh.sh

# Green Lambda prompt
# PROMPT="%(?:%{$fg_bold[green]%}λ :%{$fg_bold[red]%}λ )"

# art
PROMPT="%{$fg_bold[red]%}[ %{$fg_bold[yellow]%}%n% %{$fg_bold[green]%}@%{$fg_bold[blue]%}%m%  %{$fg_bold[red]%}]"
# dark
# PROMPT="%{$fg_bold[yellow]%}[ %{$fg_bold[blue]%}%n% %{$fg_bold[yellow]%}@%{$fg_bold[magenta]%}%m%  %{$fg_bold[yellow]%}]"

# path + git
PROMPT+=' %{$fg[cyan]%}% $(shrink_path -f) %{$reset_color%} $(git_prompt_info)'

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}‚ñì‚ñí‚ñë %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})‚Ä¶%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}‚ñì‚ñí‚ñë %F{34}Installation successful.%f%b" || \
        print -P "%F{160}‚ñì‚ñí‚ñë The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-bin-gem-node

zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-completions
zinit light zdharma/fast-syntax-highlighting

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#877C70"

### End of Zinit's installer chunk

# vi mode
bindkey -v
bindkey -M viins 'jk' vi-cmd-mode

# alias for managing my dotfiles with a git bare repository 
alias dotfiles='/usr/bin/git --git-dir=/home/gustavo/.dotfiles/ --work-tree=/home/gustavo'

# kubecfg
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# yarn global
export PATH="$PATH:`yarn global bin`"

# android sdk
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
export ANDROID_HOME=$HOME/Android/Sdk 
export PATH="$PATH:$ANDROID_HOME/emulator"
export PATH="$PATH:$ANDROID_HOME/tools"
export PATH="$PATH:$ANDROID_HOME/platform-tools"

# flutter sdk
export PATH="$PATH:$HOME/flutter/bin"

# gcloud
source <(kubectl completion zsh)

# Voltbras 
source $HOME/.voltbras.sh

# Racket
export PATH="$PATH:/usr/racket/bin"

# OCaml
eval `opam env`
[ -f "/home/gustavo/.ghcup/env" ] && source "/home/gustavo/.ghcup/env" # ghcup-env

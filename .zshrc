source "$HOME/antigen.zsh"

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle heroku
antigen bundle command-not-found
antigen bundle vi-mode
antigen bundle shrink-path
antigen bundle asdf

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-completions

antigen theme robbyrussell

antigen apply

DISABLE_UNTRACKED_FILES_DIRTY="true"
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#877C70"

# Green Lambda prompt
# PROMPT="%(?:%{$fg_bold[green]%}λ :%{$fg_bold[red]%}λ )"

# art
PROMPT="%{$fg_bold[red]%}[ %{$fg_bold[yellow]%}%n% %{$fg_bold[green]%}@%{$fg_bold[blue]%}%m%  %{$fg_bold[red]%}]"
# dark
# PROMPT="%{$fg_bold[yellow]%}[ %{$fg_bold[blue]%}%n% %{$fg_bold[yellow]%}@%{$fg_bold[magenta]%}%m%  %{$fg_bold[yellow]%}]"

# path + git
PROMPT+=' %{$fg[cyan]%}% $(shrink_path -f) %{$reset_color%} $(git_prompt_info)'

# vi mode
bindkey -v
bindkey -M viins 'jk' vi-cmd-mode

# alias for managing my dotfiles with a git bare repository 
alias dotfiles="/usr/bin/git --git-dir=/$HOME/.dotfiles/ --work-tree=/$HOME"

# Add local scripts to PATH
export PATH="$PATH:$HOME/.local/bin"

# Load computer specific configurations
source "$HOME/.local/zsh.sh"

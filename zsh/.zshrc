# -------------- EXPORTS ---------------------
export ZSH="/home/ilgaz/.oh-my-zsh"
export UPDATE_ZSH_DAYS=3
export LANG=en_US.UTF-8
export PATH=$HOME/.npm-global/bin:$PATH
# ----------- EXPORTS DONE ------------------
#############################################
# -------------- SETTINGS -------------------
ZSH_THEME="half-life"
HYPHEN_INSENSITIVE="true"
COMPLETION_WAITING_DOTS="true"
plugins=(
  zsh-autosuggestions
  git
  docker
  docker-compose
  tmux
  cabal
  cargo
  aws
  vi-mode
)
# ----------- SETTINGS DONE ----------------
source $ZSH/oh-my-zsh.sh
#############################################
# -------------- ALIASES -------------------
alias c=clear
alias l='exa --color=always'
alias ls='exa --color=always'
alias ll="exa -lah --color=always | sed -re 's/^[^ ]* //'"
alias v='nvim'
alias x=exit
alias q=exit
alias cl="clear; exa --color=always"
alias cll="clear; exa -lah --color=always | sed -re 's/^[^ ]* //'"
alias ..="cd .."
alias ...="cd ../../"
alias ....="cd ../../../"
###

# Apps
alias t=tmux
alias ta='tmux a'
####

# NPM
alias ni="npm install --save"
alias ng="npm install -g"
alias ns="npm start"
alias nt="npm test"
alias nd="npm run dev"
####


# Docker & Compose
alias dcu='docker-compose up'
alias dcd='docker-compose down'
####

# misc utils
# Typo covers
alias claer=clear
alias clare=clear
alias clera=clear
alias celar=clear
alias elar=clear
alias sl=exa
alias ls=exa
alias exıt=exit
alias nvım=nvim

# ------------- ALIASES DONE -----------------
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
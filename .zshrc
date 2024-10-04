# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

source ~/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet

# Set up fzf key bindings and fuzzy completion

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)

# Aliases

# Terminal
alias c="clear"
alias x="exit"
alias e="code -n ~/ ~/.zshrc ~/.aliases ~/.colors ~/.hooks"
alias r="source ~/.zshrc"

# APT
alias ai="sudo apt install"
alias au="sudo apt update"
alias aug="sudo apt upgrade"
alias alu="sudo apt list --upgradable"
alias adu="sudo apt dist-upgrade"

# DNF
alias ai="sudo dnf install"
alias au="sudo dnf update"

# Vim mode
bindkey -v

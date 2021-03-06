# Path to your oh-my-zsh installation.
export ZSH=/home/adam/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="pygmalion"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git lein cabal screen colorize tmux)

source $ZSH/oh-my-zsh.sh

# User configuration
NPM_PACKAGES="${HOME}/.npm-packages"

if [ $HOME = "/Users/adam" ] ; then
    export PATH="/Users/adam/Library/Python/2.7/bin/:/Users/adam/.local/bin/:/opt/local/bin:/opt/local/sbin:$PATH"
else
    export PATH=$NPM_PACKAGES/bin:/home/adam/.local/bin:/home/adam/bin:/usr/local/texlive/2015/bin/x86_64-linux:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/runtime/glnxa64:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/bin/glnxa64:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/sys/os/glnxa64:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/sys/java/jre/glnxa64/jre/lib/amd64/native_threads:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/sys/java/jre/glnxa64/jre/lib/amd64/server:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/sys/java/jre/glnxa64/jre/lib/amd64:$HOME/.cabal/bin:/usr/local/bin:/home/adam/Science/LINUX64:/opt/maple18/bin:$PATH:/usr/local/cuda-7.5/bin
fi
export XAPPLRESDIR=/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/X11/app-defaults
export LD_LIBRARY_PATH=/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/runtime/glnxa64:/usr/local/MATLAB/MATLAB_Compiler_Runtime/v82/bin/glnxa64/:/usr/local/cuda-7.5/lib64

# Unset manpath so we can inherit from /etc/manpath via the `manpath` command
unset MANPATH # delete if you already modified MANPATH elsewhere in your config
export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR=emacsclient

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias ghc-core="ghc -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes"

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

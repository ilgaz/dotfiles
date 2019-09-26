# utils
abbr -a c clear
abbr -a l 'exa'
abbr -a ls 'exa'
abbr -a ll "exa -lah --color=always | sed -re 's/^[^ ]* //'"
abbr -a ll_ss "exa -lah --color=always --sort=size | sed -re 's/^[^ ]* //'"
abbr -a lld "exa -lah --color=always -D | sed -re 's/^[^ ]* //'"
abbr -a cat 'bat'
abbr -a v 'vim'
abbr -a x exit
abbr -a q exit

# apps
abbr -a co code-insiders
abbr -a cox code-insiders .
abbr -a vim nvim
abbr -a w wtf #symlink to wtfutils

# misc utils 
abbr -a cl "clear; exa"
abbr -a cll "clear; exa -lah --color=always | sed -re 's/^[^ ]* //'"
abbr -a cll_ss "clear; exa -lah --color=always --sort=size | sed -re 's/^[^ ]* //'"
abbr -a clld "clear; exa -lah --color=always -D | sed -re 's/^[^ ]* //'"

# VPN
abbr -a vpn expressvpn connect
abbr -a vpnd expressvpn disconnect
abbr -a vpnus "expressvpn connect 'united states'"
abbr -a vpnde "expressvpn connect 'germany'"
abbr -a vpnac=t expressvpn autoconnect true 
abbr -a vpnac=f expressvpn autoconnect false
abbr -a vpns expressvpn status

# cargo 
abbr -a cn cargo new
abbr -a cr cargo run
abbr -a cb cargo build

# NPM

abbr -a ni "npm install --save"
abbr -a ng "npm install -g"
abbr -a ns "npm start"
abbr -a nt "npm test"

# Aliases to cover situations when I act like a meme

abbr -a claer clear
abbr -a clare clear
abbr -a clera clear
abbr -a celar clear
abbr -a elar clear
abbr -a sl exa

# eval

eval (starship init fish)

#PATH

set PATH ~/.cargo/bin $PATH
set PATH /usr/bin $PATH
set PATH ~/.npm-global/bin $PATH

# Functions

function fish_greeting
	echo "Welcome, master"
end


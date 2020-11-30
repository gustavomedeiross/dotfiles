# dotfiles

Git bare repository:

```sh
git init --bare $HOME/.dotfiles

# only required if the alias doesn't exist yet
echo "alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'" >> $HOME/.zshrc
source ~/.zshrc

dotfiles config --local status.showUntrackedFiles no
```


```sh
sudo pacman -S xorg xorg-xinit vim tmux nitrogen picom 
```

basic stuff: 
- xorg
- xorg-xinit (I need to make the `~/.xinitrc` file)
- lightdm & lightdm-gtk-greeter (alternative to xinit)
- nitrogen (maybe xwallpaper)
- picom
- yay
- I thing alsamixer comes by default with arch, but I'm not sure
- pulseaudio

ui stuff:
- xmonad (`sudo pacman -S xmonad xmonad-contrib`) - wm
- xmobar (`sudo pacman -S xmobar`) - status bar
- dmenu (`sudo pacman -S dmenu`) - menu/program launcher
- dunst (`sudo pacman -S dunst`) - notification manager
- scrot (create xmonad bindings for that)
- pavucontrol (this thing is gold)
- i will need some wi-fi & bluetooth manager

- maybe I can install gnome just in case, I don't know :P
- I need to add those fancy scripts of xmonad

terminal stuff:
- termite
- xterm 
- vim & vim-plug
- tmux
- zsh & oh-my-zsh & zinit
- nerd-fonts
- rsync

personal stuff
- brave (`yay -S brave-bin`)
- spotify (`yay -S spotify`) - there's an issue with a gpg key, so check the aur docs for more information
- slack
- discord
- syncthing (I need to start using this thing, looks really useful)
- wallpapers

development/programming stuff:
- docker
- asdf (nodejs/elixir/erlang/golang/...) - tbh I don't know if I should use go with asdf or stand-alone
- yarn
- vscode
- psql (`sudo pacman -S postgres`)
- maybe datagrip or some other tool like that
- java/jdk1.8 (openjdk) `sudo pacman -S jdk8-openjdk`

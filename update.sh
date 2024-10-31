#!/bin/sh
cp /home/sean/.config/Xresources .
cp /home/sean/.config/Xdefaults  .
cp -r /home/sean/.config/alacritty ./config/
cp /home/sean/.mkshrc .
mv .mkshrc mkshrc
cp /home/sean/.config/kshrc .
cp /etc/profile .
cp /etc/X11/xinit/xinitrc .
cp /usr/bin/sx .
cp -r /home/sean/.config/gtk-3.0 .
cp -r /home/sean/.config/gtk-2.0 .
cp /home/sean/.config/nvim/init.vim ./config
cp -r /home/sean/.config/dunst ./config
cp /home/sean/.config/neofetch/config.conf ./config
mv ./config/config.conf ./config/neofetch.conf
cp /home/sean/.config/btop/btop.conf ./config
cp /home/sean/.config/kitty/kitty.conf ./config
cp /home/sean/.config/rfetch.txt ./config
cp /home/sean/.config/sean/walls/* ./sean/walls
cp /usr/src/linux/.config .
mv .config kernel.config
cp /home/sean/.config/picom.conf ./config/
cp /home/sean/.config/rofi/config.rasi ./config/rofi/
cp /home/sean/.doom.d/init.el ./doom.d/
cp /home/sean/.config/xmonad/xmonad.hs .
cp /home/sean/.config/xmonad/xmobarrc .
cp /home/sean/.doom.d/config.el ./doom.d/
cp /home/sean/.doom.d/packages.el ./doom.d/
cp /home/sean/.config/nvim/init.lua ./config/nvim/
cp /home/sean/.config/BetterDiscord/themes/* ./BetterDiscord/themes
cp /home/sean/.config/.zshrc zshrc
cp -r /home/sean/.oh-my-zsh/themes/ .
cp -r /home/sean/.config/tmux ./config
cp /home/sean/.config/alias_all .
cp /home/sean/.emacs.d/init.el ./emacs
mv themes zsh_themes
touch ./BetterDiscord/plugins-list.txt
ls -l /home/sean/.config/BetterDiscord/plugins/ | grep plugin.js > ./BetterDiscord/plugins-list.txt
sed -E '/wakatime-api-key/d' emacs/init.el
notify-send "done"

#!/bin/mksh
cp /home/sean/.config/Xresources .
cp /home/sean/.config/Xdefaults  .
cp /home/sean/.mkshrc .
mv .mkshrc mkshrc
cp /home/sean/.config/kshrc .
cp /etc/profile .
cp /etc/X11/xinit/xinitrc .
cp /usr/local/bin/sx .
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
cp /home/sean/.doom.d/init.el ./doom.d/
cp /home/sean/.doom.d/config.el ./doom.d/
cp /home/sean/.doom.d/packages.el ./doom.d/
cp /home/sean/.config/BetterDiscord/themes/* ./BetterDiscord/themes
cp /home/sean/.config/.zshrc zshrc
cp -r /home/sean/.oh-my-zsh/themes/ .
mv themes zsh_themes
touch ./BetterDiscord/plugins-list.txt
ls -l /home/sean/.config/BetterDiscord/plugins/ | grep plugin.js > ./BetterDiscord/plugins-list.txt
notify-send "done"

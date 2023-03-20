#!/usr/bin/env bash

sudo sed -i '/^"cedilla"/s/:pt:/:pt:en:/' /usr/lib/gtk-*/**/immodules.cache

sudo cp /usr/share/X11/locale/en_US.UTF-8/Compose /usr/share/X11/locale/en_US.UTF-8/Compose.bak
sudo sed -i 's/ć/ç/g' /usr/share/X11/locale/en_US.UTF-8/Compose
sudo sed -i 's/Ć/Ç/g' /usr/share/X11/locale/en_US.UTF-8/Compose

grep -q GTK_IM_MODULE /etc/environment || (echo "GTK_IM_MODULE=cedilla" | sudo tee -a /etc/environment)
grep -q QT_IM_MODULE /etc/environment || (echo "QT_IM_MODULE=cedilla" | sudo tee -a /etc/environment)

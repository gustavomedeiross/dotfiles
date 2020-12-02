#!/bin/bash

setxkbmap -query | grep -q 'variant:\s\+intl' && setxkbmap -layout us || setxkbmap -layout us -variant intl

#!/bin/bash

#put your Accuweather address here
address="http://www.accuweather.com/en/gb/sheffield/s1-2/weather-forecast/326914"
 
loc_id=$(echo $address|sed 's/\/weather-forecast.*$//'|sed 's/^.*\///')
last_number=$(echo $address|sed 's/^.*\///')

curr_addr="$(echo $address|sed 's/weather-forecast.*$//')"current-weather/"$last_number"
wget -O $HOME/.xmonad/curr_cond_raw "$curr_addr"
 
addr1="$(echo $address|sed 's/weather-forecast.*$//')"daily-weather-forecast/"$last_number"
wget -O $HOME/.xmonad/tod_ton_raw "$addr1"

#current conditions
if [[ -s $HOME/.xmonad/curr_cond_raw ]]; then
 
    sed -i '/detail-now/,/#details/!d' $HOME/.xmonad/curr_cond_raw
    egrep -i '"cond"|icon i-|detail-tab-panel' $HOME/.xmonad/curr_cond_raw > $HOME/.xmonad/curr_cond
    sed -i -e 's/^.*detail-tab-panel //g' -e 's/^.*icon i-//g' -e 's/"><\/div>.*$//g' $HOME/.xmonad/curr_cond
    sed -i -e 's/^.*"cond">//g' -e 's/&deg/\n/g' -e 's/<\/span>.*"temp">/\n/g' -e 's/<.*>//g' $HOME/.xmonad/curr_cond
    sed -i -e 's/">//g' -e 's/-->//g' -e 's/\r$//g' -e 's/ i-alarm.*$//g' $HOME/.xmonad/curr_cond
 
fi


#First 5 days
if [[ -s $HOME/.xmonad/tod_ton_raw ]]; then
 
    sed -i '/feed-tabs/,/\.feed-tabs/!d' $HOME/.xmonad/tod_ton_raw
    egrep -i 'Early AM|Today|Tonight|Overnight|icon i-|cond|temp|Mon|Tue|Wed|Thu|Fri|Sat|Sun' $HOME/.xmonad/tod_ton_raw > $HOME/.xmonad/tod_ton
    sed -i -e 's/^.*#">//g' -e 's/^.*icon i-//g' -e 's/^.*cond">//g' -e 's/^.*temp">//g' $HOME/.xmonad/tod_ton
    sed -i -e 's/Lo<\/span> /\n/g' -e 's/<\/a>.*$//g' -e 's/ "><.*$//g' -e 's/&#.*$//g' -e 's/teo//g' $HOME/.xmonad/tod_ton
    sed -i -e 's/<span>.*$//g' -e 's/<\/span>//g' -e 's/\r$//g' -e 's/ i-alarm.*$//g' $HOME/.xmonad/tod_ton
        sed -i -e 's/Early AM/EARLY AM/' -e 's/Today/TODAY/' -e 's/Tonight/TONIGHT/' -e 's/Overnight/OVERNIGHT/' -e 's/Mon/MONDAY/' -e 's/Tue/TUESDAY/' -e 's/Wed/WEDNESDAY/' -e 's/Thu/THURSDAY/' -e 's/Fri/FRIDAY/' -e 's/Sat/SATURDAY/' -e 's/Sun/SUNDAY/' $HOME/.xmonad/tod_ton
 
fi

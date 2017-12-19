#!/bin/bash


notmuch config set query.sasview "from:sasview.org or to:sasview.org or (from:github subject:sasview) or (from:github subject:sasmodels) or (from:sourceforge subject:sasview) or (to:sourceforge subject:sasview)"
notmuch config set query.mantid from:mantidproject.org

notmuch config set query.geniescans "subject:Scans (from:travis-ci.org or from:codacy or from github)"

notmuch config set query.promotional "to:promotional or from:rifftrax or from:humblebundle or from:woot or from:giffgaff or from:drivethrurpg or from:amazon or from:thinkgeek or from:wish.com or from:surlatable or from:linkedin or from:nationalgeographic.com or from:magairports.net or from:game.co.uk or from:news-googleplay or from:krispykreme or from:outback or from:mylifestyle or from:myice.com or from:physicstoday or from:redroof or from:8020inc or from:virginmoney or from:nationalparks or from:failbettergames or from:zagat or from:tesco or from:glassdoor or from:steam or from:gog.com or from:kiva or from:Healthcare.gov or from:evga.com or from:paypal or from:justeat or from:subwaysubcard or from:bertin or from:discover or from:cases.com or from:umrelief or from:iucu.org or from:santander or from:dogstrust or from:consciouscityguide or from:honda or from:transferwise or from:brickyard.com or from:topcoder or from:CityPASS or from:Mint or from:sapphiretech or from:Duolingo or from:IFTTT"

notmuch config set query.paperspam "from:ResearchGate or from:Academia or from:scholarcitations or from:mendeley or from:royalsociety or from:aip-info.org"

notmuch config set query.important "tag:unread not (query:promotional or query:sasview or query:mantid or query:paperspam)"

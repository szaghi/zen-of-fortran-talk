#!/bin/bash

MaTiSSe.py -i talk.md -hs solarized_dark.css --toc-at-subsec-beginning 3
rm -rf ../publish/*
mv talk/* ../publish/

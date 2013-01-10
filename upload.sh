#!/bin/bash
rsync --delete --recursive --progress _site/ logfish.net:/home/bram/vhost/bneijt.nl/_/

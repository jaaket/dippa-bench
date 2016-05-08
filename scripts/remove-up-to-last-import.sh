#!/bin/bash

gtac | grep -m 1 -B 9999 import | gtac | tail -n +2 | sed '/./,$!d'


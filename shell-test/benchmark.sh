#!/bin/bash

time {
for i in {1..5}
do
    time cl -e "(print $i)"
done
}

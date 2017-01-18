#!/bin/bash

for file in orbit_9b_12g_20_01.data \
orbit_9b_12g_23_01.data \
orbit_9b_12g_30_01.data \
  orbit_9b_12g_31_01.data \
  orbit_9b_12g_32_01.data \
  orbit_9b_12g_33_01.data \
  orbit_9b_12g_34_01.data \
  orbit_9b_12g_35_01.data \
  orbit_9b_12g_38_01.data \
  orbit_9b_12g_49_01.data \
    orbit_9b_12g_50_01.data \
    orbit_9b_12g_51_01.data \
    orbit_9b_12g_52_01.data \
    orbit_9b_12g_53_01.data \
    orbit_9b_12g_54_01.data \
    orbit_9b_12g_56_01.data \
    orbit_9b_12g_59_01.data \
    orbit_9b_12g_60_01.data \
    orbit_9b_12g_61_01.data \
    orbit_9b_12g_69_01.data \
    orbit_9b_12g_75_01.data \
    orbit_9b_12g_76_01.data \
    orbit_9b_12g_77_01.data \
    orbit_9b_12g_78_01.data
do
cp n9d2/$( basename $file _01.data ).info todo
done




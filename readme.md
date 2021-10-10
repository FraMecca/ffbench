# FFBENCH
This program allows to test different ffmpeg transcoding settings to evaluate the best quality and ratio that can be obtained.

## Requirements
- ffmpeg
- mediainfo
- orgparse: https://github.com/karlicoss/orgparse (you can avoid installation: just clone the repo in this folder)
- ocaml and python3.8+ to build and run the program

## How it works
ffbench divides the input file into x samples of n seconds. It concatenates those samples into a single file (the _source_) and tests different transcoding settings, producing many output files. Those output files are rated using three metrics:
- size
- ssim: https://en.wikipedia.org/wiki/Structural_similarity 
- psnr: https://en.wikipedia.org/wiki/Peak_signal-to-noise_ratio

The program uses an org mode file as its input and output: see [example.org](./example.org).

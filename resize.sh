#!/bin/env bash
set -e

target_size="$1"

if ! [[ "$target_size" =~ ^[0-9]+$ ]]; then
  echo >&2 "Please call with target size, e.g. $0 750"
  exit 1
fi

function resize {
  mogrify -resize ${target_size}x${target_size} "$1"
}

img_c="$(ls img | wc -l)"
imgs=(img/*)

cpu_c=8
for (( thread_i = 0; thread_i < cpu_c; thread_i++ )); do
  (
    for (( img_i=thread_i; img_i < img_c; img_i += cpu_c )); do
      img="${imgs[img_i]}"
      resize "$img"
      echo "done (t$thread_i): $img_i/$img_c ($img)"
    done
  ) &
done
wait

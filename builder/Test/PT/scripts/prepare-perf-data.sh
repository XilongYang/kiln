#!/bin/sh
set -eu

CACHE_ROOT=".cache/PT"
NORMAL_DATASET="$CACHE_ROOT/10000-normal-10k-v1"
HUGE_DATASET="$CACHE_ROOT/5-huge-25m-v1"

mkdir -p "$CACHE_ROOT"

build_dataset() {
  dataset_dir="$1"
  post_count="$2"
  target_bytes="$3"
  body_line="$4"

  src_dir="$dataset_dir/src"
  ready_mark="$dataset_dir/.ready"

  if [ -f "$ready_mark" ]; then
    echo "[perf-data] reuse $dataset_dir"
    return
  fi

  echo "[perf-data] build $dataset_dir"
  rm -rf "$dataset_dir"
  mkdir -p "$src_dir"

  i=1
  while [ "$i" -le "$post_count" ]; do
    name="$(printf "post-%05d.md" "$i")"
    path="$src_dir/$name"

    header="$(cat <<EOF
---
title: Perf Post $i
author: Perf UT
date: 2026-03-28
---

This is abstract content.

## Section A

EOF
)"

    header_bytes="$(printf "%s" "$header" | wc -c | tr -d ' ')"
    remain=$((target_bytes - header_bytes))
    if [ "$remain" -lt 0 ]; then
      remain=0
    fi

    printf "%s" "$header" > "$path"
    yes "$body_line" | head -c "$remain" >> "$path"

    i=$((i + 1))
  done

  printf "ok\n" > "$ready_mark"
}

build_dataset "$NORMAL_DATASET" 10000 $((10 * 1024)) "normal-body-line-abcdefghijklmnopqrstuvwxyz0123456789"
build_dataset "$HUGE_DATASET" 5 $((25 * 1024 * 1024)) "huge-body-line-abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

echo "[perf-data] done"
